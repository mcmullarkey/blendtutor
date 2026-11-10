//! The effectful feedback request: drive rig's `Extractor` and map its DTO to a
//! domain verdict.
//!
//! Owns the domain [`Verdict`], the boundary DTO it maps from, the typed
//! [`FeedbackError`], and [`request_feedback`] — the only effectful function in the
//! LLM layer (§2.2). The pure prompt it sends is built by
//! [`prompt`](super::prompt). Callers depend on [`request_feedback`] + [`Verdict`],
//! never on rig types (§3.4); rig stays an implementation detail here.

use std::fmt;

use rig_core::prelude::CompletionClient;
use rig_core::providers::{anthropic, openai};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use super::grader::Grader;
use super::prompt::Prompt;
use super::provider::ProviderChoice;

/// A graded verdict on a submission: correct or incorrect, each carrying the
/// learner-facing message.
///
/// A sum type, not a `bool` plus a message, so "correct with no message" and
/// contradictory states are unrepresentable and there is no public `is_correct`
/// (§1.2). This is what callers consume; the rig DTO (`Feedback`) stays an
/// implementation detail of this module.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Verdict {
    /// The submission satisfies the exercise.
    Correct {
        /// The learner-facing feedback message.
        message: String,
    },
    /// The submission does not satisfy the exercise.
    Incorrect {
        /// The learner-facing feedback message.
        message: String,
    },
}

/// The structured feedback the model returns through its tool call — the boundary
/// DTO rig's `Extractor` fills from the tool-call arguments (ADR-0006).
///
/// `is_correct` is a non-`Option` `bool` and the struct derives **no** `Default`,
/// so a tool call that omits a field is a deserialize error, never a
/// silently-defaulted grade. It is mapped into [`Verdict`] at the boundary so the
/// bool's meaning lives in the variant; callers never see this type.
#[derive(Debug, Deserialize, Serialize, JsonSchema)]
struct Feedback {
    /// Whether the submission satisfies the exercise.
    is_correct: bool,
    /// The feedback message for the learner.
    feedback_message: String,
}

impl From<Feedback> for Verdict {
    fn from(feedback: Feedback) -> Self {
        let Feedback {
            is_correct,
            feedback_message,
        } = feedback;
        if is_correct {
            Verdict::Correct {
                message: feedback_message,
            }
        } else {
            Verdict::Incorrect {
                message: feedback_message,
            }
        }
    }
}

/// Why feedback could not be produced.
///
/// A typed error implementing [`std::error::Error`], so `core` stays free of
/// `anyhow` (ADR-0001); the CLI maps it at the edge.
#[derive(Debug)]
pub enum FeedbackError {
    /// The active provider's API key environment variable is unset or empty.
    MissingApiKey {
        /// The env var that must be set (e.g. `FIREWORKS_API_KEY`).
        var: &'static str,
    },
    /// The provider client could not be constructed.
    Client(String),
    /// The completion request itself failed — a transport, auth, or model error
    /// (e.g. the provider rejected the key, or the network was unreachable). The
    /// caller's recourse is to retry or fix credentials, not to re-prompt.
    Completion(String),
    /// The model responded, but its output could not be turned into a verdict — a
    /// malformed or missing field, or no tool call at all. The model's output, not
    /// the transport, is at fault.
    Extraction(String),
}

impl fmt::Display for FeedbackError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FeedbackError::MissingApiKey { var } => {
                write!(f, "{var} is not set — set your {var} to use this provider")
            }
            FeedbackError::Client(msg) => write!(f, "could not build the provider client: {msg}"),
            FeedbackError::Completion(msg) => write!(f, "the feedback request failed: {msg}"),
            FeedbackError::Extraction(msg) => {
                write!(
                    f,
                    "could not extract a verdict from the model response: {msg}"
                )
            }
        }
    }
}

impl std::error::Error for FeedbackError {}

impl From<rig_core::extractor::ExtractionError> for FeedbackError {
    /// Preserve rig's own distinction between a failed request and an
    /// unparseable response (§1.2): a `CompletionError` is a transport/auth/model
    /// failure, while a deserialize failure or missing tool call means the
    /// response could not be turned into a verdict.
    ///
    /// The match is deliberately exhaustive with no wildcard arm: if a future rig
    /// version adds an `ExtractionError` variant, this fails to compile until we
    /// decide which `FeedbackError` it maps to, rather than silently funnelling it
    /// into the wrong kind.
    fn from(error: rig_core::extractor::ExtractionError) -> Self {
        use rig_core::extractor::ExtractionError;
        match error {
            ExtractionError::CompletionError(e) => FeedbackError::Completion(e.to_string()),
            ExtractionError::DeserializationError(e) => FeedbackError::Extraction(e.to_string()),
            ExtractionError::NoData => {
                FeedbackError::Extraction("the model returned no structured feedback".to_string())
            }
        }
    }
}

/// Request structured feedback for a `prompt` from the chosen provider.
///
/// The effectful counterpart to [`build_prompt`](super::prompt::build_prompt)
/// (§2.2): it reads the active provider's API key, builds the rig client, runs an
/// `Extractor<Feedback>` over the prompt, and maps the extracted DTO into a
/// [`Verdict`]. Fireworks is reached through rig's OpenAI **chat-completions**
/// client (Fireworks is OpenAI-compatible); Anthropic through its native client.
/// `base_url_override` points the client at a mock server in tests — the test seam
/// (ADR-0006); production passes `None`. Callers depend only on this function and
/// [`Verdict`], never on rig types.
pub async fn request_feedback(
    grader: &Grader,
    prompt: &Prompt,
    base_url_override: Option<&str>,
) -> Result<Verdict, FeedbackError> {
    extract::<Feedback>(grader, prompt.as_str(), base_url_override)
        .await
        .map(Verdict::from)
}

/// Ask `grader` for structured output of type `T` (a tool-call DTO) for `prompt`.
///
/// The single place a provider client is built and rig's `Extractor` is driven,
/// shared by feedback and misconception suggestions (ADR-0032), so key guards,
/// base-URL overrides, and error mapping cannot drift between them.
pub(super) async fn extract<T>(
    grader: &Grader,
    prompt: &str,
    base_url_override: Option<&str>,
) -> Result<T, FeedbackError>
where
    T: JsonSchema + for<'de> Deserialize<'de> + Serialize + Send + Sync + 'static,
{
    // A judged eval makes hundreds of calls, and one transient provider error
    // used to abort the whole run. Retry 5xx and 429 a bounded number of times
    // with exponential backoff; every other error returns at once.
    let mut attempt = 0;
    loop {
        match extract_once::<T>(grader, prompt, base_url_override).await {
            Err(FeedbackError::Completion(msg)) if is_transient(&msg) && attempt < MAX_RETRIES => {
                tokio::time::sleep(RETRY_BASE_DELAY * 2u32.pow(attempt)).await;
                attempt += 1;
            }
            result => return result,
        }
    }
}

/// Retries after the first attempt on a transient provider error.
const MAX_RETRIES: u32 = 3;

/// The first backoff delay; each retry doubles it.
const RETRY_BASE_DELAY: std::time::Duration = std::time::Duration::from_millis(250);

/// Whether a completion error is a transient provider failure worth retrying: a
/// 5xx server error or a 429 rate limit. Auth and other 4xx errors are not.
fn is_transient(message: &str) -> bool {
    let Some(rest) = message.split("status code ").nth(1) else {
        return false;
    };
    let code: String = rest.chars().take_while(char::is_ascii_digit).collect();
    matches!(code.parse::<u16>(), Ok(429) | Ok(500..=599))
}

/// One provider call, with no retry.
async fn extract_once<T>(
    grader: &Grader,
    prompt: &str,
    base_url_override: Option<&str>,
) -> Result<T, FeedbackError>
where
    T: JsonSchema + for<'de> Deserialize<'de> + Serialize + Send + Sync + 'static,
{
    // The boundary guard fires first (§1.3.1): a missing key is a clear typed
    // error naming the var, before any rig client is built or any socket opened.
    let provider = grader.provider();
    let key = require_api_key(provider)?;
    let base_url = base_url_override.unwrap_or(provider.default_base_url());
    let model = grader.model().as_str();

    let extracted = match provider {
        ProviderChoice::Fireworks => {
            let client = openai::Client::builder()
                .api_key(key)
                .base_url(base_url)
                .build()
                .map_err(|e| FeedbackError::Client(e.to_string()))?;
            client
                .completions_api()
                .extractor::<T>(model)
                .build()
                .extract(prompt)
                .await
        }
        ProviderChoice::Anthropic => {
            // TODO(anthropic-happy-path): the Anthropic success path has no
            // wiremock coverage. See docs/agent-notes/llm.md.
            let client = anthropic::Client::builder()
                .api_key(key)
                .base_url(base_url)
                .build()
                .map_err(|e| FeedbackError::Client(e.to_string()))?;
            client.extractor::<T>(model).build().extract(prompt).await
        }
    };
    extracted.map_err(FeedbackError::from)
}

/// Read the active provider's API key, treating unset **or empty** as missing.
///
/// The boundary guard for [`request_feedback`] (§1.3.1): it runs before any rig
/// client is built or socket opened, so a missing key surfaces as a clear typed
/// error naming the variable rather than a downstream auth failure or connection
/// error. An empty string is treated as unset — some shells export an empty value.
fn require_api_key(provider: ProviderChoice) -> Result<String, FeedbackError> {
    let var = provider.key_var();
    match std::env::var(var) {
        Ok(key) if !key.is_empty() => Ok(key),
        _ => Err(FeedbackError::MissingApiKey { var }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn feedback_maps_to_the_matching_verdict_variant() {
        // The bool's meaning lives in the variant: is_correct true → Correct,
        // false → Incorrect, and the message passes through unchanged.
        let correct: Verdict = Feedback {
            is_correct: true,
            feedback_message: "well done".to_string(),
        }
        .into();
        assert_eq!(
            correct,
            Verdict::Correct {
                message: "well done".to_string()
            }
        );

        let incorrect: Verdict = Feedback {
            is_correct: false,
            feedback_message: "try again".to_string(),
        }
        .into();
        assert_eq!(
            incorrect,
            Verdict::Incorrect {
                message: "try again".to_string()
            }
        );
    }

    #[test]
    fn extraction_failures_map_to_the_extraction_kind() {
        use rig_core::extractor::ExtractionError;

        // No tool call at all → an Extraction error: the response, not transport,
        // failed to yield a verdict.
        let no_data: FeedbackError = ExtractionError::NoData.into();
        assert!(
            matches!(no_data, FeedbackError::Extraction(_)),
            "NoData is an extraction failure, got {no_data:?}"
        );

        // A malformed/missing field → an Extraction error, kept distinct from a
        // transport-level Completion failure (§1.2).
        let deser_err = serde_json::from_str::<i32>("not-an-int").unwrap_err();
        let bad_field: FeedbackError = ExtractionError::DeserializationError(deser_err).into();
        assert!(
            matches!(bad_field, FeedbackError::Extraction(_)),
            "a deserialize failure is an extraction failure, got {bad_field:?}"
        );
    }

    #[test]
    fn completion_errors_map_to_the_completion_kind() {
        use rig_core::completion::CompletionError;
        use rig_core::extractor::ExtractionError;

        // A provider/transport-level failure (e.g. a 401) maps to Completion, kept
        // distinct from a malformed-response Extraction failure (§1.2) so the
        // caller can tell "fix the key / retry" from "the model's output was bad".
        let provider_err = ExtractionError::CompletionError(CompletionError::ProviderError(
            "401 unauthorized".to_string(),
        ));
        let mapped: FeedbackError = provider_err.into();
        assert!(
            matches!(mapped, FeedbackError::Completion(_)),
            "a completion failure maps to Completion, got {mapped:?}"
        );
    }
}

#[cfg(test)]
mod retry_tests {
    use super::is_transient;

    #[test]
    fn server_errors_and_rate_limits_are_transient_and_nothing_else_is() {
        for transient in [
            "HttpError: Invalid status code 500 Internal Server Error with message: x",
            "HttpError: Invalid status code 503 Service Unavailable",
            "HttpError: Invalid status code 429 Too Many Requests",
        ] {
            assert!(is_transient(transient), "{transient}");
        }
        for permanent in [
            "HttpError: Invalid status code 401 Unauthorized",
            "HttpError: Invalid status code 404 Not Found",
            "connection refused",
        ] {
            assert!(!is_transient(permanent), "{permanent}");
        }
    }
}
