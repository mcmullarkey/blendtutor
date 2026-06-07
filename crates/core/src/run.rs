//! The student run loop: execute a submission, grade it, ask the LLM for a
//! verdict, and bundle the result into a typed [`RunReport`].
//!
//! This is the effect-shell that composes the [`runner`](crate::runner),
//! [`grade`](crate::grade), and [`llm`](crate::llm) layers through their public
//! types only (§2.4, §3.3): it executes the submission for its captured output,
//! grades the lesson's checks, builds the prompt, and requests a verdict. It adds
//! no domain logic of its own — it neither builds prompts nor talks to a provider
//! directly (those are the `llm` layer's), and it does not render output or map
//! exit codes (those belong to the `cli` edge). The result is a [`RunReport`], a
//! typed value carrying the [`Verdict`], the per-check outcomes, and the captured
//! output, with a stable JSON form for `--format json`.

use std::error::Error;
use std::fmt;

use serde::{Deserialize, Serialize};

use crate::grade::{CheckOutcome, run_checks, select_runner};
use crate::lesson::Lesson;
use crate::llm::{
    ExecResults, FeedbackError, ProviderChoice, Submission, Verdict, build_prompt, request_feedback,
};
use crate::runner::Runner;

/// The result of running one submission against a lesson: the graded verdict, the
/// per-check outcomes, and the submission's captured output.
///
/// A typed value (§1.1) the `run` command computes and then renders. The verdict
/// is the [`Verdict`] sum type — carrying its learner-facing message — so a
/// "correct with no feedback" or contradictory state is unrepresentable; the
/// JSON consumer's flat `{verdict, feedback}` pair is a serialization detail, not
/// a second source of truth. Its JSON form is defined once here (via the private
/// [`RunDocument`]), so `core` owns the report's canonical wire shape the way it
/// owns the lesson's; the cli only chooses human-vs-json and the output stream.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(into = "RunDocument", from = "RunDocument")]
pub struct RunReport {
    verdict: Verdict,
    checks: Vec<CheckOutcome>,
    /// The submission's captured standard output, from running it on its own (see
    /// [`run_lesson`]).
    output: String,
}

impl RunReport {
    /// The graded verdict — what the renderer reads to report correct/incorrect
    /// and the feedback message. The exit-code mapping reads it too, so "what
    /// happened" stays one typed value rather than a stringly-typed field.
    pub fn verdict(&self) -> &Verdict {
        &self.verdict
    }
}

/// The stable JSON shape of a [`RunReport`]: a flat document with a string
/// `verdict` discriminant, the `feedback` message split out of the verdict, the
/// `checks` array, and the captured `output`.
///
/// Kept separate from the domain type (the [`Verdict`] sum type) so the wire
/// shape — what `--format json` and its consumers depend on — is decoupled from
/// the internal representation. The conversions are total and inverse, so a
/// [`RunReport`] round-trips through this document losslessly.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct RunDocument {
    verdict: VerdictTag,
    feedback: String,
    checks: Vec<CheckOutcome>,
    output: String,
}

/// The verdict discriminant as it appears in JSON: the variant tag without the
/// message (which travels in `feedback`). Lowercase on the wire.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
enum VerdictTag {
    Correct,
    Incorrect,
}

impl From<RunReport> for RunDocument {
    fn from(report: RunReport) -> Self {
        let RunReport {
            verdict,
            checks,
            output,
        } = report;
        let (verdict, feedback) = match verdict {
            Verdict::Correct { message } => (VerdictTag::Correct, message),
            Verdict::Incorrect { message } => (VerdictTag::Incorrect, message),
        };
        Self {
            verdict,
            feedback,
            checks,
            output,
        }
    }
}

impl From<RunDocument> for RunReport {
    fn from(doc: RunDocument) -> Self {
        let RunDocument {
            verdict,
            feedback,
            checks,
            output,
        } = doc;
        let verdict = match verdict {
            VerdictTag::Correct => Verdict::Correct { message: feedback },
            VerdictTag::Incorrect => Verdict::Incorrect { message: feedback },
        };
        Self {
            verdict,
            checks,
            output,
        }
    }
}

/// Why a run could not produce a report.
///
/// A typed error (§1.2) keeping the two failure stages distinct: the submission
/// could not be executed at all, or the feedback request failed. `core` stays
/// `anyhow`-free (ADR-0001); the cli maps this to its exit code at the edge.
#[derive(Debug)]
pub enum RunError {
    /// The interpreter could not run the submission to capture its output (a
    /// spawn or IO failure — not a program that ran and exited non-zero).
    Run(crate::runner::RunnerError),
    /// The LLM feedback request failed — see [`FeedbackError`] for the kind.
    Feedback(FeedbackError),
}

impl fmt::Display for RunError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RunError::Run(e) => write!(f, "could not run the submission: {e}"),
            RunError::Feedback(e) => write!(f, "{e}"),
        }
    }
}

impl Error for RunError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            RunError::Run(e) => Some(e),
            RunError::Feedback(e) => Some(e),
        }
    }
}

/// Run `submission` against `lesson` and produce a [`RunReport`].
///
/// The orchestrator (§2.4): it selects the lesson's runner, executes the
/// submission **on its own** to capture the output the report carries and the
/// prompt shows, grades the lesson's checks, builds the feedback prompt, and asks
/// `provider` for a verdict — composing each layer through its public type and
/// adding no domain logic of its own. `base_url_override` points the provider at
/// a stub in tests (the [`request_feedback`] seam) and is `None` in production.
/// Short and linear: the only branch is error propagation.
///
/// The output-capturing run is separate from grading: when the lesson has checks,
/// [`run_checks`] runs the submission again as its own gate (see
/// [`grade`](crate::grade)), so a submission with side-effecting or
/// nondeterministic output could in principle diverge between the captured output
/// and what the checks graded against. This is benign under v0's trusted-local,
/// deterministic-submission model, and the common LLM-only lesson has no checks —
/// so the submission runs exactly once. Folding the gate's output back into
/// grading (to run once even with checks) is a `grade`-layer change left to a
/// later slice.
pub async fn run_lesson(
    lesson: &Lesson,
    submission: &Submission,
    provider: ProviderChoice,
    base_url_override: Option<&str>,
) -> Result<RunReport, RunError> {
    let runner = select_runner(&lesson.language);
    // Run the submission on its own to capture what the learner's code produced;
    // that output feeds both the feedback prompt and the report. A launch failure
    // is a `RunError::Run`, never a verdict.
    let output = runner
        .execute(&submission.code, &[])
        .await
        .map_err(RunError::Run)?;
    let outcomes = run_checks(runner, &submission.code, &lesson.checks).await;

    let results = ExecResults { output, outcomes };
    let prompt = build_prompt(lesson, submission, &results);
    let verdict = request_feedback(provider, &prompt, base_url_override)
        .await
        .map_err(RunError::Feedback)?;

    let ExecResults { output, outcomes } = results;
    Ok(RunReport {
        verdict,
        checks: outcomes,
        output: output.stdout,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn report(verdict: Verdict, checks: Vec<CheckOutcome>, output: &str) -> RunReport {
        RunReport {
            verdict,
            checks,
            output: output.to_string(),
        }
    }

    #[test]
    fn run_report_json_roundtrip_preserves_value() {
        // The report survives a JSON round-trip unchanged — the flat wire document
        // and the typed domain value are inverse, so neither the verdict/feedback
        // split nor the checks array loses information (pattern from
        // `lesson::lesson_json_roundtrip_preserves_value`).
        let original = report(
            Verdict::Incorrect {
                message: "Not quite — check the return value.".to_string(),
            },
            vec![
                CheckOutcome::Pass,
                CheckOutcome::Fail {
                    detail: "expected 5".to_string(),
                },
            ],
            "8 \n",
        );
        let json = serde_json::to_string(&original).expect("a report serializes to JSON");
        let roundtripped: RunReport =
            serde_json::from_str(&json).expect("a report deserializes from JSON");
        assert_eq!(roundtripped, original);
    }

    #[test]
    fn json_splits_the_verdict_into_a_string_tag_and_feedback() {
        // The wire shape the `--format json` consumers depend on: a string
        // `verdict` discriminant (not the nested enum), the message in `feedback`,
        // and `checks` as an array — pinned independently of the round-trip.
        let json = serde_json::to_string(&report(
            Verdict::Correct {
                message: "well done".to_string(),
            },
            vec![],
            "",
        ))
        .expect("a report serializes to JSON");
        let value: serde_json::Value = serde_json::from_str(&json).expect("the json parses");

        assert_eq!(value["verdict"], "correct", "verdict is a string tag");
        assert_eq!(value["feedback"], "well done", "feedback is the message");
        assert!(value["checks"].is_array(), "checks is an array, got {value}");
        assert!(
            value["checks"].as_array().expect("array").is_empty(),
            "no checks serializes to [], not a stringified empty list"
        );
        assert!(value["output"].is_string(), "output is a string, got {value}");
    }

    #[test]
    fn json_tags_an_incorrect_verdict_distinctly() {
        // The twin of the correct tag: an incorrect verdict serializes to the
        // `"incorrect"` discriminant, so the two are never conflated on the wire.
        let json = serde_json::to_string(&report(
            Verdict::Incorrect {
                message: "try again".to_string(),
            },
            vec![],
            "",
        ))
        .expect("a report serializes to JSON");
        let value: serde_json::Value = serde_json::from_str(&json).expect("the json parses");
        assert_eq!(value["verdict"], "incorrect");
    }
}
