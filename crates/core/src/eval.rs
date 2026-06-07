//! The eval-case model: a typed model and its parse boundary.
//!
//! Holds [`EvalSuite`]/[`EvalCase`], the [`ExpectedVerdict`] polarity enum, the
//! typed [`EvalParseError`], and [`parse_eval_suite`] — the pure parse that
//! turns an instructor's `eval_<lesson>.yaml` into a typed suite. Each case
//! pairs a synthetic learner `submission` with the polarity the grader *should*
//! return; `ExpectedVerdict` is a dedicated sum type rather than the
//! message-carrying runtime `llm::Verdict` (ADR-0007), so an illegal verdict is
//! rejected here instead of travelling downstream as data.
//!
//! The public types are serde-free domain values; deserialization runs through
//! the private `Raw*` boundary DTOs, mirroring the `llm` layer's
//! `Feedback` → `Verdict` split (ADR-0006). That two-phase parse is what lets a
//! bad verdict be reported against its *case index* rather than a YAML line.
//!
//! This module owns the eval *data shape* only. It does not read files (the
//! caller's concern, so the parse stays pure — §2.1), does not run cases
//! against a model (that is the `eval` command, Slice 13), and does not depend
//! on `llm::Verdict`.

use std::error::Error;
use std::fmt;

use serde::Deserialize;

/// The verdict an eval case expects the grader to return: a polarity, with no
/// learner-facing message.
///
/// A two-variant sum type rather than a bare string, so an unknown verdict is
/// rejected at the parse boundary instead of carried downstream as data (§1.2).
/// Distinct from the runtime `llm::Verdict`, which also carries the feedback
/// message; an author's *expected* outcome is pure polarity (ADR-0007).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpectedVerdict {
    /// The submission is expected to be graded correct.
    Correct,
    /// The submission is expected to be graded incorrect.
    Incorrect,
}

impl ExpectedVerdict {
    /// The YAML `expected` spelling for [`ExpectedVerdict::Correct`].
    const CORRECT_TOKEN: &'static str = "correct";
    /// The YAML `expected` spelling for [`ExpectedVerdict::Incorrect`].
    const INCORRECT_TOKEN: &'static str = "incorrect";

    /// Map a YAML `expected` token to its variant, or `None` if unrecognized.
    ///
    /// The `CORRECT_TOKEN`/`INCORRECT_TOKEN` constants are the single source for
    /// the valid spellings, and [`parse_eval_suite`]'s error path lists the same
    /// constants on a miss, so the accepted set and the error hint cannot drift.
    fn from_token(token: &str) -> Option<Self> {
        match token {
            Self::CORRECT_TOKEN => Some(ExpectedVerdict::Correct),
            Self::INCORRECT_TOKEN => Some(ExpectedVerdict::Incorrect),
            _ => None,
        }
    }
}

/// A single eval case: a synthetic submission and the polarity it should grade
/// to.
///
/// The two fields are positional partners — `submission` and its `expected`
/// verdict travel together in document order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalCase {
    /// The synthetic learner submission to grade.
    pub submission: String,
    /// The polarity this submission is expected to grade to.
    pub expected: ExpectedVerdict,
}

/// A lesson's eval suite: its cases in authored order.
///
/// Construct one only through [`parse_eval_suite`]; document order is preserved
/// so a case's index is stable for reporting (Slice 13 scoring).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalSuite {
    /// The eval cases, in the order authored.
    pub cases: Vec<EvalCase>,
}

/// The boundary DTO for a suite: structural only, before verdict tokens are
/// validated into [`ExpectedVerdict`]s. Private so callers never see the
/// stringly `expected`.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawSuite {
    cases: Vec<RawCase>,
}

/// The boundary DTO for one case: `expected` is still a raw token here. Unknown
/// keys are rejected (§1.3.1) so an author's typo surfaces rather than silently
/// dropping.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawCase {
    submission: String,
    expected: String,
}

/// Why a YAML document is not a valid eval suite.
#[derive(Debug)]
pub enum EvalParseError {
    /// The document is not structurally an eval suite: a required field is
    /// missing, a value has the wrong type, an unknown key is present, or the
    /// YAML is malformed. Carries the underlying parser message, which names the
    /// offending field.
    Structural(String),
    /// A case's `expected` value is not a known verdict token. Carries the
    /// offending case so the author can find it.
    UnknownVerdict {
        /// The zero-based index of the offending case in document order.
        index: usize,
        /// The unrecognized token, quoted back to the author.
        token: String,
    },
}

impl fmt::Display for EvalParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalParseError::Structural(msg) => write!(f, "invalid eval suite: {msg}"),
            EvalParseError::UnknownVerdict { index, token } => write!(
                f,
                "invalid eval suite: eval case {index} has unknown expected verdict \
                 {token:?} (expected {correct:?} or {incorrect:?})",
                correct = ExpectedVerdict::CORRECT_TOKEN,
                incorrect = ExpectedVerdict::INCORRECT_TOKEN,
            ),
        }
    }
}

impl Error for EvalParseError {}

/// Parse an eval suite from a YAML document.
///
/// A two-phase pure parse (§2.1): first deserialize the structure into the
/// private `RawSuite` DTO — a missing field, wrong type, or unknown key yields
/// [`EvalParseError::Structural`] — then validate each case's `expected` token
/// into an [`ExpectedVerdict`], yielding [`EvalParseError::UnknownVerdict`]
/// naming the offending case index on a miss. Document order is preserved. File
/// reading is the caller's concern, so this runs over an in-memory string.
pub fn parse_eval_suite(yaml: &str) -> Result<EvalSuite, EvalParseError> {
    let raw: RawSuite =
        serde_saphyr::from_str(yaml).map_err(|e| EvalParseError::Structural(e.to_string()))?;
    let cases = raw
        .cases
        .into_iter()
        .enumerate()
        .map(|(index, case)| {
            let RawCase {
                submission,
                expected,
            } = case;
            match ExpectedVerdict::from_token(&expected) {
                Some(verdict) => Ok(EvalCase {
                    submission,
                    expected: verdict,
                }),
                None => Err(EvalParseError::UnknownVerdict {
                    index,
                    token: expected,
                }),
            }
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(EvalSuite { cases })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_token_maps_the_two_polarity_words_and_rejects_others() {
        assert_eq!(
            ExpectedVerdict::from_token("correct"),
            Some(ExpectedVerdict::Correct)
        );
        assert_eq!(
            ExpectedVerdict::from_token("incorrect"),
            Some(ExpectedVerdict::Incorrect)
        );
        assert_eq!(ExpectedVerdict::from_token("maybe"), None);
    }

    #[test]
    fn unknown_verdict_error_names_the_case_index_and_quotes_the_token() {
        let err = EvalParseError::UnknownVerdict {
            index: 2,
            token: "maybe".to_string(),
        };
        let msg = err.to_string();
        assert!(
            msg.contains("case 2"),
            "should name the offending case: {msg}"
        );
        assert!(msg.contains("maybe"), "should quote the bad token: {msg}");
    }

    #[test]
    fn structural_error_surfaces_the_underlying_parser_message() {
        let err = EvalParseError::Structural("line 1: boom".to_string());
        assert!(
            err.to_string().contains("boom"),
            "structural error should pass through the parser message"
        );
    }

    #[test]
    fn parse_eval_suite_surfaces_unknown_verdict_with_its_case_index_and_token() {
        // The structured error, not just its Display: pins the index (0) and the
        // offending token so a mutant that drops or rewrites either is caught.
        let yaml = "cases:\n  - submission: x\n    expected: maybe\n";
        match parse_eval_suite(yaml) {
            Err(EvalParseError::UnknownVerdict { index, token }) => {
                assert_eq!(index, 0, "names the offending case index");
                assert_eq!(token, "maybe", "carries the unrecognized token verbatim");
            }
            other => panic!("expected an UnknownVerdict error, got: {other:?}"),
        }
    }

    #[test]
    fn parse_rejects_unknown_key_so_author_typos_surface() {
        // deny_unknown_fields at the boundary: a typo'd case key is a structural
        // error naming it, not a silently dropped field (§1.3.1).
        let yaml = "cases:\n  - submision: x\n    expected: correct\n";
        let err = parse_eval_suite(yaml).expect_err("an unknown key must be rejected");
        assert!(
            err.to_string().contains("submision"),
            "error should name the unknown key, got: {err}"
        );
    }
}
