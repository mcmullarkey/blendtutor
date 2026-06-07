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
/// message; an author's *expected* outcome is pure polarity (ADR-0007). The
/// variants match the YAML spelling (`correct`, `incorrect`).
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ExpectedVerdict {
    /// The submission is expected to be graded correct.
    Correct,
    /// The submission is expected to be graded incorrect.
    Incorrect,
}

/// A single eval case: a synthetic submission and the polarity it should grade
/// to.
///
/// Unknown keys are rejected (§1.3.1) so an author's typo surfaces rather than
/// silently dropping. The two fields are positional partners — `submission` and
/// its `expected` verdict travel together in document order.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
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
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EvalSuite {
    /// The eval cases, in the order authored.
    pub cases: Vec<EvalCase>,
}

/// Why a YAML document is not a valid eval suite.
#[derive(Debug)]
pub enum EvalParseError {
    /// The document is not structurally an eval suite: a required field is
    /// missing, a value has the wrong type, or the YAML is malformed. Carries
    /// the underlying parser message, which names the offending field.
    Structural(String),
}

impl fmt::Display for EvalParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalParseError::Structural(msg) => write!(f, "invalid eval suite: {msg}"),
        }
    }
}

impl Error for EvalParseError {}

/// Parse an eval suite from a YAML document.
///
/// The pure parse boundary (§2.1): it deserializes the document into the typed
/// model — a missing required field, wrong type, or unknown key yields
/// [`EvalParseError::Structural`] naming the field. File reading is the caller's
/// concern, so this function is exercisable over an in-memory string.
pub fn parse_eval_suite(yaml: &str) -> Result<EvalSuite, EvalParseError> {
    serde_saphyr::from_str(yaml).map_err(|e| EvalParseError::Structural(e.to_string()))
}
