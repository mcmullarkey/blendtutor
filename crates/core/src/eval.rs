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
//! The module owns two responsibilities at different altitudes. The *data shape*
//! and its parse are pure: reading files is the caller's concern (§2.1). Scoring
//! ([`score_case`], [`aggregate`], [`CaseResult`], [`EvalReport`]) is the pure
//! core (§2.3) — exact polarity equality with no model in sight. [`run_eval`] is
//! the thin effectful shell (§2.4) that drives the suite through the same
//! pipeline `run` uses (so the feedback evaluated is the feedback shipped, §3.2)
//! and hands each verdict to the pure scorer.

use std::error::Error;
use std::fmt;

use serde::{Deserialize, Serialize, Serializer};

use crate::lesson::Lesson;
use crate::llm::{ProviderChoice, Submission, Verdict};
use crate::run::{RunError, run_lesson};

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

    /// The canonical lowercase spelling of this polarity — the single source for
    /// the YAML `expected` token, the serialized JSON form, and the human
    /// rendering, so none can drift from the accepted set.
    pub const fn token(&self) -> &'static str {
        match self {
            ExpectedVerdict::Correct => Self::CORRECT_TOKEN,
            ExpectedVerdict::Incorrect => Self::INCORRECT_TOKEN,
        }
    }
}

impl Serialize for ExpectedVerdict {
    /// Serialize to the same canonical token the parser accepts, so a scored
    /// report's JSON spells a polarity exactly as an author writes it.
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.token())
    }
}

impl From<&Verdict> for ExpectedVerdict {
    /// Reduce a runtime verdict to its scoring polarity, dropping the
    /// learner-facing message: eval scores *whether* the grader agreed, not the
    /// words it chose (v0 — richer grading is deferred).
    fn from(verdict: &Verdict) -> Self {
        match verdict {
            Verdict::Correct { .. } => ExpectedVerdict::Correct,
            Verdict::Incorrect { .. } => ExpectedVerdict::Incorrect,
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

/// Score one case: whether the grader's `actual` polarity matched the `expected`
/// one.
///
/// Pure and total (§2.3, §5.1): exact equality of the two polarities, so a wrong
/// verdict can never be scored a match — no substring or contains slack that
/// would let a near-miss pass.
pub fn score_case(expected: &ExpectedVerdict, actual: &ExpectedVerdict) -> bool {
    expected == actual
}

/// The aggregate accuracy of scored `cases`: the fraction whose polarity matched.
///
/// Pure and total (§2.3): an empty suite scores `0.0`, not a `0/0` NaN, so the
/// value always serializes to a real JSON number.
pub fn aggregate(cases: &[CaseResult]) -> f64 {
    if cases.is_empty() {
        return 0.0;
    }
    let matched = cases.iter().filter(|case| case.matched).count();
    matched as f64 / cases.len() as f64
}

/// One scored eval case: the expected polarity, the polarity the grader actually
/// returned, and whether they matched.
///
/// `matched` is derived at construction from the two polarities ([`score_case`]),
/// never set independently (§1.1): a result cannot claim a match its polarities
/// contradict. The serialized shape — `expected`, `actual`, `matched` — is the
/// per-case artifact a built site embeds without re-scoring.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CaseResult {
    expected: ExpectedVerdict,
    actual: ExpectedVerdict,
    matched: bool,
}

impl CaseResult {
    /// Score a case: reduce the runtime `actual` verdict to its polarity and
    /// derive `matched` from it and `expected`. The only constructor, so a
    /// `matched` flag inconsistent with the polarities is unrepresentable.
    pub fn score(expected: ExpectedVerdict, actual: &Verdict) -> Self {
        let actual = ExpectedVerdict::from(actual);
        let matched = score_case(&expected, &actual);
        Self {
            expected,
            actual,
            matched,
        }
    }

    /// The polarity the case's author expected the grader to return.
    pub fn expected(&self) -> &ExpectedVerdict {
        &self.expected
    }

    /// The polarity the grader actually returned for the submission.
    pub fn actual(&self) -> &ExpectedVerdict {
        &self.actual
    }

    /// Whether the actual polarity matched the expected one.
    pub fn matched(&self) -> bool {
        self.matched
    }
}

/// The result of scoring an eval suite: every [`CaseResult`] in suite order plus
/// the aggregate accuracy.
///
/// `accuracy` is derived at construction from the cases ([`aggregate`]), never
/// set independently (§1.1). The serialized shape — `cases`, `accuracy` — is the
/// eval artifact a built site embeds without re-scoring.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct EvalReport {
    cases: Vec<CaseResult>,
    accuracy: f64,
}

impl EvalReport {
    /// Assemble a report from scored `cases`, deriving the aggregate accuracy so
    /// it cannot disagree with the per-case results.
    pub fn new(cases: Vec<CaseResult>) -> Self {
        let accuracy = aggregate(&cases);
        Self { cases, accuracy }
    }

    /// The scored cases, in suite order.
    pub fn cases(&self) -> &[CaseResult] {
        &self.cases
    }

    /// The fraction of cases whose verdict polarity matched the expected one.
    pub fn accuracy(&self) -> f64 {
        self.accuracy
    }
}

/// Why scoring an eval suite failed: a case's submission could not be run through
/// the pipeline — the interpreter failed to launch or the provider call failed.
///
/// Names the offending case so an instructor can find it; a scoring run is
/// all-or-nothing, since a missing verdict cannot be scored as either polarity.
#[derive(Debug)]
pub struct EvalRunError {
    /// The zero-based index of the case whose run failed, in suite order.
    pub index: usize,
    /// The underlying pipeline failure.
    pub source: RunError,
}

impl fmt::Display for EvalRunError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // One-based for the instructor, matching the report's `case N` rows; the
        // field stays a zero-based index. (Slice-12's `EvalParseError` reports the
        // zero-based logical index instead — a parse-time developer concern, its
        // tested contract left untouched here.)
        write!(
            f,
            "eval case {} failed to run: {}",
            self.index + 1,
            self.source
        )
    }
}

impl Error for EvalRunError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(&self.source)
    }
}

/// Run every case in `suite` through the same pipeline `run` uses and score each
/// verdict against its expected polarity.
///
/// The thin effectful shell over the pure scorer (§2.4): for each case it runs
/// the submission through [`run_lesson`] — execute, grade, ask `provider` for a
/// verdict — then pairs the verdict's polarity with the expected one as a
/// [`CaseResult`]. Driving the *same* `run_lesson` is what keeps the evaluated
/// feedback identical to the shipped feedback (§3.2). A run failure stops scoring
/// and names the offending case ([`EvalRunError`]); `base_url_override` points
/// the provider at a stub in tests, exactly as `run` does, and is `None` in
/// production.
pub async fn run_eval(
    lesson: &Lesson,
    suite: &EvalSuite,
    provider: ProviderChoice,
    base_url_override: Option<&str>,
) -> Result<EvalReport, EvalRunError> {
    let mut cases = Vec::with_capacity(suite.cases.len());
    for (index, case) in suite.cases.iter().enumerate() {
        let submission = Submission::new(case.submission.clone());
        let report = run_lesson(lesson, &submission, provider, base_url_override)
            .await
            .map_err(|source| EvalRunError { index, source })?;
        cases.push(CaseResult::score(case.expected.clone(), report.verdict()));
    }
    Ok(EvalReport::new(cases))
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

    use crate::llm::Verdict;

    fn correct() -> Verdict {
        Verdict::Correct {
            message: "well done".to_string(),
        }
    }

    fn incorrect() -> Verdict {
        Verdict::Incorrect {
            message: "try again".to_string(),
        }
    }

    #[test]
    fn score_case_matches_same_polarity_and_rejects_the_opposite() {
        // Exact polarity equality both ways, so a one-directional or constant
        // implementation cannot pass.
        assert!(score_case(
            &ExpectedVerdict::Correct,
            &ExpectedVerdict::Correct
        ));
        assert!(score_case(
            &ExpectedVerdict::Incorrect,
            &ExpectedVerdict::Incorrect
        ));
        assert!(!score_case(
            &ExpectedVerdict::Correct,
            &ExpectedVerdict::Incorrect
        ));
        assert!(!score_case(
            &ExpectedVerdict::Incorrect,
            &ExpectedVerdict::Correct
        ));
    }

    #[test]
    fn verdict_polarity_drops_the_feedback_message() {
        // The runtime verdict's message is irrelevant to scoring; both polarities
        // map so a constant mapping (always Correct/Incorrect) is caught.
        assert_eq!(ExpectedVerdict::from(&correct()), ExpectedVerdict::Correct);
        assert_eq!(
            ExpectedVerdict::from(&incorrect()),
            ExpectedVerdict::Incorrect
        );
    }

    #[test]
    fn case_result_derives_matched_from_score_case() {
        // `matched` is computed at construction from the scored polarities, never
        // set independently (§1.1): an expected-correct case graded incorrect is a
        // mismatch, and `matched` equals `score_case` of the stored polarities.
        let result = CaseResult::score(ExpectedVerdict::Correct, &incorrect());
        assert_eq!(result.expected(), &ExpectedVerdict::Correct);
        assert_eq!(result.actual(), &ExpectedVerdict::Incorrect);
        assert!(!result.matched());
        assert_eq!(
            result.matched(),
            score_case(result.expected(), result.actual())
        );
    }

    #[test]
    fn aggregate_is_matched_over_total_and_zero_for_an_empty_suite() {
        let cases = vec![
            CaseResult::score(ExpectedVerdict::Correct, &correct()),
            CaseResult::score(ExpectedVerdict::Incorrect, &incorrect()),
            CaseResult::score(ExpectedVerdict::Correct, &incorrect()),
        ];
        assert_eq!(aggregate(&cases), 2.0 / 3.0);
        // No cases means no NaN (0/0) — an empty suite is a real, serializable 0.0.
        assert_eq!(aggregate(&[]), 0.0);
    }

    #[test]
    fn scores_two_of_three_and_aggregates() {
        // The AC1 probe: two matches and one mismatch give exactly 2/3, the
        // per-case `matched` flags are [true, true, false], and each is the
        // derived `score_case` of its stored polarities.
        let report = EvalReport::new(vec![
            CaseResult::score(ExpectedVerdict::Correct, &correct()),
            CaseResult::score(ExpectedVerdict::Incorrect, &incorrect()),
            CaseResult::score(ExpectedVerdict::Correct, &incorrect()),
        ]);

        assert_eq!(report.accuracy(), 2.0 / 3.0);
        let matched: Vec<bool> = report.cases().iter().map(CaseResult::matched).collect();
        assert_eq!(matched, vec![true, true, false]);
        for case in report.cases() {
            assert_eq!(case.matched(), score_case(case.expected(), case.actual()));
        }
    }

    #[test]
    fn eval_run_error_names_the_one_based_case_and_chains_its_source() {
        use crate::llm::FeedbackError;
        use crate::run::RunError;

        let err = EvalRunError {
            index: 2,
            source: RunError::Feedback(FeedbackError::MissingApiKey {
                var: "FIREWORKS_API_KEY",
            }),
        };

        let message = err.to_string();
        // Zero-based index 2 reads as the instructor's "case 3".
        assert!(
            message.contains("eval case 3 failed to run"),
            "should name the 1-based case and the failure: {message}"
        );
        assert!(
            message.contains("FIREWORKS_API_KEY"),
            "should surface the underlying pipeline failure: {message}"
        );
        // The pipeline failure is chained as the error source, not swallowed.
        assert!(
            std::error::Error::source(&err).is_some(),
            "the RunError must be reachable via Error::source"
        );
    }

    #[test]
    fn expected_verdict_serializes_to_its_canonical_token() {
        // The JSON spelling is the same single-sourced token as the YAML one, so
        // the wire form cannot drift from the accepted set.
        assert_eq!(
            serde_json::to_string(&ExpectedVerdict::Correct).unwrap(),
            format!("{:?}", ExpectedVerdict::CORRECT_TOKEN)
        );
        assert_eq!(
            serde_json::to_string(&ExpectedVerdict::Incorrect).unwrap(),
            format!("{:?}", ExpectedVerdict::INCORRECT_TOKEN)
        );
    }
}
