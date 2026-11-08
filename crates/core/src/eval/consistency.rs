//! Grader–check consistency: does the LLM verdict contradict the lesson's own
//! deterministic checks? (ADR-0022)
//!
//! Holds the [`Consistency`] classification and [`classify`], the pure function
//! that derives it from a verdict polarity and the per-check outcomes. The two
//! are separate indicators of one construct ("does the code meet the spec"), and
//! the asymmetry between them is the point. A failed or never-run check is
//! *sufficient* evidence of "incorrect", so a `correct` verdict next to one is a
//! [`Violation`]. All checks passing is only *necessary*, so an `incorrect`
//! verdict next to it is a weaker [`Discordance`]. No checks is no evidence at
//! all: [`Consistency::Unchecked`], never consistent.
//!
//! Part of `eval`'s pure scoring core. It does no I/O and knows nothing about
//! the LLM or the runner beyond their outputs. It does not decide what a
//! violation *means* for accuracy; it stays a separate signal from `matched`.

use serde::{Serialize, Serializer};

use super::ExpectedVerdict;
use crate::grade::CheckOutcome;

/// How a case's verdict relates to its check outcomes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Consistency {
    /// The lesson has no checks, so there is no evidence to compare against.
    Unchecked,
    /// The verdict agrees with what the checks can establish.
    Consistent,
    /// Plausible but worth a look: checks are necessary, not sufficient.
    Discordant(Discordance),
    /// A logical contradiction: the grader, a check, or the item label is wrong.
    Violated(Violation),
}

/// A disagreement the checks cannot rule out on their own.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Discordance {
    /// Graded incorrect although every check passed. This is expected when
    /// success criteria (style, required functions) are not encoded as checks.
    IncorrectDespitePassingChecks,
}

/// A disagreement the checks rule out.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Violation {
    /// Graded correct although at least one check failed.
    CorrectDespiteFailedCheck,
    /// Graded correct although the submission never ran (so no check could).
    CorrectDespiteNotRun,
}

impl Consistency {
    /// The wire `status` token.
    pub const fn status(&self) -> &'static str {
        match self {
            Consistency::Unchecked => "unchecked",
            Consistency::Consistent => "consistent",
            Consistency::Discordant(_) => "discordant",
            Consistency::Violated(_) => "violated",
        }
    }

    /// The wire `kind` token: present only for a disagreement, so an agreeing or
    /// unchecked case has no `kind` key at all.
    pub const fn kind(&self) -> Option<&'static str> {
        match self {
            Consistency::Unchecked | Consistency::Consistent => None,
            Consistency::Discordant(Discordance::IncorrectDespitePassingChecks) => {
                Some("incorrect_despite_passing_checks")
            }
            Consistency::Violated(Violation::CorrectDespiteFailedCheck) => {
                Some("correct_despite_failed_check")
            }
            Consistency::Violated(Violation::CorrectDespiteNotRun) => {
                Some("correct_despite_not_run")
            }
        }
    }
}

/// The wire view of a [`Consistency`]: `{"status": .., "kind": ..}`.
#[derive(Serialize)]
struct ConsistencyDocument {
    status: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    kind: Option<&'static str>,
}

impl Serialize for Consistency {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        ConsistencyDocument {
            status: self.status(),
            kind: self.kind(),
        }
        .serialize(serializer)
    }
}

/// What the check outcomes, taken together, establish about the submission.
///
/// Collapsing the slice into this enum *before* comparing with the verdict is
/// what keeps the empty slice out of the "all passed" bucket: `[].all(..)` is
/// vacuously true, but [`CheckEvidence::NoChecks`] is its own variant, and the
/// exhaustive match in [`classify`] has to handle it (§1.2).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CheckEvidence {
    NoChecks,
    AllPassed,
    NotRun,
    Failed,
}

impl CheckEvidence {
    /// Summarize `checks`. `NotRun` takes precedence over `Fail` because "the
    /// code never ran" is the stronger fact. `run_checks` makes `NotRun`
    /// all-or-nothing, so a mix of the two does not occur today.
    fn of(checks: &[CheckOutcome]) -> Self {
        if checks.is_empty() {
            return CheckEvidence::NoChecks;
        }
        if checks
            .iter()
            .any(|c| matches!(c, CheckOutcome::NotRun { .. }))
        {
            return CheckEvidence::NotRun;
        }
        if checks
            .iter()
            .any(|c| matches!(c, CheckOutcome::Fail { .. }))
        {
            return CheckEvidence::Failed;
        }
        CheckEvidence::AllPassed
    }
}

/// Classify a verdict polarity against the check outcomes for the same
/// submission. Pure and total.
///
/// The match has no wildcard arm, so a new verdict or evidence variant fails to
/// compile until someone decides what it means.
pub fn classify(verdict: &ExpectedVerdict, checks: &[CheckOutcome]) -> Consistency {
    use CheckEvidence as E;
    use ExpectedVerdict::{Correct, Incorrect};
    match (verdict, CheckEvidence::of(checks)) {
        (_, E::NoChecks) => Consistency::Unchecked,
        (Correct, E::AllPassed) => Consistency::Consistent,
        (Correct, E::Failed) => Consistency::Violated(Violation::CorrectDespiteFailedCheck),
        (Correct, E::NotRun) => Consistency::Violated(Violation::CorrectDespiteNotRun),
        (Incorrect, E::AllPassed) => {
            Consistency::Discordant(Discordance::IncorrectDespitePassingChecks)
        }
        (Incorrect, E::Failed | E::NotRun) => Consistency::Consistent,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pass() -> CheckOutcome {
        CheckOutcome::Pass
    }
    fn fail() -> CheckOutcome {
        CheckOutcome::Fail {
            detail: "assertion".to_string(),
        }
    }
    fn not_run() -> CheckOutcome {
        CheckOutcome::NotRun {
            reason: "syntax error".to_string(),
        }
    }

    use ExpectedVerdict::{Correct, Incorrect};

    #[test]
    fn no_checks_is_unchecked_for_either_verdict() {
        // The vacuous-truth trap: an empty slice must never read as agreement.
        assert_eq!(classify(&Correct, &[]), Consistency::Unchecked);
        assert_eq!(classify(&Incorrect, &[]), Consistency::Unchecked);
    }

    #[test]
    fn correct_with_all_checks_passing_is_consistent() {
        assert_eq!(
            classify(&Correct, &[pass(), pass()]),
            Consistency::Consistent
        );
    }

    #[test]
    fn correct_with_any_failed_check_is_a_violation() {
        assert_eq!(
            classify(&Correct, &[pass(), fail()]),
            Consistency::Violated(Violation::CorrectDespiteFailedCheck)
        );
    }

    #[test]
    fn correct_when_the_submission_never_ran_is_a_violation() {
        assert_eq!(
            classify(&Correct, &[not_run(), not_run()]),
            Consistency::Violated(Violation::CorrectDespiteNotRun)
        );
    }

    #[test]
    fn incorrect_with_all_checks_passing_is_discordant_not_violated() {
        assert_eq!(
            classify(&Incorrect, &[pass(), pass()]),
            Consistency::Discordant(Discordance::IncorrectDespitePassingChecks)
        );
    }

    #[test]
    fn incorrect_with_failed_or_not_run_checks_is_consistent() {
        assert_eq!(classify(&Incorrect, &[fail()]), Consistency::Consistent);
        assert_eq!(classify(&Incorrect, &[not_run()]), Consistency::Consistent);
    }

    #[test]
    fn serializes_status_with_kind_only_for_disagreements() {
        let json = |c: Consistency| serde_json::to_value(c).expect("serializes");
        assert_eq!(
            json(Consistency::Unchecked),
            serde_json::json!({"status": "unchecked"})
        );
        assert_eq!(
            json(Consistency::Consistent),
            serde_json::json!({"status": "consistent"})
        );
        assert_eq!(
            json(Consistency::Discordant(
                Discordance::IncorrectDespitePassingChecks
            )),
            serde_json::json!({"status": "discordant", "kind": "incorrect_despite_passing_checks"})
        );
        assert_eq!(
            json(Consistency::Violated(Violation::CorrectDespiteFailedCheck)),
            serde_json::json!({"status": "violated", "kind": "correct_despite_failed_check"})
        );
        assert_eq!(
            json(Consistency::Violated(Violation::CorrectDespiteNotRun)),
            serde_json::json!({"status": "violated", "kind": "correct_despite_not_run"})
        );
    }

    #[test]
    fn not_run_takes_precedence_over_fail() {
        assert_eq!(
            classify(&Correct, &[fail(), not_run()]),
            Consistency::Violated(Violation::CorrectDespiteNotRun)
        );
    }
}
