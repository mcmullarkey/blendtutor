//! Label audit: do an eval suite's `expected` labels agree with the lesson's own
//! checks? (ADR-0023)
//!
//! The author's answer key is an instrument too. This module reuses
//! [`classify`] from slice 1 with the *label* in place of the grader's verdict:
//! the same truth table, applied to a different instrument. [`LabelAudit`] and
//! [`LabelAuditReport`] are pure values. [`audit_labels`] is the thin effectful
//! shell that runs each case's checks through the lesson's runner.
//!
//! It never builds an LLM provider and needs no API key. It does not grade, so
//! it says nothing about feedback quality; that is `run_eval`'s job.

use serde::Serialize;

use super::{Consistency, EvalSuite, ExpectedVerdict, classify};
use crate::grade::{CheckOutcome, run_checks, select_runner};
use crate::lesson::Lesson;

/// One audited case: the author's label and how it relates to the checks.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LabelAudit {
    expected: ExpectedVerdict,
    consistency: Consistency,
}

impl LabelAudit {
    /// Audit one label against the check outcomes for its submission. The only
    /// constructor, so `consistency` cannot disagree with `expected` (§1.1).
    pub fn of(expected: ExpectedVerdict, checks: &[CheckOutcome]) -> Self {
        let consistency = classify(&expected, checks);
        Self {
            expected,
            consistency,
        }
    }

    /// The author's label for this case.
    pub fn expected(&self) -> &ExpectedVerdict {
        &self.expected
    }

    /// How the label relates to the lesson's checks.
    pub fn consistency(&self) -> Consistency {
        self.consistency
    }
}

/// Every audited case in suite order.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LabelAuditReport {
    cases: Vec<LabelAudit>,
}

impl LabelAuditReport {
    /// Assemble a report from audited cases, in suite order.
    pub fn new(cases: Vec<LabelAudit>) -> Self {
        Self { cases }
    }

    /// The audited cases, in suite order.
    pub fn cases(&self) -> &[LabelAudit] {
        &self.cases
    }

    /// Whether any label contradicts the checks: the audit's failure condition.
    /// Discordant and unchecked cases do not fail it.
    pub fn has_violation(&self) -> bool {
        self.cases
            .iter()
            .any(|case| matches!(case.consistency, Consistency::Violated(_)))
    }
}

/// Run each case's submission against the lesson's checks and audit its label.
///
/// No LLM provider is built, so this needs no API key. A lesson with no checks
/// runs nothing (`run_checks` short-circuits) and every case is unchecked.
pub async fn audit_labels(lesson: &Lesson, suite: &EvalSuite) -> LabelAuditReport {
    let mut cases = Vec::with_capacity(suite.cases.len());
    for case in &suite.cases {
        let runner = select_runner(&lesson.language, &lesson.packages);
        let outcomes = run_checks(runner, &case.submission, &lesson.checks).await;
        cases.push(LabelAudit::of(case.expected.clone(), &outcomes));
    }
    LabelAuditReport::new(cases)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::Violation;

    fn fail() -> CheckOutcome {
        CheckOutcome::Fail {
            detail: "assertion".to_string(),
        }
    }

    #[test]
    fn audit_classifies_the_label_not_a_verdict() {
        // A label of `correct` on code that fails a check is the mislabeled
        // item the audit exists to catch.
        let audit = LabelAudit::of(ExpectedVerdict::Correct, &[fail()]);
        assert_eq!(
            audit.consistency(),
            Consistency::Violated(Violation::CorrectDespiteFailedCheck)
        );
        assert_eq!(audit.expected(), &ExpectedVerdict::Correct);
        // The same evidence under an `incorrect` label is fine.
        assert_eq!(
            LabelAudit::of(ExpectedVerdict::Incorrect, &[fail()]).consistency(),
            Consistency::Consistent
        );
    }

    #[test]
    fn only_a_violation_fails_the_audit() {
        let pass = CheckOutcome::Pass;
        let clean = LabelAuditReport::new(vec![
            LabelAudit::of(ExpectedVerdict::Correct, std::slice::from_ref(&pass)),
            // discordant: labeled incorrect, every check passed
            LabelAudit::of(ExpectedVerdict::Incorrect, std::slice::from_ref(&pass)),
            // unchecked
            LabelAudit::of(ExpectedVerdict::Correct, &[]),
        ]);
        assert!(!clean.has_violation());

        let dirty = LabelAuditReport::new(vec![
            LabelAudit::of(ExpectedVerdict::Correct, &[pass]),
            LabelAudit::of(ExpectedVerdict::Correct, &[fail()]),
        ]);
        assert!(dirty.has_violation());
    }
}
