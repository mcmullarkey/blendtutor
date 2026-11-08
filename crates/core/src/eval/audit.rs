//! Label audit: do an eval suite's `expected` labels agree with the lesson's own
//! checks, and which blueprint misconceptions can those checks detect?
//! (ADR-0023, ADR-0024)
//!
//! The author's answer key is an instrument too. This module reuses
//! [`classify`] from slice 1 with the *label* in place of the grader's verdict:
//! the same truth table, applied to a different instrument. It also folds the
//! per-item check results into [`CoverageRow`]s, one per blueprint
//! misconception, showing which misconceptions the checks catch
//! deterministically and which only the grader can.
//!
//! [`LabelAudit`], [`CoverageRow`], and [`LabelAuditReport`] are pure values.
//! [`audit_labels`] is the thin effectful shell that runs each case's checks
//! through the lesson's runner. It never builds an LLM provider and needs no
//! API key. It does not grade, so it says nothing about feedback quality.

use serde::Serialize;

use super::{
    CaseLabel, Consistency, EvalSuite, ExpectedVerdict, Misconception, MisconceptionId, classify,
};
use crate::grade::{CheckOutcome, run_checks, select_runner};
use crate::lesson::Lesson;

/// One audited case: the author's label and how it relates to the checks.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LabelAudit {
    expected: ExpectedVerdict,
    #[serde(skip_serializing_if = "Option::is_none")]
    misconception: Option<MisconceptionId>,
    consistency: Consistency,
    /// Whether at least one check failed on this submission. A `NotRun` item
    /// is not caught: a crash says nothing about whether the checks detect the
    /// item's misconception (ADR-0024).
    #[serde(skip)]
    caught: bool,
}

impl LabelAudit {
    /// Audit one label against the check outcomes for its submission. The only
    /// constructor, so `consistency` and `caught` cannot disagree with the
    /// label and the outcomes (§1.1).
    pub fn of(label: &CaseLabel, checks: &[CheckOutcome]) -> Self {
        let expected = label.polarity();
        Self {
            expected,
            misconception: label.misconception().cloned(),
            consistency: classify(&expected, checks),
            caught: checks
                .iter()
                .any(|c| matches!(c, CheckOutcome::Fail { .. })),
        }
    }

    /// The author's label polarity for this case.
    pub fn expected(&self) -> &ExpectedVerdict {
        &self.expected
    }

    /// The blueprint misconception this case is tagged with, if any.
    pub fn misconception(&self) -> Option<&MisconceptionId> {
        self.misconception.as_ref()
    }

    /// How the label relates to the lesson's checks.
    pub fn consistency(&self) -> Consistency {
        self.consistency
    }
}

/// How well the lesson's checks detect one blueprint misconception.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MisconceptionCoverage {
    /// No eval case is tagged with it: a gap in the item bank.
    NoItems,
    /// Every tagged item fails at least one check.
    Covered,
    /// Some tagged items fail a check and some slip past.
    Partial,
    /// No tagged item fails a check: only the grader can catch it.
    Uncovered,
}

impl MisconceptionCoverage {
    /// Derive the status from the counts, so it cannot disagree with them.
    fn of(caught: usize, items: usize) -> Self {
        match (caught, items) {
            (_, 0) => MisconceptionCoverage::NoItems,
            (0, _) => MisconceptionCoverage::Uncovered,
            (c, n) if c == n => MisconceptionCoverage::Covered,
            _ => MisconceptionCoverage::Partial,
        }
    }

    /// The wire `status` token.
    pub const fn token(self) -> &'static str {
        match self {
            MisconceptionCoverage::NoItems => "no_items",
            MisconceptionCoverage::Covered => "covered",
            MisconceptionCoverage::Partial => "partial",
            MisconceptionCoverage::Uncovered => "uncovered",
        }
    }
}

impl Serialize for MisconceptionCoverage {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.token())
    }
}

/// One blueprint misconception's check coverage across its tagged items.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CoverageRow {
    id: MisconceptionId,
    label: String,
    status: MisconceptionCoverage,
    caught: usize,
    items: usize,
}

impl CoverageRow {
    /// Count `cases` tagged with `misconception` and how many the checks caught.
    fn of(misconception: &Misconception, cases: &[LabelAudit]) -> Self {
        let tagged: Vec<&LabelAudit> = cases
            .iter()
            .filter(|case| case.misconception.as_ref() == Some(&misconception.id))
            .collect();
        let items = tagged.len();
        let caught = tagged.iter().filter(|case| case.caught).count();
        Self {
            id: misconception.id.clone(),
            label: misconception.label.clone(),
            status: MisconceptionCoverage::of(caught, items),
            caught,
            items,
        }
    }

    /// The misconception's id.
    pub fn id(&self) -> &MisconceptionId {
        &self.id
    }

    /// The misconception's label, in the author's words.
    pub fn label(&self) -> &str {
        &self.label
    }

    /// The derived coverage status.
    pub fn status(&self) -> MisconceptionCoverage {
        self.status
    }

    /// Tagged items on which at least one check failed.
    pub fn caught(&self) -> usize {
        self.caught
    }

    /// Items tagged with this misconception.
    pub fn items(&self) -> usize {
        self.items
    }
}

/// Every audited case in suite order, plus one coverage row per blueprint
/// misconception in blueprint order (empty without a blueprint).
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LabelAuditReport {
    cases: Vec<LabelAudit>,
    coverage: Vec<CoverageRow>,
}

impl LabelAuditReport {
    /// Assemble a report from audited cases, deriving coverage for each entry
    /// of `blueprint`.
    pub fn new(blueprint: &[Misconception], cases: Vec<LabelAudit>) -> Self {
        let coverage = blueprint
            .iter()
            .map(|m| CoverageRow::of(m, &cases))
            .collect();
        Self { cases, coverage }
    }

    /// The audited cases, in suite order.
    pub fn cases(&self) -> &[LabelAudit] {
        &self.cases
    }

    /// One row per blueprint misconception, in blueprint order.
    pub fn coverage(&self) -> &[CoverageRow] {
        &self.coverage
    }

    /// Whether any label contradicts the checks: the audit's failure condition.
    /// Discordant and unchecked cases, and coverage gaps, do not fail it.
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
        cases.push(LabelAudit::of(case.label(), &outcomes));
    }
    LabelAuditReport::new(suite.blueprint(), cases)
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
    fn not_run() -> CheckOutcome {
        CheckOutcome::NotRun {
            reason: "syntax error".to_string(),
        }
    }
    fn correct() -> CaseLabel {
        CaseLabel::Correct
    }
    fn incorrect() -> CaseLabel {
        CaseLabel::Incorrect {
            misconception: None,
        }
    }
    fn tagged(id: &str) -> CaseLabel {
        CaseLabel::Incorrect {
            misconception: Some(MisconceptionId::new(id)),
        }
    }
    fn blueprint(ids: &[&str]) -> Vec<Misconception> {
        ids.iter()
            .map(|id| Misconception {
                id: MisconceptionId::new(*id),
                label: format!("label {id}"),
            })
            .collect()
    }

    #[test]
    fn audit_classifies_the_label_not_a_verdict() {
        // A label of `correct` on code that fails a check is the mislabeled
        // item the audit exists to catch.
        let audit = LabelAudit::of(&correct(), &[fail()]);
        assert_eq!(
            audit.consistency(),
            Consistency::Violated(Violation::CorrectDespiteFailedCheck)
        );
        assert_eq!(audit.expected(), &ExpectedVerdict::Correct);
        // The same evidence under an `incorrect` label is fine.
        assert_eq!(
            LabelAudit::of(&incorrect(), &[fail()]).consistency(),
            Consistency::Consistent
        );
    }

    #[test]
    fn only_a_violation_fails_the_audit() {
        let pass = CheckOutcome::Pass;
        let clean = LabelAuditReport::new(
            &[],
            vec![
                LabelAudit::of(&correct(), std::slice::from_ref(&pass)),
                // discordant: labeled incorrect, every check passed
                LabelAudit::of(&incorrect(), std::slice::from_ref(&pass)),
                // unchecked
                LabelAudit::of(&correct(), &[]),
            ],
        );
        assert!(!clean.has_violation());

        let dirty = LabelAuditReport::new(
            &[],
            vec![
                LabelAudit::of(&correct(), &[pass]),
                LabelAudit::of(&correct(), &[fail()]),
            ],
        );
        assert!(dirty.has_violation());
    }

    #[test]
    fn coverage_counts_tagged_items_per_misconception_in_blueprint_order() {
        let pass = CheckOutcome::Pass;
        let report = LabelAuditReport::new(
            &blueprint(&["M1", "M2", "M3", "M4"]),
            vec![
                LabelAudit::of(&tagged("M1"), &[fail()]),
                LabelAudit::of(&tagged("M1"), &[fail()]),
                LabelAudit::of(&tagged("M2"), std::slice::from_ref(&pass)),
                LabelAudit::of(&tagged("M3"), &[fail()]),
                LabelAudit::of(&tagged("M3"), std::slice::from_ref(&pass)),
                // untagged items never count toward any misconception
                LabelAudit::of(&incorrect(), &[fail()]),
            ],
        );
        let rows: Vec<(&str, MisconceptionCoverage, usize, usize)> = report
            .coverage()
            .iter()
            .map(|r| (r.id().as_str(), r.status(), r.caught(), r.items()))
            .collect();
        assert_eq!(
            rows,
            vec![
                ("M1", MisconceptionCoverage::Covered, 2, 2),
                ("M2", MisconceptionCoverage::Uncovered, 0, 1),
                ("M3", MisconceptionCoverage::Partial, 1, 2),
                ("M4", MisconceptionCoverage::NoItems, 0, 0),
            ]
        );
    }

    #[test]
    fn a_crashing_item_is_counted_but_not_caught() {
        let report = LabelAuditReport::new(
            &blueprint(&["M1"]),
            vec![LabelAudit::of(&tagged("M1"), &[not_run(), not_run()])],
        );
        let row = &report.coverage()[0];
        assert_eq!((row.caught(), row.items()), (0, 1));
        assert_eq!(row.status(), MisconceptionCoverage::Uncovered);
    }
}
