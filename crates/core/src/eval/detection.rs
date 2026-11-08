//! Per-misconception grader detection (ADR-0025).
//!
//! Holds [`DetectionRow`]: for one blueprint misconception, how many of its
//! tagged items the **grader** caught (graded incorrect), with a 95% Wilson
//! interval. Every tagged item is labeled incorrect (ADR-0024), so "caught" is
//! polarity scoring restricted to one tag. No judge is involved.
//!
//! Pure: rows are derived from already-scored [`CaseResult`]s. This module does
//! not run anything, and it does not say whether the grader *named* the
//! misconception in its feedback; that needs a judge and is a later construct.

use serde::Serialize;

use super::{CaseResult, ExpectedVerdict, Misconception, MisconceptionId, wilson};

/// One misconception's grader detection across its tagged items.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct DetectionRow {
    id: MisconceptionId,
    label: String,
    caught: usize,
    items: usize,
    /// Absent when there are no items: a gap in the bank, not a 0% rate.
    #[serde(skip_serializing_if = "Option::is_none")]
    interval: Option<(f64, f64)>,
}

impl DetectionRow {
    /// Count `cases` tagged with `misconception` and how many the grader caught.
    pub(super) fn of(misconception: &Misconception, cases: &[CaseResult]) -> Self {
        let tagged: Vec<&CaseResult> = cases
            .iter()
            .filter(|case| case.misconception.as_ref() == Some(&misconception.id))
            .collect();
        let items = tagged.len();
        let caught = tagged
            .iter()
            .filter(|case| case.actual == ExpectedVerdict::Incorrect)
            .count();
        Self {
            id: misconception.id.clone(),
            label: misconception.label.clone(),
            caught,
            items,
            interval: wilson(caught, items),
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

    /// Tagged items the grader graded incorrect.
    pub fn caught(&self) -> usize {
        self.caught
    }

    /// Items tagged with this misconception.
    pub fn items(&self) -> usize {
        self.items
    }

    /// The 95% Wilson interval on the detection rate, or `None` with no items.
    pub fn interval(&self) -> Option<(f64, f64)> {
        self.interval
    }
}

#[cfg(test)]
mod tests {
    use super::super::{CaseLabel, EvalReport};
    use super::*;
    use crate::llm::Verdict;

    fn tagged(id: &str) -> CaseLabel {
        CaseLabel::Incorrect {
            misconception: Some(MisconceptionId::new(id)),
        }
    }
    fn graded(correct: bool) -> Verdict {
        let message = String::new();
        if correct {
            Verdict::Correct { message }
        } else {
            Verdict::Incorrect { message }
        }
    }

    #[test]
    fn detection_counts_the_grader_verdict_on_tagged_items_only() {
        let blueprint = vec![
            Misconception {
                id: MisconceptionId::new("M1"),
                label: "a".into(),
            },
            Misconception {
                id: MisconceptionId::new("M2"),
                label: "b".into(),
            },
        ];
        let report = EvalReport::new(
            &blueprint,
            vec![
                CaseResult::score(tagged("M1"), &graded(false), &[]),
                CaseResult::score(tagged("M1"), &graded(true), &[]),
                // untagged: counts toward no misconception
                CaseResult::score(ExpectedVerdict::Incorrect.into(), &graded(false), &[]),
            ],
        );
        let rows: Vec<(&str, usize, usize, bool)> = report
            .by_misconception()
            .iter()
            .map(|r| {
                (
                    r.id().as_str(),
                    r.caught(),
                    r.items(),
                    r.interval().is_some(),
                )
            })
            .collect();
        assert_eq!(rows, vec![("M1", 1, 2, true), ("M2", 0, 0, false)]);
    }
}
