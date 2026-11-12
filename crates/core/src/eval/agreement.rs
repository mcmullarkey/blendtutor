//! How well the feedback judge agrees with people (ADR-0035).
//!
//! [`JudgedFile`] is the part of `judge-feedback` output this needs: each item's
//! id and majority verdicts. [`HumanLabel`] is one person's blind pass/fail on
//! the same id. [`JudgeAgreement::of`] pairs them on id, separately per
//! criterion, and reports TPR and TNR with Wilson intervals, the disagreeing
//! items, and the ids that could not be paired. Pure: no I/O, no provider. This
//! module does not split dev from test or optimize the judge.

use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};

use super::wilson;

/// The verdicts `judge-feedback` reported, by item id.
#[derive(Debug, Clone, Deserialize)]
pub struct JudgedFile {
    items: Vec<JudgedItem>,
}

#[derive(Debug, Clone, Deserialize)]
struct JudgedItem {
    id: String,
    judge: JudgedVerdicts,
}

#[derive(Debug, Clone, Deserialize)]
struct JudgedVerdicts {
    #[serde(default)]
    diagnosis: Option<Majority>,
    actionability: Majority,
}

#[derive(Debug, Clone, Deserialize)]
struct Majority {
    pass: bool,
}

/// A person's label on one item. Fields other than these (a note, a
/// timestamp) are ignored.
#[derive(Debug, Clone, Deserialize)]
pub struct HumanLabel {
    id: String,
    #[serde(default)]
    diagnosis: Option<LabelVerdict>,
    #[serde(default)]
    actionability: Option<LabelVerdict>,
}

/// A person's pass/fail.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum LabelVerdict {
    /// Meets the criterion.
    Pass,
    /// Does not.
    Fail,
}

impl LabelVerdict {
    fn passes(self) -> bool {
        self == Self::Pass
    }
}

/// A proportion with its 95% Wilson interval.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Rate {
    hits: usize,
    of: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    interval: Option<(f64, f64)>,
}

impl Rate {
    fn new(hits: usize, of: usize) -> Self {
        Self {
            hits,
            of,
            interval: wilson(hits, of),
        }
    }

    /// Matching verdicts.
    pub fn hits(&self) -> usize {
        self.hits
    }

    /// Pairs in the denominator.
    pub fn of(&self) -> usize {
        self.of
    }

    /// The 95% Wilson interval, absent when the denominator is zero.
    pub fn interval(&self) -> Option<(f64, f64)> {
        self.interval
    }
}

/// One criterion's agreement: TPR over items people passed, TNR over items
/// people failed.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CriterionAgreement {
    pairs: usize,
    tpr: Rate,
    tnr: Rate,
}

impl CriterionAgreement {
    fn of(pairs: &[(bool, bool)]) -> Self {
        let count =
            |human: bool, judge: bool| pairs.iter().filter(|&&p| p == (judge, human)).count();
        let (tp, fn_) = (count(true, true), count(true, false));
        let (tn, fp) = (count(false, false), count(false, true));
        Self {
            pairs: pairs.len(),
            tpr: Rate::new(tp, tp + fn_),
            tnr: Rate::new(tn, tn + fp),
        }
    }

    /// Items with both a judge verdict and a human label.
    pub fn pairs(&self) -> usize {
        self.pairs
    }

    /// The judge passes what people pass.
    pub fn tpr(&self) -> &Rate {
        &self.tpr
    }

    /// The judge fails what people fail.
    pub fn tnr(&self) -> &Rate {
        &self.tnr
    }
}

/// An item and criterion where the judge and the person differ.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Disagreement {
    id: String,
    criterion: &'static str,
    judge: bool,
    human: bool,
}

impl Disagreement {
    /// The item.
    pub fn id(&self) -> &str {
        &self.id
    }

    /// `"diagnosis"` or `"actionability"`.
    pub fn criterion(&self) -> &'static str {
        self.criterion
    }
}

/// Judge-vs-human agreement per criterion.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct JudgeAgreement {
    diagnosis: CriterionAgreement,
    actionability: CriterionAgreement,
    disagreements: Vec<Disagreement>,
    unlabeled: Vec<String>,
    unjudged: Vec<String>,
}

impl JudgeAgreement {
    /// Pair judged items with human labels on id, in the judged file's order.
    pub fn of(judged: &JudgedFile, labels: &[HumanLabel]) -> Self {
        let by_id: BTreeMap<&str, &HumanLabel> =
            labels.iter().map(|l| (l.id.as_str(), l)).collect();
        let judged_ids: BTreeSet<&str> = judged.items.iter().map(|i| i.id.as_str()).collect();
        let (mut diagnosis, mut actionability) = (Vec::new(), Vec::new());
        let mut disagreements = Vec::new();
        let mut unlabeled = Vec::new();
        for item in &judged.items {
            let Some(label) = by_id.get(item.id.as_str()) else {
                unlabeled.push(item.id.clone());
                continue;
            };
            let criteria = [
                (
                    "diagnosis",
                    item.judge.diagnosis.as_ref().map(|m| m.pass),
                    label.diagnosis,
                    &mut diagnosis,
                ),
                (
                    "actionability",
                    Some(item.judge.actionability.pass),
                    label.actionability,
                    &mut actionability,
                ),
            ];
            for (criterion, judge, human, pairs) in criteria {
                let (Some(judge), Some(human)) = (judge, human.map(LabelVerdict::passes)) else {
                    continue;
                };
                pairs.push((judge, human));
                if judge != human {
                    disagreements.push(Disagreement {
                        id: item.id.clone(),
                        criterion,
                        judge,
                        human,
                    });
                }
            }
        }
        let unjudged = labels
            .iter()
            .filter(|l| !judged_ids.contains(l.id.as_str()))
            .map(|l| l.id.clone())
            .collect();
        Self {
            diagnosis: CriterionAgreement::of(&diagnosis),
            actionability: CriterionAgreement::of(&actionability),
            disagreements,
            unlabeled,
            unjudged,
        }
    }

    /// Diagnosis agreement.
    pub fn diagnosis(&self) -> &CriterionAgreement {
        &self.diagnosis
    }

    /// Actionability agreement.
    pub fn actionability(&self) -> &CriterionAgreement {
        &self.actionability
    }

    /// Where the judge and the person differ, in judged order.
    pub fn disagreements(&self) -> &[Disagreement] {
        &self.disagreements
    }

    /// Judged ids with no human label.
    pub fn unlabeled(&self) -> &[String] {
        &self.unlabeled
    }

    /// Labeled ids the judge never saw.
    pub fn unjudged(&self) -> &[String] {
        &self.unjudged
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tpr_counts_human_passes_and_tnr_counts_human_fails() {
        // (judge, human): TP, FN, TN, TN, FP
        let a = CriterionAgreement::of(&[
            (true, true),
            (false, true),
            (false, false),
            (false, false),
            (true, false),
        ]);
        assert_eq!((a.tpr().hits(), a.tpr().of()), (1, 2));
        assert_eq!((a.tnr().hits(), a.tnr().of()), (2, 3));
        assert_eq!(a.pairs(), 5);
    }

    #[test]
    fn a_yes_sayer_has_perfect_tpr_and_zero_tnr() {
        let a = CriterionAgreement::of(&[(true, true), (true, true), (true, false)]);
        assert_eq!((a.tpr().hits(), a.tpr().of()), (2, 2));
        assert_eq!((a.tnr().hits(), a.tnr().of()), (0, 1));
    }

    #[test]
    fn an_empty_class_has_no_interval() {
        let a = CriterionAgreement::of(&[(true, true)]);
        assert_eq!(a.tnr().of(), 0);
        assert_eq!(a.tnr().interval(), None);
    }

    #[test]
    fn a_label_value_other_than_pass_or_fail_is_refused() {
        let parsed: Result<HumanLabel, _> =
            serde_json::from_str(r#"{"id":"a","actionability":"maybe"}"#);
        assert!(parsed.is_err());
    }
}
