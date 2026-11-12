//! The feedback judge inside `eval` (ADR-0033): consensus over k samples, the
//! mismatched-label control, and per-criterion summaries.
//!
//! Pure parts: [`Consensus`] (majority of k verdicts, kept with their
//! explanations), [`control_label`] (which wrong misconception to test the judge
//! against), and [`JudgeSummary`] (pass rates with Wilson intervals, plus how
//! often the k samples agreed). [`judge_case`] is the thin effectful step that
//! asks the judge model. It judges only feedback on cases the grader called
//! incorrect; this module does not validate the judge against people.

use serde::{Deserialize, Serialize};

use super::{CaseResult, Misconception, MisconceptionId, wilson};
use crate::llm::{
    Criterion, FeedbackError, Grader, JudgeVerdict, build_judge_prompt, request_judgment,
};

/// Who judges and how many samples each judgment takes.
#[derive(Debug, Clone)]
pub struct JudgeConfig {
    /// The judge model (may differ from the grader).
    pub judge: Grader,
    /// Samples per judgment; the verdict is their majority.
    pub samples: usize,
    /// Which criteria to judge.
    pub scope: JudgeScope,
    /// A candidate actionability definition replacing the built-in one.
    pub actionability_definition: Option<String>,
}

/// Which criteria a judge run covers. Actionability is always judged.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum JudgeScope {
    /// Diagnosis and its control (for tagged items), and actionability.
    #[default]
    All,
    /// Actionability alone: what scoring an actionability candidate needs.
    ActionabilityOnly,
}

/// The majority of k sampled verdicts on one criterion, with their explanations.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Consensus {
    pass: bool,
    passes: usize,
    samples: usize,
    explanations: Vec<String>,
}

impl Consensus {
    /// Fold sampled verdicts: pass when strictly more than half pass.
    pub fn of(verdicts: Vec<JudgeVerdict>) -> Self {
        let samples = verdicts.len();
        let passes = verdicts.iter().filter(|v| v.pass).count();
        Self {
            pass: passes * 2 > samples,
            passes,
            samples,
            explanations: verdicts.into_iter().map(|v| v.explanation).collect(),
        }
    }

    /// The majority verdict.
    pub fn pass(&self) -> bool {
        self.pass
    }

    /// Whether every sample gave the same verdict.
    pub fn unanimous(&self) -> bool {
        self.passes == 0 || self.passes == self.samples
    }
}

/// One judged case: diagnosis and its control (when the case is tagged and the
/// blueprint has another misconception), and actionability.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CaseJudgment {
    #[serde(skip_serializing_if = "Option::is_none")]
    diagnosis: Option<Consensus>,
    #[serde(skip_serializing_if = "Option::is_none")]
    diagnosis_control: Option<Consensus>,
    actionability: Consensus,
}

/// The mismatched misconception to test a diagnosis judgment against: the next
/// blueprint entry after `own`, wrapping around. `None` when the blueprint has no
/// other entry.
pub fn control_label<'a>(blueprint: &'a [Misconception], own: &MisconceptionId) -> Option<&'a str> {
    let at = blueprint.iter().position(|m| &m.id == own)?;
    if blueprint.len() < 2 {
        return None;
    }
    Some(blueprint[(at + 1) % blueprint.len()].label.as_str())
}

/// One criterion's pass rate over judged cases.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CriterionRate {
    passed: usize,
    judged: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    interval: Option<(f64, f64)>,
}

impl CriterionRate {
    fn of<'a>(judgments: impl Iterator<Item = &'a Consensus>) -> Self {
        let (mut passed, mut judged) = (0, 0);
        for c in judgments {
            judged += 1;
            passed += usize::from(c.pass);
        }
        Self {
            passed,
            judged,
            interval: wilson(passed, judged),
        }
    }

    /// Judged cases that passed.
    pub fn passed(&self) -> usize {
        self.passed
    }

    /// Cases judged on this criterion.
    pub fn judged(&self) -> usize {
        self.judged
    }

    /// The 95% Wilson interval, absent when nothing was judged.
    pub fn interval(&self) -> Option<(f64, f64)> {
        self.interval
    }
}

/// Per-criterion pass rates, the control rate, and the judge's consistency.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct JudgeSummary {
    samples: usize,
    diagnosis: CriterionRate,
    diagnosis_control: CriterionRate,
    actionability: CriterionRate,
    unanimous: usize,
    judgments: usize,
}

impl JudgeSummary {
    /// Summarize the judged cases, or `None` when no case was judged.
    pub(super) fn of(cases: &[CaseResult]) -> Option<Self> {
        Self::of_judgments(cases.iter().filter_map(|c| c.judge.as_ref()))
    }

    /// Summarize judgments however they were gathered (an eval run or an items
    /// file), or `None` when there are none.
    pub(super) fn of_judgments<'a>(
        judgments: impl Iterator<Item = &'a CaseJudgment>,
    ) -> Option<Self> {
        let judged: Vec<&CaseJudgment> = judgments.collect();
        let first = judged.first()?;
        let all: Vec<&Consensus> = judged
            .iter()
            .flat_map(|j| {
                [
                    j.diagnosis.as_ref(),
                    j.diagnosis_control.as_ref(),
                    Some(&j.actionability),
                ]
            })
            .flatten()
            .collect();
        Some(Self {
            samples: first.actionability.samples,
            diagnosis: CriterionRate::of(judged.iter().filter_map(|j| j.diagnosis.as_ref())),
            diagnosis_control: CriterionRate::of(
                judged.iter().filter_map(|j| j.diagnosis_control.as_ref()),
            ),
            actionability: CriterionRate::of(judged.iter().map(|j| &j.actionability)),
            unanimous: all.iter().filter(|c| c.unanimous()).count(),
            judgments: all.len(),
        })
    }

    /// Samples per judgment.
    pub fn samples(&self) -> usize {
        self.samples
    }

    /// Diagnosis against the item's own misconception.
    pub fn diagnosis(&self) -> &CriterionRate {
        &self.diagnosis
    }

    /// Diagnosis against a mismatched misconception (the control).
    pub fn diagnosis_control(&self) -> &CriterionRate {
        &self.diagnosis_control
    }

    /// Actionable next step.
    pub fn actionability(&self) -> &CriterionRate {
        &self.actionability
    }

    /// Judgments whose k samples all agreed.
    pub fn unanimous(&self) -> usize {
        self.unanimous
    }

    /// All judgments made (criteria × judged cases).
    pub fn judgments(&self) -> usize {
        self.judgments
    }
}

/// Ask the judge `config.samples` times per criterion about one case's feedback.
/// `labels` is `(own, control)`: the item's misconception label and the
/// mismatched one, when the case is tagged.
pub async fn judge_case(
    config: &JudgeConfig,
    task: &str,
    submission: &str,
    feedback: &str,
    labels: Option<(&str, Option<&str>)>,
    base_url_override: Option<&str>,
) -> Result<CaseJudgment, FeedbackError> {
    let sample = |criterion: Criterion<'_>| {
        let prompt = build_judge_prompt(criterion, task, submission, feedback);
        async move {
            let mut verdicts = Vec::with_capacity(config.samples);
            for _ in 0..config.samples {
                verdicts.push(request_judgment(&config.judge, &prompt, base_url_override).await?);
            }
            Ok::<_, FeedbackError>(Consensus::of(verdicts))
        }
    };
    let labels = match config.scope {
        JudgeScope::All => labels,
        JudgeScope::ActionabilityOnly => None,
    };
    let (diagnosis, diagnosis_control) = match labels {
        Some((own, control)) => {
            let diagnosis = sample(Criterion::Diagnosis { misconception: own }).await?;
            let control = match control {
                Some(wrong) => Some(
                    sample(Criterion::Diagnosis {
                        misconception: wrong,
                    })
                    .await?,
                ),
                None => None,
            };
            (Some(diagnosis), control)
        }
        None => (None, None),
    };
    let actionability = sample(Criterion::Actionability {
        definition: config.actionability_definition.as_deref(),
    })
    .await?;
    Ok(CaseJudgment {
        diagnosis,
        diagnosis_control,
        actionability,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn verdicts(passes: &[bool]) -> Vec<JudgeVerdict> {
        passes
            .iter()
            .map(|p| JudgeVerdict {
                explanation: format!("e{p}"),
                pass: *p,
            })
            .collect()
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
    fn consensus_is_a_strict_majority_and_tracks_unanimity() {
        let two_of_three = Consensus::of(verdicts(&[true, true, false]));
        assert!(two_of_three.pass() && !two_of_three.unanimous());
        let tie = Consensus::of(verdicts(&[true, false]));
        assert!(!tie.pass(), "a tie is not a majority");
        assert!(Consensus::of(verdicts(&[false, false, false])).unanimous());
    }

    #[test]
    fn the_control_is_the_next_misconception_wrapping_around() {
        let bp = blueprint(&["M1", "M2", "M3"]);
        assert_eq!(
            control_label(&bp, &MisconceptionId::new("M1")),
            Some("label M2")
        );
        assert_eq!(
            control_label(&bp, &MisconceptionId::new("M3")),
            Some("label M1")
        );
        assert_eq!(
            control_label(&blueprint(&["M1"]), &MisconceptionId::new("M1")),
            None
        );
    }
}
