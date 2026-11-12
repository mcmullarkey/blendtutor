//! Judging feedback that already exists (ADR-0034).
//!
//! [`FeedbackItem`] is one piece of recorded feedback with what the judge needs
//! to see: the task, the learner's submission, and optionally the misconception
//! it should diagnose plus a mismatched control label. [`judge_item`] applies
//! [`judge_case`] to one item, and [`ItemsReport::of`] summarizes finished
//! judgments the same way `eval --judge` does, so a caller can checkpoint
//! between items and resume. Nothing here grades or runs code. This module does
//! not read or write files or compare the results with human labels.

use serde::{Deserialize, Serialize};

use super::judge::{CaseJudgment, JudgeConfig, JudgeSummary, judge_case};
use crate::llm::FeedbackError;

/// The labels a diagnosis judgment needs: the item's own misconception and,
/// optionally, a mismatched one for the control. A control never appears
/// without its own label (§1.1).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemLabels {
    misconception: String,
    control: Option<String>,
}

/// One recorded piece of feedback to judge.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(try_from = "RawItem")]
pub struct FeedbackItem {
    id: String,
    task: String,
    submission: String,
    feedback: String,
    labels: Option<ItemLabels>,
}

impl FeedbackItem {
    /// The item's id, carried through to its judgment.
    pub fn id(&self) -> &str {
        &self.id
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawItem {
    id: String,
    task: String,
    submission: String,
    feedback: String,
    #[serde(default)]
    misconception: Option<String>,
    #[serde(default)]
    control: Option<String>,
}

impl TryFrom<RawItem> for FeedbackItem {
    type Error = String;

    fn try_from(raw: RawItem) -> Result<Self, Self::Error> {
        let labels = match (raw.misconception, raw.control) {
            (Some(misconception), control) => Some(ItemLabels {
                misconception,
                control,
            }),
            (None, None) => None,
            (None, Some(_)) => {
                return Err(format!(
                    "item {:?}: a control label needs the item's own misconception",
                    raw.id
                ));
            }
        };
        Ok(Self {
            id: raw.id,
            task: raw.task,
            submission: raw.submission,
            feedback: raw.feedback,
            labels,
        })
    }
}

/// One item's judgment, keyed by the item's id. It round-trips through JSON,
/// one per line, as a run's checkpoint.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ItemJudgment {
    id: String,
    judge: CaseJudgment,
}

impl ItemJudgment {
    /// The judged item's id.
    pub fn id(&self) -> &str {
        &self.id
    }
}

/// Every item's judgment plus the summary `eval --judge` reports.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ItemsReport {
    #[serde(skip_serializing_if = "Option::is_none")]
    judge: Option<JudgeSummary>,
    items: Vec<ItemJudgment>,
}

impl ItemsReport {
    /// Summarize finished judgments, in the order given.
    pub fn of(items: Vec<ItemJudgment>) -> Self {
        Self {
            judge: JudgeSummary::of_judgments(items.iter().map(|i| &i.judge)),
            items,
        }
    }

    /// The summary, absent when there were no items.
    pub fn judge(&self) -> Option<&JudgeSummary> {
        self.judge.as_ref()
    }

    /// How many items were judged.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Whether no item was judged.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

/// A judge call failed on one item.
#[derive(Debug)]
pub struct ItemJudgeError {
    /// The item that failed.
    pub id: String,
    /// The provider failure.
    pub source: FeedbackError,
}

impl std::fmt::Display for ItemJudgeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "judging item {:?} failed", self.id)
    }
}

impl std::error::Error for ItemJudgeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.source)
    }
}

/// Judge one item: diagnosis and its control when the item is tagged, and
/// actionability.
pub async fn judge_item(
    config: &JudgeConfig,
    item: &FeedbackItem,
    base_url_override: Option<&str>,
) -> Result<ItemJudgment, ItemJudgeError> {
    let labels = item
        .labels
        .as_ref()
        .map(|l| (l.misconception.as_str(), l.control.as_deref()));
    let judge = judge_case(
        config,
        &item.task,
        &item.submission,
        &item.feedback,
        labels,
        base_url_override,
    )
    .await
    .map_err(|source| ItemJudgeError {
        id: item.id.clone(),
        source,
    })?;
    Ok(ItemJudgment {
        id: item.id.clone(),
        judge,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(json: &str) -> Result<FeedbackItem, serde_json::Error> {
        serde_json::from_str(json)
    }

    #[test]
    fn an_item_with_a_misconception_and_control_carries_both_labels() {
        let item = parse(
            r#"{"id":"a","task":"t","submission":"s","feedback":"f",
                "misconception":"m","control":"c"}"#,
        )
        .expect("parses");
        assert_eq!(
            item.labels,
            Some(ItemLabels {
                misconception: "m".into(),
                control: Some("c".into())
            })
        );
    }

    #[test]
    fn an_untagged_item_has_no_labels() {
        let item =
            parse(r#"{"id":"a","task":"t","submission":"s","feedback":"f"}"#).expect("parses");
        assert_eq!(item.labels, None);
    }

    #[test]
    fn a_control_without_its_own_misconception_is_refused() {
        let err = parse(r#"{"id":"a","task":"t","submission":"s","feedback":"f","control":"c"}"#)
            .expect_err("refused");
        assert!(
            err.to_string()
                .contains("needs the item's own misconception"),
            "{err}"
        );
    }

    #[test]
    fn an_unknown_field_is_refused() {
        assert!(
            parse(r#"{"id":"a","task":"t","submission":"s","feedback":"f","order":1}"#).is_err()
        );
    }
}
