//! The eval suite's misconception blueprint and the case label that may name one
//! (ADR-0024).
//!
//! A blueprint lists the misconceptions a lesson's eval items are meant to
//! sample. [`CaseLabel`] is an eval case's author-written label: `Correct`, or
//! `Incorrect` optionally tagged with one [`MisconceptionId`]. A tagged correct
//! case is unrepresentable. [`CaseLabel::polarity`] projects the label down to
//! the [`ExpectedVerdict`] that scoring, `classify`, and the audit compare, so
//! the tag never leaks into polarity equality.
//!
//! Also holds the order-preserving map deserializer the blueprint's YAML needs.
//! This module does not check tags against a blueprint; `parse_eval_suite` does
//! that at the suite level, where both are in view.

use std::fmt;

use serde::de::{self, Deserializer, MapAccess, Visitor};
use serde::{Serialize, Serializer};

use super::ExpectedVerdict;

/// A misconception's id in a blueprint, e.g. `M2`. A newtype so an id is never
/// confused with its human-readable label (§1.4).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MisconceptionId(String);

impl MisconceptionId {
    /// Wrap a raw id.
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }

    /// The id as written in the eval suite.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for MisconceptionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl Serialize for MisconceptionId {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&self.0)
    }
}

/// One blueprint entry: an id and the misconception it names.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Misconception {
    /// The id eval cases tag themselves with.
    pub id: MisconceptionId,
    /// What the misconception is, in the author's words.
    pub label: String,
}

/// An eval case's label: the author's claim about the submission.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CaseLabel {
    /// The submission meets the exercise.
    Correct,
    /// The submission does not, optionally because of one blueprint
    /// misconception.
    Incorrect {
        /// The misconception this item was built to exhibit, if tagged.
        misconception: Option<MisconceptionId>,
    },
}

impl CaseLabel {
    /// The label's polarity: what the grader's verdict and the checks are
    /// compared against.
    pub fn polarity(&self) -> ExpectedVerdict {
        match self {
            CaseLabel::Correct => ExpectedVerdict::Correct,
            CaseLabel::Incorrect { .. } => ExpectedVerdict::Incorrect,
        }
    }

    /// The tagged misconception, if any. Always `None` for a correct label.
    pub fn misconception(&self) -> Option<&MisconceptionId> {
        match self {
            CaseLabel::Correct => None,
            CaseLabel::Incorrect { misconception } => misconception.as_ref(),
        }
    }
}

impl From<ExpectedVerdict> for CaseLabel {
    /// An untagged label of the given polarity.
    fn from(polarity: ExpectedVerdict) -> Self {
        match polarity {
            ExpectedVerdict::Correct => CaseLabel::Correct,
            ExpectedVerdict::Incorrect => CaseLabel::Incorrect {
                misconception: None,
            },
        }
    }
}

/// Deserialize a YAML map of `id: label` into `(id, label)` pairs in document
/// order, refusing a repeated id. A `BTreeMap` would reorder ids as strings
/// (`M10` before `M2`), and a `HashMap` has no order at all.
pub(super) fn ordered_entries<'de, D>(deserializer: D) -> Result<Vec<(String, String)>, D::Error>
where
    D: Deserializer<'de>,
{
    struct Entries;

    impl<'de> Visitor<'de> for Entries {
        type Value = Vec<(String, String)>;

        fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str("a map of misconception id to label")
        }

        fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Self::Value, A::Error> {
            let mut entries: Vec<(String, String)> = Vec::new();
            while let Some((id, label)) = map.next_entry::<String, String>()? {
                if entries.iter().any(|(seen, _)| *seen == id) {
                    return Err(de::Error::custom(format!(
                        "duplicate misconception id {id:?} in the blueprint"
                    )));
                }
                entries.push((id, label));
            }
            Ok(entries)
        }
    }

    deserializer.deserialize_map(Entries)
}
