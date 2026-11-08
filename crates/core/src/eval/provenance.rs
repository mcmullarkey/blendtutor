//! Item provenance: where an eval case came from, and grader accuracy split by
//! it (ADR-0026).
//!
//! Holds [`ItemSource`] (`hand` or `generated`) and [`SourceRow`]: grader
//! accuracy over one source's cases, with a 95% Wilson interval. A gap between
//! the two sources screens the item generator for bias. It is a signal to
//! investigate, not a verdict.
//!
//! Pure: rows are folded from already-scored [`CaseResult`]s. This module does
//! not generate items; the `blendtutor-course` skill does that.

use serde::{Serialize, Serializer};

use super::{CaseResult, wilson};

/// Where an eval case came from. A closed enum so an unknown source is refused
/// at parse time rather than silently pooled (§1.2).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ItemSource {
    /// Written by a person. The default, so existing suites are all hand.
    #[default]
    Hand,
    /// Produced by a generator (e.g. the course skill) and gated by `audit`.
    Generated,
}

impl ItemSource {
    /// Every source, in report order.
    pub const ALL: [ItemSource; 2] = [ItemSource::Hand, ItemSource::Generated];

    /// The YAML and JSON spelling.
    pub const fn token(self) -> &'static str {
        match self {
            ItemSource::Hand => "hand",
            ItemSource::Generated => "generated",
        }
    }

    /// Map a YAML token to its source, or `None` if unrecognized.
    pub(super) fn from_token(token: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|source| source.token() == token)
    }

    /// Whether this is the default source; the wire omits it then.
    pub(super) fn is_hand(&self) -> bool {
        *self == ItemSource::Hand
    }
}

impl Serialize for ItemSource {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.token())
    }
}

/// Grader accuracy over the cases from one source.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SourceRow {
    source: ItemSource,
    matched: usize,
    items: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    interval: Option<(f64, f64)>,
}

impl SourceRow {
    /// One row per source, but only when some case is generated: a suite of
    /// hand items alone has nothing to compare, so it gets no split.
    pub(super) fn split(cases: &[CaseResult]) -> Vec<SourceRow> {
        if !cases
            .iter()
            .any(|case| case.source == ItemSource::Generated)
        {
            return Vec::new();
        }
        ItemSource::ALL
            .into_iter()
            .map(|source| {
                let from: Vec<&CaseResult> =
                    cases.iter().filter(|case| case.source == source).collect();
                let items = from.len();
                let matched = from.iter().filter(|case| case.matched).count();
                SourceRow {
                    source,
                    matched,
                    items,
                    interval: wilson(matched, items),
                }
            })
            .collect()
    }

    /// The source this row covers.
    pub fn source(&self) -> ItemSource {
        self.source
    }

    /// Cases from this source whose verdict polarity matched the label.
    pub fn matched(&self) -> usize {
        self.matched
    }

    /// Cases from this source.
    pub fn items(&self) -> usize {
        self.items
    }

    /// The 95% Wilson interval on accuracy, or `None` with no items.
    pub fn interval(&self) -> Option<(f64, f64)> {
        self.interval
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::ExpectedVerdict;
    use crate::llm::Verdict;

    fn case(source: ItemSource, matched: bool) -> CaseResult {
        let verdict = if matched {
            Verdict::Correct {
                message: String::new(),
            }
        } else {
            Verdict::Incorrect {
                message: String::new(),
            }
        };
        CaseResult::score(ExpectedVerdict::Correct.into(), &verdict, &[]).with_source(source)
    }

    #[test]
    fn tokens_round_trip_and_unknown_is_refused() {
        for source in ItemSource::ALL {
            assert_eq!(ItemSource::from_token(source.token()), Some(source));
        }
        assert_eq!(ItemSource::from_token("synthetic"), None);
    }

    #[test]
    fn hand_only_suites_get_no_split() {
        let cases = vec![case(ItemSource::Hand, true), case(ItemSource::Hand, false)];
        assert!(SourceRow::split(&cases).is_empty());
    }

    #[test]
    fn a_mixed_suite_splits_accuracy_by_source_in_report_order() {
        let cases = vec![
            case(ItemSource::Generated, true),
            case(ItemSource::Hand, true),
            case(ItemSource::Hand, false),
            case(ItemSource::Generated, true),
        ];
        let rows: Vec<(ItemSource, usize, usize)> = SourceRow::split(&cases)
            .iter()
            .map(|r| (r.source(), r.matched(), r.items()))
            .collect();
        assert_eq!(
            rows,
            vec![(ItemSource::Hand, 1, 2), (ItemSource::Generated, 2, 2)]
        );
    }

    #[test]
    fn an_all_generated_suite_still_shows_the_empty_hand_row() {
        let rows = SourceRow::split(&[case(ItemSource::Generated, true)]);
        assert_eq!(rows[0].source(), ItemSource::Hand);
        assert_eq!((rows[0].items(), rows[0].interval()), (0, None));
    }
}
