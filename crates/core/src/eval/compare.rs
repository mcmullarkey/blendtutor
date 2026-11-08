//! Paired comparison of two grading prompts on the same eval items, and the
//! pre-registered ship decision (ADR-0029).
//!
//! Holds [`Margin`], [`PairedChange`] (one construct's change, candidate minus
//! baseline, with a 95% Newcombe interval), and [`CompareReport`], which pairs
//! two [`EvalReport`]s case by case and applies the rule: ship only when
//! accuracy is non-inferior and leakage, when assessable, is improved.
//!
//! Pure: it reads two finished reports. Running the two evals and guarding that
//! both lessons share their checks belong to the command that calls it.

use std::fmt;
use std::str::FromStr;

use serde::{Serialize, Serializer};

use super::{EvalReport, Leakage, newcombe_paired};

/// The non-inferiority margin: how much worse (as a proportion) the candidate's
/// accuracy may be and still count as non-inferior. Strictly between 0 and 1.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Margin(f64);

impl Margin {
    /// The default 5-point margin.
    pub const DEFAULT: Margin = Margin(0.05);

    /// The margin as a proportion.
    pub fn value(self) -> f64 {
        self.0
    }
}

impl Default for Margin {
    fn default() -> Self {
        Margin::DEFAULT
    }
}

/// Why a string is not a [`Margin`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InvalidMargin(String);

impl fmt::Display for InvalidMargin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "margin must be a proportion strictly between 0 and 1 (e.g. 0.05), got {:?}",
            self.0
        )
    }
}

impl std::error::Error for InvalidMargin {}

impl FromStr for Margin {
    type Err = InvalidMargin;

    fn from_str(raw: &str) -> Result<Self, Self::Err> {
        match raw.trim().parse::<f64>() {
            Ok(value) if value > 0.0 && value < 1.0 => Ok(Margin(value)),
            _ => Err(InvalidMargin(raw.to_string())),
        }
    }
}

impl Serialize for Margin {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_f64(self.0)
    }
}

/// What a paired change shows for one construct.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Conclusion {
    /// Accuracy: the lower bound is above −margin.
    NonInferior,
    /// Accuracy: the interval reaches below −margin; too little evidence.
    NotShownNonInferior,
    /// Leakage: the upper bound is below 0.
    Improved,
    /// Leakage: the lower bound is above 0.
    Worse,
    /// Leakage: the interval includes 0.
    NotShownImproved,
}

impl Conclusion {
    /// The wire token.
    pub const fn token(self) -> &'static str {
        match self {
            Conclusion::NonInferior => "non_inferior",
            Conclusion::NotShownNonInferior => "not_shown_non_inferior",
            Conclusion::Improved => "improved",
            Conclusion::Worse => "worse",
            Conclusion::NotShownImproved => "not_shown_improved",
        }
    }
}

impl Serialize for Conclusion {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.token())
    }
}

/// One construct's paired change: successes per arm over `n` paired cases, the
/// change (candidate − baseline), its 95% Newcombe interval, and the conclusion.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct PairedChange {
    baseline: usize,
    candidate: usize,
    n: usize,
    change: f64,
    interval: (f64, f64),
    conclusion: Conclusion,
}

impl PairedChange {
    /// Fold `(baseline, candidate)` outcome pairs, or `None` with no pairs.
    fn of(pairs: &[(bool, bool)], conclude: impl Fn((f64, f64)) -> Conclusion) -> Option<Self> {
        let count = |want: (bool, bool)| pairs.iter().filter(|p| **p == want).count();
        let (both, only_baseline, only_candidate, neither) = (
            count((true, true)),
            count((true, false)),
            count((false, true)),
            count((false, false)),
        );
        let interval = newcombe_paired(both, only_baseline, only_candidate, neither)?;
        let n = pairs.len();
        Some(Self {
            baseline: both + only_baseline,
            candidate: both + only_candidate,
            n,
            change: (only_candidate as f64 - only_baseline as f64) / n as f64,
            interval,
            conclusion: conclude(interval),
        })
    }

    /// Successes in the baseline arm.
    pub fn baseline(&self) -> usize {
        self.baseline
    }

    /// Successes in the candidate arm.
    pub fn candidate(&self) -> usize {
        self.candidate
    }

    /// Paired cases.
    pub fn n(&self) -> usize {
        self.n
    }

    /// Candidate minus baseline, as a proportion.
    pub fn change(&self) -> f64 {
        self.change
    }

    /// The 95% Newcombe interval on the change.
    pub fn interval(&self) -> (f64, f64) {
        self.interval
    }

    /// What the change shows.
    pub fn conclusion(&self) -> Conclusion {
        self.conclusion
    }
}

/// The ship decision.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Decision {
    /// Accuracy non-inferior and leakage (when assessable) improved.
    Ship,
    /// Anything else; the report's reasons say why.
    Hold,
}

impl Serialize for Decision {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(match self {
            Decision::Ship => "ship",
            Decision::Hold => "hold",
        })
    }
}

/// Why two reports cannot be paired.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnpairedReports {
    /// Cases in the baseline report.
    pub baseline: usize,
    /// Cases in the candidate report.
    pub candidate: usize,
}

impl fmt::Display for UnpairedReports {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.baseline == 0 && self.candidate == 0 {
            return f.write_str("the eval suite has no cases to compare");
        }
        write!(
            f,
            "cannot pair reports of different sizes ({} baseline cases, {} candidate cases)",
            self.baseline, self.candidate
        )
    }
}

impl std::error::Error for UnpairedReports {}

/// The paired comparison of two eval runs over the same suite, with the decision.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CompareReport {
    paired_cases: usize,
    margin: Margin,
    accuracy: PairedChange,
    /// Absent when no case was assessable for leakage in both arms.
    #[serde(skip_serializing_if = "Option::is_none")]
    leakage: Option<PairedChange>,
    decision: Decision,
    reasons: Vec<String>,
    reports: Reports,
}

/// The two underlying eval reports, for inspection.
#[derive(Debug, Clone, PartialEq, Serialize)]
struct Reports {
    baseline: EvalReport,
    candidate: EvalReport,
}

impl CompareReport {
    /// Pair `baseline` and `candidate` case by case and apply the rule at
    /// `margin`. The reports must come from the same suite (same length and
    /// order); an empty suite or a size mismatch is refused.
    pub fn new(
        baseline: EvalReport,
        candidate: EvalReport,
        margin: Margin,
    ) -> Result<Self, UnpairedReports> {
        let (base_cases, cand_cases) = (baseline.cases(), candidate.cases());
        if base_cases.len() != cand_cases.len() || base_cases.is_empty() {
            return Err(UnpairedReports {
                baseline: base_cases.len(),
                candidate: cand_cases.len(),
            });
        }
        let accuracy_pairs: Vec<(bool, bool)> = base_cases
            .iter()
            .zip(cand_cases)
            .map(|(b, c)| (b.matched(), c.matched()))
            .collect();
        let leakage_pairs: Vec<(bool, bool)> = base_cases
            .iter()
            .zip(cand_cases)
            .filter_map(|(b, c)| match (b.leakage(), c.leakage()) {
                (Some(b), Some(c)) => Some((b == Leakage::Leaked, c == Leakage::Leaked)),
                _ => None,
            })
            .collect();

        let floor = -margin.value();
        let accuracy = PairedChange::of(&accuracy_pairs, |(lo, _)| {
            if lo > floor {
                Conclusion::NonInferior
            } else {
                Conclusion::NotShownNonInferior
            }
        })
        // Unreachable: an empty suite was refused above, and every case yields
        // one accuracy pair, so there is at least one pair here.
        .expect("a non-empty suite has accuracy pairs");
        let leakage = PairedChange::of(&leakage_pairs, |(lo, hi)| {
            if hi < 0.0 {
                Conclusion::Improved
            } else if lo > 0.0 {
                Conclusion::Worse
            } else {
                Conclusion::NotShownImproved
            }
        });

        let (decision, reasons) = decide(&accuracy, leakage.as_ref(), margin);
        Ok(Self {
            paired_cases: base_cases.len(),
            margin,
            accuracy,
            leakage,
            decision,
            reasons,
            reports: Reports {
                baseline,
                candidate,
            },
        })
    }

    /// Cases graded by both arms.
    pub fn paired_cases(&self) -> usize {
        self.paired_cases
    }

    /// The pre-registered margin.
    pub fn margin(&self) -> Margin {
        self.margin
    }

    /// The paired accuracy change.
    pub fn accuracy(&self) -> &PairedChange {
        &self.accuracy
    }

    /// The paired leakage change, if any case was assessable.
    pub fn leakage(&self) -> Option<&PairedChange> {
        self.leakage.as_ref()
    }

    /// The decision.
    pub fn decision(&self) -> Decision {
        self.decision
    }

    /// Why the decision is hold (empty for ship).
    pub fn reasons(&self) -> &[String] {
        &self.reasons
    }
}

/// The pre-registered rule: accuracy is the guardrail, leakage the target.
fn decide(
    accuracy: &PairedChange,
    leakage: Option<&PairedChange>,
    margin: Margin,
) -> (Decision, Vec<String>) {
    let mut reasons = Vec::new();
    if accuracy.conclusion() != Conclusion::NonInferior {
        reasons.push(format!(
            "accuracy not shown non-inferior at a {:.0}-point margin",
            margin.value() * 100.0
        ));
    }
    match leakage.map(PairedChange::conclusion) {
        None | Some(Conclusion::Improved) => {}
        Some(Conclusion::Worse) => reasons.push("leakage worse".to_string()),
        Some(_) => reasons.push("leakage not shown improved".to_string()),
    }
    let decision = if reasons.is_empty() {
        Decision::Ship
    } else {
        Decision::Hold
    };
    (decision, reasons)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::{CaseResult, ExpectedVerdict};
    use crate::grade::CheckOutcome;
    use crate::llm::Verdict;

    /// A report of `n` incorrect-labeled cases, each graded right or wrong and
    /// leaking or not, per the given flags.
    fn report(outcomes: &[(bool, Option<bool>)]) -> EvalReport {
        let cases = outcomes
            .iter()
            .map(|(right, leak)| {
                let verdict = if *right {
                    Verdict::Incorrect {
                        message: String::new(),
                    }
                } else {
                    Verdict::Correct {
                        message: String::new(),
                    }
                };
                let fail = CheckOutcome::Fail {
                    detail: String::new(),
                };
                CaseResult::score(ExpectedVerdict::Incorrect.into(), &verdict, &[fail])
                    .with_leakage(leak.map(|l| {
                        if l {
                            Leakage::Leaked
                        } else {
                            Leakage::Withheld
                        }
                    }))
            })
            .collect();
        EvalReport::new(&[], cases)
    }

    #[test]
    fn a_margin_must_lie_strictly_between_zero_and_one() {
        assert_eq!("0.1".parse::<Margin>().map(Margin::value), Ok(0.1));
        for bad in ["0", "1", "-0.1", "1.5", "five", ""] {
            assert!(bad.parse::<Margin>().is_err(), "{bad:?} must be refused");
        }
        assert!(
            "2".parse::<Margin>()
                .unwrap_err()
                .to_string()
                .contains("strictly between 0 and 1")
        );
    }

    #[test]
    fn equal_accuracy_on_few_items_holds_at_the_default_margin() {
        let base = report(&[(true, Some(true)); 6]);
        let cand = report(&[(true, Some(false)); 6]);
        let cmp = CompareReport::new(base, cand, Margin::DEFAULT).expect("pairs");
        assert_eq!(cmp.accuracy().conclusion(), Conclusion::NotShownNonInferior);
        assert_eq!(cmp.leakage().unwrap().conclusion(), Conclusion::Improved);
        assert_eq!(cmp.decision(), Decision::Hold);
        assert_eq!(
            cmp.reasons(),
            ["accuracy not shown non-inferior at a 5-point margin"]
        );
    }

    #[test]
    fn worse_leakage_holds_even_when_accuracy_is_fine() {
        let base = report(&[(true, Some(false)); 8]);
        let cand = report(&[(true, Some(true)); 8]);
        let cmp = CompareReport::new(base, cand, "0.9".parse().unwrap()).expect("pairs");
        assert_eq!(cmp.leakage().unwrap().conclusion(), Conclusion::Worse);
        assert_eq!(cmp.decision(), Decision::Hold);
        assert_eq!(cmp.reasons(), ["leakage worse"]);
    }

    #[test]
    fn without_assessable_leakage_accuracy_alone_decides() {
        let base = report(&[(true, None); 6]);
        let cand = report(&[(true, None); 6]);
        let cmp = CompareReport::new(base, cand, "0.5".parse().unwrap()).expect("pairs");
        assert!(cmp.leakage().is_none());
        assert_eq!(cmp.decision(), Decision::Ship);
    }

    #[test]
    fn reports_of_different_sizes_cannot_be_paired() {
        let err = CompareReport::new(
            report(&[(true, None); 2]),
            report(&[(true, None); 3]),
            Margin::DEFAULT,
        )
        .unwrap_err();
        assert_eq!((err.baseline, err.candidate), (2, 3));
    }
}
