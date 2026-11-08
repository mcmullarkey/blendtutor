//! Run-to-run stability of an evaluation (ADR-0031).
//!
//! An LLM grader is stochastic, so a single eval run is one sample of the
//! measurement. [`StabilityReport`] folds N runs of the same suite into a
//! test–retest summary: how many cases got the same verdict on every run (with a
//! 95% Wilson interval), which cases flipped, and accuracy and leakage per run.
//! It says how much to trust one run's numbers. It does not say whether those
//! numbers are right; that is validity, which the other eval signals cover.
//!
//! Pure: it reads finished reports. Running the suite N times is the command's
//! job.

use std::fmt;

use serde::Serialize;

use super::{EvalReport, wilson};

/// Why N reports cannot be folded into a stability summary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StabilityError {
    /// Fewer than two runs: there is nothing to compare.
    TooFewRuns(usize),
    /// The runs graded different numbers of cases, so they are not repeats.
    MismatchedRuns,
}

impl fmt::Display for StabilityError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StabilityError::TooFewRuns(n) => {
                write!(f, "stability needs at least 2 runs, got {n}")
            }
            StabilityError::MismatchedRuns => {
                f.write_str("the runs graded different numbers of cases")
            }
        }
    }
}

impl std::error::Error for StabilityError {}

/// Test–retest summary over N runs of one eval suite.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct StabilityReport {
    runs: usize,
    cases: usize,
    stable: usize,
    interval: Option<(f64, f64)>,
    /// 1-based numbers of cases whose verdict differed between runs.
    flipped_cases: Vec<usize>,
    accuracy_per_run: Vec<f64>,
    /// Leak rate per run over assessable cases; absent when nothing was
    /// assessable.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    leakage_per_run: Vec<f64>,
}

impl StabilityReport {
    /// Fold `reports` (one per run, same suite, same case order).
    pub fn of(reports: &[EvalReport]) -> Result<Self, StabilityError> {
        if reports.len() < 2 {
            return Err(StabilityError::TooFewRuns(reports.len()));
        }
        let cases = reports[0].cases().len();
        if reports.iter().any(|r| r.cases().len() != cases) {
            return Err(StabilityError::MismatchedRuns);
        }
        let flipped_cases: Vec<usize> = (0..cases)
            .filter(|&i| {
                let first = reports[0].cases()[i].actual();
                reports.iter().any(|r| r.cases()[i].actual() != first)
            })
            .map(|i| i + 1)
            .collect();
        let stable = cases - flipped_cases.len();
        let leakage_per_run: Vec<f64> = reports
            .iter()
            .filter_map(|r| {
                r.leakage()
                    .map(|l| l.leaked() as f64 / l.assessable() as f64)
            })
            .collect();
        Ok(Self {
            runs: reports.len(),
            cases,
            stable,
            interval: wilson(stable, cases),
            flipped_cases,
            accuracy_per_run: reports.iter().map(EvalReport::accuracy).collect(),
            leakage_per_run,
        })
    }

    /// Runs folded.
    pub fn runs(&self) -> usize {
        self.runs
    }

    /// Cases per run.
    pub fn cases(&self) -> usize {
        self.cases
    }

    /// Cases with the same verdict on every run.
    pub fn stable(&self) -> usize {
        self.stable
    }

    /// The 95% Wilson interval on the stable proportion.
    pub fn interval(&self) -> Option<(f64, f64)> {
        self.interval
    }

    /// 1-based numbers of the cases that flipped.
    pub fn flipped_cases(&self) -> &[usize] {
        &self.flipped_cases
    }

    /// Accuracy of each run, in run order.
    pub fn accuracy_per_run(&self) -> &[f64] {
        &self.accuracy_per_run
    }

    /// Leak rate of each run, or empty when leakage was never assessable.
    pub fn leakage_per_run(&self) -> &[f64] {
        &self.leakage_per_run
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::{CaseResult, ExpectedVerdict};
    use crate::llm::Verdict;

    fn run(correct: &[bool]) -> EvalReport {
        let cases = correct
            .iter()
            .map(|c| {
                let v = if *c {
                    Verdict::Correct {
                        message: String::new(),
                    }
                } else {
                    Verdict::Incorrect {
                        message: String::new(),
                    }
                };
                CaseResult::score(ExpectedVerdict::Correct.into(), &v, &[])
            })
            .collect();
        EvalReport::new(&[], cases)
    }

    #[test]
    fn counts_cases_with_the_same_verdict_on_every_run() {
        let report = StabilityReport::of(&[
            run(&[true, false, true]),
            run(&[true, false, false]),
            run(&[true, false, true]),
        ])
        .expect("three runs");
        assert_eq!((report.runs(), report.cases(), report.stable()), (3, 3, 2));
        assert_eq!(report.flipped_cases(), [3]);
        let acc = report.accuracy_per_run();
        assert!((acc[0] - 2.0 / 3.0).abs() < 1e-12 && (acc[1] - 1.0 / 3.0).abs() < 1e-12);
        assert!(report.leakage_per_run().is_empty());
    }

    #[test]
    fn one_run_or_mismatched_runs_are_refused() {
        assert_eq!(
            StabilityReport::of(&[run(&[true])]),
            Err(StabilityError::TooFewRuns(1))
        );
        assert_eq!(
            StabilityReport::of(&[run(&[true]), run(&[true, true])]),
            Err(StabilityError::MismatchedRuns)
        );
    }
}
