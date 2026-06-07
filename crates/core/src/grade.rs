//! The lesson↔runner grading join: pick a language's runner and turn each lesson
//! check code-string into a typed pass/fail outcome.
//!
//! This is where [`lesson`](crate::lesson) meets [`runner`](crate::runner). It
//! depends on both only through their public types — a [`Language`] to choose a
//! runner, the [`Runner`] trait to execute, and a lesson's check code-strings to
//! grade against (§3.1). Selecting the runner is pure; running checks is
//! effectful (§2.1, §2.2). This module classifies outcomes; it does NOT build
//! prompts or call LLMs — that is the provider layer's job (§4.1).

use crate::lesson::Language;
use crate::runner::{ExecutionResult, PythonRunner, RRunner, Runner, RunnerError};

/// The verdict for a single lesson check.
///
/// A sum type, not a `bool`, so per-check verdicts stay distinct and the grader
/// never folds them into one aggregate (§1.2). Slice 9 begins with the two
/// states a *running* submission produces; a submission that cannot run at all
/// gains its own variant in AC3 rather than masquerading as a failure.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CheckOutcome {
    /// The submission satisfied the check.
    Pass,
    /// The check ran and the submission violated it.
    Fail {
        /// What the interpreter wrote when the check failed — typically the
        /// failing assertion on stderr.
        detail: String,
    },
}

/// A runner chosen for a lesson's language.
///
/// A closed enum holding one concrete runner. [`Runner::execute`] returns
/// `impl Future` (an RPITIT), so the trait is not object-safe and a
/// runtime-chosen runner cannot be a `Box<dyn Runner>`; an enum is how we carry
/// that choice instead. Consumers still depend on the [`Runner`] trait, not on
/// these concrete variants — `RunnerKind` itself impls it, so [`run_checks`]
/// takes `impl Runner` and never names `RRunner`/`PythonRunner` (§3.4). The
/// `match` over it is exhaustive: adding a [`Language`] variant forces a new arm
/// both here and in [`select_runner`], so a language can never be silently
/// dropped (§1.2).
#[derive(Debug, Clone)]
pub enum RunnerKind {
    /// The R runner.
    R(RRunner),
    /// The Python runner.
    Python(PythonRunner),
}

impl Runner for RunnerKind {
    async fn execute(&self, code: &str, checks: &[String]) -> Result<ExecutionResult, RunnerError> {
        match self {
            RunnerKind::R(r) => r.execute(code, checks).await,
            RunnerKind::Python(p) => p.execute(code, checks).await,
        }
    }
}

/// Select the runner for a lesson's `language`.
///
/// Pure: it maps a [`Language`] to a runner with no I/O (§2.1). The `match` is
/// exhaustive with no wildcard arm, so a new [`Language`] variant fails to
/// compile here until it is dispatched explicitly — a language can never
/// silently fall through to the wrong runner.
pub fn select_runner(language: &Language) -> RunnerKind {
    match language {
        Language::R => RunnerKind::R(RRunner::default()),
        Language::Python => RunnerKind::Python(PythonRunner::default()),
    }
}

/// Run each `check` against `submission` through `runner`, returning one
/// [`CheckOutcome`] per check, in order.
///
/// Effectful: each check is the `submission` followed by the check code-string,
/// executed in the runner; the check passes when that program exits cleanly and
/// fails otherwise (§2.2). Outcomes are element-wise — never folded into a single
/// aggregate verdict — so a caller sees exactly which checks held.
pub async fn run_checks(
    runner: impl Runner,
    submission: &str,
    checks: &[String],
) -> Vec<CheckOutcome> {
    let mut outcomes = Vec::with_capacity(checks.len());
    for check in checks {
        let program = format!("{submission}\n{check}");
        let outcome = match runner.execute(&program, &[]).await {
            Ok(result) if result.exit == Some(0) => CheckOutcome::Pass,
            Ok(result) => CheckOutcome::Fail {
                detail: result.stderr,
            },
            Err(error) => CheckOutcome::Fail {
                detail: error.to_string(),
            },
        };
        outcomes.push(outcome);
    }
    outcomes
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A [`Runner`] that fabricates a fixed result instead of spawning an
    /// interpreter, so `run_checks`'s classification — exit code → outcome, and
    /// the spawn-failure arm a real interpreter never reaches — is exercised
    /// deterministically with no `Rscript`/`uv` on `PATH`. It impls the real
    /// [`Runner`] trait (so the signature can't drift) and returns the real
    /// [`ExecutionResult`]/[`RunnerError`] built through their own constructors,
    /// honouring the seam contract: a program that *ran* (any exit) is `Ok`; a
    /// failure to launch is `Err`.
    enum FakeRunner {
        /// The program ran and exited with `code`, writing `stderr`.
        Exited { code: i32, stderr: String },
        /// The interpreter could not be launched at all.
        FailedToSpawn,
    }

    impl Runner for FakeRunner {
        async fn execute(
            &self,
            _code: &str,
            _checks: &[String],
        ) -> Result<ExecutionResult, RunnerError> {
            match self {
                FakeRunner::Exited { code, stderr } => Ok(ExecutionResult::from_capture(
                    b"",
                    stderr.as_bytes(),
                    Some(*code),
                    false,
                )),
                FakeRunner::FailedToSpawn => Err(RunnerError::new(
                    "spawn fake",
                    std::io::Error::new(std::io::ErrorKind::NotFound, "no such interpreter"),
                )),
            }
        }
    }

    fn checks(one: &str) -> Vec<String> {
        vec![one.to_string()]
    }

    #[test]
    fn select_runner_dispatches_an_r_lesson_to_the_r_runner() {
        assert!(
            matches!(select_runner(&Language::R), RunnerKind::R(_)),
            "an R lesson must select the R runner"
        );
    }

    #[test]
    fn select_runner_dispatches_a_python_lesson_to_the_python_runner() {
        assert!(
            matches!(select_runner(&Language::Python), RunnerKind::Python(_)),
            "a Python lesson must select the Python runner"
        );
    }

    #[tokio::test]
    async fn a_check_whose_program_exits_cleanly_is_a_pass() {
        let runner = FakeRunner::Exited {
            code: 0,
            stderr: String::new(),
        };
        let outcomes = run_checks(runner, "submission", &checks("check")).await;
        assert_eq!(outcomes, vec![CheckOutcome::Pass]);
    }

    #[tokio::test]
    async fn a_check_whose_program_exits_nonzero_is_a_fail_carrying_stderr() {
        let runner = FakeRunner::Exited {
            code: 1,
            stderr: "assertion failed".to_string(),
        };
        let outcomes = run_checks(runner, "submission", &checks("check")).await;
        assert_eq!(
            outcomes,
            vec![CheckOutcome::Fail {
                detail: "assertion failed".to_string()
            }],
            "a non-zero exit fails the check and the interpreter's stderr is the detail"
        );
    }

    #[tokio::test]
    async fn a_check_whose_interpreter_cannot_launch_is_a_fail() {
        // The spawn-failure arm: a real interpreter present on `PATH` never
        // produces it, so only this fake reaches the `Err` branch of run_checks.
        let runner = FakeRunner::FailedToSpawn;
        let outcomes = run_checks(runner, "submission", &checks("check")).await;
        assert!(
            matches!(outcomes.as_slice(), [CheckOutcome::Fail { .. }]),
            "a launch failure cannot leave the check unclassified, got {outcomes:?}"
        );
    }
}
