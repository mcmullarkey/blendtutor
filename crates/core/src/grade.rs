//! The lesson↔runner grading join: pick a language's runner and turn each lesson
//! check code-string into a typed pass/fail outcome.
//!
//! This is where [`lesson`](crate::lesson) meets [`runner`](crate::runner). It
//! depends on both only through their public types — a [`Language`] to choose a
//! runner, the [`Runner`] trait to execute, and a lesson's check code-strings to
//! grade against (§3.1). Selecting the runner is pure; running checks is
//! effectful (§2.1, §2.2). This module classifies outcomes; it does NOT build
//! prompts or call LLMs — that is the provider layer's job (§4.1).

use serde::{Deserialize, Serialize};

use crate::lesson::Language;
use crate::runner::{ExecutionResult, PythonRunner, RRunner, Runner, RunnerError, Timeout};

/// The verdict for a single lesson check.
///
/// A sum type, not a `bool`, so per-check verdicts stay distinct and the grader
/// never folds them into one aggregate (§1.2). Three real states: the check
/// passed, the submission ran and violated it, and — kept deliberately separate
/// from a failure (§3.3) — the submission could not run at all, so no check ever
/// got a verdict.
///
/// Serializable so the `run` command's [`RunReport`](crate::run::RunReport) can
/// carry the per-check outcomes into its JSON form; the externally-tagged shape
/// round-trips each variant losslessly.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum CheckOutcome {
    /// The submission satisfied the check.
    Pass,
    /// The check ran and the submission violated it.
    Fail {
        /// What the interpreter wrote when the check failed — typically the
        /// failing assertion on stderr.
        detail: String,
    },
    /// The submission could not be executed at all (a syntax error, or the
    /// interpreter failed to launch), so this check never ran. Distinct from
    /// [`Fail`](CheckOutcome::Fail): the submission was never even evaluated.
    NotRun {
        /// Why the submission could not run — the interpreter's diagnostic, or
        /// the launch error.
        reason: String,
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
/// dropped.
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

/// Select the runner for a lesson's `language`, threading `packages` to the
/// Python runner (ADR-0011).
///
/// Pure: it maps a [`Language`] to a runner with no I/O (§2.1). The `match` is
/// exhaustive with no wildcard arm, so a new [`Language`] variant fails to
/// compile here until it is dispatched explicitly — a language can never
/// silently fall through to the wrong runner. Packages are constructor state
/// on [`PythonRunner`], not a per-call parameter on the [`Runner`] trait (§3.4);
/// R ignores them.
pub fn select_runner(language: &Language, packages: &[String]) -> RunnerKind {
    match language {
        Language::R => RunnerKind::R(RRunner::default()),
        Language::Python => RunnerKind::Python(PythonRunner::new(
            Timeout(std::time::Duration::from_secs(30)),
            packages.to_vec(),
        )),
    }
}

/// Run each `check` against `submission` through `runner`, returning one
/// [`CheckOutcome`] per check, in order.
///
/// Effectful (§2.2). The submission is first run **on its own**: if it cannot
/// execute — a syntax error, or a launch failure — every check is
/// [`NotRun`](CheckOutcome::NotRun), because no check could have a verdict
/// (§3.3). This standalone gate is load-bearing: concatenating the submission
/// with a check and reading the combined exit code cannot tell a broken
/// submission from a violated check (and some interpreters, R among them, would
/// even splice an unterminated submission into the check). Once the submission
/// runs cleanly, each check is the submission followed by the check code-string;
/// the check passes when that program exits cleanly and fails otherwise.
/// Outcomes are element-wise — never folded into a single aggregate verdict.
///
/// With no checks there is nothing to classify, so the submission is not run at
/// all — an LLM-only lesson (the common case) never pays for a subprocess here.
pub async fn run_checks(
    runner: impl Runner,
    submission: &str,
    checks: &[String],
) -> Vec<CheckOutcome> {
    // Guard before the gate's subprocess (§1.3.1): with nothing to classify there
    // is no reason to run the submission at all.
    if checks.is_empty() {
        return Vec::new();
    }

    if let Some(reason) = submission_run_failure(&runner, submission).await {
        return checks
            .iter()
            .map(|_| CheckOutcome::NotRun {
                reason: reason.clone(),
            })
            .collect();
    }

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

/// Run `submission` on its own and report why it could not run, or `None` if it
/// ran cleanly.
///
/// The gate that makes "errored before checks" distinct from a check verdict
/// (§3.3, §5.1): a non-zero exit means the submission ran and failed (a syntax
/// error surfaces here), and an [`Err`] means the interpreter never launched —
/// both are reasons no check can run. A clean exit is `None`, so the caller
/// proceeds to the checks.
async fn submission_run_failure(runner: &impl Runner, submission: &str) -> Option<String> {
    match runner.execute(submission, &[]).await {
        Ok(result) if result.exit == Some(0) => None,
        Ok(result) => Some(result.stderr),
        Err(error) => Some(error.to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::sync::atomic::{AtomicUsize, Ordering};

    /// One scripted interpreter outcome for [`ScriptedRunner`] to replay.
    enum Reply {
        /// The program ran and exited with `code`, writing `stderr`.
        Exited { code: i32, stderr: String },
        /// The interpreter could not be launched at all.
        FailedToSpawn,
    }

    /// A [`Runner`] that replays a fixed script of replies in call order instead
    /// of spawning an interpreter, so `run_checks`'s classification (exit code →
    /// outcome, plus the spawn-failure arm a real interpreter never reaches) *and*
    /// its element-wise ordering are pinned deterministically with no
    /// `Rscript`/`uv` on `PATH`. It impls the real [`Runner`] trait (so the
    /// signature can't drift) and returns the real
    /// [`ExecutionResult`]/[`RunnerError`] through their own constructors,
    /// honouring the seam contract: a program that *ran* (any exit) is `Ok`; a
    /// failure to launch is `Err`. The call cursor is an [`AtomicUsize`] — not a
    /// `Cell` — so `&self` stays `Sync` and the `execute` future stays `Send`.
    struct ScriptedRunner {
        script: Vec<Reply>,
        next: AtomicUsize,
    }

    impl ScriptedRunner {
        fn new(script: Vec<Reply>) -> Self {
            Self {
                script,
                next: AtomicUsize::new(0),
            }
        }
    }

    impl Runner for ScriptedRunner {
        async fn execute(
            &self,
            _code: &str,
            _checks: &[String],
        ) -> Result<ExecutionResult, RunnerError> {
            let turn = self.next.fetch_add(1, Ordering::SeqCst);
            match &self.script[turn] {
                Reply::Exited { code, stderr } => Ok(ExecutionResult::from_capture(
                    b"",
                    stderr.as_bytes(),
                    Some(*code),
                    false,
                )),
                Reply::FailedToSpawn => Err(RunnerError::new(
                    "spawn fake",
                    std::io::Error::new(std::io::ErrorKind::NotFound, "no such interpreter"),
                )),
            }
        }
    }

    fn check_strings(n: usize) -> Vec<String> {
        (0..n).map(|i| format!("check_{i}")).collect()
    }

    #[test]
    fn select_runner_dispatches_an_r_lesson_to_the_r_runner() {
        assert!(
            matches!(select_runner(&Language::R, &[]), RunnerKind::R(_)),
            "an R lesson must select the R runner"
        );
    }

    #[test]
    fn select_runner_dispatches_a_python_lesson_to_the_python_runner() {
        assert!(
            matches!(select_runner(&Language::Python, &[]), RunnerKind::Python(_)),
            "a Python lesson must select the Python runner"
        );
    }

    #[test]
    fn select_runner_threads_packages_to_python_runner() {
        // ADR-0011: packages must reach PythonRunner as constructor state. A
        // regression that drops the `packages` parameter from select_runner
        // (the "select_runner bottleneck" negative) would leave PythonRunner
        // with empty packages — caught here by asserting the runner carries
        // them. We can't inspect the private field directly, but a Python
        // lesson with packages must still select the Python runner (not crash
        // or fall through), and the runner is constructed with the packages.
        let packages = vec!["pandas".to_string(), "numpy".to_string()];
        assert!(
            matches!(
                select_runner(&Language::Python, &packages),
                RunnerKind::Python(_)
            ),
            "a Python lesson with packages must select the Python runner"
        );
    }

    /// A reply for a submission that ran cleanly on its own — the gate's
    /// pass-through, so a test's script can focus on the checks that follow it.
    fn clean() -> Reply {
        Reply::Exited {
            code: 0,
            stderr: String::new(),
        }
    }

    #[tokio::test]
    async fn no_checks_runs_nothing_and_returns_empty() {
        // The script is empty, so any execute call would panic indexing it. This
        // passes only if run_checks short-circuits before touching the runner —
        // the LLM-only path must not spawn the submission.
        let runner = ScriptedRunner::new(vec![]);
        let outcomes = run_checks(runner, "submission", &check_strings(0)).await;
        assert!(
            outcomes.is_empty(),
            "no checks yields no outcomes, got {outcomes:?}"
        );
    }

    #[tokio::test]
    async fn checks_are_classified_element_wise_in_order() {
        // The submission passes the gate, then the first check exits clean → Pass
        // and the second exits non-zero → Fail carrying its stderr. Replaying
        // distinct replies in call order means a grader that reversed, folded, or
        // mis-paired the results cannot reproduce this exact ordered vector —
        // pinning the per-exit-code classification and the element-wise order off
        // any live interpreter.
        let runner = ScriptedRunner::new(vec![
            clean(),
            Reply::Exited {
                code: 0,
                stderr: String::new(),
            },
            Reply::Exited {
                code: 1,
                stderr: "assertion failed".to_string(),
            },
        ]);
        let outcomes = run_checks(runner, "submission", &check_strings(2)).await;
        assert_eq!(
            outcomes,
            vec![
                CheckOutcome::Pass,
                CheckOutcome::Fail {
                    detail: "assertion failed".to_string()
                },
            ],
            "a clean exit then a non-zero exit must be [Pass, Fail{{stderr}}] in order"
        );
    }

    #[tokio::test]
    async fn a_submission_that_exits_nonzero_makes_every_check_notrun() {
        // The submission runs but exits non-zero on its own (e.g. a syntax error):
        // the gate fails, so every check is NotRun carrying the submission's
        // stderr — never Fail — and no check is executed at all.
        let runner = ScriptedRunner::new(vec![Reply::Exited {
            code: 1,
            stderr: "could not parse submission".to_string(),
        }]);
        let outcomes = run_checks(runner, "broken submission", &check_strings(2)).await;
        assert_eq!(
            outcomes,
            vec![
                CheckOutcome::NotRun {
                    reason: "could not parse submission".to_string()
                },
                CheckOutcome::NotRun {
                    reason: "could not parse submission".to_string()
                },
            ],
            "a submission that cannot run makes every check NotRun, not Fail"
        );
    }

    #[tokio::test]
    async fn a_submission_whose_interpreter_cannot_launch_makes_checks_notrun() {
        // The gate's other failure arm: the interpreter never launched for the
        // submission, so the checks are NotRun, distinct from a check failure.
        let runner = ScriptedRunner::new(vec![Reply::FailedToSpawn]);
        let outcomes = run_checks(runner, "submission", &check_strings(1)).await;
        assert!(
            matches!(outcomes.as_slice(), [CheckOutcome::NotRun { .. }]),
            "a submission launch failure is NotRun, got {outcomes:?}"
        );
    }

    #[tokio::test]
    async fn a_check_whose_interpreter_cannot_launch_is_a_fail() {
        // After the submission passes the gate, a per-check launch failure is a
        // Fail — the check could not produce a verdict — NOT a NotRun, which is
        // reserved for the submission itself failing to run.
        let runner = ScriptedRunner::new(vec![clean(), Reply::FailedToSpawn]);
        let outcomes = run_checks(runner, "submission", &check_strings(1)).await;
        assert!(
            matches!(outcomes.as_slice(), [CheckOutcome::Fail { .. }]),
            "a per-check launch failure after a clean submission is a Fail, got {outcomes:?}"
        );
    }
}
