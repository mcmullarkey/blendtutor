//! The language-runner seam: a [`Runner`] trait and the normalized result of
//! running learner code.
//!
//! [`Runner`] is the boundary every execution and grading slice depends on, so a
//! second language (Python, Slice 8) or a hosted backend is a new `impl Runner`
//! rather than an edit at each call site (ADR-0005, §3.4). This module owns the
//! seam and the [`ExecutionResult`] shape; the R-specific subprocess mechanics
//! live in the private `r` submodule. It does not know about lessons or LLMs
//! (§4.1).

use std::error::Error;
use std::fmt;
use std::future::Future;
use std::time::Duration;

mod python;
mod r;
mod subprocess;

pub use python::PythonRunner;
pub use r::RRunner;

/// A wall-clock bound on a single execution.
///
/// A newtype over [`Duration`] so a timeout is never confused with an arbitrary
/// duration argument elsewhere (§1.4).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Timeout(pub Duration);

/// The normalized outcome of running learner code to completion or timeout.
///
/// The observable channels are kept in distinct fields rather than one merged
/// buffer (§1.2): a later grading prompt reads `stdout` and `stderr`
/// independently and must never see one bleed into the other. An
/// [`ExecutionResult`] only ever represents a process that actually *ran* — a
/// failure to launch the interpreter is a [`RunnerError`], never a value here.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExecutionResult {
    /// Everything the program wrote to standard output.
    pub stdout: String,
    /// Everything the program wrote to standard error.
    pub stderr: String,
    /// The process exit code, or `None` when it was terminated by a signal
    /// (e.g. killed on timeout).
    pub exit: Option<i32>,
    /// The final evaluated expression's value, reserved for the grading prompt.
    /// `None` until its producer lands with grading (Slice 9, ADR-0005).
    pub final_value: Option<String>,
    /// Whether the run was killed for exceeding its [`Timeout`].
    pub timed_out: bool,
}

impl ExecutionResult {
    /// Assemble a result from raw captured bytes — the pure normalization step,
    /// kept separate from the effectful spawn so it is testable on bytes alone
    /// (§2.3, §5.3). Output is decoded lossily; a runner never rejects learner
    /// output for not being valid UTF-8.
    pub(crate) fn from_capture(
        stdout: &[u8],
        stderr: &[u8],
        exit: Option<i32>,
        timed_out: bool,
    ) -> Self {
        Self {
            stdout: String::from_utf8_lossy(stdout).into_owned(),
            stderr: String::from_utf8_lossy(stderr).into_owned(),
            exit,
            final_value: None,
            timed_out,
        }
    }
}

/// Runs learner code and reports what it did.
///
/// The seam every execution and grading slice depends on (§3.4). `execute` is
/// fallible: an [`Err`] means the interpreter never ran (spawn or IO failure),
/// which is categorically distinct from the program running and writing to
/// stderr — so [`ExecutionResult`] never has to encode "we could not start".
///
/// Declared returning `impl Future` rather than with `async fn` so the public
/// trait stays clear of the `async_fn_in_trait` lint under `-D warnings`.
pub trait Runner {
    /// Execute `code`, returning its normalized [`ExecutionResult`].
    ///
    /// `checks` is reserved for the grading slice (which refines its element
    /// type); v1 execution does not yet consume it.
    fn execute(
        &self,
        code: &str,
        checks: &[String],
    ) -> impl Future<Output = Result<ExecutionResult, RunnerError>> + Send;
}

/// A failure to *run* learner code: the interpreter could not be spawned, or its
/// output pipes could not be read.
///
/// Distinct from a program that ran and failed — that is an [`ExecutionResult`]
/// with a non-zero `exit`. Kept a typed error so `core` stays free of `anyhow`
/// (ADR-0001).
#[derive(Debug)]
pub struct RunnerError {
    context: String,
    source: std::io::Error,
}

impl RunnerError {
    /// Wrap an IO failure with the runner stage that produced it. `context`
    /// accepts both a `&'static str` (a fixed stage like `"collect stdout"`) and
    /// an owned `String` (a stage that names the interpreter, e.g.
    /// `format!("spawn {program}")`), so the shared subprocess core can report
    /// which interpreter failed without each stage label being a separate const.
    pub(crate) fn new(context: impl Into<String>, source: std::io::Error) -> Self {
        Self {
            context: context.into(),
            source,
        }
    }
}

impl fmt::Display for RunnerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.context, self.source)
    }
}

impl Error for RunnerError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(&self.source)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_capture_keeps_streams_distinct_and_passes_exit_through() {
        let result = ExecutionResult::from_capture(b"OUT", b"ERR", Some(0), false);
        assert_eq!(result.stdout, "OUT");
        assert_eq!(result.stderr, "ERR");
        assert!(
            !result.stdout.contains("ERR"),
            "stderr must not bleed into stdout"
        );
        assert_eq!(result.exit, Some(0));
        assert!(!result.timed_out);
        assert_eq!(result.final_value, None);
    }

    #[test]
    fn runner_error_displays_its_stage_and_exposes_the_io_source() {
        let io = std::io::Error::new(std::io::ErrorKind::NotFound, "boom");
        let err = RunnerError::new("spawn Rscript", io);

        assert_eq!(err.to_string(), "spawn Rscript: boom");
        let source = Error::source(&err).expect("the io::Error is reachable as the source");
        assert_eq!(source.to_string(), "boom");
    }
}
