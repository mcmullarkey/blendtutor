//! Python execution mechanics: the Python-specific half of a language run —
//! launching the interpreter through `uv` — over the shared subprocess core in
//! [`super::subprocess`].
//!
//! This module owns *only* the Python specifics (ADR-0005, §4.1); callers depend
//! on the [`Runner`](super::Runner) trait, not on [`PythonRunner`]. It mirrors
//! the R runner: both are a small `Interpreter` descriptor over the shared
//! spawn/timeout/temp-cwd dance (§4.2) — adding a language is a descriptor, not a
//! re-implementation.
//!
//! When the lesson declares `packages` (ADR-0011), the runner injects
//! `--with <pkg>` flags into the uv invocation so the submission can import
//! them. The packages are constructor state — the [`Runner`] trait's
//! `execute(&self, code, checks)` signature is unchanged (§3.4).

use std::time::Duration;

use super::subprocess::{self, Interpreter};
use super::{ExecutionResult, Runner, RunnerError, Timeout};

/// A [`Runner`] backed by a real Python subprocess spawned through `uv`.
///
/// Stores the lesson's `packages` at construction (ADR-0011); `execute` builds
/// the `uv run --with <pkg>` invocation from them. The [`Runner`] trait stays
/// clean of the packages concern — they are constructor state, not a per-call
/// parameter (§3.4).
#[derive(Debug, Clone)]
pub struct PythonRunner {
    timeout: Timeout,
    packages: Vec<String>,
}

impl PythonRunner {
    /// Build a runner that kills any execution exceeding `timeout`, spawning
    /// `uv run --with <pkg>` for each package in `packages` (ADR-0011).
    pub fn new(timeout: Timeout, packages: Vec<String>) -> Self {
        Self { timeout, packages }
    }

    /// Build the interpreter descriptor: `uv run --no-project --quiet
    /// [--with <pkg>...] python -I -c`. When `packages` is empty, no `--with`
    /// flags are emitted — the invocation is `uv run --no-project --quiet
    /// python -I -c`, identical to the pre-packages runner.
    fn interpreter(&self) -> Interpreter {
        let mut args: Vec<String> = vec!["run".into(), "--no-project".into(), "--quiet".into()];
        for pkg in &self.packages {
            args.push("--with".into());
            args.push(pkg.clone());
        }
        args.extend(["python".into(), "-I".into(), "-c".into()]);
        Interpreter {
            program: "uv",
            code_args: args,
        }
    }
}

impl Default for PythonRunner {
    /// A 30-second bound — generous for a lesson exercise, finite for a runaway.
    /// No packages (the pre-ADR-0011 default).
    fn default() -> Self {
        Self::new(Timeout(Duration::from_secs(30)), Vec::new())
    }
}

impl Runner for PythonRunner {
    async fn execute(
        &self,
        code: &str,
        _checks: &[String],
    ) -> Result<ExecutionResult, RunnerError> {
        let interpreter = self.interpreter();
        subprocess::run(&interpreter, code, self.timeout).await
    }
}
