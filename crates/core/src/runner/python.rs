//! Python execution mechanics: the Python-specific half of a language run —
//! launching the interpreter through `uv` — over the shared subprocess core in
//! [`super::subprocess`].
//!
//! This module owns *only* the Python specifics (ADR-0005, §4.1); callers depend
//! on the [`Runner`](super::Runner) trait, not on [`PythonRunner`]. It mirrors
//! the R runner: both are a small `Interpreter` descriptor over the shared
//! spawn/timeout/temp-cwd dance (§4.2) — adding a language is a descriptor, not a
//! re-implementation.

use std::time::Duration;

use super::subprocess::{self, Interpreter};
use super::{ExecutionResult, Runner, RunnerError, Timeout};

/// The Python invocation, launched through `uv` per the project's always-uv
/// rule: `uv run --no-project --quiet python -I -c <code>`. `--no-project`
/// ignores any surrounding pyproject; `--quiet` keeps uv's own output out of the
/// captured streams; `-I` runs Python isolated (no user site, no `PYTHON*` env)
/// — the analog of R's `--vanilla`.
const PYTHON_INTERPRETER: Interpreter = Interpreter {
    program: "uv",
    code_args: &["run", "--no-project", "--quiet", "python", "-I", "-c"],
};

/// A [`Runner`] backed by a real Python subprocess spawned through `uv`.
#[derive(Debug, Clone)]
pub struct PythonRunner {
    timeout: Timeout,
}

impl PythonRunner {
    /// Build a runner that kills any execution exceeding `timeout`.
    pub fn new(timeout: Timeout) -> Self {
        Self { timeout }
    }
}

impl Default for PythonRunner {
    /// A 30-second bound — generous for a lesson exercise, finite for a runaway.
    fn default() -> Self {
        Self::new(Timeout(Duration::from_secs(30)))
    }
}

impl Runner for PythonRunner {
    async fn execute(
        &self,
        code: &str,
        _checks: &[String],
    ) -> Result<ExecutionResult, RunnerError> {
        subprocess::run(&PYTHON_INTERPRETER, code, self.timeout).await
    }
}
