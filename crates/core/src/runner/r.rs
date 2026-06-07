//! R execution mechanics: the R-specific half of a language run — which program
//! and which flags — over the shared subprocess core in [`super::subprocess`].
//!
//! This module owns *only* the R specifics (ADR-0005, §4.1); callers depend on
//! the [`Runner`](super::Runner) trait, not on [`RRunner`], and the
//! spawn/timeout/temp-cwd dance is shared with every other language (§4.2).

use std::time::Duration;

use super::subprocess::{self, Interpreter};
use super::{ExecutionResult, Runner, RunnerError, Timeout};

/// The R invocation: `Rscript --vanilla -e <code>`. `--vanilla` skips user/site
/// `.Rprofile`/`.Renviron`, so capture is deterministic across machines and no
/// profile output bleeds into stdout.
const R_INTERPRETER: Interpreter = Interpreter {
    program: "Rscript",
    code_args: &["--vanilla", "-e"],
};

/// A [`Runner`] backed by a real `Rscript` subprocess.
#[derive(Debug, Clone)]
pub struct RRunner {
    timeout: Timeout,
}

impl RRunner {
    /// Build a runner that kills any execution exceeding `timeout`.
    pub fn new(timeout: Timeout) -> Self {
        Self { timeout }
    }
}

impl Default for RRunner {
    /// A 30-second bound — generous for a lesson exercise, finite for a runaway.
    fn default() -> Self {
        Self::new(Timeout(Duration::from_secs(30)))
    }
}

impl Runner for RRunner {
    async fn execute(
        &self,
        code: &str,
        _checks: &[String],
    ) -> Result<ExecutionResult, RunnerError> {
        subprocess::run(&R_INTERPRETER, code, self.timeout).await
    }
}
