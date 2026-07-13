//! R execution mechanics: the R-specific half of a language run — which program
//! and which flags — over the shared subprocess core in [`super::subprocess`].
//!
//! This module owns *only* the R specifics (ADR-0005, §4.1); callers depend
//! on the [`Runner`](super::Runner) trait, not on [`RRunner`], and the
//! spawn/timeout/temp-cwd dance is shared with every other language (§4.2).
//!
//! R does not declare packages (ADR-0011): R lessons use system-installed
//! libraries, and the runner stays `Rscript --vanilla -e`. The `Interpreter`
//! type changed from `&'static [&'static str]` to `Vec<String>` (so Python can
//! inject `--with` flags), so R builds its fixed args dynamically — but the
//! invocation is byte-identical.

use std::time::Duration;

use super::subprocess::{self, Interpreter};
use super::{ExecutionResult, Runner, RunnerError, Timeout};

/// Build the R interpreter descriptor: `Rscript --vanilla -e`. The args are
/// fixed (R has no packages concern, ADR-0011) but built as a `Vec<String>`
/// because `Interpreter.code_args` is now `Vec<String>` (relaxed from
/// `&'static [&'static str]` so Python can inject runtime `--with` flags).
fn r_interpreter() -> Interpreter {
    Interpreter {
        program: "Rscript",
        code_args: vec!["--vanilla".into(), "-e".into()],
    }
}

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
        let interpreter = r_interpreter();
        subprocess::run(&interpreter, code, self.timeout).await
    }
}
