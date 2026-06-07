//! `blendtutor run <lesson> [--code <file>]` — the headline student loop.
//!
//! A thin orchestration shell (§4.1, §5.1): it gathers inputs (the lesson, the
//! submission, the provider override), drives the pure-ish core
//! [`run_lesson`](blendtutor_core::run::run_lesson) on a runtime, and renders the
//! result through the [`output`](crate::output) seam. It contains no execution,
//! grading, or HTTP logic — those live in `core`.

use std::io::{self, IsTerminal};
use std::path::Path;
use std::process::ExitCode;

use blendtutor_core::lesson::read_lesson_file;
use blendtutor_core::llm::{ProviderChoice, Submission};
use blendtutor_core::run::run_lesson;

use crate::output::{self, OutputFormat};

/// The environment variable that overrides the provider's base URL — the test
/// seam pointing the rig client at a stub (ADR-0006). Unset in production.
const PROVIDER_URL_VAR: &str = "BLENDTUTOR_PROVIDER_URL";

/// Load the lesson and submission, run the student loop, and render the report.
///
/// Reads the lesson (a read/parse failure propagates as an error → exit 1), reads
/// the submission from `code_path` or stdin, then drives `run_lesson` on a
/// current-thread runtime (the binary owns its async runtime; `core` stays a
/// library). The report is rendered via [`output::emit_run`]; the exit code is
/// the success code until the reserved verdict codes land with AC2.
pub fn run(
    lesson_path: &Path,
    code_path: Option<&Path>,
    format: OutputFormat,
) -> anyhow::Result<ExitCode> {
    let lesson = read_lesson_file(lesson_path)?;
    let submission = Submission::new(read_submission(code_path)?);
    let base_url = std::env::var(PROVIDER_URL_VAR).ok();

    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;
    let report = runtime.block_on(run_lesson(
        &lesson,
        &submission,
        ProviderChoice::default(),
        base_url.as_deref(),
    ))?;

    output::emit_run(&report, format)?;
    Ok(ExitCode::SUCCESS)
}

/// Read the student's submission from `code_path`, or from stdin when no file is
/// given.
///
/// A missing or unreadable `--code` file is a genuine IO error that propagates
/// (→ exit 1), kept distinct from a "not yet correct" verdict. With no `--code`,
/// piped stdin is read; an interactive terminal (no piped input) yields an empty
/// submission rather than blocking on a TTY.
fn read_submission(code_path: Option<&Path>) -> io::Result<String> {
    match code_path {
        Some(path) => std::fs::read_to_string(path),
        None => {
            let stdin = io::stdin();
            if stdin.is_terminal() {
                Ok(String::new())
            } else {
                io::read_to_string(stdin)
            }
        }
    }
}
