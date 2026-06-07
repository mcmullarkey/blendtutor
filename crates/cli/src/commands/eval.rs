//! `blendtutor eval <lesson>` — score the feedback pipeline against a suite.
//!
//! A thin orchestration shell (§4.1, §5.1): it loads the lesson and its sibling
//! `eval_<lesson>.yaml` suite, drives the pure-cored [`run_eval`] on a runtime —
//! the *same* pipeline `run` uses, so the feedback scored is the feedback
//! shipped (§3.2) — and renders the report through the [`output`] seam. No
//! scoring, execution, or HTTP logic lives here; those are `core`'s.

use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use anyhow::Context;
use blendtutor_core::eval::{parse_eval_suite, run_eval};
use blendtutor_core::lesson::read_lesson_file;
use blendtutor_core::llm::ProviderChoice;

use crate::commands::PROVIDER_URL_VAR;
use crate::output::{self, OutputFormat};

/// Load the lesson and its sibling eval suite, score every case through the run
/// pipeline, and render the report.
///
/// A lesson read/parse failure, a missing or malformed suite, or a pipeline
/// failure on any case propagates as an error (→ exit 1). The command itself
/// always succeeds (exit 0) when it produces a report: `eval` measures feedback
/// quality, it is not a pass/fail gate, so a low accuracy is still a successful
/// run. The provider is driven on a current-thread runtime (the binary owns its
/// async runtime; `core` stays a library).
pub fn run(lesson_path: &Path, format: OutputFormat) -> anyhow::Result<ExitCode> {
    let lesson = read_lesson_file(lesson_path)?;
    let suite_path = sibling_suite_path(lesson_path);
    let suite_yaml = std::fs::read_to_string(&suite_path)
        .with_context(|| format!("reading eval suite {}", suite_path.display()))?;
    let suite = parse_eval_suite(&suite_yaml)?;
    let base_url = std::env::var(PROVIDER_URL_VAR).ok();

    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;
    let report = runtime.block_on(run_eval(
        &lesson,
        &suite,
        ProviderChoice::default(),
        base_url.as_deref(),
    ))?;

    output::emit_eval(&report, format)?;
    Ok(ExitCode::SUCCESS)
}

/// The eval suite that sits beside `lesson_path`: the lesson's file name prefixed
/// with `eval_`, so `lessons/foo.yaml` pairs with `lessons/eval_foo.yaml`. This
/// is the instructor-only sibling convention — the suite is authored next to its
/// lesson and is never bundled into a built site.
fn sibling_suite_path(lesson_path: &Path) -> PathBuf {
    let file_name = lesson_path.file_name().unwrap_or_else(|| OsStr::new(""));
    let mut suite_name = OsString::from("eval_");
    suite_name.push(file_name);
    lesson_path.with_file_name(suite_name)
}
