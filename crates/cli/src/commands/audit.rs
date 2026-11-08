//! `blendtutor audit <lesson>`: check eval labels against the lesson's checks.
//!
//! A thin orchestration shell (§4.1, ADR-0023). It loads the lesson and its
//! sibling `eval_<lesson>.yaml` suite, drives `core`'s [`audit_labels`] on a
//! runtime, renders the report through the [`output`] seam, and maps it to an
//! exit code. No LLM provider is involved, so no API key is needed. Running the
//! checks and classifying the labels live in `core`.

use std::path::Path;
use std::process::ExitCode;

use anyhow::Context;
use blendtutor_core::eval::{audit_labels, parse_eval_suite};
use blendtutor_core::lesson::read_lesson_file;

use crate::commands::sibling_suite_path;
use crate::output::{self, OutputFormat};

/// Audit every eval label for the lesson at `lesson_path` and render the report.
///
/// Exits 1 when any label contradicts the checks, so the audit can gate CI.
/// It exits 0 otherwise, including discordant labels and lessons with no
/// checks (not assessable is not a failure). A lesson or suite that fails to
/// load propagates as an error, which also exits 1.
pub fn run(lesson_path: &Path, format: OutputFormat) -> anyhow::Result<ExitCode> {
    let lesson = read_lesson_file(lesson_path)?;
    let suite_path = sibling_suite_path(lesson_path);
    let suite_yaml = std::fs::read_to_string(&suite_path)
        .with_context(|| format!("reading eval suite {}", suite_path.display()))?;
    let suite = parse_eval_suite(&suite_yaml)?;

    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;
    let report = runtime.block_on(audit_labels(&lesson, &suite));

    output::emit_audit(&report, format)?;
    Ok(if report.has_violation() {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    })
}
