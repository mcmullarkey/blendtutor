//! `blendtutor validate <path>` — load a lesson file and report whether it is
//! valid, exiting zero on success and non-zero on failure, in either the
//! human-readable or JSON output format.

use std::path::Path;
use std::process::ExitCode;

use blendtutor_core::lesson::{LoadError, read_lesson_file};

use crate::output::{self, OutputFormat, ValidateReport};

/// Load the lesson at `path`, then render the validation result in `format`.
///
/// The command computes a typed [`ValidateReport`] and hands it to the renderer;
/// it never formats output itself (§5.1). The exit code is read from the report
/// (§3.4), so it is identical across formats. A genuine read failure (missing
/// file, bad permissions) is distinct from an invalid lesson and propagates to
/// `main` as an error rather than a finding.
pub fn run(path: &Path, format: OutputFormat) -> anyhow::Result<ExitCode> {
    let report = match read_lesson_file(path) {
        Ok(lesson) => ValidateReport::valid(lesson.lesson_name.to_string()),
        Err(LoadError::Invalid(error)) => ValidateReport::invalid(error.to_string(), Vec::new()),
        Err(LoadError::Read(error)) => return Err(error.into()),
    };
    output::emit_validate(&report, format)?;
    Ok(report.exit_code())
}
