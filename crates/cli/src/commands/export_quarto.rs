//! `blendtutor export-quarto <lesson.yaml>` — convert a lesson YAML file to a
//! Quarto `.qmd` fenced-div snippet on stdout.
//!
//! A thin effectful shell (§2.2): read the file, delegate to the pure
//! [`blendtutor_core::quarto_export::export_lesson_to_qmd`] transform, write
//! the result to stdout, and return the exit code. No domain logic lives
//! here — that is `core`'s responsibility.

use std::path::Path;
use std::process::ExitCode;

use blendtutor_core::lesson::{LoadError, read_lesson_file};
use blendtutor_core::quarto_export::export_lesson_to_qmd;

/// Load the lesson at `path`, transform it to a `.qmd` snippet, and write it
/// to stdout.
///
/// A read failure (missing file, bad permissions) propagates to `main` as an
/// error. An invalid lesson (failed validation) prints the error to stderr
/// and returns a nonzero exit code — distinct from a read error, which is an
/// `anyhow` error.
pub fn run(path: &Path) -> anyhow::Result<ExitCode> {
    let lesson = match read_lesson_file(path) {
        Ok(lesson) => lesson,
        Err(LoadError::Invalid(error)) => {
            eprintln!("{error}");
            return Ok(ExitCode::FAILURE);
        }
        Err(LoadError::Read(error)) => return Err(error.into()),
    };
    let qmd = export_lesson_to_qmd(&lesson);
    print!("{qmd}");
    Ok(ExitCode::SUCCESS)
}
