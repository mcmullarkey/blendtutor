//! `blendtutor export-quarto` — print Quarto source on stdout: a lesson as a
//! fenced-div snippet or complete page, or the API key page (ADR-0019).
//!
//! A thin effectful shell (§2.2): read the lesson file when there is one,
//! delegate to the pure [`blendtutor_core::quarto_export`] transforms, write
//! the result to stdout, and return the exit code. No domain logic lives
//! here — that is `core`'s responsibility.

use std::path::{Path, PathBuf};
use std::process::ExitCode;

use blendtutor_core::lesson::{LoadError, read_lesson_file};
use blendtutor_core::quarto_export::{
    ExportShape, export_lesson_to_qmd, key_page_qmd, thin_lesson_warning,
};

/// What the author asked `export-quarto` to print. Built from the clap flags
/// in `main`, where clap has already refused invalid combinations (§1.3).
pub enum ExportRequest {
    /// Export the lesson at `path` in the given shape.
    Lesson {
        /// Path to the lesson YAML file.
        path: PathBuf,
        /// Snippet or complete page.
        shape: ExportShape,
    },
    /// Print the static API key page.
    KeyPage,
}

/// Print the Quarto source for `request` to stdout.
pub fn run(request: ExportRequest) -> anyhow::Result<ExitCode> {
    match request {
        ExportRequest::Lesson { path, shape } => export_lesson(&path, shape),
        ExportRequest::KeyPage => {
            print!("{}", key_page_qmd());
            Ok(ExitCode::SUCCESS)
        }
    }
}

/// Load the lesson at `path`, transform it to `.qmd` in `shape`, and write it
/// to stdout, printing any thin-lesson warning to stderr first.
///
/// A read failure (missing file, bad permissions) propagates to `main` as an
/// error. An invalid lesson (failed validation) prints the error to stderr
/// and returns a nonzero exit code — distinct from a read error, which is an
/// `anyhow` error.
fn export_lesson(path: &Path, shape: ExportShape) -> anyhow::Result<ExitCode> {
    let lesson = match read_lesson_file(path) {
        Ok(lesson) => lesson,
        Err(LoadError::Invalid(error)) => {
            eprintln!("{error}");
            return Ok(ExitCode::FAILURE);
        }
        Err(LoadError::Read(error)) => return Err(error.into()),
    };
    if let Some(warning) = thin_lesson_warning(&lesson) {
        eprintln!("{warning}");
    }
    print!("{}", export_lesson_to_qmd(&lesson, shape));
    Ok(ExitCode::SUCCESS)
}
