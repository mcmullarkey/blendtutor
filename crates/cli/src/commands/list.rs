//! `blendtutor list <course>` — discover the lessons in a course directory and
//! report each one's id, language, and title, including any that failed to load.

use std::path::Path;
use std::process::ExitCode;

use blendtutor_core::course::Course;

use crate::output::{self, ListReport, OutputFormat};

/// Open the course at `dir`, discover its lessons, and render the listing in
/// `format`.
///
/// The command shapes data and defers all formatting to the renderer (§5.1). A
/// missing or malformed manifest is a whole-course failure that propagates to
/// `main` as an error; an individual lesson that fails to load is reported as a
/// row in the listing, not a command failure, so discovery still exits zero.
pub fn run(dir: &Path, format: OutputFormat) -> anyhow::Result<ExitCode> {
    let course = Course::open(dir)?;
    let report = ListReport::from_discovery(course.discover());
    output::emit_list(&report, format)?;
    Ok(ExitCode::SUCCESS)
}
