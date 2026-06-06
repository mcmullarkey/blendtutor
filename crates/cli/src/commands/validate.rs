//! `blendtutor validate <path>` — load a lesson file and report whether it is
//! valid, exiting zero on success and non-zero (with the specific problem) on
//! failure.

use std::path::Path;

/// Load and validate the lesson at `path`, printing an OK line on success.
///
/// All parsing and validation live in `blendtutor-core`; on failure the typed
/// `LoadError` propagates to `main`, which prints it to stderr and exits
/// non-zero. The error's message names the offending field or rule.
pub fn run(path: &Path) -> anyhow::Result<()> {
    let lesson = blendtutor_core::lesson::read_lesson_file(path)?;
    println!("OK: \"{}\" is a valid lesson", lesson.lesson_name);
    Ok(())
}
