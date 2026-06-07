//! `blendtutor init <dir>` — scaffold a new, ready-to-edit course directory.
//!
//! A thin shell over [`blendtutor_core::scaffold`]: hand the target to the core
//! scaffolder and report success. The decision of *what* a course contains lives
//! in `core` (§4.1); this command only names the target and frames the outcome.

use std::path::Path;
use std::process::ExitCode;

use blendtutor_core::scaffold::scaffold_course;

/// Scaffold a course into `dir`.
///
/// Delegates to [`scaffold_course`]; a write failure (or, once the guard lands, a
/// refusal) propagates to `main` as an error with a nonzero exit, so this command
/// never half-reports success.
pub fn run(dir: &Path) -> anyhow::Result<ExitCode> {
    scaffold_course(dir)?;
    println!("Scaffolded a new course in {}", dir.display());
    Ok(ExitCode::SUCCESS)
}
