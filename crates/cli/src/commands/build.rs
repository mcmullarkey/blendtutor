//! `blendtutor build --target <target> <course> -o <out>` — assemble a static,
//! browser-deployable lesson site from a course (ADR-0008).

use std::path::Path;
use std::process::ExitCode;

use blendtutor_core::course::Course;
use blendtutor_core::site::{self, BuildTarget};

/// Parse the `--target` value into a [`BuildTarget`].
///
/// A custom parser rather than deriving `clap::ValueEnum` on the core type, so
/// `core` stays free of the `clap` dependency — the dependency only ever points
/// cli → core (mirrors `new`'s `parse_language`).
pub fn parse_target(value: &str) -> Result<BuildTarget, String> {
    match value {
        "webr" => Ok(BuildTarget::Webr),
        "pyodide" => Ok(BuildTarget::Pyodide),
        other => Err(format!(
            "unknown build target {other:?}; expected `webr` or `pyodide`"
        )),
    }
}

/// Open the course, load its lessons, plan the site, and write it to `out`.
///
/// Gather → transform (pure) → persist: opening the course and writing the site
/// are the effectful shell around the pure [`site::plan_site`]. A language/target
/// mismatch is refused by `plan_site` *before* `write_site` runs (the `?`
/// short-circuits), so a refused build never creates `out` (§1.3.1).
pub fn run(dir: &Path, target: BuildTarget, out: &Path) -> anyhow::Result<ExitCode> {
    let course = Course::open(dir)?;
    let lessons = course.load_lessons()?;
    let site = site::plan_site(&lessons, target)?;
    site::write_site(out, &site)?;
    println!(
        "built {} lesson(s) for {target} into {}",
        lessons.len(),
        out.display()
    );
    Ok(ExitCode::SUCCESS)
}
