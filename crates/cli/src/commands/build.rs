//! `blendtutor build --target <target> <course> -o <out>` — assemble a static,
//! browser-deployable lesson site from a course (ADR-0008).

use std::path::Path;
use std::process::ExitCode;

use anyhow::Context;
use blendtutor_core::course::Course;
use blendtutor_core::site::{self, BuildTarget, EvalSummary};

/// The conventional name of the Slice-13 eval report a course bundles to have its
/// accuracy folded into the built site — the JSON `blendtutor eval --format json`
/// emits, dropped next to the course manifest.
const EVAL_REPORT_FILE: &str = "eval-report.json";

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

/// Open the course, load its lessons, fold in any eval report, plan the site, and
/// write it to `out`.
///
/// Gather → transform (pure) → persist: opening the course, reading the eval
/// report, and writing the site are the effectful shell around the pure
/// [`site::plan_site`]. A language/target mismatch is refused by `plan_site`
/// *before* `write_site` runs (the `?` short-circuits), so a refused build never
/// creates `out` (§1.3.1).
pub fn run(dir: &Path, target: BuildTarget, out: &Path) -> anyhow::Result<ExitCode> {
    let course = Course::open(dir)?;
    let lessons = course.load_lessons()?;
    let eval = load_eval_summary(dir)?;
    let site = site::plan_site(&lessons, target, &eval)?;
    site::write_site(out, &site)?;
    println!(
        "built {} lesson(s) for {target} into {}",
        lessons.len(),
        out.display()
    );
    Ok(ExitCode::SUCCESS)
}

/// Read the course's bundled eval report, if any, into an [`EvalSummary`].
///
/// The effectful edge of the pure fold (§2.3): an *absent* `eval-report.json` is
/// the represented [`EvalSummary::NotValidated`] state (§1.2) — not an error — so a
/// course that simply hasn't been evaluated still builds. A *present* report is
/// parsed as-is (§3.2); a present-but-unreadable one fails the build, so a corrupt
/// report never silently unvalidates a course.
fn load_eval_summary(dir: &Path) -> anyhow::Result<EvalSummary> {
    let path = dir.join(EVAL_REPORT_FILE);
    match std::fs::read_to_string(&path) {
        Ok(json) => Ok(site::eval_summary_from_report_json(&json)?),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(EvalSummary::NotValidated),
        Err(e) => Err(e).with_context(|| format!("reading {}", path.display())),
    }
}
