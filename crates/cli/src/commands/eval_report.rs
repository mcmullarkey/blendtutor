//! `blendtutor eval-report <lesson>` — generate a static smevals eval report.
//!
//! A thin orchestration shell (§5.1): generate the smevals eval dir (AC-2's
//! pure generator + effectful `write_eval_dir`), drive the pinned `uvx
//! smevals==0.2.0` package to run and build the report, and publish it under
//! `<repo>/docs/evals/<lesson>/` — the directory AC-6 assembles into the Pages
//! artifact. No eval, grading, or model logic lives here; those are `core`'s.
//!
//! Exit-code semantics (grade-fail is evidence, not a gate):
//! - `run` exiting non-zero still proceeds to `build` when it recorded runs
//!   (`runs/` non-empty) — a low accuracy is a result, not a failure. Only a
//!   `run` that exited non-zero *without* producing runs fails the command,
//!   naming the `run` stage.
//! - A `build` failure always fails the command, naming the `build` stage, and
//!   discards the partial temp build so a prior committed report survives.
//!   When the run stage also failed, its exit code rides in the build error's
//!   context.
//! - A missing `uvx` on PATH is a clean, stage-named error (never a panic),
//!   with an install hint.

use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode, ExitStatus};

use anyhow::{Context, anyhow};
use blendtutor_core::eval::parse_eval_suite;
use blendtutor_core::lesson::read_lesson_file;
use blendtutor_core::smevals_gen::{course_root_for, lesson_id_from_path, write_eval_dir};

use crate::commands::sibling_suite_path;

/// The smevals package pinned for `uvx` — the single source for the pin in
/// both the `run` and `build` invocations (the round-trip is verified by the
/// real-package smoke test, evidence in `docs/evidence/198/`).
const SMEVALS_PIN: &str = "smevals==0.2.0";
/// The docs subdirectory (under the repo root) that holds published reports.
const EVALS_DIR_REL: &str = "docs/evals";
/// The per-stage error contexts, asserted by the integration suite.
const STAGE_GENERATE: &str = "generate";
const STAGE_RUN: &str = "run";
const STAGE_BUILD: &str = "build";

/// Generate an eval dir, run it through `smevals`, and publish the report.
///
/// Stage-named failures (→ exit 1): the generate phase names `generate`, the
/// run phase names `run` (only when it produced no runs), and the build phase
/// names `build`. A successful report exits 0.
pub fn run(lesson_path: &Path) -> anyhow::Result<ExitCode> {
    // ---- generate stage: lesson → suite → eval dir ---------------------
    let lesson = read_lesson_file(lesson_path)
        .with_context(|| format!("{STAGE_GENERATE}: reading lesson"))?;
    let lesson_id = lesson_id_from_path(lesson_path).ok_or_else(|| {
        anyhow!(
            "{STAGE_GENERATE}: cannot derive a lesson id from {}",
            lesson_path.display()
        )
    })?;
    let suite_yaml =
        std::fs::read_to_string(sibling_suite_path(lesson_path)).with_context(|| {
            format!(
                "{STAGE_GENERATE}: reading eval suite for {}",
                lesson_path.display()
            )
        })?;
    let suite = parse_eval_suite(&suite_yaml)
        .with_context(|| format!("{STAGE_GENERATE}: parsing eval suite"))?;
    let course_root = course_root_for(lesson_path)
        .ok_or_else(|| {
            anyhow!(
                "{STAGE_GENERATE}: no blendtutor.toml course root found above {}",
                lesson_path.display()
            )
        })?
        .canonicalize()
        .with_context(|| format!("{STAGE_GENERATE}: resolving course root"))?;
    let gen_dir = course_root.join(".smevals");
    // Stale runs are false evidence: a previous report's runs/ would be built
    // into the next report, so clean before regenerating.
    clean_stale(&gen_dir).with_context(|| format!("{STAGE_GENERATE}: cleaning stale eval dir"))?;
    write_eval_dir(&course_root, &lesson, &suite, lesson_id)
        .with_context(|| format!("{STAGE_GENERATE}: writing eval dir"))?;

    // ---- run stage: grade every case through the pinned smevals ---------
    let run_status = run_uvx(
        &[OsStr::new("run"), gen_dir.as_os_str(), OsStr::new("-g")],
        STAGE_RUN,
    )?;
    let mut run_failed: Option<ExitStatus> = None;
    if !run_status.success() {
        if has_runs(&gen_dir) {
            // A grade verdict, not a harness failure: the recorded runs are
            // the evidence, so proceed to build regardless of the exit code.
            println!(
                "smevals run exited {run_status} after recording runs — \
                 treating the grade as evidence and building the report"
            );
            run_failed = Some(run_status);
        } else {
            return Err(anyhow!(
                "{STAGE_RUN}: smevals run exited {run_status} without recording any \
                 runs in {}",
                gen_dir.display()
            ));
        }
    } else {
        println!("smevals run: ok ({})", gen_dir.display());
    }

    // ---- build stage: publish the report into docs/evals/<lesson> -------
    let repo_root = repo_root_for(&course_root).ok_or_else(|| {
        anyhow!(
            "{STAGE_BUILD}: no repo root (a directory with .git) found above {} — \
             cannot locate docs/evals/",
            course_root.display()
        )
    })?;
    let docs_evals = repo_root.join(EVALS_DIR_REL).join(lesson_id);
    let temp_dir = docs_evals.with_file_name(format!(".{lesson_id}.tmp"));
    std::fs::create_dir_all(&docs_evals)
        .with_context(|| format!("{STAGE_BUILD}: creating docs/evals"))?;
    let build_status = run_uvx(
        &[
            OsStr::new("build"),
            gen_dir.as_os_str(),
            OsStr::new("-o"),
            temp_dir.as_os_str(),
        ],
        STAGE_BUILD,
    )?;
    if !build_status.success() {
        // Discard the partial temp build; any prior committed report survives.
        let _ = std::fs::remove_dir_all(&temp_dir);
        return Err(match run_failed {
            Some(run_status) => anyhow!(
                "{STAGE_BUILD}: smevals build exited {build_status} \
                 (run stage previously exited {run_status})"
            ),
            None => anyhow!("{STAGE_BUILD}: smevals build exited {build_status}"),
        });
    }
    replace_dir(&temp_dir, &docs_evals)
        .with_context(|| format!("{STAGE_BUILD}: publishing report"))?;
    println!("smevals build: ok");
    println!("eval report: {}", docs_evals.display());
    Ok(ExitCode::SUCCESS)
}

/// Invoke `uvx smevals==0.2.0 <args…>` and return its exit status.
///
/// A missing `uvx` on PATH surfaces as a clean, stage-named error — never a
/// panic: the spawn error is wrapped with the pin and an install hint, so the
/// message names `uvx` for the user.
fn run_uvx(args: &[&OsStr], stage: &str) -> anyhow::Result<ExitStatus> {
    Command::new("uvx")
        .arg(SMEVALS_PIN)
        .args(args)
        .status()
        .with_context(|| {
            format!(
                "{stage}: failed to spawn `uvx {SMEVALS_PIN}` — is uvx installed? \
                 Install with `curl -LsSf https://astral.sh/uv/install.sh | sh`"
            )
        })
}

/// Whether the eval dir holds at least one recorded run — the signal that a
/// non-zero `smevals run` exit was a grade verdict rather than a harness
/// failure. A missing or empty `runs/` means no usable artifacts.
fn has_runs(gen_dir: &Path) -> bool {
    match std::fs::read_dir(gen_dir.join("runs")) {
        Ok(entries) => entries.flatten().next().is_some(),
        Err(_) => false,
    }
}

/// Remove a previous report's `.smevals/` tree so stale runs are never built
/// into the new report. A missing tree is fine.
fn clean_stale(gen_dir: &Path) -> std::io::Result<()> {
    match std::fs::remove_dir_all(gen_dir) {
        Ok(()) => Ok(()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(e) => Err(e),
    }
}

/// Replace `target` (a prior committed report) with the freshly built `source`
/// (the temp build): remove the old report, then rename the new one into
/// place. Rename is the atomic step — the temp is a sibling of the target, so
/// the two share a filesystem. On success the new report is complete and the
/// old one is gone; on failure (the caller has already discarded `source`) a
/// prior committed report survives intact.
fn replace_dir(source: &Path, target: &Path) -> std::io::Result<()> {
    match std::fs::symlink_metadata(target) {
        Ok(meta) if meta.is_dir() => std::fs::remove_dir_all(target)?,
        Ok(_) => std::fs::remove_file(target)?,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(e) => return Err(e),
    }
    std::fs::rename(source, target)
}

/// The repo root above `course_root` — the nearest ancestor with a `.git`
/// directory — which owns `docs/evals/` (the directory AC-6 assembles into the
/// Pages artifact). `None` when the course sits outside a git checkout.
fn repo_root_for(course_root: &Path) -> Option<PathBuf> {
    let mut current = Some(course_root);
    while let Some(dir) = current {
        if dir.join(".git").exists() {
            return Some(dir.to_path_buf());
        }
        current = dir.parent();
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn repo_root_is_the_nearest_ancestor_with_a_git_dir() {
        let root = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(root.path().join(".git")).unwrap();
        let course = root.path().join("examples").join("demo-course");
        std::fs::create_dir_all(&course).unwrap();

        assert_eq!(repo_root_for(&course).unwrap(), root.path());
        assert_eq!(
            repo_root_for(&course.join("nested").join("deeper")).unwrap(),
            root.path()
        );
        assert_eq!(repo_root_for(Path::new("/nonexistent/course")), None);
    }

    #[test]
    fn replace_dir_swaps_a_prior_report_for_the_new_one() {
        let dir = tempfile::tempdir().unwrap();
        let old = dir.path().join("report");
        let new = dir.path().join(".report.tmp");
        std::fs::create_dir_all(&old).unwrap();
        std::fs::write(old.join("KEEP.txt"), "committed\n").unwrap();
        std::fs::create_dir_all(&new).unwrap();
        std::fs::write(new.join("index.html"), "<h1>new</h1>\n").unwrap();

        replace_dir(&new, &old).unwrap();

        assert!(
            !old.join("KEEP.txt").exists(),
            "the prior report is replaced"
        );
        assert!(
            old.join("index.html").is_file(),
            "the new report is in place"
        );
        assert!(!new.exists(), "the temp is consumed by the rename");
    }

    #[test]
    fn replace_dir_publishes_into_a_fresh_location() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("report");
        let source = dir.path().join(".report.tmp");
        std::fs::create_dir_all(&source).unwrap();
        std::fs::write(source.join("index.html"), "<h1>first</h1>\n").unwrap();

        replace_dir(&source, &target).unwrap();

        assert!(target.join("index.html").is_file());
        assert!(!source.exists());
    }

    #[test]
    fn clean_stale_removes_or_ignores_an_absent_tree() {
        let dir = tempfile::tempdir().unwrap();
        let eval_dir = dir.path().join(".smevals");
        std::fs::create_dir_all(&eval_dir).unwrap();
        std::fs::write(eval_dir.join("stale.txt"), "stale\n").unwrap();
        clean_stale(&eval_dir).unwrap();
        assert!(!eval_dir.exists(), "the stale tree is removed");
        // A second clean on the now-absent tree is a no-op, not an error.
        clean_stale(&eval_dir).unwrap();
    }

    #[test]
    fn has_runs_is_true_only_for_a_non_empty_runs_dir() {
        let dir = tempfile::tempdir().unwrap();
        assert!(!has_runs(dir.path()), "no runs/ dir → no usable artifacts");
        std::fs::create_dir_all(dir.path().join("runs")).unwrap();
        assert!(
            !has_runs(dir.path()),
            "an empty runs/ dir → no usable artifacts"
        );
        std::fs::write(dir.path().join("runs/run-1.json"), "{}").unwrap();
        assert!(has_runs(dir.path()), "a recorded run → usable artifacts");
    }
}
