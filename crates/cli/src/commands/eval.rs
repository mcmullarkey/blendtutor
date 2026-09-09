//! `blendtutor eval <lesson>` — score the feedback pipeline against a suite.
//!
//! A thin orchestration shell (§4.1, §5.1): it loads the lesson and its sibling
//! `eval_<lesson>.yaml` suite, drives the pure-cored [`run_eval`] on a runtime —
//! the *same* pipeline `run` uses, so the feedback scored is the feedback
//! shipped (§3.2) — and renders the report through the [`output`] seam. With
//! `--write-report` it additionally persists the full-shape report as
//! `eval-report.json` at the course root — the durable artifact `build` folds
//! into the site's eval-results page. No scoring, execution, or HTTP logic
//! lives here; those are `core`'s.

use std::path::{Path, PathBuf};
use std::process::ExitCode;

use anyhow::{Context, anyhow};
use blendtutor_core::eval::{EvalReport, parse_eval_suite, run_eval};
use blendtutor_core::lesson::read_lesson_file;
use blendtutor_core::llm::ProviderChoice;
use blendtutor_core::smevals_gen::course_root_for;

use crate::commands::{PROVIDER_URL_VAR, sibling_suite_path};
use crate::output::{self, OutputFormat};

/// The conventional name of the durable eval report `--write-report` writes at
/// the course root — the same artifact `build` folds into the site's
/// eval-results page. The consumer's copy of this name is `build.rs`'s
/// `EVAL_REPORT_FILE` (the contract source); the duplication is deliberate —
/// extraction would touch the frozen build/core boundary — and the build
/// round-trip test fails on drift.
const EVAL_REPORT_FILE: &str = "eval-report.json";

/// Load the lesson and its sibling eval suite, score every case — or, with
/// `case`, only the one 1-based case — through the run pipeline, and render the
/// report.
///
/// A lesson read/parse failure, a missing or malformed suite, or a pipeline
/// failure on any case propagates as an error (→ exit 1). An out-of-range
/// `case` selection also propagates (→ exit 1, naming the suite size); a
/// non-numeric `--case` is rejected by clap at parse time (→ exit 2). The
/// command itself always succeeds (exit 0) when it produces a report: `eval`
/// measures feedback quality, it is not a pass/fail gate, so a low accuracy is
/// still a successful run. The provider is driven on a current-thread runtime
/// (the binary owns its async runtime; `core` stays a library).
///
/// With `write_report`, the full-shape report is additionally persisted as
/// `eval-report.json` at the course root — the nearest `blendtutor.toml`
/// ancestor of the lesson, found wherever the command runs from — after
/// scoring and rendering, as the command's single extra effect. A pre-existing
/// report is overwritten with a warning; `--case` combined with
/// `--write-report` is refused (a single-case report would masquerade as
/// course-level accuracy in the built site), and so is a missing course root.
/// Both refusals precede any scoring side effect.
pub fn run(
    lesson_path: &Path,
    format: OutputFormat,
    case: Option<usize>,
    write_report: bool,
) -> anyhow::Result<ExitCode> {
    // Canonicalize FIRST (mirrors eval_report.rs): course_root_for climbs the
    // lesson's ancestor chain, and a relative path from a CWD outside the
    // course would walk relative ancestors and miss the manifest.
    let lesson_path = lesson_path
        .canonicalize()
        .with_context(|| format!("resolving {} to an absolute path", lesson_path.display()))?;

    // Refusals precede any scoring side effect (§1.3.1): the durable artifact
    // is opt-in, so both refusal arms are checked before the provider runs.
    let course_root = if write_report {
        if case.is_some() {
            return Err(anyhow!(
                "--case N and --write-report are incompatible: a single-case report would \
                 render as the course-level accuracy in the built site; drop one of the flags"
            ));
        }
        Some(course_root_for(&lesson_path).ok_or_else(|| {
            anyhow!(
                "--write-report: no blendtutor.toml course root found above {} — an eval \
                 report is written at the course root",
                lesson_path.display()
            )
        })?)
    } else {
        None
    };

    let lesson = read_lesson_file(&lesson_path)?;
    let suite_path = sibling_suite_path(&lesson_path);
    let suite_yaml = std::fs::read_to_string(&suite_path)
        .with_context(|| format!("reading eval suite {}", suite_path.display()))?;
    let suite = parse_eval_suite(&suite_yaml)?;
    let base_url = std::env::var(PROVIDER_URL_VAR).ok();

    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;
    let report = runtime.block_on(run_eval(
        &lesson,
        &suite,
        ProviderChoice::default(),
        base_url.as_deref(),
        case,
    ))?;

    output::emit_eval(&report, format)?;

    if let Some(course_root) = course_root {
        let written = write_report_artifact(&report, &course_root)?;
        eprintln!("wrote eval-report.json to {}", written.display());
    }
    Ok(ExitCode::SUCCESS)
}

/// Serialize `report` and atomically write it as `eval-report.json` at
/// `course_root`, returning the written path.
///
/// The single effectful step of `--write-report` (§2.3): the bytes are exactly
/// `serde_json::to_string(&report)` — the same serialization `--format json`
/// stdout uses — so the file and the machine output cannot drift, and the file
/// is always full-shape regardless of `--format` (which controls stdout only).
/// The write is atomic in practice: the bytes land in a sibling `.tmp` file
/// that is renamed into place, so a failed write never leaves a half-written
/// report or a `.tmp` leftover. A pre-existing report is overwritten with a
/// warning (re-running eval on one's own course is the common case; committed
/// artifacts are git's concern, not exit codes').
fn write_report_artifact(report: &EvalReport, course_root: &Path) -> anyhow::Result<PathBuf> {
    let target = course_root.join(EVAL_REPORT_FILE);
    if target.exists() {
        eprintln!(
            "WARNING: overwriting existing eval report at {}",
            target.display()
        );
    }
    let tmp = course_root.join(format!(".{EVAL_REPORT_FILE}.tmp"));
    std::fs::write(
        &tmp,
        serde_json::to_string(report).expect("an EvalReport serializes to JSON infallibly"),
    )
    .with_context(|| format!("writing {}", tmp.display()))?;
    let renamed = std::fs::rename(&tmp, &target).or_else(|e| {
        // Windows only: rename fails while target exists. Accepted-untested on
        // unix CI (rename overwrites) — precedent: eval_report.rs replace_dir.
        if cfg!(windows) && target.exists() {
            std::fs::remove_file(&target)?;
            std::fs::rename(&tmp, &target)
        } else {
            Err(e)
        }
    });
    if let Err(e) = renamed {
        let _ = std::fs::remove_file(&tmp);
        return Err(e).with_context(|| format!("writing {}", target.display()));
    }
    Ok(target)
}
