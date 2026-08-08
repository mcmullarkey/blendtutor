//! `blendtutor eval <lesson>` — score the feedback pipeline against a suite.
//!
//! A thin orchestration shell (§4.1, §5.1): it loads the lesson and its sibling
//! `eval_<lesson>.yaml` suite, drives the pure-cored [`run_eval`] on a runtime —
//! the *same* pipeline `run` uses, so the feedback scored is the feedback
//! shipped (§3.2) — and renders the report through the [`output`] seam. No
//! scoring, execution, or HTTP logic lives here; those are `core`'s.

use std::path::Path;
use std::process::ExitCode;

use anyhow::Context;
use blendtutor_core::eval::{parse_eval_suite, run_eval};
use blendtutor_core::lesson::read_lesson_file;
use blendtutor_core::llm::ProviderChoice;

use crate::commands::PROVIDER_URL_VAR;
use crate::commands::sibling_suite_path;
use crate::output::{self, OutputFormat};

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
pub fn run(
    lesson_path: &Path,
    format: OutputFormat,
    case: Option<usize>,
) -> anyhow::Result<ExitCode> {
    let lesson = read_lesson_file(lesson_path)?;
    let suite_path = sibling_suite_path(lesson_path);
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
    Ok(ExitCode::SUCCESS)
}
