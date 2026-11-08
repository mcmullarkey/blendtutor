//! `blendtutor eval-compare <baseline> <candidate>`: paired comparison of two
//! grading prompts and the pre-registered ship decision (ADR-0029).
//!
//! A thin orchestration shell. It loads both lessons, refuses a candidate whose
//! language or checks differ from the baseline's (that would compare lessons,
//! not graders), grades the baseline's eval suite once per lesson through the
//! same `run_eval` pipeline, and hands both reports to `core`'s pure
//! [`CompareReport`]. Exit 0 = ship, 3 = hold; 1 is reserved for errors and 2 is
//! clap's usage-error code.

use std::path::Path;
use std::process::ExitCode;

use anyhow::{Context, bail};
use blendtutor_core::eval::{CompareReport, Decision, Margin, parse_eval_suite, run_eval};
use blendtutor_core::lesson::read_lesson_file;
use blendtutor_core::llm::ModelId;

use crate::commands::{PROVIDER_URL_VAR, grader_for, sibling_suite_path};
use crate::output::{self, OutputFormat};

/// The exit code for a "hold" decision: distinct from success, from errors (1),
/// and from clap's usage errors (2), so CI can gate on it.
const HOLD: u8 = 3;

/// Compare the candidate lesson's grading prompt against the baseline's on the
/// baseline's eval suite, render the comparison, and exit by its decision.
pub fn run(
    baseline_path: &Path,
    candidate_path: &Path,
    format: OutputFormat,
    margin: Margin,
    model: Option<ModelId>,
) -> anyhow::Result<ExitCode> {
    let baseline = read_lesson_file(baseline_path)?;
    let candidate = read_lesson_file(candidate_path)?;
    // Guard before any paid grading (§1.3.1).
    if baseline.language != candidate.language || baseline.checks != candidate.checks {
        bail!(
            "the candidate must have the same checks and language as the baseline \
             ({} vs {}); comparing different checks would compare lessons, not graders",
            baseline_path.display(),
            candidate_path.display()
        );
    }
    let suite_path = sibling_suite_path(baseline_path);
    let suite_yaml = std::fs::read_to_string(&suite_path)
        .with_context(|| format!("reading eval suite {}", suite_path.display()))?;
    let suite = parse_eval_suite(&suite_yaml)?;
    let base_url = std::env::var(PROVIDER_URL_VAR).ok();
    let grader = grader_for(model);

    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;
    let base_report = runtime.block_on(run_eval(
        &baseline,
        &suite,
        &grader,
        base_url.as_deref(),
        None,
    ))?;
    let cand_report = runtime.block_on(run_eval(
        &candidate,
        &suite,
        &grader,
        base_url.as_deref(),
        None,
    ))?;
    let comparison = CompareReport::new(base_report, cand_report, margin)?;

    output::emit_compare(&comparison, format)?;
    Ok(match comparison.decision() {
        Decision::Ship => ExitCode::SUCCESS,
        Decision::Hold => ExitCode::from(HOLD),
    })
}
