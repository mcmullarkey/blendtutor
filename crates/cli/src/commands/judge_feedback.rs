//! `blendtutor judge-feedback <items.json>` — judge feedback that already exists
//! (ADR-0034).
//!
//! A thin shell: it reads the items file, builds the judge from `--judge-model`
//! (or the provider default), drives [`judge_items`] on a runtime, and renders
//! the report. No grading or code execution happens here or below it; the
//! criteria, consensus, and control are `core`'s.

use std::path::Path;
use std::process::ExitCode;

use anyhow::Context;
use blendtutor_core::eval::{FeedbackItem, JudgeConfig, judge_items};
use blendtutor_core::llm::ModelId;

use crate::commands::{PROVIDER_URL_VAR, grader_for};
use crate::output::{self, OutputFormat};

/// Judge every item in `items_path` with `samples` samples per judgment.
///
/// An unreadable or malformed items file, or a judge failure on any item,
/// propagates as an error (→ exit 1), naming the item that failed.
pub fn run(
    items_path: &Path,
    format: OutputFormat,
    samples: u32,
    model: Option<ModelId>,
) -> anyhow::Result<ExitCode> {
    let raw = std::fs::read_to_string(items_path)
        .with_context(|| format!("reading items {}", items_path.display()))?;
    let items: Vec<FeedbackItem> = serde_json::from_str(&raw)
        .with_context(|| format!("parsing items {}", items_path.display()))?;
    let config = JudgeConfig {
        judge: grader_for(model),
        samples: samples as usize,
    };
    let base_url = std::env::var(PROVIDER_URL_VAR).ok();
    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;
    let report = runtime.block_on(judge_items(&config, &items, base_url.as_deref()))?;
    output::emit_items(&report, format)?;
    Ok(ExitCode::SUCCESS)
}
