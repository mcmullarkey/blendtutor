//! `blendtutor judge-feedback <items.json>` — judge feedback that already exists
//! (ADR-0034).
//!
//! A thin shell: it reads the items file, builds the judge from `--judge-model`
//! (or the provider default), drives [`judge_item`] on a runtime item by item,
//! and renders the report. With `--checkpoint`, each finished judgment is
//! appended to a JSON-lines file as soon as it is made, and items already there
//! are not judged again, so an interrupted run resumes instead of starting
//! over. No grading or code execution happens here or below it; the criteria,
//! consensus, and control are `core`'s.

use std::collections::HashMap;
use std::io::Write;
use std::path::Path;
use std::process::ExitCode;

use anyhow::Context;
use blendtutor_core::eval::{FeedbackItem, ItemJudgment, ItemsReport, JudgeConfig, judge_item};
use blendtutor_core::llm::ModelId;

use crate::commands::{PROVIDER_URL_VAR, grader_for};
use crate::output::{self, OutputFormat};

/// Judgments already in `path`, by item id; none when the file does not exist.
fn read_checkpoint(path: &Path) -> anyhow::Result<HashMap<String, ItemJudgment>> {
    let raw = match std::fs::read_to_string(path) {
        Ok(raw) => raw,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(HashMap::new()),
        Err(e) => return Err(e).with_context(|| format!("reading checkpoint {}", path.display())),
    };
    raw.lines()
        .filter(|line| !line.trim().is_empty())
        .enumerate()
        .map(|(n, line)| {
            let judged: ItemJudgment = serde_json::from_str(line)
                .with_context(|| format!("parsing checkpoint {} line {}", path.display(), n + 1))?;
            Ok((judged.id().to_string(), judged))
        })
        .collect()
}

/// Append one judgment to the checkpoint as a JSON line, flushed to disk.
fn append_checkpoint(path: &Path, judged: &ItemJudgment) -> anyhow::Result<()> {
    let mut file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .with_context(|| format!("opening checkpoint {}", path.display()))?;
    let line = serde_json::to_string(judged).expect("an ItemJudgment serializes infallibly");
    writeln!(file, "{line}")?;
    file.sync_data()?;
    Ok(())
}

/// Judge every item in `items_path` with `samples` samples per judgment,
/// skipping and appending to `checkpoint` when given.
///
/// An unreadable or malformed items or checkpoint file, or a judge failure on
/// any item, propagates as an error (→ exit 1), naming the item that failed.
/// Judgments made before a failure stay in the checkpoint.
pub fn run(
    items_path: &Path,
    format: OutputFormat,
    samples: u32,
    model: Option<ModelId>,
    checkpoint: Option<&Path>,
) -> anyhow::Result<ExitCode> {
    let raw = std::fs::read_to_string(items_path)
        .with_context(|| format!("reading items {}", items_path.display()))?;
    let items: Vec<FeedbackItem> = serde_json::from_str(&raw)
        .with_context(|| format!("parsing items {}", items_path.display()))?;
    let mut done = match checkpoint {
        Some(path) => read_checkpoint(path)?,
        None => HashMap::new(),
    };
    let config = JudgeConfig {
        judge: grader_for(model),
        samples: samples as usize,
    };
    let base_url = std::env::var(PROVIDER_URL_VAR).ok();
    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;
    let mut judged = Vec::with_capacity(items.len());
    for item in &items {
        if let Some(previous) = done.remove(item.id()) {
            judged.push(previous);
            continue;
        }
        let fresh = runtime.block_on(judge_item(&config, item, base_url.as_deref()))?;
        if let Some(path) = checkpoint {
            append_checkpoint(path, &fresh)?;
        }
        judged.push(fresh);
    }
    output::emit_items(&ItemsReport::of(judged), format)?;
    Ok(ExitCode::SUCCESS)
}
