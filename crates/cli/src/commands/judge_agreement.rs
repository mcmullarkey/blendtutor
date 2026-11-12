//! `blendtutor judge-agreement <judged.json> <labels.json>` — how well the
//! feedback judge agrees with people (ADR-0035).
//!
//! A thin shell: it reads `judge-feedback --format json` output and the human
//! labels, hands both to the pure [`JudgeAgreement::of`], and renders the
//! report. No provider call, so it can be re-run as labels accumulate.

use std::path::Path;
use std::process::ExitCode;

use anyhow::Context;
use blendtutor_core::eval::{HumanLabel, JudgeAgreement, JudgedFile};
use serde::de::DeserializeOwned;

use crate::output::{self, OutputFormat};

fn read_json<T: DeserializeOwned>(path: &Path, what: &str) -> anyhow::Result<T> {
    let raw = std::fs::read_to_string(path)
        .with_context(|| format!("reading {what} {}", path.display()))?;
    serde_json::from_str(&raw).with_context(|| format!("parsing {what} {}", path.display()))
}

/// Pair the judged items with the labels and render TPR/TNR per criterion. An
/// unreadable or malformed file is an error (→ exit 1).
pub fn run(
    judged_path: &Path,
    labels_path: &Path,
    format: OutputFormat,
) -> anyhow::Result<ExitCode> {
    let judged: JudgedFile = read_json(judged_path, "judged items")?;
    let labels: Vec<HumanLabel> = read_json(labels_path, "labels")?;
    output::emit_agreement(&JudgeAgreement::of(&judged, &labels), format)?;
    Ok(ExitCode::SUCCESS)
}
