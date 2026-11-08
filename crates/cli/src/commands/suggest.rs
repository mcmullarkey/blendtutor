//! `blendtutor suggest-misconceptions <lesson>`: propose misconceptions the eval
//! blueprint does not cover yet (ADR-0032).
//!
//! A thin shell. It loads the lesson and, if present, its sibling eval suite's
//! blueprint; asks the grader model for new misconceptions with example
//! submissions (`core::llm`); runs each example through the lesson's checks
//! (`core::eval`); and renders the result. Nothing is written: an author decides
//! what enters the blueprint.

use std::path::Path;
use std::process::ExitCode;

use blendtutor_core::eval::{SuggestionCheck, check_example, parse_eval_suite};
use blendtutor_core::lesson::read_lesson_file;
use blendtutor_core::llm::{ModelId, request_suggestions};
use serde::Serialize;

use crate::commands::{PROVIDER_URL_VAR, grader_for, sibling_suite_path};
use crate::output::{self, OutputFormat};

/// One suggestion as rendered: the model's label and example, plus the checks'
/// response to the example.
#[derive(Debug, Serialize)]
pub struct CheckedSuggestion {
    /// The proposed misconception.
    pub label: String,
    /// The model's minimal example submission.
    pub example_submission: String,
    /// What the lesson's checks make of the example.
    pub checks: SuggestionCheck,
}

/// Suggest misconceptions for the lesson at `lesson_path` and render them.
pub fn run(
    lesson_path: &Path,
    format: OutputFormat,
    model: Option<ModelId>,
) -> anyhow::Result<ExitCode> {
    let lesson = read_lesson_file(lesson_path)?;
    let suite_path = sibling_suite_path(lesson_path);
    let blueprint = match std::fs::read_to_string(&suite_path) {
        Ok(yaml) => parse_eval_suite(&yaml)?.blueprint().to_vec(),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Vec::new(),
        Err(e) => return Err(e.into()),
    };
    let covered: Vec<&str> = blueprint.iter().map(|m| m.label.as_str()).collect();
    let base_url = std::env::var(PROVIDER_URL_VAR).ok();
    let grader = grader_for(model);

    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;
    let suggestions = runtime.block_on(request_suggestions(
        &grader,
        &lesson,
        &covered,
        base_url.as_deref(),
    ))?;
    let checked: Vec<CheckedSuggestion> = suggestions
        .into_iter()
        .map(|s| {
            let checks = runtime.block_on(check_example(&lesson, &s.example_submission));
            CheckedSuggestion {
                label: s.label,
                example_submission: s.example_submission,
                checks,
            }
        })
        .collect();

    output::emit_suggestions(&checked, format)?;
    Ok(ExitCode::SUCCESS)
}
