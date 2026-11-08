//! Misconception suggestions: ask the grader model what learners might get wrong
//! that the eval blueprint does not cover yet (ADR-0032).
//!
//! A content-validity helper. [`build_suggestion_prompt`] is pure: the task, the
//! success criteria, the checks, and the misconceptions already covered (so the
//! model proposes new ones). [`request_suggestions`] sends it through the shared
//! `extract` path. This module does not judge suggestions; running each
//! example against the lesson's checks is `eval::suggestion`'s job, and a person
//! decides what enters the blueprint.

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use super::feedback::{FeedbackError, extract};
use super::grader::Grader;
use crate::lesson::{Language, Lesson};

/// One proposed misconception and a minimal submission that shows it.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
pub struct SuggestedMisconception {
    /// A short description of the mistake, in the style of a blueprint label.
    pub label: String,
    /// A minimal, runnable submission that exhibits only this mistake.
    pub example_submission: String,
}

/// The tool-call DTO the model fills.
#[derive(Debug, Deserialize, Serialize, JsonSchema)]
struct Suggestions {
    /// Misconceptions not already covered, each with an example submission.
    misconceptions: Vec<SuggestedMisconception>,
}

/// Build the suggestion prompt for `lesson`, listing the `covered` misconception
/// labels so the model proposes different ones.
pub fn build_suggestion_prompt(lesson: &Lesson, covered: &[&str]) -> String {
    let language = match lesson.language {
        Language::R => "R",
        Language::Python => "Python",
    };
    let mut lines = vec![
        "You help write evaluation items for a programming exercise.".to_string(),
        String::new(),
        "Task:".to_string(),
        lesson.exercise.prompt.trim_end().to_string(),
        String::new(),
    ];
    if let Some(criteria) = lesson
        .exercise
        .success_criteria
        .as_deref()
        .filter(|c| !c.trim().is_empty())
    {
        lines.extend([
            "Success criteria:".to_string(),
            criteria.trim_end().to_string(),
            String::new(),
        ]);
    }
    if !lesson.checks.is_empty() {
        lines.push("Automated checks:".to_string());
        lines.extend(lesson.checks.iter().map(|c| format!("- {c}")));
        lines.push(String::new());
    }
    lines.push("Misconceptions already covered:".to_string());
    if covered.is_empty() {
        lines.push("- (none yet)".to_string());
    } else {
        lines.extend(covered.iter().map(|c| format!("- {c}")));
    }
    lines.extend([
        String::new(),
        format!(
            "List up to 5 other distinct misconceptions a beginner might have about this \
             exercise. For each, give a short label and a minimal {language} submission that \
             exhibits only that misconception and still runs without an error. Prefer \
             conceptual mistakes over typos, and include mistakes the automated checks would \
             not catch."
        ),
    ]);
    lines.join("\n")
}

/// Ask `grader` for misconception suggestions for `lesson`.
pub async fn request_suggestions(
    grader: &Grader,
    lesson: &Lesson,
    covered: &[&str],
    base_url_override: Option<&str>,
) -> Result<Vec<SuggestedMisconception>, FeedbackError> {
    let prompt = build_suggestion_prompt(lesson, covered);
    let suggestions: Suggestions = extract(grader, &prompt, base_url_override).await?;
    Ok(suggestions.misconceptions)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lesson() -> Lesson {
        Lesson::parse(
            "lesson_name: x\nlanguage: R\nexercise:\n  prompt: \"Write add_two.\"\n  success_criteria: \"- returns the sum\"\nchecks:\n  - \"stopifnot(add_two(2, 3) == 5)\"\n",
        )
        .expect("valid")
    }

    #[test]
    fn the_prompt_names_the_task_criteria_checks_and_covered_misconceptions() {
        let prompt = build_suggestion_prompt(&lesson(), &["returns x", "uses integer division"]);
        for part in [
            "Write add_two.",
            "- returns the sum",
            "- stopifnot(add_two(2, 3) == 5)",
            "- returns x",
            "- uses integer division",
            "minimal R submission",
        ] {
            assert!(prompt.contains(part), "missing {part:?}:\n{prompt}");
        }
    }

    #[test]
    fn an_empty_blueprint_says_so() {
        assert!(build_suggestion_prompt(&lesson(), &[]).contains("- (none yet)"));
    }
}
