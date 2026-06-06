//! The lesson schema: a typed model and its validating parse boundary.
//!
//! Holds the [`Lesson`]/[`Exercise`] types, the [`Language`] enum, the
//! [`LessonId`] newtype, and [`Lesson::parse`] — the only constructor, which
//! deserializes a YAML document and then enforces the semantic rules (notably
//! the `{student_code}` placeholder) so a constructed [`Lesson`] is valid by
//! type (ADR-0003). This module does not execute code, call LLMs, or render
//! output; that belongs to the runner, provider, and cli layers.

use std::error::Error;
use std::fmt;
use std::path::Path;

use serde::{Deserialize, Serialize};

/// The literal placeholder a lesson's evaluation prompt must contain so the
/// runner can splice the learner's submission into it before grading.
const STUDENT_CODE_PLACEHOLDER: &str = "{student_code}";

/// The language a lesson is authored in.
///
/// An enum rather than a free string, so an unknown language is rejected at the
/// parse boundary instead of travelling downstream as data (§1.2). The variant
/// names match the YAML/JSON spelling (`R`, `Python`).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Language {
    /// The R language.
    R,
    /// The Python language.
    Python,
}

/// A lesson's stable identity.
///
/// A newtype over `String` so a lesson id is never confused with arbitrary text
/// (§1.4). In v1 it carries the `lesson_name` value; a distinct slug id arrives
/// with lesson discovery (Slice 6, see ADR-0003).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct LessonId(pub String);

/// The exercise a lesson poses.
///
/// The two required prompts carry the learner-facing task and the grading
/// template; the optional fields are authoring aids. `llm_evaluation_prompt`
/// must contain the `{student_code}` placeholder — enforced by [`Lesson::parse`],
/// not by this type alone.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Exercise {
    /// What the learner is asked to do. Required.
    pub prompt: String,
    /// The prompt sent to the LLM to grade a submission. Required; must contain
    /// the `{student_code}` placeholder.
    pub llm_evaluation_prompt: String,
    /// Optional starter code shown to the learner.
    #[serde(default)]
    pub code_template: Option<String>,
    /// Optional example invocations.
    #[serde(default)]
    pub example_usage: Option<String>,
    /// Optional human-readable success criteria.
    #[serde(default)]
    pub success_criteria: Option<String>,
}

/// A single lesson: its identity, language, and exercise, with optional prose.
///
/// Construct one only through [`Lesson::parse`] — a value of this type is, by
/// construction, structurally complete and semantically valid (ADR-0003).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Lesson {
    /// The lesson's identity (v1: its name). Required.
    pub lesson_name: LessonId,
    /// The language the lesson is authored in. Required.
    pub language: Language,
    /// The exercise the lesson poses. Required.
    pub exercise: Exercise,
    /// Optional one-line summary.
    #[serde(default)]
    pub description: Option<String>,
    /// Optional pointer to a textbook section.
    #[serde(default)]
    pub textbook_reference: Option<String>,
}

/// Why a YAML document is not a valid lesson.
#[derive(Debug)]
pub enum ValidationError {
    /// The document is not structurally a lesson: a required field is missing,
    /// a value has the wrong type, or the YAML is malformed. Carries the
    /// underlying parser message, which names the offending field.
    Parse(String),
    /// `exercise.llm_evaluation_prompt` is present but lacks the literal
    /// `{student_code}` placeholder, so a learner's submission could never be
    /// inserted into it.
    MissingStudentCodePlaceholder,
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValidationError::Parse(msg) => write!(f, "invalid lesson: {msg}"),
            ValidationError::MissingStudentCodePlaceholder => write!(
                f,
                "exercise.llm_evaluation_prompt must contain the literal \
                 {STUDENT_CODE_PLACEHOLDER} placeholder so the learner's \
                 submission can be inserted",
            ),
        }
    }
}

impl Error for ValidationError {}

impl Lesson {
    /// Parse a lesson from a YAML document.
    ///
    /// The pure parse boundary (§2.1, §1.3.1): it deserializes the document into
    /// the typed model — a missing required field or wrong type yields
    /// [`ValidationError::Parse`] naming the field — then runs
    /// [`validate_semantics`](Lesson::validate_semantics) for the rules structure
    /// alone cannot express. Returns a [`Lesson`] only if both succeed.
    pub fn parse(yaml: &str) -> Result<Lesson, ValidationError> {
        let lesson: Lesson =
            serde_saphyr::from_str(yaml).map_err(|e| ValidationError::Parse(e.to_string()))?;
        lesson.validate_semantics()?;
        Ok(lesson)
    }

    /// Enforce the semantic rules that structure alone cannot.
    ///
    /// Currently a single rule: the evaluation prompt must contain the
    /// `{student_code}` placeholder. Split from the structural deserialize so
    /// each name covers its body (§5.1).
    fn validate_semantics(&self) -> Result<(), ValidationError> {
        if !self
            .exercise
            .llm_evaluation_prompt
            .contains(STUDENT_CODE_PLACEHOLDER)
        {
            return Err(ValidationError::MissingStudentCodePlaceholder);
        }
        Ok(())
    }
}

/// Why a lesson file could not be loaded.
#[derive(Debug)]
pub enum LoadError {
    /// The file could not be read (missing, permissions, or not UTF-8).
    Read(std::io::Error),
    /// The file was read, but its contents are not a valid lesson.
    Invalid(ValidationError),
}

impl fmt::Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoadError::Read(e) => write!(f, "could not read lesson file: {e}"),
            LoadError::Invalid(e) => write!(f, "{e}"),
        }
    }
}

impl Error for LoadError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            LoadError::Read(e) => Some(e),
            LoadError::Invalid(e) => Some(e),
        }
    }
}

/// Read and parse a lesson from a file on disk.
///
/// The thin effectful shell over the pure [`Lesson::parse`] (§2.2): it performs
/// the file read, then delegates all structure and validation to `parse`. The
/// two failure modes stay distinct in the type so a read error is never
/// mistaken for a validation error (§3.1).
pub fn read_lesson_file(path: &Path) -> Result<Lesson, LoadError> {
    let text = std::fs::read_to_string(path).map_err(LoadError::Read)?;
    Lesson::parse(&text).map_err(LoadError::Invalid)
}

#[cfg(test)]
mod tests {
    use super::*;

    const VALID_YAML: &str = r#"
lesson_name: "Adder"
language: R
description: "Add two numbers"
textbook_reference: "Chapter 2"
exercise:
  prompt: "Write a function add_two(x, y)."
  code_template: "add_two <- function(x, y) {}"
  example_usage: "add_two(3, 5)  # 8"
  success_criteria: "Returns the sum of its two arguments."
  llm_evaluation_prompt: "Grade this submission:\n{student_code}\nReply with feedback."
"#;

    const MISSING_EVAL_PROMPT_YAML: &str = r#"
lesson_name: "Adder"
language: R
exercise:
  prompt: "Write a function add_two(x, y)."
"#;

    const PROMPT_WITHOUT_PLACEHOLDER_YAML: &str = r#"
lesson_name: "Adder"
language: R
exercise:
  prompt: "Write a function add_two(x, y)."
  llm_evaluation_prompt: "Grade the student's submission and reply with feedback."
"#;

    #[test]
    fn parse_accepts_valid_lesson() {
        let lesson = Lesson::parse(VALID_YAML).expect("valid lesson should parse");
        assert_eq!(lesson.lesson_name, LessonId("Adder".to_string()));
        assert_eq!(lesson.language, Language::R);
    }

    #[test]
    fn parse_rejects_missing_required_field_naming_it() {
        let err = Lesson::parse(MISSING_EVAL_PROMPT_YAML)
            .expect_err("a lesson without llm_evaluation_prompt is invalid");
        let msg = err.to_string();
        assert!(
            msg.contains("llm_evaluation_prompt"),
            "error should name the missing field, got: {msg}"
        );
    }

    #[test]
    fn parse_rejects_prompt_missing_student_code_placeholder() {
        let err = Lesson::parse(PROMPT_WITHOUT_PLACEHOLDER_YAML)
            .expect_err("a prompt without {student_code} is invalid");
        let msg = err.to_string();
        assert!(
            msg.contains("llm_evaluation_prompt"),
            "error should name the field, got: {msg}"
        );
        assert!(
            msg.contains("{student_code}"),
            "error should name the placeholder rule, got: {msg}"
        );
    }

    #[test]
    fn lesson_json_roundtrip_preserves_value() {
        let original = Lesson::parse(VALID_YAML).expect("valid lesson should parse");
        let json = serde_json::to_string(&original).expect("lesson should serialize to JSON");
        let roundtripped: Lesson =
            serde_json::from_str(&json).expect("lesson should deserialize from JSON");
        assert_eq!(roundtripped, original);
    }
}
