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
pub struct LessonId(String);

impl fmt::Display for LessonId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

/// The kind of exercise a lesson poses.
///
/// An enum, not a string, so an unknown kind is rejected at the parse boundary
/// rather than carried downstream (§1.2). The R package authors only
/// function-writing exercises today; new variants are added as the runner gains
/// support for them (Slice 9).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ExerciseKind {
    /// Write a function to a specification.
    FunctionWriting,
}

/// The exercise a lesson poses.
///
/// The two required prompts carry the learner-facing task and the grading
/// template; the optional fields are authoring aids. `llm_evaluation_prompt`
/// must contain the `{student_code}` placeholder — enforced by [`Lesson::parse`],
/// not by this type alone. Unknown keys are rejected (§1.3.1) so an author's
/// typo in an optional field surfaces rather than silently dropping to `None`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Exercise {
    /// What the learner is asked to do. Required.
    pub prompt: String,
    /// The prompt sent to the LLM to grade a submission. Required; must contain
    /// the `{student_code}` placeholder.
    pub llm_evaluation_prompt: String,
    /// The kind of exercise, from the YAML `type` key. Optional.
    #[serde(rename = "type")]
    pub kind: Option<ExerciseKind>,
    /// Optional starter code shown to the learner.
    pub code_template: Option<String>,
    /// Optional example invocations.
    pub example_usage: Option<String>,
    /// Optional human-readable success criteria.
    pub success_criteria: Option<String>,
}

/// A single lesson: its identity, language, and exercise, with optional prose.
///
/// Construct one only through [`Lesson::parse`] — a value of this type is, by
/// construction, structurally complete and semantically valid (ADR-0003).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Lesson {
    /// The lesson's identity (v1: its name). Required.
    pub lesson_name: LessonId,
    /// The language the lesson is authored in. Required.
    pub language: Language,
    /// The exercise the lesson poses. Required.
    pub exercise: Exercise,
    /// Optional one-line summary.
    pub description: Option<String>,
    /// Optional pointer to a textbook section.
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

    const UNKNOWN_LANGUAGE_YAML: &str = r#"
lesson_name: "Adder"
language: Go
exercise:
  prompt: "Write a function add_two(x, y)."
  llm_evaluation_prompt: "Grade this: {student_code}"
"#;

    const UNKNOWN_FIELD_YAML: &str = r#"
lesson_name: "Adder"
language: R
descriptio: "typo'd optional field"
exercise:
  prompt: "Write a function add_two(x, y)."
  llm_evaluation_prompt: "Grade this: {student_code}"
"#;

    #[test]
    fn parse_rejects_unknown_language() {
        let err = Lesson::parse(UNKNOWN_LANGUAGE_YAML)
            .expect_err("an unknown language is not a valid lesson");
        assert!(
            err.to_string().contains("Go"),
            "error should name the rejected language, got: {err}"
        );
    }

    const UNKNOWN_EXERCISE_TYPE_YAML: &str = r#"
lesson_name: "Adder"
language: R
exercise:
  type: "essay_writing"
  prompt: "Write a function add_two(x, y)."
  llm_evaluation_prompt: "Grade this: {student_code}"
"#;

    #[test]
    fn parse_rejects_unknown_exercise_type() {
        let err = Lesson::parse(UNKNOWN_EXERCISE_TYPE_YAML)
            .expect_err("an unknown exercise type is not a valid lesson");
        assert!(
            err.to_string().contains("essay_writing"),
            "error should name the rejected exercise type, got: {err}"
        );
    }

    #[test]
    fn parse_rejects_unknown_field_so_author_typos_surface() {
        let err = Lesson::parse(UNKNOWN_FIELD_YAML)
            .expect_err("a typo'd optional field must not be silently dropped");
        assert!(
            err.to_string().contains("descriptio"),
            "error should name the unknown field, got: {err}"
        );
    }

    #[test]
    fn read_lesson_file_loads_and_parses_the_ported_fixture() {
        let path = Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/lessons/add_two_numbers.yaml"
        ));
        let lesson = read_lesson_file(path).expect("ported fixture should load and validate");
        assert_eq!(lesson.language, Language::R);
        assert_eq!(lesson.exercise.kind, Some(ExerciseKind::FunctionWriting));
    }

    #[test]
    fn read_lesson_file_missing_path_is_a_read_error_not_a_validation_error() {
        let err = read_lesson_file(Path::new("/no/such/lesson.yaml"))
            .expect_err("a missing file cannot load");
        assert!(
            matches!(err, LoadError::Read(_)),
            "a missing file is a read error, got: {err:?}"
        );
    }

    #[test]
    fn read_lesson_file_invalid_contents_is_a_validation_error() {
        // A unique, auto-removed temp file (no fixed-path race, no leak on panic).
        let mut file = tempfile::NamedTempFile::new().unwrap();
        std::io::Write::write_all(&mut file, PROMPT_WITHOUT_PLACEHOLDER_YAML.as_bytes()).unwrap();
        let err =
            read_lesson_file(file.path()).expect_err("a {student_code}-less lesson is invalid");
        assert!(
            matches!(
                err,
                LoadError::Invalid(ValidationError::MissingStudentCodePlaceholder)
            ),
            "invalid contents should surface as a validation error, got: {err:?}"
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

    #[test]
    fn lesson_id_displays_its_value() {
        let lesson = Lesson::parse(VALID_YAML).expect("valid lesson should parse");
        assert_eq!(lesson.lesson_name.to_string(), "Adder");
    }

    #[test]
    fn load_error_display_and_source_surface_the_cause() {
        let invalid = LoadError::Invalid(ValidationError::MissingStudentCodePlaceholder);
        assert!(
            invalid.to_string().contains("{student_code}"),
            "Invalid should display the validation message, got: {invalid}"
        );
        assert!(
            std::error::Error::source(&invalid).is_some(),
            "Invalid should expose the ValidationError as its source"
        );

        let read = LoadError::Read(std::io::Error::new(std::io::ErrorKind::NotFound, "nope"));
        assert!(
            read.to_string().contains("could not read"),
            "Read should label itself a read failure, got: {read}"
        );
        assert!(
            std::error::Error::source(&read).is_some(),
            "Read should expose the io::Error as its source"
        );
    }
}
