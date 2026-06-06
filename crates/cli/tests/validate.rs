//! Integration tests for `blendtutor validate`.
//!
//! Exercises the command end to end via the built binary (`assert_cmd`),
//! observing exit status and user-facing output — the slice's "when I run
//! validate, I see Y" behavior. Pure parse/validate rules are unit-tested in
//! `blendtutor-core`.

use std::io::Write;

use assert_cmd::Command;

/// The ported example lesson, valid and complete. Lives under `core`'s fixtures
/// (the schema's home) and is referenced cross-crate by path.
const VALID_LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/lessons/add_two_numbers.yaml"
);

#[test]
fn validate_valid_lesson_reports_ok_and_exit_zero() {
    Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("validate")
        .arg(VALID_LESSON)
        .assert()
        .success()
        // Word-bounded so a stray "invalid" in error text can't masquerade as a pass.
        .stdout(predicates::str::is_match(r"(?i)\bOK\b|\bvalid\b").unwrap());
}

#[test]
fn validate_invalid_lesson_exits_nonzero_naming_problem() {
    // A lesson missing the required `exercise.llm_evaluation_prompt`.
    let mut file = tempfile::NamedTempFile::new().unwrap();
    file.write_all(
        b"lesson_name: \"Adder\"\nlanguage: R\nexercise:\n  prompt: \"Write add_two\"\n",
    )
    .unwrap();

    Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("validate")
        .arg(file.path())
        .assert()
        .failure()
        .stderr(predicates::str::contains("llm_evaluation_prompt"));
}

#[test]
fn validate_lesson_without_student_code_placeholder_exits_nonzero_naming_the_rule() {
    // The other AC2 branch: llm_evaluation_prompt is present but lacks the
    // {student_code} placeholder. The error must name the field and the rule.
    let mut file = tempfile::NamedTempFile::new().unwrap();
    file.write_all(
        b"lesson_name: \"Adder\"\nlanguage: R\nexercise:\n  prompt: \"Write add_two\"\n  llm_evaluation_prompt: \"Grade the submission.\"\n",
    )
    .unwrap();

    Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("validate")
        .arg(file.path())
        .assert()
        .failure()
        .stderr(predicates::str::contains("llm_evaluation_prompt"))
        .stderr(predicates::str::contains("{student_code}"));
}
