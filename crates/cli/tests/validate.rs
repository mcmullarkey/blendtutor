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

/// A structurally complete lesson whose `llm_evaluation_prompt` lacks the
/// `{student_code}` placeholder, so `validate` rejects it. Exercises the failing
/// exit code for the format-parity check.
const INVALID_LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/lessons/missing_student_code.yaml"
);

/// Run `validate <fixture> --format <format>` via the built binary and return
/// the raw process output (status + captured streams).
fn run_validate(fixture: &str, format: &str) -> std::process::Output {
    Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("validate")
        .arg(fixture)
        .arg("--format")
        .arg(format)
        .output()
        .unwrap()
}

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
fn validate_json_emits_documented_object_with_exit_code_parity() {
    // For both a passing and a failing lesson, `--format json` must emit a
    // documented object (string `status` + a `findings` key) on stdout, and the
    // exit code must match the human path for the same input — the exit code is
    // a property of the result, not the renderer.
    for (fixture, expect_success) in [(VALID_LESSON, true), (INVALID_LESSON, false)] {
        let human = run_validate(fixture, "human");
        let json = run_validate(fixture, "json");

        // The fixture exercises the intended exit code under both formats.
        assert_eq!(
            human.status.success(),
            expect_success,
            "human exit code wrong for {fixture}"
        );
        assert_eq!(
            json.status.success(),
            expect_success,
            "json exit code wrong for {fixture} (a hardcoded exit 0 would trip this)"
        );

        // Exit-code parity across the two formats for the same input.
        assert_eq!(
            human.status.code(),
            json.status.code(),
            "exit codes diverge between --format human and --format json for {fixture}"
        );

        // The json document is machine-readable: a string `status` and a
        // `findings` key (human text in braces would not parse here).
        let stdout = String::from_utf8_lossy(&json.stdout);
        let doc: serde_json::Value = serde_json::from_str(&stdout)
            .unwrap_or_else(|e| panic!("json stdout did not parse for {fixture}: {e}; stdout={stdout:?}"));
        assert!(
            doc.get("status").and_then(serde_json::Value::as_str).is_some(),
            "json output for {fixture} lacks a string `status`: {doc}"
        );
        assert!(
            doc.get("findings").is_some(),
            "json output for {fixture} lacks a `findings` key: {doc}"
        );
    }
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
