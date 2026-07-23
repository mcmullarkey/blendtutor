//! Integration tests for `blendtutor export-quarto`.
//!
//! Exercises the command end to end via the built binary (`assert_cmd`),
//! observing exit status and stdout structure. The pure transform is
//! unit-tested in `blendtutor-core`; these tests verify the CLI wiring and
//! the 16-clause executable spec (positive structure, negative exclusions,
//! adversarial guards).

use std::io::Write;

use assert_cmd::Command;

/// The R fixture with all optional fields populated (prompt, code_template,
/// solution, hints, checks). Lives under `core`'s fixtures (the schema's home)
/// and is referenced cross-crate by path.
const R_LESSON_FULL: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/r-course/add_two.yaml"
);

/// The Python fixture with prompt, code_template, solution, checks — but no
/// hints. Exercises language="python" and the absent-hints path.
const PYTHON_LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/python-course/add.yaml"
);

/// The R fixture with prompt + code_template only — no solution, no hints,
/// no checks. Exercises the "no empty blocks for absent fields" clause.
const R_LESSON_MINIMAL: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/lessons/add_two_numbers.yaml"
);

/// The R fixture with gotchas — verifies gotchas text is excluded from output.
const R_LESSON_WITH_GOTCHAS: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/lessons/gotcha_lesson.yaml"
);

/// Run `export-quarto <fixture>` via the built binary and return stdout.
fn export_stdout(fixture: &str) -> String {
    let output = Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("export-quarto")
        .arg(fixture)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "export-quarto should exit 0 for valid lesson {fixture}, got {:?}\nstderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).expect("stdout should be UTF-8")
}

// ── Clauses 1–7: positive structure ─────────────────────────────────────

#[test]
fn clause_1_opens_with_blendtutor_div_and_language_attribute() {
    let stdout = export_stdout(R_LESSON_FULL);
    assert!(
        stdout.contains("::: {.blendtutor language=\"r\"}"),
        "output should open with ::: {{.blendtutor language=\"r\"}}, got:\n{stdout}"
    );
}

#[test]
fn clause_1b_python_lesson_uses_python_language_attribute() {
    let stdout = export_stdout(PYTHON_LESSON);
    assert!(
        stdout.contains("::: {.blendtutor language=\"python\"}"),
        "Python lesson should use language=\"python\", got:\n{stdout}"
    );
}

#[test]
fn clause_2_prompt_renders_as_prose() {
    let stdout = export_stdout(R_LESSON_FULL);
    assert!(
        stdout.contains("Write a function called 'add_two'"),
        "prompt text should appear as prose, got:\n{stdout}"
    );
}

#[test]
fn clause_3_code_template_renders_as_first_code_block() {
    let stdout = export_stdout(R_LESSON_FULL);
    assert!(
        stdout.contains("```r\nadd_two <- function(x, y) {"),
        "code_template should be the first fenced code block with ```r tag, got:\n{stdout}"
    );
}

#[test]
fn clause_4_checks_render_in_checks_block() {
    let stdout = export_stdout(R_LESSON_FULL);
    assert!(
        stdout.contains("```{.r .checks}"),
        "checks should be in a ```{{.r .checks}} block, got:\n{stdout}"
    );
    assert!(
        stdout.contains("stopifnot(add_two(2, 3) == 5)"),
        "check content should be present, got:\n{stdout}"
    );
}

#[test]
fn clause_5_solution_renders_in_solution_block() {
    let stdout = export_stdout(R_LESSON_FULL);
    assert!(
        stdout.contains("```{.r .solution}"),
        "solution should be in a ```{{.r .solution}} block, got:\n{stdout}"
    );
    assert!(
        stdout.contains("x + y"),
        "solution content should be present, got:\n{stdout}"
    );
}

#[test]
fn clause_6_hints_render_in_hints_div() {
    let stdout = export_stdout(R_LESSON_FULL);
    assert!(
        stdout.contains("::: {.hints}"),
        "hints should be in a ::: {{.hints}} div, got:\n{stdout}"
    );
    assert!(
        stdout.contains("R uses '<-' for assignment"),
        "hints content should be present, got:\n{stdout}"
    );
}

#[test]
fn clause_7_closes_with_div_marker() {
    let stdout = export_stdout(R_LESSON_FULL);
    // The output must end with the closing ::: (after trimming trailing
    // whitespace). The last non-empty line should be :::.
    let last_line = stdout.trim_end().lines().last().unwrap_or("");
    assert_eq!(
        last_line, ":::",
        "output should close with ::: on its own line, got last line: {last_line:?}\nfull:\n{stdout}"
    );
}

// ── Clauses 8–12: negative exclusions ────────────────────────────────────

#[test]
fn clause_8_llm_evaluation_prompt_absent() {
    let stdout = export_stdout(R_LESSON_FULL);
    assert!(
        !stdout.contains("llm_evaluation_prompt"),
        "llm_evaluation_prompt must be ABSENT from output, got:\n{stdout}"
    );
    assert!(
        !stdout.contains("Evaluate the student's submission"),
        "llm_evaluation_prompt text must be ABSENT, got:\n{stdout}"
    );
}

#[test]
fn clause_9_gotchas_absent() {
    let stdout = export_stdout(R_LESSON_WITH_GOTCHAS);
    assert!(
        !stdout.contains("Vectorized operations apply element-wise"),
        "gotchas text must be ABSENT from output, got:\n{stdout}"
    );
    assert!(
        !stdout.contains("gotchas"),
        "the word 'gotchas' must not appear in output, got:\n{stdout}"
    );
}

#[test]
fn clause_10_no_yaml_keys_leak() {
    let stdout = export_stdout(R_LESSON_FULL);
    for key in [
        "lesson_name:",
        "language:",
        "exercise:",
        "description:",
        "textbook_reference:",
        "type:",
        "prompt:",
        "code_template:",
        "solution:",
        "hints:",
        "checks:",
        "llm_evaluation_prompt:",
    ] {
        assert!(
            !stdout.contains(key),
            "YAML key '{key}' must not leak into output, got:\n{stdout}"
        );
    }
}

#[test]
fn clause_11_no_empty_blocks_for_absent_fields() {
    // The minimal lesson has no solution, no hints, no checks.
    let stdout = export_stdout(R_LESSON_MINIMAL);
    assert!(
        !stdout.contains(".solution"),
        "no .solution block when solution is absent, got:\n{stdout}"
    );
    assert!(
        !stdout.contains(".checks"),
        "no .checks block when checks are empty, got:\n{stdout}"
    );
    assert!(
        !stdout.contains("{.hints}"),
        "no {{.hints}} div when hints are absent, got:\n{stdout}"
    );
}

#[test]
fn clause_12_packages_omitted() {
    // The Python fixture has no packages, but we also verify the word doesn't
    // appear as an attribute or block.
    let stdout = export_stdout(PYTHON_LESSON);
    assert!(
        !stdout.contains("packages"),
        "packages must be OMITTED from output, got:\n{stdout}"
    );
}

// ── Clauses 13–16: adversarial guards ────────────────────────────────────

#[test]
fn clause_13_cross_fixture_uniqueness() {
    let r_out = export_stdout(R_LESSON_FULL);
    let py_out = export_stdout(PYTHON_LESSON);
    assert_ne!(
        r_out, py_out,
        "different lessons must produce different output"
    );
    // Also verify the two R fixtures (full vs minimal) differ.
    let r_min = export_stdout(R_LESSON_MINIMAL);
    assert_ne!(
        r_out, r_min,
        "lessons with different fields must produce different output"
    );
}

#[test]
fn clause_14_exit_code_zero_on_valid_nonzero_on_invalid() {
    // Valid lesson → exit 0.
    Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("export-quarto")
        .arg(R_LESSON_FULL)
        .assert()
        .success();

    // Invalid lesson (missing llm_evaluation_prompt) → nonzero.
    let mut file = tempfile::NamedTempFile::new().unwrap();
    file.write_all(b"lesson_name: \"Bad\"\nlanguage: R\nexercise:\n  prompt: \"Write add\"\n")
        .unwrap();
    Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("export-quarto")
        .arg(file.path())
        .assert()
        .failure();

    // Missing file → nonzero.
    Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("export-quarto")
        .arg("/no/such/lesson.yaml")
        .assert()
        .failure();
}

#[test]
fn clause_15_backtick_in_template_yields_valid_fence() {
    // A code_template containing triple backticks must not break the fence.
    // The transform must use a longer fence (4+ backticks) than any run in
    // the content.
    let yaml = "\
lesson_name: \"Backtick Lesson\"
language: R
exercise:
  prompt: \"Write a function.\"
  code_template: |
    # This has ``` in it
    f <- function() {}
  llm_evaluation_prompt: \"Grade this: {student_code}\"
";
    let mut file = tempfile::NamedTempFile::new().unwrap();
    file.write_all(yaml.as_bytes()).unwrap();

    let output = Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("export-quarto")
        .arg(file.path())
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "backtick-in-template lesson should export successfully, got {:?}\nstderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);

    // The fence must be at least 4 backticks (content has 3).
    assert!(
        stdout.contains("````r\n"),
        "fence should be 4+ backticks when content has ```, got:\n{stdout}"
    );
    // The content's triple backticks should appear inside the fence, not
    // break it.
    assert!(
        stdout.contains("# This has ``` in it"),
        "backtick content should be preserved inside the fence, got:\n{stdout}"
    );
}

#[test]
fn clause_16_prompt_with_div_marker_doesnt_break_structure() {
    // A prompt containing ::: must not corrupt the div structure. The
    // transform's own opening and closing ::: must still be present and
    // the output must end with :::.
    let yaml = "\
lesson_name: \"Div Lesson\"
language: R
exercise:
  prompt: \"See the example ::: below.\"
  llm_evaluation_prompt: \"Grade this: {student_code}\"
";
    let mut file = tempfile::NamedTempFile::new().unwrap();
    file.write_all(yaml.as_bytes()).unwrap();

    let stdout = export_stdout(file.path().to_str().unwrap());
    assert!(
        stdout.contains("::: {.blendtutor language=\"r\"}"),
        "opening div must be present despite ::: in prompt, got:\n{stdout}"
    );
    assert!(
        stdout.contains("See the example ::: below."),
        "prompt text with ::: must be preserved, got:\n{stdout}"
    );
    // The output must end with ::: (the closing div).
    let last_line = stdout.trim_end().lines().last().unwrap_or("");
    assert_eq!(
        last_line, ":::",
        "output must end with ::: despite ::: in prompt, got last line: {last_line:?}"
    );
}
