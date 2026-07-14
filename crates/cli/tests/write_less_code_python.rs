//! Integration tests for the Python example course at `examples/write-less-code-python/`.
//!
//! Validates the 9-point compound predicate from AC-4:
//! 1. Course structure — `blendtutor list` returns 5 rows, all `language: "python"`
//! 2. Lesson validity — each YAML parses with `language == Python`, `{student_code}`
//!    placeholder, `textbook_reference` non-empty
//! 3. Packages bidirectional — `packages` contains `"pandas"` ⟺ solution/checks
//!    reference pandas
//! 4. Checks non-empty and specific — no vacuous assertions, every check contains
//!    `assert`
//! 5. Lesson 4 explicit for-loop — positive grep `for\s+\w+\s+in\s+` AND no list
//!    comprehension
//! 6. Copy-paste-trap concrete bug — lesson 2 prompt contains literal `stress_6`
//! 7. Validate gate — `blendtutor validate` exits 0
//! 8. Run gate — `blendtutor run` exits 0 with `correct` (wiremock stub)
//! 9. Eval suites — ≥4 cases, ≥2 correct, ≥2 incorrect, ≥1 near-miss

mod common;

use std::io::Write;
use std::path::{Path, PathBuf};

use assert_cmd::Command;
use blendtutor_core::eval::{EvalSuite, ExpectedVerdict, parse_eval_suite};
use blendtutor_core::lesson::{Language, Lesson, read_lesson_file};
use regex_lite::Regex;
use wiremock::MockServer;

/// The Python example course directory, relative to the CLI crate.
const COURSE_DIR: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../examples/write-less-code-python"
);

/// The 5 lesson file names in manifest order.
const LESSON_FILES: &[&str] = &[
    "01_seed_data.yaml",
    "02_copy_paste_trap.yaml",
    "03_write_a_function.yaml",
    "04_map_over_columns.yaml",
    "05_rule_of_three.yaml",
];

/// The 5 eval-suite file names, one per lesson.
const EVAL_FILES: &[&str] = &[
    "eval_01_seed_data.yaml",
    "eval_02_copy_paste_trap.yaml",
    "eval_03_write_a_function.yaml",
    "eval_04_map_over_columns.yaml",
    "eval_05_rule_of_three.yaml",
];

fn course_dir() -> &'static Path {
    Path::new(COURSE_DIR)
}

fn lesson_path(filename: &str) -> PathBuf {
    course_dir().join(filename)
}

fn eval_path(filename: &str) -> PathBuf {
    course_dir().join(filename)
}

fn load_lesson(filename: &str) -> Lesson {
    read_lesson_file(&lesson_path(filename))
        .unwrap_or_else(|e| panic!("lesson {filename} should parse: {e}"))
}

fn load_eval_suite(filename: &str) -> EvalSuite {
    let text = std::fs::read_to_string(eval_path(filename))
        .unwrap_or_else(|e| panic!("eval suite {filename} should be readable: {e}"));
    parse_eval_suite(&text).unwrap_or_else(|e| panic!("eval suite {filename} should parse: {e}"))
}

/// True (after printing a notice) when `uv` is not on `PATH`, so a real-interpreter
/// test can `return` early instead of failing on machines without `uv` installed.
fn uv_absent() -> bool {
    let present = std::process::Command::new("uv")
        .arg("--version")
        .output()
        .map(|out| out.status.success())
        .unwrap_or(false);
    if !present {
        eprintln!("SKIP: uv absent — skipping Python interpreter test");
    }
    !present
}

// ─── Predicate 1: Course structure ───────────────────────────────────────────

/// `blendtutor list` returns exactly 5 rows, every row `language == "python"`.
#[test]
fn list_returns_five_python_lessons() {
    let output = Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("list")
        .arg(course_dir())
        .arg("--format")
        .arg("json")
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "`list` should exit 0, got {:?}; stderr={:?}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let rows: Vec<serde_json::Value> = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("list json did not parse: {e}; stdout={stdout:?}"));

    assert_eq!(rows.len(), 5, "course should list exactly 5 lessons");

    for (i, row) in rows.iter().enumerate() {
        assert_eq!(
            row["language"], "python",
            "lesson {i} should be python, got {:?}",
            row["language"]
        );
    }
}

// ─── Predicate 2: Lesson validity ────────────────────────────────────────────

/// Each lesson parses with `language == Python`, `{student_code}` placeholder,
/// and non-empty `textbook_reference`.
#[test]
fn each_lesson_is_valid_python_with_student_code_and_textbook() {
    for (idx, filename) in LESSON_FILES.iter().enumerate() {
        let lesson = load_lesson(filename);
        assert_eq!(
            lesson.language,
            Language::Python,
            "lesson {idx} ({filename}) should be Python"
        );
        assert!(
            lesson
                .exercise
                .llm_evaluation_prompt
                .contains("{student_code}"),
            "lesson {idx} ({filename}) should contain the {{student_code}} placeholder"
        );
        assert!(
            lesson
                .textbook_reference
                .as_ref()
                .map(|s| !s.is_empty())
                .unwrap_or(false),
            "lesson {idx} ({filename}) should have a non-empty textbook_reference"
        );
    }
}

// ─── Predicate 3: Packages bidirectional ─────────────────────────────────────

/// `packages` contains `"pandas"` ⟺ the lesson's solution or checks reference
/// pandas (`import\s+pandas|pd\.`). Declaring without using OR using without
/// declaring both fail.
#[test]
fn packages_bidirectional_pandas_iff_used() {
    let pandas_re = Regex::new(r"import\s+pandas|pd\.").unwrap();
    for (idx, filename) in LESSON_FILES.iter().enumerate() {
        let lesson = load_lesson(filename);
        let solution = lesson.exercise.solution.as_deref().unwrap_or("");
        let checks_joined = lesson.checks.join("\n");
        let combined = format!("{solution}\n{checks_joined}");
        let uses_pandas = pandas_re.is_match(&combined);
        let declares_pandas = lesson.packages.iter().any(|p| p == "pandas");
        assert_eq!(
            uses_pandas, declares_pandas,
            "lesson {idx} ({filename}): packages bidirectional violated — \
             uses pandas ({uses_pandas}) should equal declares pandas ({declares_pandas})"
        );
    }
}

// ─── Predicate 4: Checks non-empty and specific ──────────────────────────────

/// Every lesson has non-empty `checks`; no check matches the vacuous-assertion
/// regex; every check contains `assert`.
#[test]
fn checks_non_empty_and_specific() {
    let vacuous_re =
        Regex::new(r"(?m)^\s*assert\s+(True|1\s*==\s*1|1\b)\s*$|^\s*pass\s*$").unwrap();
    for (idx, filename) in LESSON_FILES.iter().enumerate() {
        let lesson = load_lesson(filename);
        assert!(
            !lesson.checks.is_empty(),
            "lesson {idx} ({filename}) should have non-empty checks"
        );
        for (ci, check) in lesson.checks.iter().enumerate() {
            assert!(
                check.contains("assert"),
                "lesson {idx} ({filename}) check {ci} should contain 'assert': {check:?}"
            );
            assert!(
                !vacuous_re.is_match(check),
                "lesson {idx} ({filename}) check {ci} should not be a vacuous assertion: {check:?}"
            );
        }
    }
}

// ─── Predicate 5: Lesson 4 explicit for-loop ────────────────────────────────

/// Lesson 4 (map-over-columns) solution matches `for\s+\w+\s+in\s+` (positive)
/// AND does NOT match the list-comprehension regex (negative).
#[test]
fn lesson_4_solution_uses_explicit_for_loop_not_list_comprehension() {
    let lesson = load_lesson(LESSON_FILES[3]); // 0-indexed: lesson 4
    let solution = lesson.exercise.solution.as_deref().unwrap_or("");
    let for_loop_re = Regex::new(r"for\s+\w+\s+in\s+").unwrap();
    let list_comp_re = Regex::new(r"\[[^\]]*\s+for\s+[^\]]*\s+in\s+[^\]]*\]").unwrap();
    assert!(
        for_loop_re.is_match(solution),
        "lesson 4 solution should contain an explicit for loop: {solution:?}"
    );
    assert!(
        !list_comp_re.is_match(solution),
        "lesson 4 solution should NOT contain a list comprehension: {solution:?}"
    );
}

// ─── Predicate 6: Copy-paste-trap concrete bug ──────────────────────────────

/// Lesson 2 (copy-paste-trap) exercise prompt contains the literal token
/// `stress_6`.
#[test]
fn lesson_2_prompt_contains_literal_stress_6() {
    let lesson = load_lesson(LESSON_FILES[1]); // 0-indexed: lesson 2
    assert!(
        lesson.exercise.prompt.contains("stress_6"),
        "lesson 2 prompt should contain the literal 'stress_6': {:?}",
        lesson.exercise.prompt
    );
}

// ─── Predicate 7: Validate gate ─────────────────────────────────────────────

/// `blendtutor validate <lesson>` exits 0 for every lesson in the course.
#[test]
fn validate_each_lesson_exits_zero() {
    for filename in LESSON_FILES {
        let path = lesson_path(filename);
        Command::cargo_bin("blendtutor")
            .unwrap()
            .arg("validate")
            .arg(&path)
            .assert()
            .success();
    }
}

// ─── Predicate 8: Run gate (wiremock stub) ──────────────────────────────────

/// `blendtutor run <lesson> --code <solution_file>` exits 0 and stdout contains
/// `correct`, with the LLM provider stubbed via wiremock.
#[tokio::test]
async fn run_solution_exits_zero_with_correct() {
    if uv_absent() {
        return;
    }
    let server = MockServer::start().await;
    common::mount_feedback(&server, true, "Well done — your solution is correct.").await;

    // Write the lesson 1 solution to a temp file.
    let lesson = load_lesson(LESSON_FILES[0]);
    let solution = lesson.exercise.solution.as_deref().unwrap();
    let mut temp_file = tempfile::NamedTempFile::new().unwrap();
    temp_file.write_all(solution.as_bytes()).unwrap();

    let lesson_path = lesson_path(LESSON_FILES[0]);
    let code_path = temp_file.path().to_path_buf();
    let uri = server.uri();

    let output = tokio::task::spawn_blocking(move || {
        common::blendtutor_output(
            vec![
                "run".to_string(),
                lesson_path.to_string_lossy().into_owned(),
                "--code".to_string(),
                code_path.to_string_lossy().into_owned(),
            ],
            uri,
        )
    })
    .await
    .expect("the blocking command task should join");

    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    assert_eq!(
        output.status.code(),
        Some(0),
        "a correct submission exits 0; stdout={stdout:?} stderr={stderr:?}"
    );
    assert!(
        stdout.to_lowercase().contains("correct"),
        "stdout should report a `correct` verdict, got: {stdout:?}"
    );
}

// ─── Predicate 9: Eval suites ───────────────────────────────────────────────

/// Each eval suite has ≥4 cases, ≥2 correct, ≥2 incorrect.
#[test]
fn eval_suites_have_correct_case_distribution() {
    for (idx, filename) in EVAL_FILES.iter().enumerate() {
        let suite = load_eval_suite(filename);
        assert!(
            suite.cases.len() >= 4,
            "eval suite {idx} ({filename}) should have >= 4 cases, got {}",
            suite.cases.len()
        );
        let correct_count = suite
            .cases
            .iter()
            .filter(|c| c.expected == ExpectedVerdict::Correct)
            .count();
        let incorrect_count = suite
            .cases
            .iter()
            .filter(|c| c.expected == ExpectedVerdict::Incorrect)
            .count();
        assert!(
            correct_count >= 2,
            "eval suite {idx} ({filename}) should have >= 2 correct cases, got {correct_count}"
        );
        assert!(
            incorrect_count >= 2,
            "eval suite {idx} ({filename}) should have >= 2 incorrect cases, got {incorrect_count}"
        );
    }
}

/// Each eval suite has at least one near-miss: an incorrect case whose submission
/// runs cleanly via `uv run --with pandas python -I -c <submission>` with exit 0.
#[test]
fn eval_suites_have_near_miss_that_runs_cleanly() {
    if uv_absent() {
        return;
    }
    for (idx, filename) in EVAL_FILES.iter().enumerate() {
        let suite = load_eval_suite(filename);
        let has_near_miss = suite
            .cases
            .iter()
            .filter(|c| c.expected == ExpectedVerdict::Incorrect)
            .any(|c| submission_runs_cleanly(&c.submission));
        assert!(
            has_near_miss,
            "eval suite {idx} ({filename}) should have at least one near-miss \
             (incorrect case whose submission runs cleanly)"
        );
    }
}

/// Run `submission` via `uv run --with pandas python -I -c <submission>` and
/// return `true` if it exits 0 (the submission is valid Python that runs cleanly).
fn submission_runs_cleanly(submission: &str) -> bool {
    std::process::Command::new("uv")
        .args(["run", "--with", "pandas", "python", "-I", "-c", submission])
        .output()
        .map(|out| out.status.success())
        .unwrap_or(false)
}
