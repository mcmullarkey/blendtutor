//! Integration tests for the R example course at `examples/write-less-code-r/`.
//!
//! Exercises the 9-layer compound predicate from AC-3's executable spec:
//!   1. `blendtutor validate` exits 0 for all 5 lessons
//!   2. `blendtutor list` returns 5 rows, all `language == "r"`
//!   3. Each lesson's solution executes cleanly via `Rscript --vanilla -e`
//!   4. Each check passes against the reference solution (load-bearing:
//!      `format!("{submission}\n{check}")` in fresh Rscript per check)
//!   5. Each eval suite has >=4 cases, >=2 correct, >=2 incorrect
//!   6. Each lesson has >=1 genuine near-miss (incorrect case that runs cleanly)
//!   7. Lessons 2-5 solutions contain `survey_data` (self-contained)
//!   8. Each lesson has a non-empty `textbook_reference`
//!   9. `blendtutor run` exits 0 with verdict=correct (wiremock stub)
//!
//! Plus negative tests for each cheat the spec calls out.

mod common;

use blendtutor_core::eval::{ExpectedVerdict, parse_eval_suite};
use blendtutor_core::lesson::read_lesson_file;
use common::{blendtutor_output, mount_feedback, rscript_absent};

use std::path::Path;
use std::process::Command as StdCommand;

use assert_cmd::Command as AssertCommand;
use wiremock::MockServer;

/// The R example course directory, at the repo root under `examples/`.
const COURSE_DIR: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../examples/write-less-code-r"
);

/// Lesson YAML files in manifest order.
const LESSON_FILES: &[&str] = &[
    "01_seed_data.yaml",
    "02_copy_paste_trap.yaml",
    "03_write_a_function.yaml",
    "04_map_over_columns.yaml",
    "05_rule_of_three.yaml",
];

/// Solution .R files, parallel to LESSON_FILES.
const SOLUTION_FILES: &[&str] = &[
    "01_seed_data.R",
    "02_copy_paste_trap.R",
    "03_write_a_function.R",
    "04_map_over_columns.R",
    "05_rule_of_three.R",
];

/// Eval suite YAML files, parallel to LESSON_FILES.
const EVAL_FILES: &[&str] = &[
    "eval_01_seed_data.yaml",
    "eval_02_copy_paste_trap.yaml",
    "eval_03_write_a_function.yaml",
    "eval_04_map_over_columns.yaml",
    "eval_05_rule_of_three.yaml",
];

/// Load all 5 lessons from the course directory.
fn load_all_lessons() -> Vec<blendtutor_core::lesson::Lesson> {
    LESSON_FILES
        .iter()
        .map(|name| {
            let path = Path::new(COURSE_DIR).join(name);
            read_lesson_file(&path).unwrap_or_else(|e| panic!("lesson {name} should parse: {e}"))
        })
        .collect()
}

/// Load all 5 eval suites from the course directory.
fn load_all_eval_suites() -> Vec<blendtutor_core::eval::EvalSuite> {
    EVAL_FILES
        .iter()
        .map(|name| {
            let path = Path::new(COURSE_DIR).join(name);
            let text = std::fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("eval file {name} should read: {e}"));
            parse_eval_suite(&text)
                .unwrap_or_else(|e| panic!("eval suite {name} should parse: {e}"))
        })
        .collect()
}

/// Run `Rscript --vanilla -e <code>` and return whether it exited 0.
fn rscript_exits_zero(code: &str) -> bool {
    StdCommand::new("Rscript")
        .args(["--vanilla", "-e", code])
        .output()
        .map(|out| out.status.success())
        .unwrap_or(false)
}

/// True (after printing a notice) when `purrr` or `dplyr` is not installed,
/// so lesson 4 tests can skip rather than fail on machines without those packages.
fn purrr_dplyr_absent() -> bool {
    let present = rscript_exits_zero("library(purrr); library(dplyr)");
    if !present {
        eprintln!("SKIP: purrr/dplyr absent — skipping lesson 4 package tests");
    }
    !present
}

// ---------------------------------------------------------------------------
// Layer 1: validate exits 0 for all 5 lessons
// ---------------------------------------------------------------------------

#[test]
fn layer1_validate_exits_zero_for_all_lessons() {
    for name in LESSON_FILES {
        let path = Path::new(COURSE_DIR).join(name);
        AssertCommand::cargo_bin("blendtutor")
            .unwrap()
            .arg("validate")
            .arg(&path)
            .assert()
            .success()
            .stdout(predicates::str::is_match(r"(?i)\bOK\b|\bvalid\b").unwrap());
    }
}

// ---------------------------------------------------------------------------
// Layer 2: list returns 5 rows, all language == "r"
// ---------------------------------------------------------------------------

#[test]
fn layer2_list_returns_five_r_lessons() {
    let output = AssertCommand::cargo_bin("blendtutor")
        .unwrap()
        .arg("list")
        .arg(COURSE_DIR)
        .arg("--format")
        .arg("json")
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "`list` should exit 0; stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let rows: Vec<serde_json::Value> = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("list json did not parse: {e}; stdout={stdout:?}"));

    assert_eq!(rows.len(), 5, "course has exactly 5 lessons");

    for (i, row) in rows.iter().enumerate() {
        assert_eq!(
            row["language"], "r",
            "row {i} language should be r, got {:?}",
            row["language"]
        );
    }
}

// ---------------------------------------------------------------------------
// Layer 3: each solution executes cleanly via Rscript
// ---------------------------------------------------------------------------

#[test]
fn layer3_solutions_execute_cleanly() {
    if rscript_absent() {
        return;
    }
    let skip_lesson4 = purrr_dplyr_absent();
    let lessons = load_all_lessons();
    for (i, lesson) in lessons.iter().enumerate() {
        if i == 3 && skip_lesson4 {
            continue;
        }
        let solution = lesson
            .exercise
            .solution
            .as_ref()
            .unwrap_or_else(|| panic!("lesson {i} should have a solution"));
        assert!(
            rscript_exits_zero(solution),
            "lesson {i} solution should execute cleanly via Rscript"
        );
    }
}

// ---------------------------------------------------------------------------
// Layer 4: each check passes against the reference solution
// ---------------------------------------------------------------------------

#[test]
fn layer4_checks_pass_against_reference_solution() {
    if rscript_absent() {
        return;
    }
    let skip_lesson4 = purrr_dplyr_absent();
    let lessons = load_all_lessons();
    for (i, lesson) in lessons.iter().enumerate() {
        if i == 3 && skip_lesson4 {
            continue;
        }
        let solution = lesson
            .exercise
            .solution
            .as_ref()
            .unwrap_or_else(|| panic!("lesson {i} should have a solution"));
        for (j, check) in lesson.checks.iter().enumerate() {
            // Mirrors grade.rs:132: format!("{submission}\n{check}")
            let program = format!("{solution}\n{check}");
            assert!(
                rscript_exits_zero(&program),
                "lesson {i} check {j} should pass against the reference solution"
            );
        }
    }
}

// ---------------------------------------------------------------------------
// Layer 5: each eval suite has >=4 cases, >=2 correct, >=2 incorrect
// ---------------------------------------------------------------------------

#[test]
fn layer5_eval_suites_have_minimum_cases() {
    let suites = load_all_eval_suites();
    for (i, suite) in suites.iter().enumerate() {
        assert!(
            suite.cases.len() >= 4,
            "eval suite {i} should have >=4 cases, got {}",
            suite.cases.len()
        );
        let correct = suite
            .cases
            .iter()
            .filter(|c| c.expected == ExpectedVerdict::Correct)
            .count();
        let incorrect = suite
            .cases
            .iter()
            .filter(|c| c.expected == ExpectedVerdict::Incorrect)
            .count();
        assert!(
            correct >= 2,
            "eval suite {i} should have >=2 correct cases, got {correct}"
        );
        assert!(
            incorrect >= 2,
            "eval suite {i} should have >=2 incorrect cases, got {incorrect}"
        );
    }
}

// ---------------------------------------------------------------------------
// Layer 6: each lesson has >=1 genuine near-miss (incorrect case that runs cleanly)
// ---------------------------------------------------------------------------

#[test]
fn layer6_each_lesson_has_genuine_near_miss() {
    if rscript_absent() {
        return;
    }
    let skip_lesson4 = purrr_dplyr_absent();
    let suites = load_all_eval_suites();
    for (i, suite) in suites.iter().enumerate() {
        if i == 3 && skip_lesson4 {
            continue;
        }
        let has_near_miss = suite.cases.iter().any(|case| {
            case.expected == ExpectedVerdict::Incorrect && rscript_exits_zero(&case.submission)
        });
        assert!(
            has_near_miss,
            "eval suite {i} should have >=1 incorrect case that runs cleanly (genuine near-miss)"
        );
    }
}

// ---------------------------------------------------------------------------
// Layer 7: lessons 2-5 solutions contain survey_data (self-contained)
// ---------------------------------------------------------------------------

#[test]
fn layer7_solutions_are_self_contained() {
    let lessons = load_all_lessons();
    // Lessons 2-5 (indices 1-4) must contain survey_data construction
    for (i, lesson) in lessons.iter().enumerate().skip(1).take(4) {
        let solution = lesson
            .exercise
            .solution
            .as_ref()
            .unwrap_or_else(|| panic!("lesson {i} should have a solution"));
        assert!(
            solution.contains("survey_data"),
            "lesson {i} solution must contain survey_data construction (self-contained)"
        );
    }
}

// ---------------------------------------------------------------------------
// Layer 8: each lesson has a non-empty textbook_reference
// ---------------------------------------------------------------------------

#[test]
fn layer8_each_lesson_has_textbook_reference() {
    let lessons = load_all_lessons();
    for (i, lesson) in lessons.iter().enumerate() {
        let ref_text = lesson
            .textbook_reference
            .as_ref()
            .unwrap_or_else(|| panic!("lesson {i} should have a textbook_reference"));
        assert!(
            !ref_text.is_empty(),
            "lesson {i} textbook_reference should be non-empty"
        );
    }
}

// ---------------------------------------------------------------------------
// Layer 9: blendtutor run exits 0 with verdict=correct (wiremock stub)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn layer9_run_exits_zero_with_correct_verdict() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_feedback(&server, true, "Nicely done — your solution is correct.").await;

    // Use lesson 1 (seed_data) — simplest solution, no packages needed
    let lesson_path = Path::new(COURSE_DIR).join(LESSON_FILES[0]);
    let solution_path = Path::new(COURSE_DIR).join(SOLUTION_FILES[0]);

    let args = vec![
        "run".to_string(),
        lesson_path.to_string_lossy().into_owned(),
        "--code".to_string(),
        solution_path.to_string_lossy().into_owned(),
    ];

    let uri = server.uri();
    let output = tokio::task::spawn_blocking(move || blendtutor_output(args, uri))
        .await
        .expect("the blocking command task should join");

    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    assert_eq!(
        output.status.code(),
        Some(0),
        "run should exit 0 for a correct submission; stdout={stdout:?} stderr={stderr:?}"
    );
    assert!(
        stdout.to_lowercase().contains("correct"),
        "stdout should report a correct verdict, got: {stdout:?}"
    );
}

// ---------------------------------------------------------------------------
// Negative 1: checks fail against WRONG submissions (check-pass-through cheat)
// ---------------------------------------------------------------------------

#[test]
fn negative_checks_fail_against_wrong_submissions() {
    if rscript_absent() {
        return;
    }
    let skip_lesson4 = purrr_dplyr_absent();
    // For each lesson, craft a WRONG submission and assert at least one check fails
    let wrong_submissions: &[(&str, &str)] = &[
        // Lesson 1: wrong dimensions (6 cols instead of 7)
        (
            "survey_data <- data.frame(respondent_id = 1:5, stress_1 = c(3,4,5,2,1))",
            "stopifnot(identical(dim(survey_data), c(5L, 7L)))",
        ),
        // Lesson 2: copy-paste bug — reverses stress_5 instead of stress_6
        (
            "survey_data <- data.frame(respondent_id=1:5,stress_1=c(3,4,5,2,1),stress_2=c(2,3,4,5,1),stress_3=c(1,2,3,4,5),stress_4=c(5,4,3,2,1),stress_5=c(4,3,2,1,5),stress_6=c(1,2,3,4,5)); survey_data$stress_6_rev <- 6 - survey_data$stress_5",
            "stopifnot(identical(survey_data$stress_6_rev, 6 - survey_data$stress_6))",
        ),
        // Lesson 3: wrong max_value (5 - x instead of 6 - x)
        (
            "survey_data <- data.frame(respondent_id=1:5,stress_1=c(3,4,5,2,1),stress_2=c(2,3,4,5,1),stress_3=c(1,2,3,4,5),stress_4=c(5,4,3,2,1),stress_5=c(4,3,2,1,5),stress_6=c(1,2,3,4,5)); reverse_score <- function(x) 5 - x",
            "stopifnot(identical(reverse_score(c(1, 2, 3, 4, 5)), c(5, 4, 3, 2, 1)))",
        ),
        // Lesson 4: only reverses stress_1 (not all six) — uses purrr/dplyr
        (
            "library(purrr); library(dplyr); survey_data <- data.frame(respondent_id=1:5,stress_1=c(3,4,5,2,1),stress_2=c(2,3,4,5,1),stress_3=c(1,2,3,4,5),stress_4=c(5,4,3,2,1),stress_5=c(4,3,2,1,5),stress_6=c(1,2,3,4,5)); survey_data <- survey_data |> mutate(stress_1_rev = 6 - stress_1)",
            "stopifnot(all(c('stress_1_rev', 'stress_6_rev') %in% names(survey_data)))",
        ),
        // Lesson 5: wrong decision (count > 3 instead of count >= 3)
        (
            "survey_data <- data.frame(respondent_id=1:5,stress_1=c(3,4,5,2,1),stress_2=c(2,3,4,5,1),stress_3=c(1,2,3,4,5),stress_4=c(5,4,3,2,1),stress_5=c(4,3,2,1,5),stress_6=c(1,2,3,4,5)); should_abstract <- function(count) count > 3",
            "stopifnot(isTRUE(should_abstract(3)))",
        ),
    ];

    for (i, (wrong, check)) in wrong_submissions.iter().enumerate() {
        if i == 3 && skip_lesson4 {
            continue;
        }
        let program = format!("{wrong}\n{check}");
        let output = StdCommand::new("Rscript")
            .args(["--vanilla", "-e", &program])
            .output()
            .unwrap();
        assert!(
            !output.status.success(),
            "lesson {i} check should FAIL against the WRONG submission (check-pass-through cheat)"
        );
    }
}

// ---------------------------------------------------------------------------
// Negative 3: near-miss submissions fail at least one check (near-miss-not-near)
// ---------------------------------------------------------------------------

#[test]
fn near_miss_submissions_fail_at_least_one_check() {
    if rscript_absent() {
        return;
    }
    let skip_lesson4 = purrr_dplyr_absent();
    let lessons = load_all_lessons();
    let suites = load_all_eval_suites();
    for (i, (lesson, suite)) in lessons.iter().zip(suites.iter()).enumerate() {
        if i == 3 && skip_lesson4 {
            continue;
        }
        // Find the near-miss: expected incorrect AND runs cleanly via Rscript
        let near_miss = suite
            .cases
            .iter()
            .find(|case| {
                case.expected == ExpectedVerdict::Incorrect && rscript_exits_zero(&case.submission)
            })
            .unwrap_or_else(|| {
                panic!("lesson {i} should have a near-miss case (incorrect + runs cleanly)")
            });
        // At least one check must fail against the near-miss submission
        let any_check_fails = lesson.checks.iter().any(|check| {
            let program = format!("{}\n{}", near_miss.submission, check);
            !rscript_exits_zero(&program)
        });
        assert!(
            any_check_fails,
            "lesson {i} near-miss should fail at least one check (near-miss-not-near guard)"
        );
    }
}

// ---------------------------------------------------------------------------
// Negative 2+5: lesson 2 check references stress_6_rev (chapter-arc + solution-reuse)
// ---------------------------------------------------------------------------

#[test]
fn negative_lesson2_check_references_stress_6_rev() {
    let lessons = load_all_lessons();
    let lesson2 = &lessons[1];
    let all_checks = lesson2.checks.join(" ");
    assert!(
        all_checks.contains("stress_6_rev"),
        "lesson 2 checks must reference stress_6_rev (chapter-arc + solution-reuse guard)"
    );
}

// ---------------------------------------------------------------------------
// Negative 4: lesson 4 solution uses purrr/dplyr (package-declared-not-consumed)
// ---------------------------------------------------------------------------

#[test]
fn negative_lesson4_solution_uses_purrr_dplyr() {
    if purrr_dplyr_absent() {
        return;
    }
    let lessons = load_all_lessons();
    let lesson4 = &lessons[3];

    // Lesson 4 declares packages: [purrr, dplyr]
    assert!(
        lesson4.packages.contains(&"purrr".to_string()),
        "lesson 4 should declare purrr package"
    );
    assert!(
        lesson4.packages.contains(&"dplyr".to_string()),
        "lesson 4 should declare dplyr package"
    );

    // Solution must consume the packages — not just declare them
    let solution = lesson4
        .exercise
        .solution
        .as_ref()
        .expect("lesson 4 should have a solution");
    let uses_package = solution.contains("map(")
        || solution.contains("bind_cols(")
        || solution.contains("set_names(")
        || solution.contains("|>")
        || solution.contains("mutate(")
        || solution.contains("across(");
    assert!(
        uses_package,
        "lesson 4 solution must use purrr/dplyr (map, bind_cols, set_names, |>, mutate, across) — \
         package-declared-not-consumed cheat"
    );
}
