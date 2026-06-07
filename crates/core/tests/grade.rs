//! Integration tests for the lesson↔runner grading join: selecting a runner by
//! language and turning each lesson check code-string into a typed
//! pass/fail/not-run outcome, exercised against a real interpreter.
//!
//! The check-running tests spawn a real `Rscript`, so they skip-with-notice when
//! it is absent (the Slice-1 convention; see
//! `docs/agent-notes/workspace-and-ci.md`). The pure runner-selection test needs
//! no interpreter and always runs.

use blendtutor_core::grade::{run_checks, select_runner, CheckOutcome};
use blendtutor_core::lesson::Lesson;

/// True (after printing a notice) when `Rscript` is not on `PATH`, so a
/// real-interpreter test can `return` early instead of failing on machines
/// without R installed.
fn rscript_absent() -> bool {
    let present = std::process::Command::new("Rscript")
        .arg("--version")
        .output()
        .map(|out| out.status.success())
        .unwrap_or(false);
    if !present {
        eprintln!("SKIP: Rscript absent — skipping real-interpreter grade test");
    }
    !present
}

/// An R lesson whose two checks split on the submission: the first asserts a
/// property the submission has, the second asserts one it lacks. A grader that
/// folds the checks into one aggregate bool, or always passes, cannot reproduce
/// the ordered `[Pass, Fail]` this fixture forces.
const LESSON_R_TWO_CHECKS_SPLIT: &str = r#"
lesson_name: "Adder"
language: R
checks:
  - "stopifnot(add_two(2, 3) == 5)"
  - "stopifnot(add_two(2, 3) == 6)"
exercise:
  prompt: "Write a function add_two(x, y)."
  llm_evaluation_prompt: "Grade this submission: {student_code}"
"#;

/// A correct `add_two`: it satisfies check 1 (`add_two(2, 3) == 5`) and violates
/// check 2 (`add_two(2, 3) == 6`).
const SUBMISSION_PASSES_C1_FAILS_C2: &str = "add_two <- function(x, y) x + y";

/// AC1 — each lesson check runs through the selected R runner and reports
/// pass/fail independently and in order. The correct submission satisfies the
/// first check and violates the second, so the outcomes are exactly
/// `[Pass, Fail]` — position 1 pinned to `Pass` and position 2 to `Fail` so an
/// aggregate-bool, single-outcome, or always-pass impl cannot pass.
#[tokio::test]
async fn r_runner_reports_per_check_pass_fail() {
    if rscript_absent() {
        return;
    }

    let lesson = Lesson::parse(LESSON_R_TWO_CHECKS_SPLIT).expect("fixture lesson is valid");
    let runner = select_runner(&lesson.language);

    let outcomes = run_checks(runner, SUBMISSION_PASSES_C1_FAILS_C2, &lesson.checks).await;

    assert_eq!(outcomes.len(), 2, "one outcome per check, in order");
    assert_eq!(
        outcomes[0],
        CheckOutcome::Pass,
        "check 1 (a property the submission has) must pass"
    );
    assert!(
        matches!(outcomes[1], CheckOutcome::Fail { .. }),
        "check 2 (a property the submission lacks) must fail, got {:?}",
        outcomes[1]
    );
}
