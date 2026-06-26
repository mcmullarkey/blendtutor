//! Integration tests for the lesson↔runner grading join: selecting a runner by
//! language and turning each lesson check code-string into a typed
//! pass/fail/not-run outcome, exercised against a real interpreter.
//!
//! The check-running tests spawn a real `Rscript`, so they skip-with-notice when
//! it is absent (the Slice-1 convention; see
//! `docs/agent-notes/workspace-and-ci.md`). The pure runner-selection test needs
//! no interpreter and always runs.

use blendtutor_core::grade::{CheckOutcome, RunnerKind, run_checks, select_runner};
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

/// A minimal `language: Python` lesson with one trivial check and the required
/// valid exercise. Parsing it (rather than hand-building a `Language`) proves the
/// real lesson field drives selection.
const LESSON_PYTHON_MINIMAL: &str = r#"
lesson_name: "Py Adder"
language: Python
checks:
  - "assert True"
exercise:
  prompt: "Write a function add_two(x, y)."
  llm_evaluation_prompt: "Grade this submission: {student_code}"
"#;

/// AC2 — a `language: Python` lesson auto-selects the Python runner. The
/// assertion is on the *selected runner's identity*, not on any executed output,
/// so it needs no interpreter on `PATH`. `select_runner` matches the language
/// exhaustively with no wildcard arm, so an R-hardcoded or `_`-defaulting impl
/// would return the R runner here and fail the `RunnerKind::Python` match.
#[test]
fn python_lesson_selects_python_runner() {
    let lesson = Lesson::parse(LESSON_PYTHON_MINIMAL).expect("fixture lesson is valid");
    assert!(
        matches!(select_runner(&lesson.language), RunnerKind::Python(_)),
        "a language: Python lesson must select the Python runner"
    );
}

/// An R lesson with a single check, for the submission-error case.
const LESSON_R_ONE_CHECK: &str = r#"
lesson_name: "Adder"
language: R
checks:
  - "stopifnot(add_two(2, 3) == 5)"
exercise:
  prompt: "Write a function add_two(x, y)."
  llm_evaluation_prompt: "Grade this submission: {student_code}"
"#;

/// An un-parseable R submission: `x <-` has no right-hand side, so run *on its
/// own* it is a syntax error (`Rscript` exits non-zero with "unexpected end of
/// input"). This pins the standalone-gate mechanism: the grader must evaluate the
/// submission alone, not concatenated with a check — R would splice `x <-` onto
/// the next line and the failure would surface as a check-style "could not find
/// function" error, masquerading as a `Fail`.
const SUBMISSION_R_SYNTAX_ERROR: &str = "x <- ";

/// AC3 — a submission that errors *before* any check can run is reported as
/// `NotRun`, distinct from a check the submission ran and failed. Both halves are
/// pinned: the outcome is `NotRun{..}` AND it is not `Fail{..}`, so an impl that
/// maps "interpreter exited non-zero" onto `Fail` (the bug this AC guards
/// against) is caught — "not Pass" alone would let a `Fail` through.
#[tokio::test]
async fn submission_error_is_notrun_not_fail() {
    if rscript_absent() {
        return;
    }

    let lesson = Lesson::parse(LESSON_R_ONE_CHECK).expect("fixture lesson is valid");
    let runner = select_runner(&lesson.language);

    let outcomes = run_checks(runner, SUBMISSION_R_SYNTAX_ERROR, &lesson.checks).await;

    assert_eq!(outcomes.len(), 1, "one outcome for the single check");
    assert!(
        matches!(outcomes[0], CheckOutcome::NotRun { .. }),
        "a submission that cannot execute is NotRun, got {:?}",
        outcomes[0]
    );
    assert!(
        !matches!(outcomes[0], CheckOutcome::Fail { .. }),
        "a submission error must not be reported as a check failure, got {:?}",
        outcomes[0]
    );
}
