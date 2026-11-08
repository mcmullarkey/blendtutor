//! Integration tests for grader-vs-checks consistency in `blendtutor eval`.
//!
//! A lesson's deterministic checks and the LLM grader's verdict are two
//! measures of the same construct ("does the code meet the spec"). A failed or
//! never-run check is conclusive evidence of "incorrect", so a `correct` verdict
//! alongside one is a *violation* — one of the two instruments is broken. All
//! checks passing is only necessary, not sufficient, so an `incorrect` verdict
//! alongside it is a weaker *discordance*. A lesson with no checks offers no
//! evidence at all and must read as *unchecked*, never as consistent.
//!
//! The provider is a `wiremock` stub keyed on a `zz_*` token in each
//! submission; the checks run through the real `Rscript`, so these tests
//! skip-with-notice when R is absent.

mod common;

use common::{blendtutor_output, mount_feedback_for, rscript_absent};
use wiremock::MockServer;

/// A checked lesson whose five eval cases each land in one consistency category.
const CHECKED_LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_consistency/add_two.yaml"
);

/// An LLM-only lesson (no checks) — consistency cannot be assessed.
const UNCHECKED_LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_command/demo_lesson.yaml"
);

/// Script the grader so each checked-lesson case lands in its category: the
/// verdict polarity paired with the real check outcomes decides it.
async fn mount_checked_provider(server: &MockServer) {
    mount_feedback_for(server, "zz_agree_correct", true, "adds correctly").await;
    mount_feedback_for(server, "zz_failed_check", true, "looks fine").await;
    mount_feedback_for(server, "zz_crashes", true, "looks fine").await;
    mount_feedback_for(server, "zz_style_nit", false, "argument order").await;
    mount_feedback_for(server, "zz_agree_incorrect", false, "multiplies").await;
}

/// Script the three demo-lesson verdicts (alpha/beta/gamma).
async fn mount_unchecked_provider(server: &MockServer) {
    mount_feedback_for(server, "alpha", true, "alpha looks right").await;
    mount_feedback_for(server, "beta", false, "beta is off").await;
    mount_feedback_for(server, "gamma", false, "gamma is off").await;
}

async fn eval_lesson(server: &MockServer, lesson: &str, extra: &[&str]) -> std::process::Output {
    let uri = server.uri();
    let mut args = vec!["eval".to_string(), lesson.to_string()];
    args.extend(extra.iter().map(|s| s.to_string()));
    tokio::task::spawn_blocking(move || blendtutor_output(args, uri))
        .await
        .expect("the blocking command task should join")
}

/// The `(status, kind)` pair of each case's `consistency` object, in order.
fn consistency_of(doc: &serde_json::Value) -> Vec<(String, Option<String>)> {
    doc["cases"]
        .as_array()
        .expect("cases should be an array")
        .iter()
        .map(|case| {
            let consistency = &case["consistency"];
            let status = consistency["status"]
                .as_str()
                .unwrap_or_else(|| panic!("consistency.status should be a string: {case}"))
                .to_string();
            let kind = consistency.get("kind").map(|k| {
                k.as_str()
                    .unwrap_or_else(|| panic!("consistency.kind should be a string: {case}"))
                    .to_string()
            });
            (status, kind)
        })
        .collect()
}

/// When I run `eval` on a checked lesson, I see each case's verdict classified
/// against its real check outcomes: violations where the grader said correct
/// despite a failed or never-run check, a discordance where it said incorrect
/// despite every check passing, and consistent otherwise.
///
/// Pinning all five categories in order is load-bearing: a classifier that only
/// reads the verdict, only reads the checks, or treats `NotRun` like `Pass`
/// lands at least one case in the wrong bucket. The `matched` column is pinned
/// too, to show consistency is independent of it: case 2 is a *matched*
/// violation — a mislabeled eval item (the author marked broken code correct)
/// that polarity accuracy scores as a success and only the checks expose.
#[tokio::test]
async fn eval_classifies_each_verdict_against_its_check_outcomes() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_checked_provider(&server).await;

    let output = eval_lesson(&server, CHECKED_LESSON, &["--format", "json"]).await;
    assert_eq!(
        output.status.code(),
        Some(0),
        "eval should succeed; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("stdout should be exactly one JSON document");

    let s = |status: &str, kind: Option<&str>| (status.to_string(), kind.map(str::to_string));
    assert_eq!(
        consistency_of(&doc),
        vec![
            s("consistent", None),
            s("violated", Some("correct_despite_failed_check")),
            s("violated", Some("correct_despite_not_run")),
            s("discordant", Some("incorrect_despite_passing_checks")),
            s("consistent", None),
        ]
    );
    let matched: Vec<bool> = doc["cases"]
        .as_array()
        .expect("cases array")
        .iter()
        .map(|case| case["matched"].as_bool().expect("matched is a boolean"))
        .collect();
    assert_eq!(matched, vec![true, true, false, false, true]);

    let human = eval_lesson(&server, CHECKED_LESSON, &[]).await;
    let stdout = String::from_utf8_lossy(&human.stdout).into_owned();
    assert!(
        stdout.contains("Consistency: 2 violations, 1 discordant (5/5 cases checked)"),
        "the human report should summarize consistency, got: {stdout}"
    );
    assert_eq!(
        stdout
            .matches("  consistency: violation — graded correct but a check failed")
            .count(),
        1,
        "the failed-check violation is called out under its row, got: {stdout}"
    );
    assert_eq!(
        stdout
            .matches("  consistency: violation — graded correct but the submission never ran")
            .count(),
        1,
        "the not-run violation is called out under its row, got: {stdout}"
    );
    assert_eq!(
        stdout
            .matches("  consistency: discordant — graded incorrect but every check passed")
            .count(),
        1,
        "the discordance is called out under its row, got: {stdout}"
    );
}

/// When I run `eval` on a lesson with no checks, I see consistency reported as
/// unassessable — never as consistent — so an LLM-only lesson cannot claim
/// agreement it has no evidence for.
#[tokio::test]
async fn eval_reports_consistency_unchecked_when_lesson_has_no_checks() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_unchecked_provider(&server).await;

    let output = eval_lesson(&server, UNCHECKED_LESSON, &["--format", "json"]).await;
    assert_eq!(output.status.code(), Some(0));
    let doc: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("stdout should be exactly one JSON document");
    assert_eq!(
        consistency_of(&doc),
        vec![("unchecked".to_string(), None); 3]
    );

    let human = eval_lesson(&server, UNCHECKED_LESSON, &[]).await;
    let stdout = String::from_utf8_lossy(&human.stdout).into_owned();
    assert!(
        stdout.contains("Consistency: not assessable (lesson has no checks)"),
        "an unchecked lesson must say consistency is not assessable, got: {stdout}"
    );
}
