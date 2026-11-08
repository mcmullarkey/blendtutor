//! Integration tests for `blendtutor audit <lesson>`: a free label audit.
//!
//! `audit` checks each eval case's author-written `expected` label against the
//! lesson's own deterministic checks (ADR-0023). It reuses slice 1's
//! consistency classification, applied to the *label* instead of the grader's
//! verdict, so a mislabeled eval item is caught before any paid `eval` run.
//! It makes no LLM call. These tests prove that by running with no API key and
//! pointing the provider URL at a mock that must record zero requests. Checks
//! run through the real `Rscript`, so the tests skip-with-notice without R.

mod common;

use assert_cmd::Command;
use common::rscript_absent;
use wiremock::MockServer;

/// A checked lesson; its suite's case 2 labels a subtracting `add_two` correct.
const CHECKED_LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_consistency/add_two.yaml"
);

/// An LLM-only lesson (no checks), so labels cannot be audited.
const UNCHECKED_LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_command/demo_lesson.yaml"
);

/// Run `blendtutor audit <lesson> [extra]` with no API key in the environment
/// and the provider pointed at `server`, so any LLM call would either fail on
/// the missing key or show up in the server's request log.
async fn audit(server: &MockServer, lesson: &str, extra: &[&str]) -> std::process::Output {
    let uri = server.uri();
    let mut args = vec!["audit".to_string(), lesson.to_string()];
    args.extend(extra.iter().map(|s| s.to_string()));
    tokio::task::spawn_blocking(move || {
        Command::cargo_bin("blendtutor")
            .expect("binary `blendtutor` should be built")
            .args(&args)
            .env_remove("FIREWORKS_API_KEY")
            .env_remove("ANTHROPIC_API_KEY")
            .env("BLENDTUTOR_PROVIDER_URL", uri)
            .output()
            .expect("running `blendtutor audit` should produce output")
    })
    .await
    .expect("the blocking command task should join")
}

async fn assert_no_provider_calls(server: &MockServer) {
    let requests = server
        .received_requests()
        .await
        .expect("request recording is enabled");
    assert!(
        requests.is_empty(),
        "audit must never call the LLM provider, got {} request(s)",
        requests.len()
    );
}

/// When I run `audit` on a checked lesson whose suite mislabels one case, I see
/// that case flagged as a label violation and the command exits 1. Every other
/// label agrees with the checks, and no LLM call is made.
///
/// Pinning all five statuses, not just the exit code, is load-bearing. An audit
/// that classified the grader's verdict (there is none here), ignored `NotRun`,
/// or flagged any disagreement would land a different case in the wrong bucket.
#[tokio::test]
async fn audit_flags_a_label_that_contradicts_the_checks_without_calling_the_llm() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;

    let output = audit(&server, CHECKED_LESSON, &["--format", "json"]).await;
    assert_eq!(
        output.status.code(),
        Some(1),
        "a label violation must fail the audit; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("stdout should be exactly one JSON document");
    let statuses: Vec<(String, Option<String>)> = doc["cases"]
        .as_array()
        .expect("cases should be an array")
        .iter()
        .map(|case| {
            (
                case["consistency"]["status"]
                    .as_str()
                    .expect("status is a string")
                    .to_string(),
                case["consistency"]
                    .get("kind")
                    .map(|k| k.as_str().expect("kind is a string").to_string()),
            )
        })
        .collect();
    let s = |status: &str, kind: Option<&str>| (status.to_string(), kind.map(str::to_string));
    assert_eq!(
        statuses,
        vec![
            s("consistent", None),
            s("violated", Some("correct_despite_failed_check")),
            s("consistent", None),
            s("consistent", None),
            s("consistent", None),
        ]
    );
    let expected: Vec<&str> = doc["cases"]
        .as_array()
        .expect("cases array")
        .iter()
        .map(|case| case["expected"].as_str().expect("expected is a string"))
        .collect();
    assert_eq!(
        expected,
        vec!["correct", "correct", "incorrect", "correct", "incorrect"]
    );

    let human = audit(&server, CHECKED_LESSON, &[]).await;
    assert_eq!(human.status.code(), Some(1));
    let stdout = String::from_utf8_lossy(&human.stdout).into_owned();
    assert!(
        stdout.contains("Label audit: 1 violation, 0 discordant (5/5 cases checked)"),
        "the human report should summarize the audit, got: {stdout}"
    );
    assert!(
        stdout.contains("case 2: violation — labeled correct but a check failed"),
        "the mislabeled case is named in label language, got: {stdout}"
    );
    assert!(
        !stdout.contains("graded"),
        "the audit speaks about labels, never grader verdicts, got: {stdout}"
    );

    assert_no_provider_calls(&server).await;
}

/// When I run `audit` on a lesson with no checks, I see the labels reported as
/// not assessable and the command exits 0: no evidence is not a failure.
#[tokio::test]
async fn audit_reports_not_assessable_and_passes_when_lesson_has_no_checks() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;

    let output = audit(&server, UNCHECKED_LESSON, &[]).await;
    assert_eq!(
        output.status.code(),
        Some(0),
        "an unchecked lesson is not a failed audit; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    assert!(
        stdout.contains("Label audit: not assessable (lesson has no checks)"),
        "got: {stdout}"
    );
    assert_no_provider_calls(&server).await;
}
