//! Integration tests for `blendtutor eval-compare <baseline> <candidate>`
//! (ADR-0029).
//!
//! Both lessons' grading prompts grade the same items (the baseline's eval
//! suite), and the report gives the paired change in accuracy and in solution
//! leakage with 95% Newcombe intervals. The pre-registered rule ships only when
//! accuracy is non-inferior (lower bound above -margin) and leakage improved
//! (upper bound below 0). Exit 0 = ship, 3 = hold (2 is clap's usage-error code,
//! so a CI script can tell "hold" from "typo").

//!
//! The mock grader keys on the prompt variant marker and the submission token;
//! checks run through the real `Rscript`, so these skip without R.

mod common;

use common::{blendtutor_output, mount_feedback_for_both, rscript_absent};
use wiremock::MockServer;

const BASELINE: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_compare/baseline.yaml"
);
const CANDIDATE: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_compare/candidate.yaml"
);
/// A lesson whose checks differ from the baseline's, so it cannot be compared.
const DIFFERENT_CHECKS: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_command/demo_lesson.yaml"
);

const FIX: &str = "Try:\n```r\nadd_two <- function(x, y) x + y\n```";

async fn mount_provider(server: &MockServer) {
    for token in ["zz_a", "zz_f"] {
        mount_feedback_for_both(server, ("PROMPT_V1", token), true, "Nice.").await;
        mount_feedback_for_both(server, ("PROMPT_V2", token), true, "Nice.").await;
    }
    for token in ["zz_b", "zz_c", "zz_d", "zz_e"] {
        mount_feedback_for_both(server, ("PROMPT_V1", token), false, FIX).await;
        mount_feedback_for_both(server, ("PROMPT_V2", token), false, "Check the operator.").await;
    }
}

async fn compare(server: &MockServer, extra: &[&str]) -> std::process::Output {
    let uri = server.uri();
    let mut args = vec![
        "eval-compare".to_string(),
        BASELINE.to_string(),
        CANDIDATE.to_string(),
    ];
    args.extend(extra.iter().map(|s| s.to_string()));
    tokio::task::spawn_blocking(move || blendtutor_output(args, uri))
        .await
        .expect("the blocking command task should join")
}

fn close(value: &serde_json::Value, want: f64) -> bool {
    (value.as_f64().expect("a number") - want).abs() < 1e-3
}

/// When I compare two prompts on six items where both are always right and only
/// the candidate stops leaking fixes, I see leakage improved but accuracy not
/// shown non-inferior at the default 5-point margin, so the decision is hold.
///
/// Equal observed accuracy is not evidence of non-inferiority: with six paired
/// items the accuracy change's interval is about -39 to +39 points. An
/// implementation that compares point estimates ships here, and fails.
#[tokio::test]
async fn equal_accuracy_on_few_items_is_not_shown_non_inferior() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_provider(&server).await;

    let output = compare(&server, &["--format", "json"]).await;
    assert_eq!(
        output.status.code(),
        Some(3),
        "hold exits 3; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("stdout should be exactly one JSON document");
    assert_eq!(doc["decision"], "hold", "{doc}");
    assert_eq!(doc["paired_cases"].as_u64(), Some(6));
    assert!(close(&doc["margin"], 0.05));

    let accuracy = &doc["accuracy"];
    assert_eq!(accuracy["baseline"].as_u64(), Some(6), "{accuracy}");
    assert_eq!(accuracy["candidate"].as_u64(), Some(6), "{accuracy}");
    assert_eq!(accuracy["n"].as_u64(), Some(6), "{accuracy}");
    assert!(close(&accuracy["change"], 0.0), "{accuracy}");
    assert!(close(&accuracy["interval"][0], -0.390), "{accuracy}");
    assert!(close(&accuracy["interval"][1], 0.390), "{accuracy}");
    assert_eq!(accuracy["conclusion"], "not_shown_non_inferior");

    let leakage = &doc["leakage"];
    assert_eq!(leakage["baseline"].as_u64(), Some(4), "{leakage}");
    assert_eq!(leakage["candidate"].as_u64(), Some(0), "{leakage}");
    assert_eq!(leakage["n"].as_u64(), Some(4), "{leakage}");
    assert!(close(&leakage["change"], -1.0), "{leakage}");
    assert!(close(&leakage["interval"][0], -1.0), "{leakage}");
    assert!(close(&leakage["interval"][1], -0.307), "{leakage}");
    assert_eq!(leakage["conclusion"], "improved");

    let human = compare(&server, &[]).await;
    let stdout = String::from_utf8_lossy(&human.stdout).into_owned();
    for line in [
        "Paired comparison on 6 cases (margin 5 points)",
        "Accuracy: baseline 6/6, candidate 6/6, change +0 points (95% CI -39 to +39): not shown non-inferior",
        "Solution leakage: baseline 4/4, candidate 0/4, change -100 points (95% CI -100 to -31): improved",
        "Decision: hold (accuracy not shown non-inferior at a 5-point margin)",
    ] {
        assert!(stdout.contains(line), "missing {line:?} in: {stdout}");
    }
}

/// When I pre-register a margin the evidence can support, I see the same data
/// ship: accuracy non-inferior at 50 points and leakage improved.
#[tokio::test]
async fn a_wider_preregistered_margin_ships_the_same_evidence() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_provider(&server).await;

    let output = compare(&server, &["--format", "json", "--margin", "0.5"]).await;
    assert_eq!(
        output.status.code(),
        Some(0),
        "ship exits 0; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("stdout should be exactly one JSON document");
    assert_eq!(doc["decision"], "ship", "{doc}");
    assert_eq!(doc["accuracy"]["conclusion"], "non_inferior");
}

/// When the candidate changes the lesson's checks, I see the comparison refused
/// before any grading: it would compare lessons, not graders.
#[tokio::test]
async fn a_candidate_with_different_checks_is_refused_before_grading() {
    let server = MockServer::start().await;
    let uri = server.uri();
    let args = vec![
        "eval-compare".to_string(),
        BASELINE.to_string(),
        DIFFERENT_CHECKS.to_string(),
    ];
    let output = tokio::task::spawn_blocking(move || blendtutor_output(args, uri))
        .await
        .expect("join");
    assert_eq!(output.status.code(), Some(1));
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("same checks"),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        server
            .received_requests()
            .await
            .expect("recording")
            .is_empty()
    );
}
