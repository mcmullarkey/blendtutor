//! Integration tests for `blendtutor eval --repeat N`: run-to-run stability of
//! an evaluation (ADR-0031).
//!
//! An LLM grader is stochastic, so one eval run is one sample. `--repeat N`
//! grades the suite N times and reports how many cases got the same verdict on
//! every run (with a Wilson interval), which cases flipped, and accuracy and
//! leakage per run. The provider is a `wiremock` stub that answers the first
//! request for one submission differently from later ones.

mod common;

use common::{blendtutor_output, mount_feedback_for, rscript_absent};
use wiremock::matchers::{body_string_contains, method};
use wiremock::{Mock, MockServer, ResponseTemplate};

const LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_command/demo_lesson.yaml"
);

/// alpha and beta answer the same way every run; gamma is graded correct on its
/// first request and incorrect afterwards, so it flips between runs.
async fn mount_provider(server: &MockServer) {
    let first_gamma = serde_json::json!({
        "id": "x", "object": "chat.completion", "created": 0, "model": "m",
        "choices": [{"index": 0, "finish_reason": "tool_calls", "message": {
            "role": "assistant", "content": null,
            "tool_calls": [{"id": "c", "type": "function", "function": {
                "name": "submit",
                "arguments": "{\"is_correct\":true,\"feedback_message\":\"gamma ok\"}"
            }}]
        }}]
        , "usage": {"prompt_tokens": 1, "completion_tokens": 1, "total_tokens": 2}
    });
    Mock::given(method("POST"))
        .and(body_string_contains("gamma"))
        .respond_with(ResponseTemplate::new(200).set_body_json(first_gamma))
        .up_to_n_times(1)
        .with_priority(1)
        .mount(server)
        .await;
    mount_feedback_for(server, "alpha", true, "alpha ok").await;
    mount_feedback_for(server, "beta", false, "beta off").await;
    mount_feedback_for(server, "gamma", false, "gamma off").await;
}

async fn eval(server: &MockServer, extra: &[&str]) -> std::process::Output {
    let uri = server.uri();
    let mut args = vec!["eval".to_string(), LESSON.to_string()];
    args.extend(extra.iter().map(|s| s.to_string()));
    tokio::task::spawn_blocking(move || blendtutor_output(args, uri))
        .await
        .expect("join")
}

/// When I run `eval --repeat 3`, I see how many cases got the same verdict on
/// every run, which case flipped, and accuracy per run.
#[tokio::test]
async fn repeat_reports_verdict_stability_and_per_run_accuracy() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_provider(&server).await;

    let output = eval(&server, &["--repeat", "3", "--format", "json"]).await;
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value = serde_json::from_slice(&output.stdout).expect("one JSON document");
    assert_eq!(doc["runs"].as_u64(), Some(3), "{doc}");
    assert_eq!(doc["cases"].as_u64(), Some(3), "{doc}");
    assert_eq!(doc["stable"].as_u64(), Some(2), "{doc}");
    assert_eq!(doc["flipped_cases"], serde_json::json!([3]), "{doc}");
    // gamma's label is `correct`: run 1 matched, runs 2-3 did not.
    assert_eq!(
        doc["accuracy_per_run"],
        serde_json::json!([1.0, 2.0 / 3.0, 2.0 / 3.0]),
        "{doc}"
    );
    assert_eq!(
        server.received_requests().await.unwrap().len(),
        9,
        "3 runs x 3 cases"
    );

    // A fresh server: the one-shot gamma response was consumed by the run above.
    let server = MockServer::start().await;
    mount_provider(&server).await;
    let human = eval(&server, &["--repeat", "3"]).await;
    let stdout = String::from_utf8_lossy(&human.stdout).into_owned();
    assert!(
        stdout.contains(
            "Stability over 3 runs: same verdict on 2/3 cases (21%–94%); flipped: case 3"
        ),
        "got: {stdout}"
    );
}

/// When I ask for fewer than two runs, or combine --repeat with --case or
/// --write-report, I see a refusal before any grading.
#[tokio::test]
async fn repeat_refuses_meaningless_combinations() {
    let server = MockServer::start().await;
    for extra in [
        vec!["--repeat", "1"],
        vec!["--repeat", "2", "--case", "1"],
        vec!["--repeat", "2", "--write-report"],
    ] {
        let output = eval(&server, &extra).await;
        assert_ne!(output.status.code(), Some(0), "{extra:?} must be refused");
    }
    assert!(server.received_requests().await.unwrap().is_empty());
}
