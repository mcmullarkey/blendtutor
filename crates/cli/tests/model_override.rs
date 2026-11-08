//! Integration tests for choosing the grader model: `--model` on `run` and
//! `eval` (ADR-0027).
//!
//! A pinned default model can disappear from a provider (the Fireworks 0731
//! checkpoint did), which used to break every graded path with no workaround.
//! `--model <id>` overrides the provider's default for one invocation; it is
//! also the seam a prompt/model comparison needs. These tests read the `model`
//! field of the request the `wiremock` provider actually received.

mod common;

use blendtutor_core::llm::ProviderChoice;
use common::{blendtutor_output, mount_feedback, rscript_absent};
use wiremock::MockServer;

const EVAL_LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_command/demo_lesson.yaml"
);

const OVERRIDE: &str = "accounts/test/models/override-x";

async fn blendtutor(server: &MockServer, args: Vec<String>) -> std::process::Output {
    let uri = server.uri();
    tokio::task::spawn_blocking(move || blendtutor_output(args, uri))
        .await
        .expect("the blocking command task should join")
}

/// The `model` field of every request the provider received, in order.
async fn requested_models(server: &MockServer) -> Vec<String> {
    server
        .received_requests()
        .await
        .expect("request recording is enabled")
        .iter()
        .map(|request| {
            let body: serde_json::Value =
                serde_json::from_slice(&request.body).expect("the request body is JSON");
            body["model"]
                .as_str()
                .expect("the request names a model")
                .to_string()
        })
        .collect()
}

fn args(parts: &[&str]) -> Vec<String> {
    parts.iter().map(|s| s.to_string()).collect()
}

/// When I run `eval --model <id>`, I see every grading request sent to that
/// model; without the flag, every request uses the provider's default.
#[tokio::test]
async fn eval_sends_every_grading_request_to_the_chosen_model() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_feedback(&server, true, "ok").await;
    let output = blendtutor(&server, args(&["eval", EVAL_LESSON, "--model", OVERRIDE])).await;
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(requested_models(&server).await, vec![OVERRIDE; 3]);

    let server = MockServer::start().await;
    mount_feedback(&server, true, "ok").await;
    let output = blendtutor(&server, args(&["eval", EVAL_LESSON])).await;
    assert_eq!(output.status.code(), Some(0));
    assert_eq!(
        requested_models(&server).await,
        vec![ProviderChoice::Fireworks.default_model(); 3]
    );
}

/// When I run `run --model <id>`, I see the grading request sent to that model.
#[tokio::test]
async fn run_sends_its_grading_request_to_the_chosen_model() {
    if rscript_absent() {
        return;
    }
    let code = tempfile::NamedTempFile::new().expect("a temp submission file");
    std::fs::write(code.path(), "cat(\"alpha\\n\")\n").expect("write the submission");
    let server = MockServer::start().await;
    mount_feedback(&server, true, "ok").await;
    let output = blendtutor(
        &server,
        args(&[
            "run",
            EVAL_LESSON,
            "--code",
            &code.path().to_string_lossy(),
            "--model",
            OVERRIDE,
        ]),
    )
    .await;
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(requested_models(&server).await, vec![OVERRIDE]);
}

/// When I pass an empty `--model`, I see a usage error and no request is made.
#[tokio::test]
async fn an_empty_model_is_refused_before_any_request() {
    let server = MockServer::start().await;
    let output = blendtutor(&server, args(&["eval", EVAL_LESSON, "--model", "  "])).await;
    assert_eq!(output.status.code(), Some(2), "a usage error exits 2");
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("model id must not be empty"),
        "the refusal says why, not just that the flag is unknown: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(requested_models(&server).await.is_empty());
}
