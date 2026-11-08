//! Integration tests for `blendtutor suggest-misconceptions <lesson>`, a
//! content-validity helper (ADR-0032).
//!
//! It asks the grader model for misconceptions the lesson's eval blueprint does
//! not cover yet, each with a minimal example submission, then runs every example
//! through the lesson's real checks. Each suggestion is reported as caught by
//! the checks, not caught (only the grader could catch it), or not runnable. The
//! provider is a `wiremock` stub; checks run through the real `Rscript`.

mod common;

use common::{blendtutor_output, rscript_absent};
use wiremock::matchers::method;
use wiremock::{Mock, MockServer, ResponseTemplate};

const LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_blueprint/add_two.yaml"
);

/// Three suggestions whose examples land in each check outcome: `y - x` fails
/// the checks, `paste(x + y)` returns "5", which R's `==` accepts, and the
/// third does not parse.
async fn mount_suggestions(server: &MockServer) {
    let suggestions = serde_json::json!({ "misconceptions": [
        { "label": "subtracts in the wrong order", "example_submission": "add_two <- function(x, y) y - x" },
        { "label": "returns the sum as a string", "example_submission": "add_two <- function(x, y) paste(x + y)" },
        { "label": "leaves the function body unfinished", "example_submission": "add_two <- function(x, y) x +" },
    ]});
    let body = serde_json::json!({
        "id": "x", "object": "chat.completion", "created": 0, "model": "m",
        "choices": [{"index": 0, "finish_reason": "tool_calls", "message": {
            "role": "assistant", "content": null,
            "tool_calls": [{"id": "c", "type": "function", "function": {
                "name": "submit", "arguments": suggestions.to_string()
            }}]
        }}],
        "usage": {"prompt_tokens": 1, "completion_tokens": 1, "total_tokens": 2}
    });
    Mock::given(method("POST"))
        .respond_with(ResponseTemplate::new(200).set_body_json(body))
        .mount(server)
        .await;
}

async fn suggest(server: &MockServer, extra: &[&str]) -> std::process::Output {
    let uri = server.uri();
    let mut args = vec!["suggest-misconceptions".to_string(), LESSON.to_string()];
    args.extend(extra.iter().map(|s| s.to_string()));
    tokio::task::spawn_blocking(move || blendtutor_output(args, uri))
        .await
        .expect("join")
}

/// When I ask for misconception suggestions, I see each one with its example and
/// whether the lesson's checks catch it, and the model was told which
/// misconceptions the blueprint already covers.
#[tokio::test]
async fn suggestions_are_checked_against_the_lessons_checks() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_suggestions(&server).await;

    let output = suggest(&server, &["--format", "json"]).await;
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value = serde_json::from_slice(&output.stdout).expect("one JSON document");
    let rows: Vec<(String, String)> = doc["suggestions"]
        .as_array()
        .expect("suggestions array")
        .iter()
        .map(|s| {
            (
                s["label"].as_str().expect("label").to_string(),
                s["checks"].as_str().expect("checks").to_string(),
            )
        })
        .collect();
    let r = |l: &str, c: &str| (l.to_string(), c.to_string());
    assert_eq!(
        rows,
        vec![
            r("subtracts in the wrong order", "caught"),
            r("returns the sum as a string", "not_caught"),
            r("leaves the function body unfinished", "does_not_run"),
        ]
    );

    let requests = server.received_requests().await.expect("recording");
    let prompt = String::from_utf8_lossy(&requests[0].body).into_owned();
    for covered in ["returns x instead of x + y", "uses integer division"] {
        assert!(
            prompt.contains(covered),
            "the prompt must list existing misconceptions so the model avoids them: {covered}"
        );
    }

    let server = MockServer::start().await;
    mount_suggestions(&server).await;
    let human = suggest(&server, &[]).await;
    let stdout = String::from_utf8_lossy(&human.stdout).into_owned();
    for line in [
        "1. subtracts in the wrong order: caught by the checks",
        "2. returns the sum as a string: not caught by the checks (only the grader can catch it)",
        "3. leaves the function body unfinished: example does not run (fix or drop it)",
    ] {
        assert!(stdout.contains(line), "missing {line:?} in: {stdout}");
    }
}
