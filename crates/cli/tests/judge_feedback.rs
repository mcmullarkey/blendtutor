//! Integration tests for `blendtutor judge-feedback <items.json>`: judge feedback
//! that already exists, with no grader call and no code run (ADR-0034).
//!
//! Validating the judge against people needs the judge to score the *same*
//! feedback the people labeled. `eval --judge` cannot: it grades afresh, so
//! every run judges new feedback. This command takes the items as data — task,
//! submission, feedback, and optionally the misconception and a mismatched
//! control label — and applies the same criteria, consensus, and control as
//! `eval --judge` (ADR-0033).
//!
//! The judge is a `wiremock` stub keyed on the `CRITERION:` marker and the
//! feedback text.

mod common;

use common::blendtutor_output;
use wiremock::matchers::{body_string_contains, method};
use wiremock::{Mock, MockServer, ResponseTemplate};

fn tool_reply(pass: bool) -> ResponseTemplate {
    let arguments = serde_json::json!({ "explanation": "scripted", "pass": pass });
    ResponseTemplate::new(200).set_body_json(serde_json::json!({
        "id": "x", "object": "chat.completion", "created": 0, "model": "m",
        "choices": [{"index": 0, "finish_reason": "tool_calls", "message": {
            "role": "assistant", "content": null,
            "tool_calls": [{"id": "c", "type": "function", "function": {
                "name": "submit", "arguments": arguments.to_string()
            }}]
        }}],
        "usage": {"prompt_tokens": 1, "completion_tokens": 1, "total_tokens": 2}
    }))
}

async fn judge_rule(server: &MockServer, needles: &[&str], pass: bool, priority: u8) {
    let mut mock = Mock::given(method("POST"));
    for needle in needles {
        mock = mock.and(body_string_contains(*needle));
    }
    mock.respond_with(tool_reply(pass))
        .with_priority(priority)
        .mount(server)
        .await;
}

/// Item `hint` names its misconception and gives a next step; item `paste`
/// hands over the solution (fails actionability) and carries no misconception,
/// so only actionability is asked of it.
const ITEMS: &str = r#"[
  {"id": "hint", "task": "Write add(x, y).", "submission": "add <- function(x, y) x",
   "feedback": "FB_HINT you return x; what else should you add?",
   "misconception": "returns x instead of x + y", "control": "prints instead of returning"},
  {"id": "paste", "task": "Write add(x, y).", "submission": "add <- function(x, y) x",
   "feedback": "FB_PASTE use add <- function(x, y) x + y"}
]"#;

async fn mount_judge(server: &MockServer) {
    judge_rule(
        server,
        &[
            "CRITERION: diagnosis",
            "FB_HINT",
            "returns x instead of x + y",
        ],
        true,
        1,
    )
    .await;
    judge_rule(server, &["CRITERION: actionability", "FB_HINT"], true, 1).await;
    judge_rule(server, &["CRITERION:"], false, 2).await;
}

async fn judge_feedback(
    server: &MockServer,
    items: &std::path::Path,
    extra: &[&str],
) -> std::process::Output {
    let uri = server.uri();
    let mut args = vec!["judge-feedback".to_string(), items.display().to_string()];
    args.extend(extra.iter().map(|s| s.to_string()));
    tokio::task::spawn_blocking(move || blendtutor_output(args, uri))
        .await
        .expect("join")
}

/// When I run `judge-feedback` on a file of existing feedback, I see each item's
/// judgment by id and the same summary `eval --judge` reports, with no grader
/// call made.
#[tokio::test]
async fn judges_existing_feedback_by_item_id() {
    let dir = tempfile::tempdir().expect("tempdir");
    let items = dir.path().join("items.json");
    std::fs::write(&items, ITEMS).expect("write items");
    let server = MockServer::start().await;
    mount_judge(&server).await;

    let output = judge_feedback(&server, &items, &["--format", "json"]).await;
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value = serde_json::from_slice(&output.stdout).expect("one JSON document");
    let judge = &doc["judge"];
    assert_eq!(judge["samples"].as_u64(), Some(3), "{doc}");
    let pair = |key: &str| (judge[key]["passed"].as_u64(), judge[key]["judged"].as_u64());
    assert_eq!(pair("diagnosis"), (Some(1), Some(1)), "{doc}");
    assert_eq!(pair("diagnosis_control"), (Some(0), Some(1)), "{doc}");
    assert_eq!(pair("actionability"), (Some(1), Some(2)), "{doc}");

    let items = doc["items"].as_array().expect("items array");
    assert_eq!(items.len(), 2, "{doc}");
    assert_eq!(items[0]["id"], "hint");
    assert_eq!(items[0]["judge"]["actionability"]["pass"], true, "{doc}");
    assert_eq!(items[1]["id"], "paste");
    assert_eq!(items[1]["judge"]["actionability"]["pass"], false, "{doc}");
    assert!(
        items[1]["judge"].get("diagnosis").is_none(),
        "an untagged item is not judged on diagnosis: {doc}"
    );

    let requests = server.received_requests().await.expect("recording");
    assert!(
        requests
            .iter()
            .all(|r| String::from_utf8_lossy(&r.body).contains("CRITERION:")),
        "only judge calls, never a grader call"
    );
    assert_eq!(requests.len(), 12, "(3 + 1) judgments x 3 samples");

    let server = MockServer::start().await;
    mount_judge(&server).await;
    let human = judge_feedback(&server, dir.path().join("items.json").as_path(), &[]).await;
    let stdout = String::from_utf8_lossy(&human.stdout).into_owned();
    for line in [
        "Feedback judge (majority of 3 samples, on 2 items):",
        "  Diagnosis names the item's misconception: 1/1",
        "  Gives an actionable next step: 1/2",
    ] {
        assert!(stdout.contains(line), "missing {line:?} in: {stdout}");
    }
}
