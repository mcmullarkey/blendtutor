//! Integration tests for `blendtutor eval --judge`: an LLM judge of feedback
//! quality (ADR-0033).
//!
//! For each case the grader called incorrect, separate judge calls score two
//! binary criteria, each with its explanation written before its verdict:
//! **diagnosis** (does the feedback name this item's misconception?) and
//! **actionability** (does it give a concrete next step?). Each judgment is the
//! majority of k samples. Every run also includes a **control**: the same
//! feedback judged against a mismatched misconception, which a judge that reads
//! the feedback should almost never pass.
//!
//! The grader and the judge are one `wiremock` stub. Judge requests carry a
//! `CRITERION:` marker and the grader's feedback text, so each lands on a
//! scripted verdict. Checks run through the real `Rscript`.

mod common;

use common::{blendtutor_output, mount_feedback_for, rscript_absent};
use wiremock::matchers::{body_string_contains, method};
use wiremock::{Mock, MockServer, ResponseTemplate};

const LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_blueprint/add_two.yaml"
);

fn tool_reply(arguments: serde_json::Value) -> ResponseTemplate {
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

/// A judge verdict for requests containing every needle; lower `priority`
/// wins, so specific rules beat the catch-all.
async fn judge_rule(server: &MockServer, needles: &[&str], pass: bool, priority: u8) {
    let mut mock = Mock::given(method("POST"));
    for needle in needles {
        mock = mock.and(body_string_contains(*needle));
    }
    mock.respond_with(tool_reply(
        serde_json::json!({ "explanation": "scripted", "pass": pass }),
    ))
    .with_priority(priority)
    .mount(server)
    .await;
}

/// Grader: M1 items (m1a, m1b) and m2a graded incorrect with marked feedback;
/// the rest graded correct, so they are not judged. Judge: diagnosis passes
/// only for M1 feedback against M1's own label; actionability passes for the
/// two M1 messages; everything else (including every control) fails.
async fn mount_provider(server: &MockServer) {
    let m1_label = "returns x instead of x + y";
    judge_rule(
        server,
        &["CRITERION: diagnosis", "FB_M1A", m1_label],
        true,
        1,
    )
    .await;
    judge_rule(
        server,
        &["CRITERION: diagnosis", "FB_M1B", m1_label],
        true,
        1,
    )
    .await;
    judge_rule(server, &["CRITERION: actionability", "FB_M1A"], true, 1).await;
    judge_rule(server, &["CRITERION: actionability", "FB_M1B"], true, 1).await;
    judge_rule(server, &["CRITERION:"], false, 2).await;

    mount_feedback_for(server, "zz_ok", true, "FB_OK adds correctly").await;
    mount_feedback_for(server, "zz_m1a", false, "FB_M1A you return x; add y too").await;
    mount_feedback_for(server, "zz_m1b", false, "FB_M1B you return x; add y too").await;
    mount_feedback_for(server, "zz_m2a", false, "FB_M2A not quite").await;
    mount_feedback_for(server, "zz_m2b", true, "FB_M2B fine").await;
    mount_feedback_for(server, "zz_m3", true, "FB_M3 fine").await;
}

async fn eval(server: &MockServer, extra: &[&str]) -> std::process::Output {
    let uri = server.uri();
    let mut args = vec![
        "eval".to_string(),
        LESSON.to_string(),
        "--judge".to_string(),
    ];
    args.extend(extra.iter().map(|s| s.to_string()));
    tokio::task::spawn_blocking(move || blendtutor_output(args, uri))
        .await
        .expect("join")
}

fn close(value: &serde_json::Value, want: f64) -> bool {
    (value.as_f64().expect("a number") - want).abs() < 1e-3
}

/// When I run `eval --judge`, I see per-criterion pass rates over the cases the
/// grader called incorrect, the mismatched-label control rate, and the judge's
/// own consistency across samples.
#[tokio::test]
async fn judge_scores_diagnosis_and_actionability_with_a_control() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_provider(&server).await;

    let output = eval(&server, &["--format", "json"]).await;
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value = serde_json::from_slice(&output.stdout).expect("one JSON document");
    let judge = &doc["judge"];
    assert_eq!(judge["samples"].as_u64(), Some(3), "{judge}");

    let diagnosis = &judge["diagnosis"];
    assert_eq!(
        (diagnosis["passed"].as_u64(), diagnosis["judged"].as_u64()),
        (Some(2), Some(3)),
        "{judge}"
    );
    assert!(
        close(&diagnosis["interval"][0], 0.208) && close(&diagnosis["interval"][1], 0.939),
        "{judge}"
    );
    let control = &judge["diagnosis_control"];
    assert_eq!(
        (control["passed"].as_u64(), control["judged"].as_u64()),
        (Some(0), Some(3)),
        "{judge}"
    );
    let actionability = &judge["actionability"];
    assert_eq!(
        (
            actionability["passed"].as_u64(),
            actionability["judged"].as_u64()
        ),
        (Some(2), Some(3)),
        "{judge}"
    );
    assert_eq!(
        (judge["unanimous"].as_u64(), judge["judgments"].as_u64()),
        (Some(9), Some(9)),
        "3 cases x (diagnosis, control, actionability), all unanimous: {judge}"
    );

    let rationale = &doc["cases"][1]["judge"]["diagnosis"]["explanations"][0];
    assert_eq!(
        rationale, "scripted",
        "per-case explanations are kept for review: {doc}"
    );

    let requests = server.received_requests().await.expect("recording");
    let judge_calls = requests
        .iter()
        .filter(|r| String::from_utf8_lossy(&r.body).contains("CRITERION:"))
        .count();
    assert_eq!(judge_calls, 27, "3 judged cases x 3 criteria x 3 samples");

    let server = MockServer::start().await;
    mount_provider(&server).await;
    let human = eval(&server, &[]).await;
    let stdout = String::from_utf8_lossy(&human.stdout).into_owned();
    for line in [
        "Feedback judge (majority of 3 samples, on cases graded incorrect):",
        "  Diagnosis names the item's misconception: 2/3 (21%–94%)",
        "    control, against a mismatched misconception: 0/3 (0%–56%)",
        "  Gives an actionable next step: 2/3 (21%–94%)",
        "  Judge consistency: 9/9 judgments unanimous across samples",
    ] {
        assert!(stdout.contains(line), "missing {line:?} in: {stdout}");
    }
}
