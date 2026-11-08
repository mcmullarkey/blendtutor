//! Integration tests for `exercise.feedback_guidance` reaching the grader
//! (ADR-0030).
//!
//! `feedback_guidance` is the lesson author's lever on *how* feedback is written
//! (e.g. "don't hand over a working solution"). It must be in the grading prompt
//! the provider actually receives, placed after the success criteria and before
//! the fenced code. A lesson without it keeps its prior prompt exactly. The
//! provider is a `wiremock` stub whose request log is inspected; the real
//! `Rscript` runs the submission, so these skip without R.

mod common;

use common::{blendtutor_output, mount_feedback, rscript_absent};
use wiremock::MockServer;

const GUIDED: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/feedback_guidance/add_two.yaml"
);
const UNGUIDED: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_consistency/add_two.yaml"
);

/// Run `blendtutor run <lesson>` on a fixed submission and return the single
/// prompt the provider received.
async fn prompt_sent_for(lesson: &str) -> String {
    let server = MockServer::start().await;
    mount_feedback(&server, false, "not yet").await;
    let code = tempfile::NamedTempFile::new().expect("a temp submission file");
    std::fs::write(code.path(), "add_two <- function(x, y) x - y\n").expect("write submission");
    let args = vec![
        "run".to_string(),
        lesson.to_string(),
        "--code".to_string(),
        code.path().to_string_lossy().into_owned(),
    ];
    let uri = server.uri();
    let output = tokio::task::spawn_blocking(move || blendtutor_output(args, uri))
        .await
        .expect("join");
    assert!(
        output.status.code().is_some_and(|c| c == 0 || c == 2),
        "run should grade; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let requests = server.received_requests().await.expect("recording");
    assert_eq!(requests.len(), 1, "one grading request");
    let body: serde_json::Value = serde_json::from_slice(&requests[0].body).expect("JSON body");
    body["messages"]
        .as_array()
        .expect("messages")
        .iter()
        .filter_map(|m| m["content"].as_str())
        .collect::<Vec<_>>()
        .join("\n")
}

/// When I grade a lesson that has `feedback_guidance`, I see the guidance in the
/// prompt the grader receives, after the success criteria and before the code.
#[tokio::test]
async fn feedback_guidance_reaches_the_grading_prompt_after_the_criteria() {
    if rscript_absent() {
        return;
    }
    let prompt = prompt_sent_for(GUIDED).await;
    let guidance = prompt
        .find("Feedback guidance:\nGuide the learner to the fix without giving a working solution.")
        .unwrap_or_else(|| panic!("guidance section missing from prompt:\n{prompt}"));
    let criteria = prompt.find("Success criteria:").expect("criteria section");
    let code = prompt.find("<<<STUDENT_CODE_BEGIN>>>").expect("code fence");
    assert!(
        criteria < guidance && guidance < code,
        "guidance must sit after the criteria and before the code:\n{prompt}"
    );
}

/// When I grade a lesson without `feedback_guidance`, I see no guidance section:
/// the prompt keeps its prior layout.
#[tokio::test]
async fn a_lesson_without_guidance_has_no_guidance_section() {
    if rscript_absent() {
        return;
    }
    let prompt = prompt_sent_for(UNGUIDED).await;
    assert!(!prompt.contains("Feedback guidance:"), "{prompt}");
}
