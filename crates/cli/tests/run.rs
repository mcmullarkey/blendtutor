//! Integration tests for `blendtutor run <lesson> --code <file>` — the headline
//! student loop: execute the submission, grade its checks, ask the LLM provider
//! for a verdict, and report verdict + feedback with a reserved exit code.
//!
//! The provider is a `wiremock` stub reached through the `BLENDTUTOR_PROVIDER_URL`
//! base-URL override; the interpreter is the real `Rscript`, so these skip-with-
//! notice when R is absent. See `tests/common/mod.rs`.

mod common;

use common::{CORRECT_CODE, LESSON, blendtutor_output, mount_feedback, rscript_absent};
use predicates::prelude::*;
use wiremock::MockServer;

/// AC1 — a correct submission with a mock `is_correct=true` yields a "correct"
/// verdict, surfaces the model's feedback, and exits 0.
///
/// The feedback-body assertion (not bare "correct") is load-bearing: it forces
/// `RunReport.feedback` to flow from the provider through the rig pipeline into
/// stdout, defeating a stub that hardcodes exit 0 and prints "correct". The
/// verdict derives from the mock's `is_correct=true`, not a rendered literal.
#[tokio::test]
async fn run_correct_code_reports_correct_and_exit_zero() {
    if rscript_absent() {
        return;
    }
    let feedback = "Nicely done — your function returns the sum.";
    let server = MockServer::start().await;
    mount_feedback(&server, true, feedback).await;
    let uri = server.uri();

    let output = tokio::task::spawn_blocking(move || {
        blendtutor_output(
            vec![
                "run".to_string(),
                LESSON.to_string(),
                "--code".to_string(),
                CORRECT_CODE.to_string(),
            ],
            uri,
        )
    })
    .await
    .expect("the blocking command task should join");

    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    assert_eq!(
        output.status.code(),
        Some(0),
        "a correct submission exits 0; stdout={stdout:?} stderr={stderr:?}"
    );
    assert!(
        predicate::str::is_match(r"(?i)\bcorrect\b")
            .unwrap()
            .eval(stdout.as_str()),
        "stdout should report a `correct` verdict, got: {stdout:?}"
    );
    assert!(
        stdout.contains(feedback),
        "the model's feedback must reach stdout, got: {stdout:?}"
    );
}
