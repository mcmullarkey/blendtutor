//! Integration tests for `blendtutor run <lesson> --code <file>` — the headline
//! student loop: execute the submission, grade its checks, ask the LLM provider
//! for a verdict, and report verdict + feedback with a reserved exit code.
//!
//! The provider is a `wiremock` stub reached through the `BLENDTUTOR_PROVIDER_URL`
//! base-URL override; the interpreter is the real `Rscript`, so these skip-with-
//! notice when R is absent. See `tests/common/mod.rs`.

mod common;

use common::{
    CORRECT_CODE, LESSON, WRONG_CODE, blendtutor_output, dead_provider_url, mount_feedback,
    rscript_absent,
};
use predicates::prelude::*;
use wiremock::MockServer;

/// Run `blendtutor run` with `args`, serving `server` as the provider, inside
/// `spawn_blocking` so the test runtime keeps answering the child's request.
async fn run_against(server: &MockServer, args: Vec<String>) -> std::process::Output {
    let uri = server.uri();
    tokio::task::spawn_blocking(move || blendtutor_output(args, uri))
        .await
        .expect("the blocking command task should join")
}

/// The argument vector for `run <LESSON> --code <code>`.
fn run_args(code: &str) -> Vec<String> {
    vec![
        "run".to_string(),
        LESSON.to_string(),
        "--code".to_string(),
        code.to_string(),
    ]
}

fn matches(pattern: &str, haystack: &str) -> bool {
    predicate::str::is_match(pattern).unwrap().eval(haystack)
}

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

    let output = run_against(&server, run_args(CORRECT_CODE)).await;

    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    assert_eq!(
        output.status.code(),
        Some(0),
        "a correct submission exits 0; stdout={stdout:?} stderr={stderr:?}"
    );
    assert!(
        matches(r"(?i)\bcorrect\b", &stdout),
        "stdout should report a `correct` verdict, got: {stdout:?}"
    );
    // Boundary (disjointness): a correct verdict must not also read as incorrect,
    // catching a stuck "always incorrect" rendering.
    assert!(
        !matches(r"(?i)\bincorrect\b", &stdout),
        "a correct verdict must not read as incorrect, got: {stdout:?}"
    );
    assert!(
        stdout.contains(feedback),
        "the model's feedback must reach stdout, got: {stdout:?}"
    );
}

/// AC2 — an incorrect submission (mock `is_correct=false`) reports an "incorrect"
/// verdict with feedback and exits 2, the code reserved for "not yet correct".
///
/// Exit 2 is disjoint from both 0 (correct) and 1 (error): a not-yet-correct
/// submission must drive the retry loop, never be mistaken for a pass or a crash.
#[tokio::test]
async fn run_incorrect_submission_exits_two_with_feedback() {
    if rscript_absent() {
        return;
    }
    let feedback = "Not yet — your function subtracts instead of adding.";
    let server = MockServer::start().await;
    mount_feedback(&server, false, feedback).await;

    let output = run_against(&server, run_args(WRONG_CODE)).await;

    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    let code = output.status.code();
    assert_eq!(
        code,
        Some(2),
        "an incorrect submission exits 2 (reserved 'not yet correct'); \
         stdout={stdout:?} stderr={stderr:?}"
    );
    assert_ne!(code, Some(0), "exit 2 is disjoint from the correct code (0)");
    assert_ne!(code, Some(1), "exit 2 is disjoint from the error code (1)");
    assert!(
        matches(r"(?i)\bincorrect\b", &stdout),
        "stdout should report an `incorrect` verdict, got: {stdout:?}"
    );
    assert!(
        !matches(r"(?i)\bcorrect\b", &stdout),
        "a word-bounded `correct` must not match an incorrect verdict, got: {stdout:?}"
    );
    assert!(
        stdout.contains(feedback),
        "the model's feedback must reach stdout, got: {stdout:?}"
    );
}

/// AC2 — a missing/unreadable `--code` file is an error (exit 1), distinct from
/// the not-yet-correct code (2): a bad input must not leak the retry code, which
/// would make a submit loop never terminate.
///
/// A live provider isolates the read path: the read must fail *before* any
/// provider call, so the mock records zero requests. A regression that swallowed
/// the read error would reach the mock and exit 0/2 — the request count is the
/// built-in negative control. The interpreter is never reached either.
#[tokio::test]
async fn run_missing_code_exits_one() {
    let server = MockServer::start().await;
    mount_feedback(&server, true, "should never be requested").await;

    let output = run_against(&server, run_args("/no/such/submission.R")).await;

    let code = output.status.code();
    assert_eq!(
        code,
        Some(1),
        "an unreadable --code file exits 1; stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_ne!(
        code,
        Some(2),
        "a bad input must not leak the 'not yet correct' retry code"
    );
    assert_eq!(
        server.received_requests().await.unwrap().len(),
        0,
        "the read fails before any provider call is made"
    );
}

/// AC2 — an unreachable provider is a transport error (exit 1), distinct from the
/// not-yet-correct code (2): a dead endpoint is a failure to grade, not a verdict.
/// The submission runs before the provider call, so this needs a real interpreter.
#[test]
fn run_provider_error_exits_one() {
    if rscript_absent() {
        return;
    }
    let output = blendtutor_output(run_args(CORRECT_CODE), dead_provider_url());

    let code = output.status.code();
    assert_eq!(
        code,
        Some(1),
        "an unreachable provider exits 1; stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_ne!(
        code,
        Some(2),
        "a transport failure is an error (1), not a 'not yet correct' verdict (2)"
    );
}
