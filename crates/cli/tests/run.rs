//! Integration tests for `blendtutor run <lesson> --code <file>` — the headline
//! student loop: execute the submission, grade its checks, ask the LLM provider
//! for a verdict, and report verdict + feedback with a reserved exit code.
//!
//! The provider is a `wiremock` stub reached through the `BLENDTUTOR_PROVIDER_URL`
//! base-URL override; the interpreter is the real `Rscript`, so these skip-with-
//! notice when R is absent. See `tests/common/mod.rs`.

mod common;

use common::{
    CORRECT_CODE, LESSON, WRONG_CODE, blendtutor_output, blendtutor_output_env, dead_provider_url,
    mount_feedback, rscript_absent,
};
use predicates::prelude::*;
use wiremock::MockServer;

/// Run `blendtutor run` with `args`, serving `server` as the provider, inside
/// `spawn_blocking` so the test runtime keeps answering the child's request.
async fn run_against(server: &MockServer, args: Vec<String>) -> std::process::Output {
    run_against_env(server, args, Vec::new()).await
}

/// Like [`run_against`], but also sets `extra_env` on the child.
async fn run_against_env(
    server: &MockServer,
    args: Vec<String>,
    extra_env: Vec<(&'static str, &'static str)>,
) -> std::process::Output {
    let uri = server.uri();
    tokio::task::spawn_blocking(move || blendtutor_output_env(args, uri, extra_env))
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
    assert_ne!(
        code,
        Some(0),
        "exit 2 is disjoint from the correct code (0)"
    );
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

/// The argument vector for `run <LESSON> --code <code> --format json`.
fn run_json_args(code: &str) -> Vec<String> {
    let mut args = run_args(code);
    args.push("--format".to_string());
    args.push("json".to_string());
    args
}

/// True when `bytes` are exactly one parseable JSON document (the whole buffer
/// parses; trailing log/prose bytes would make this `false`).
fn is_one_json_document(bytes: &[u8]) -> bool {
    serde_json::from_slice::<serde_json::Value>(bytes).is_ok()
}

/// AC3 — `--format json` emits exactly one JSON document on stdout with the four
/// well-typed keys, and keeps diagnostics off stdout.
///
/// Parsing the whole stdout buffer is the "exactly one document" check: any
/// interleaved log line or trailing prose would make it fail. Each key is typed
/// (verdict/feedback strings, checks an array, output a string or object), so a
/// stringified `"[]"` or a missing key is caught.
#[tokio::test]
async fn run_format_json_emits_one_typed_report() {
    if rscript_absent() {
        return;
    }
    let feedback = "Looks right — the sum is returned.";
    let server = MockServer::start().await;
    mount_feedback(&server, true, feedback).await;

    let output = run_against(&server, run_json_args(CORRECT_CODE)).await;

    assert_eq!(
        output.status.code(),
        Some(0),
        "a correct submission in json mode exits 0; stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    let value: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap_or_else(|e| {
        panic!(
            "stdout must be exactly one JSON document, parse failed: {e}; stdout={:?}",
            String::from_utf8_lossy(&output.stdout)
        )
    });
    assert!(
        value.is_object(),
        "the report is a JSON object, got {value}"
    );
    assert_eq!(value["verdict"], "correct", "verdict is the string tag");
    assert!(
        value["verdict"].is_string(),
        "verdict is a string, got {value}"
    );
    assert_eq!(
        value["feedback"], feedback,
        "feedback is the model's message"
    );
    assert!(
        value["feedback"].is_string(),
        "feedback is a string, got {value}"
    );
    assert!(
        value["checks"].is_array(),
        "checks is an array, got {value}"
    );
    assert!(
        value["checks"].as_array().expect("checks array").is_empty(),
        "a checkless lesson serializes checks as [], not a stringified list, got {value}"
    );
    let output_kind = &value["output"];
    assert!(
        output_kind.is_string() || output_kind.is_object(),
        "output is a string or object, got {value}"
    );

    // Diagnostics belong on stderr: stdout is the machine document alone.
    let stderr_is_json_object = serde_json::from_slice::<serde_json::Value>(&output.stderr)
        .map(|v| v.is_object())
        .unwrap_or(false);
    assert!(
        !stderr_is_json_object,
        "stdout carries the only JSON document; stderr must not, got: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );
}

/// AC3 (negative) — the default human format is NOT JSON, so a consumer cannot
/// accidentally parse human text as a machine document.
#[tokio::test]
async fn run_human_format_is_not_json() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_feedback(&server, true, "Looks right.").await;

    let output = run_against(&server, run_args(CORRECT_CODE)).await;

    assert!(
        output.status.success(),
        "the human run still succeeds; stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        !is_one_json_document(&output.stdout),
        "human output must not parse as JSON, got: {:?}",
        String::from_utf8_lossy(&output.stdout)
    );
}

/// AC3 — requesting logs (`RUST_LOG`) does not contaminate the json document:
/// diagnostics stay off stdout, so the whole stdout buffer still re-parses clean.
#[tokio::test]
async fn run_format_json_stays_clean_under_logging() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_feedback(&server, true, "Looks right.").await;

    let output = run_against_env(
        &server,
        run_json_args(CORRECT_CODE),
        vec![("RUST_LOG", "debug")],
    )
    .await;

    assert!(
        output.status.success(),
        "the json run still succeeds under RUST_LOG; stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        is_one_json_document(&output.stdout),
        "stdout must stay one clean JSON document even with logging on, got: {:?}",
        String::from_utf8_lossy(&output.stdout)
    );
}
