//! Integration tests for `blendtutor eval <lesson>` — scoring feedback quality.
//!
//! `eval` loads a lesson and its sibling `eval_<lesson>.yaml` suite, runs every
//! synthetic submission through the exact `run` pipeline (execute → grade → ask
//! the LLM for a verdict), and scores each verdict's polarity against the case's
//! expected polarity, reporting an aggregate accuracy and per-case results.
//!
//! Like the `run` tests, the provider is a `wiremock` stub reached through the
//! `BLENDTUTOR_PROVIDER_URL` override and the interpreter is the real `Rscript`,
//! so these skip-with-notice when R is absent. One server returns a distinct
//! verdict per submission via [`mount_feedback_for`], keyed on a token the
//! submission fences into the request body.

mod common;

use common::{blendtutor_output, mount_feedback_for, rscript_absent};
use wiremock::MockServer;

/// The demo lesson and its sibling eval suite (three cases: alpha/beta/gamma).
const EVAL_LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_command/demo_lesson.yaml"
);

/// Mount the three scripted verdicts that, against the suite's expected
/// polarities `[correct, incorrect, correct]`, yield two matches and one
/// mismatch (accuracy 2/3): alpha→correct (match), beta→incorrect (match),
/// gamma→incorrect (mismatch).
async fn mount_three_case_provider(server: &MockServer) {
    mount_feedback_for(server, "alpha", true, "alpha looks right").await;
    mount_feedback_for(server, "beta", false, "beta is off").await;
    mount_feedback_for(server, "gamma", false, "gamma is off").await;
}

/// Run `blendtutor eval <lesson> [extra args]` against `server`, inside
/// `spawn_blocking` so the test runtime keeps serving the mock while the child's
/// requests are in flight.
async fn eval_against(server: &MockServer, extra: &[&str]) -> std::process::Output {
    let uri = server.uri();
    let mut args = vec!["eval".to_string(), EVAL_LESSON.to_string()];
    args.extend(extra.iter().map(|s| s.to_string()));
    tokio::task::spawn_blocking(move || blendtutor_output(args, uri))
        .await
        .expect("the blocking command task should join")
}

/// AC1 — `eval` scores each case match/mismatch and reports the aggregate
/// accuracy.
///
/// Three cases, two of whose verdicts match their expected polarity and one of
/// which does not, must report `2/3` and surface exactly one mismatch in the
/// per-case rows. Asserting the fraction *and* the single mismatch row is
/// load-bearing: a stub that prints a constant accuracy or omits per-case
/// results cannot pass, because the mismatch is driven by the mock's mixed
/// verdicts flowing through the real scoring path.
#[tokio::test]
async fn eval_reports_aggregate_accuracy_and_per_case_results() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_three_case_provider(&server).await;

    let output = eval_against(&server, &[]).await;

    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    assert_eq!(
        output.status.code(),
        Some(0),
        "eval should succeed; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        stdout.contains("2/3"),
        "stdout should report the aggregate accuracy 2/3, got: {stdout:?}"
    );
    assert_eq!(
        stdout.matches("[match]").count(),
        2,
        "two cases should be reported as matches, got: {stdout:?}"
    );
    assert_eq!(
        stdout.matches("[mismatch]").count(),
        1,
        "exactly one case should be reported as a mismatch, got: {stdout:?}"
    );
}

/// AC2 — `eval --format json` emits one JSON document carrying per-case
/// verdicts, their expected polarity, the derived `matched` flag, and the
/// aggregate accuracy — the artifact a built site embeds without re-scoring.
///
/// The whole stdout must parse as a *single* JSON value (so no log line or
/// second document interleaves), `accuracy` must be the number `2/3` (not a
/// string), and each case must carry non-null `expected`/`actual` strings and a
/// boolean `matched` consistent with `expected == actual` — pinning the array to
/// `[true, true, false]` proves the serialized flags are the real derived scores.
#[tokio::test]
async fn eval_json_emits_per_case_verdicts_and_aggregate() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_three_case_provider(&server).await;

    let output = eval_against(&server, &["--format", "json"]).await;

    assert_eq!(
        output.status.code(),
        Some(0),
        "eval --format json should succeed; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("stdout should be exactly one JSON document");

    let accuracy = doc["accuracy"]
        .as_f64()
        .expect("accuracy should be a JSON number, not a string");
    assert!(
        (accuracy - 2.0 / 3.0).abs() < 1e-12,
        "accuracy should be 2/3, got {accuracy}"
    );

    let cases = doc["cases"].as_array().expect("cases should be an array");
    assert_eq!(cases.len(), 3, "one entry per case");
    let matched: Vec<bool> = cases
        .iter()
        .map(|case| {
            assert!(
                case["expected"].is_string(),
                "expected should be a non-null string: {case}"
            );
            assert!(
                case["actual"].is_string(),
                "actual should be a non-null string: {case}"
            );
            let matched = case["matched"]
                .as_bool()
                .expect("matched should be a boolean");
            assert_eq!(
                matched,
                case["expected"] == case["actual"],
                "matched must reflect expected == actual: {case}"
            );
            matched
        })
        .collect();
    assert_eq!(matched, vec![true, true, false]);
}
