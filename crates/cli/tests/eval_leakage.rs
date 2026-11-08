//! Integration tests for solution-leakage measurement in `blendtutor eval`
//! (ADR-0028).
//!
//! Feedback **leaks** when one of its fenced code blocks, appended to the
//! learner's submission, makes the lesson's checks pass when the submission
//! alone did not. The measure is deterministic: it runs the grader's code
//! through the real `Rscript`, so these tests skip without R. A case is
//! assessable only when it is labeled incorrect, the lesson has checks, and the
//! submission alone fails a check.

mod common;

use common::{blendtutor_output, mount_feedback_for, rscript_absent};
use wiremock::MockServer;

const LEAKAGE_LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_leakage/add_two.yaml"
);

const UNCHECKED_LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_command/demo_lesson.yaml"
);

const FIX: &str = "Close! Try this:\n\n```r\nadd_two <- function(x, y) x + y\n```\n";

async fn mount_provider(server: &MockServer) {
    mount_feedback_for(server, "zz_leak", false, FIX).await;
    mount_feedback_for(
        server,
        "zz_hint",
        false,
        "Look again at the operator you used.",
    )
    .await;
    mount_feedback_for(
        server,
        "zz_badcode",
        false,
        "Maybe:\n```r\nadd_two <- function(x, y) x / y\n```",
    )
    .await;
    mount_feedback_for(server, "zz_uncovered", false, FIX).await;
    mount_feedback_for(server, "zz_ok", true, FIX).await;
}

async fn eval(server: &MockServer, lesson: &str, extra: &[&str]) -> std::process::Output {
    let uri = server.uri();
    let mut args = vec!["eval".to_string(), lesson.to_string()];
    args.extend(extra.iter().map(|s| s.to_string()));
    tokio::task::spawn_blocking(move || blendtutor_output(args, uri))
        .await
        .expect("the blocking command task should join")
}

/// When I run `eval` on a checked lesson, I see each assessable incorrect case
/// marked leaked or withheld by actually running the feedback's code, cases that
/// cannot be assessed marked as such, and a leak rate over the assessable cases
/// with a Wilson interval.
///
/// The categories are only distinguishable by execution: case 3's feedback has a
/// code block but it does not fix the submission, and case 4's leaking block
/// is not assessable because the submission already passes. A "contains a code
/// block" heuristic lands both in the wrong bucket.
#[tokio::test]
async fn eval_measures_solution_leakage_by_running_the_feedbacks_code() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_provider(&server).await;

    let output = eval(&server, LEAKAGE_LESSON, &["--format", "json"]).await;
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("stdout should be exactly one JSON document");
    let per_case: Vec<Option<&str>> = doc["cases"]
        .as_array()
        .expect("cases array")
        .iter()
        .map(|case| {
            case.get("leakage")
                .map(|l| l.as_str().expect("leakage is a string"))
        })
        .collect();
    assert_eq!(
        per_case,
        vec![
            Some("leaked"),
            Some("withheld"),
            Some("withheld"),
            None,
            None
        ]
    );
    let summary = &doc["leakage"];
    assert_eq!(summary["leaked"].as_u64(), Some(1), "{summary}");
    assert_eq!(summary["assessable"].as_u64(), Some(3), "{summary}");
    assert_eq!(summary["incorrect"].as_u64(), Some(4), "{summary}");
    let interval = summary["interval"]
        .as_array()
        .expect("interval is [lo, hi]");
    let (lo, hi) = (interval[0].as_f64().unwrap(), interval[1].as_f64().unwrap());
    assert!(
        (lo - 0.062).abs() < 1e-3 && (hi - 0.792).abs() < 1e-3,
        "expected ~[0.062, 0.792], got [{lo}, {hi}]"
    );

    let human = eval(&server, LEAKAGE_LESSON, &[]).await;
    let stdout = String::from_utf8_lossy(&human.stdout).into_owned();
    assert!(
        stdout.contains(
            "Solution leakage: 1/3 assessable incorrect cases leaked a working fix (6%–79%); 1 of 4 incorrect cases not assessable"
        ),
        "got: {stdout}"
    );
}

/// When I run `eval` on a lesson with no checks, I see no leakage measure: the
/// definition needs checks, so the report keeps its prior shape.
#[tokio::test]
async fn eval_reports_no_leakage_without_checks() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_feedback_for(&server, "alpha", true, FIX).await;
    mount_feedback_for(&server, "beta", false, FIX).await;
    mount_feedback_for(&server, "gamma", false, FIX).await;

    let output = eval(&server, UNCHECKED_LESSON, &["--format", "json"]).await;
    let doc: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("stdout should be exactly one JSON document");
    assert!(doc.get("leakage").is_none(), "{doc}");
    assert!(doc["cases"][1].get("leakage").is_none(), "{doc}");
}
