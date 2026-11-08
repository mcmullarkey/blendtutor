//! Integration tests for item provenance in `blendtutor eval` (ADR-0026).
//!
//! Eval cases may be marked `source: generated` (untagged cases are hand-written).
//! When a suite contains generated items, `eval` reports grader accuracy
//! separately for each source with a 95% Wilson interval. A gap between the two
//! screens the item generator for bias, for example items that are easier or
//! that share the grader's blind spots.
//!
//! The demo lesson has no checks; the grader is a `wiremock` stub keyed on the
//! word each submission prints.

mod common;

use common::{blendtutor_output, mount_feedback_for, rscript_absent};
use wiremock::MockServer;

const MIXED_LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_provenance/demo_lesson.yaml"
);

const HAND_ONLY_LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_command/demo_lesson.yaml"
);

async fn eval(server: &MockServer, lesson: &str, extra: &[&str]) -> std::process::Output {
    let uri = server.uri();
    let mut args = vec!["eval".to_string(), lesson.to_string()];
    args.extend(extra.iter().map(|s| s.to_string()));
    tokio::task::spawn_blocking(move || blendtutor_output(args, uri))
        .await
        .expect("the blocking command task should join")
}

/// When I run `eval` on a suite mixing hand-written and generated items, I see
/// grader accuracy for each source with a Wilson interval, and an untagged case
/// counts as hand-written.
///
/// Hand is 1/2 only if the untagged case counts as hand; generated is 3/3. A
/// split that ignored the untagged case, or pooled the sources, lands on
/// different counts.
#[tokio::test]
async fn eval_reports_grader_accuracy_by_item_source() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_feedback_for(&server, "handa", true, "ok").await;
    mount_feedback_for(&server, "handb", true, "ok").await; // expected incorrect -> mismatch
    mount_feedback_for(&server, "gena", true, "ok").await;
    mount_feedback_for(&server, "genb", false, "no").await;
    mount_feedback_for(&server, "genc", false, "no").await;

    let output = eval(&server, MIXED_LESSON, &["--format", "json"]).await;
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("stdout should be exactly one JSON document");
    let rows = doc["by_source"]
        .as_array()
        .expect("by_source should be an array");
    let want = [
        ("hand", 1, 2, (0.095, 0.905)),
        ("generated", 3, 3, (0.439, 1.0)),
    ];
    assert_eq!(rows.len(), want.len(), "{rows:?}");
    for (row, (source, matched, items, (lo, hi))) in rows.iter().zip(want) {
        assert_eq!(row["source"], source, "{row}");
        assert_eq!(row["matched"].as_u64(), Some(matched), "{row}");
        assert_eq!(row["items"].as_u64(), Some(items), "{row}");
        let got = row["interval"].as_array().expect("interval is [lo, hi]");
        let (g_lo, g_hi) = (got[0].as_f64().unwrap(), got[1].as_f64().unwrap());
        assert!(
            (g_lo - lo).abs() < 1e-3 && (g_hi - hi).abs() < 1e-3,
            "{source}: expected ~[{lo}, {hi}], got [{g_lo}, {g_hi}]"
        );
    }
    assert_eq!(doc["cases"][2]["source"], "generated");
    assert!(
        doc["cases"][0].get("source").is_none(),
        "a hand-written case keeps its prior wire shape"
    );

    let human = eval(&server, MIXED_LESSON, &[]).await;
    let stdout = String::from_utf8_lossy(&human.stdout).into_owned();
    for line in [
        "Grader accuracy by item source (95% interval):",
        "  hand: 1/2 matched (9%–91%)",
        "  generated: 3/3 matched (44%–100%)",
    ] {
        assert!(stdout.contains(line), "missing {line:?} in: {stdout}");
    }
}

/// When I run `eval` on a suite with only hand-written items, I see no source
/// split: there is nothing to compare, and the report keeps its prior shape.
#[tokio::test]
async fn eval_omits_the_source_split_without_generated_items() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_feedback_for(&server, "alpha", true, "ok").await;
    mount_feedback_for(&server, "beta", false, "no").await;
    mount_feedback_for(&server, "gamma", false, "no").await;

    let output = eval(&server, HAND_ONLY_LESSON, &["--format", "json"]).await;
    let doc: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("stdout should be exactly one JSON document");
    assert!(
        doc.get("by_source").is_none(),
        "no split without generated items: {doc}"
    );
    let human = eval(&server, HAND_ONLY_LESSON, &[]).await;
    assert!(!String::from_utf8_lossy(&human.stdout).contains("by item source"));
}
