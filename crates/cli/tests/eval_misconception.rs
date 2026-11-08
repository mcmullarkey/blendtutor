//! Integration tests for per-misconception grader detection in `blendtutor eval`.
//!
//! When a suite has a misconception blueprint (ADR-0024), `eval` reports, for
//! each misconception, how many of its tagged items the grader caught (graded
//! incorrect), with a 95% Wilson score interval (ADR-0025). Small-n results are
//! shown honestly: 2/2 is reported as roughly 34%–100%, never as a bare 100%.
//!
//! The provider is a `wiremock` stub keyed on a `zz_*` comment token in each
//! submission; checks run through the real `Rscript`, so these skip without R.

mod common;

use common::{blendtutor_output, mount_feedback_for, rscript_absent};
use wiremock::MockServer;

const BLUEPRINT_LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_blueprint/add_two.yaml"
);

/// Script the grader so the misconceptions land at different detection rates:
/// M1 caught 2/2, M2 caught 1/2, M3 missed 0/1, M4 has no items.
/// One expected detection row: id, caught, items, and the ~interval (absent
/// when there are no items).
type ExpectedRow = (&'static str, u64, u64, Option<(f64, f64)>);

async fn mount_provider(server: &MockServer) {
    mount_feedback_for(server, "zz_ok", true, "adds correctly").await;
    mount_feedback_for(server, "zz_m1a", false, "returns only x").await;
    mount_feedback_for(server, "zz_m1b", false, "returns only x").await;
    mount_feedback_for(server, "zz_m2a", false, "prints instead of returning").await;
    mount_feedback_for(server, "zz_m2b", true, "looks fine").await;
    mount_feedback_for(server, "zz_m3", true, "looks fine").await;
}

async fn eval(server: &MockServer, extra: &[&str]) -> std::process::Output {
    let uri = server.uri();
    let mut args = vec!["eval".to_string(), BLUEPRINT_LESSON.to_string()];
    args.extend(extra.iter().map(|s| s.to_string()));
    tokio::task::spawn_blocking(move || blendtutor_output(args, uri))
        .await
        .expect("the blocking command task should join")
}

/// When I run `eval` on a suite with a blueprint, I see each misconception's
/// grader detection count and a 95% Wilson interval, a "no items" row for an
/// untested misconception, and untagged cases counted toward none.
///
/// Pinning the interval bounds catches a Wald interval (which collapses to
/// [1, 1] at 2/2 and [0, 0] at 0/1) and an implementation that counts the
/// untagged correct case or scores by `expected` instead of the grader.
#[tokio::test]
async fn eval_reports_grader_detection_per_misconception_with_wilson_intervals() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_provider(&server).await;

    let output = eval(&server, &["--format", "json"]).await;
    assert_eq!(
        output.status.code(),
        Some(0),
        "eval should succeed; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("stdout should be exactly one JSON document");
    let rows = doc["by_misconception"]
        .as_array()
        .expect("by_misconception should be an array");

    let expected: [ExpectedRow; 4] = [
        ("M1", 2, 2, Some((0.342, 1.0))),
        ("M2", 1, 2, Some((0.095, 0.905))),
        ("M3", 0, 1, Some((0.0, 0.793))),
        ("M4", 0, 0, None),
    ];
    assert_eq!(
        rows.len(),
        expected.len(),
        "one row per blueprint entry: {rows:?}"
    );
    for (row, (id, caught, items, interval)) in rows.iter().zip(expected) {
        assert_eq!(row["id"], id, "{row}");
        assert_eq!(row["caught"].as_u64(), Some(caught), "{row}");
        assert_eq!(row["items"].as_u64(), Some(items), "{row}");
        match interval {
            Some((lo, hi)) => {
                let got = row["interval"].as_array().expect("interval is [lo, hi]");
                let (g_lo, g_hi) = (got[0].as_f64().unwrap(), got[1].as_f64().unwrap());
                assert!(
                    (g_lo - lo).abs() < 1e-3 && (g_hi - hi).abs() < 1e-3,
                    "{id}: expected ~[{lo}, {hi}], got [{g_lo}, {g_hi}]"
                );
            }
            None => assert!(
                row.get("interval").is_none(),
                "no interval without items: {row}"
            ),
        }
    }

    let human = eval(&server, &[]).await;
    let stdout = String::from_utf8_lossy(&human.stdout).into_owned();
    for line in [
        "Grader detection by misconception (95% interval):",
        "  M1 returns x instead of x + y: 2/2 caught (34%–100%)",
        "  M2 prints the sum instead of returning it: 1/2 caught (9%–91%)",
        "  M3 hard-codes the example's answer: 0/1 caught (0%–79%)",
        "  M4 uses integer division: no items",
    ] {
        assert!(stdout.contains(line), "missing {line:?} in: {stdout}");
    }
}
