//! Integration tests for `blendtutor judge-agreement <judged.json> <labels.json>`:
//! how well the feedback judge agrees with people, per criterion (ADR-0035).
//!
//! The judged file is `judge-feedback --format json` output; the labels file is
//! people's blind pass/fail labels on the same item ids. Agreement is reported
//! as TPR (the judge passes what people pass) and TNR (the judge fails what
//! people fail), each with its Wilson interval, rather than one accuracy that a
//! skewed base rate can flatter. No provider is called.

mod common;

use common::blendtutor_output;

/// Items a and b are tagged (judged on diagnosis); c and d are not. Item d has
/// no human label, and the labels include an id the judge never saw.
const JUDGED: &str = r#"{
  "items": [
    {"id": "a", "judge": {
      "diagnosis": {"pass": true, "passes": 3, "samples": 3, "explanations": []},
      "actionability": {"pass": true, "passes": 3, "samples": 3, "explanations": []}}},
    {"id": "b", "judge": {
      "diagnosis": {"pass": true, "passes": 2, "samples": 3, "explanations": []},
      "actionability": {"pass": true, "passes": 3, "samples": 3, "explanations": []}}},
    {"id": "c", "judge": {
      "actionability": {"pass": true, "passes": 3, "samples": 3, "explanations": []}}},
    {"id": "d", "judge": {
      "actionability": {"pass": false, "passes": 0, "samples": 3, "explanations": []}}},
    {"id": "e", "judge": {
      "actionability": {"pass": false, "passes": 1, "samples": 3, "explanations": []}}}
  ]
}"#;

const LABELS: &str = r#"[
  {"id": "a", "diagnosis": "pass", "actionability": "pass", "note": "clear hint"},
  {"id": "b", "diagnosis": "fail", "actionability": "fail", "note": "pastes the fix"},
  {"id": "c", "actionability": "pass"},
  {"id": "e", "actionability": "fail"},
  {"id": "zz", "actionability": "pass"}
]"#;

fn agreement(dir: &std::path::Path, extra: &[&str]) -> std::process::Output {
    let judged = dir.join("judged.json");
    let labels = dir.join("labels.json");
    std::fs::write(&judged, JUDGED).expect("write judged");
    std::fs::write(&labels, LABELS).expect("write labels");
    let mut args = vec![
        "judge-agreement".to_string(),
        judged.display().to_string(),
        labels.display().to_string(),
    ];
    args.extend(extra.iter().map(|s| s.to_string()));
    blendtutor_output(args, common::dead_provider_url())
}

/// When I run `judge-agreement`, I see each criterion's TPR and TNR against the
/// human labels with intervals, the items where judge and person disagree, and
/// which ids could not be paired.
#[test]
fn reports_tpr_and_tnr_per_criterion_against_human_labels() {
    let dir = tempfile::tempdir().expect("tempdir");
    let output = agreement(dir.path(), &["--format", "json"]);
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value = serde_json::from_slice(&output.stdout).expect("one JSON document");

    let rate = |criterion: &str, which: &str| {
        let r = &doc[criterion][which];
        (r["hits"].as_u64(), r["of"].as_u64())
    };
    // Diagnosis pairs: a (judge pass, human pass), b (judge pass, human fail).
    assert_eq!(doc["diagnosis"]["pairs"].as_u64(), Some(2), "{doc}");
    assert_eq!(rate("diagnosis", "tpr"), (Some(1), Some(1)), "{doc}");
    assert_eq!(rate("diagnosis", "tnr"), (Some(0), Some(1)), "{doc}");
    // Actionability pairs: a P/P, b P/F, c P/P, e F/F (d has no label).
    assert_eq!(doc["actionability"]["pairs"].as_u64(), Some(4), "{doc}");
    assert_eq!(rate("actionability", "tpr"), (Some(2), Some(2)), "{doc}");
    assert_eq!(rate("actionability", "tnr"), (Some(1), Some(2)), "{doc}");

    let disagreements = doc["disagreements"].as_array().expect("disagreements");
    assert_eq!(disagreements.len(), 2, "{doc}");
    assert!(
        disagreements
            .iter()
            .all(|d| d["id"] == "b" && d["judge"] == true && d["human"] == false),
        "b is the item the judge passed and the person failed, on both criteria: {doc}"
    );
    assert_eq!(doc["unlabeled"], serde_json::json!(["d"]), "{doc}");
    assert_eq!(doc["unjudged"], serde_json::json!(["zz"]), "{doc}");

    let human = agreement(dir.path(), &[]);
    let stdout = String::from_utf8_lossy(&human.stdout).into_owned();
    for line in [
        "Judge vs human labels:",
        "  Diagnosis (2 pairs): TPR 1/1 (21%–100%), TNR 0/1 (0%–79%)",
        "  Actionability (4 pairs): TPR 2/2 (34%–100%), TNR 1/2 (9%–91%)",
        "  Disagreements: 2 (b diagnosis, b actionability)",
        "  Unpaired: 1 judged item without a label, 1 label without a judgment",
    ] {
        assert!(stdout.contains(line), "missing {line:?} in: {stdout}");
    }
}
