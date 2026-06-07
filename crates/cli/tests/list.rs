//! Integration tests for `blendtutor list`.
//!
//! Exercises lesson discovery end to end via the built binary (`assert_cmd`):
//! point `list` at a course directory and observe the rows it reports — the
//! slice's "when I run list, I see every lesson" behavior. Discovery rules are
//! unit-tested in `blendtutor-core`.

use assert_cmd::Command;
use serde_json::Value;

/// A course directory with a manifest and two valid lessons, one `r` and one
/// `python`. Lives under `core`'s fixtures (the schema's home) and is referenced
/// cross-crate by path.
const COURSE_BASIC: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/course_basic"
);

/// Run `list <course> --format json` via the built binary and parse stdout as a
/// JSON array of lesson rows.
fn list_json(course: &str) -> (std::process::Output, Vec<Value>) {
    let output = Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("list")
        .arg(course)
        .arg("--format")
        .arg("json")
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let rows: Vec<Value> = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("list json did not parse: {e}; stdout={stdout:?}"));
    (output, rows)
}

#[test]
fn list_reports_each_lessons_id_language_and_title() {
    let (output, rows) = list_json(COURSE_BASIC);

    assert!(
        output.status.success(),
        "`list` on a valid course should exit 0, got {:?}; stderr={:?}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(rows.len(), 2, "course_basic has exactly two lessons");

    // Both languages are present and lowercased — not collapsed to one default.
    let mut languages: Vec<&str> = rows
        .iter()
        .map(|r| r["language"].as_str().expect("each row carries a language"))
        .collect();
    languages.sort_unstable();
    assert_eq!(languages, ["python", "r"]);

    // Each row's title is the lesson's parsed name, not the manifest id nor an
    // empty default. Asserting the specific (id, language, title) triples kills a
    // "title defaults to id" fake-pass that a bare non-empty check would miss.
    let triple = |id: &str| -> (String, String) {
        let row = rows
            .iter()
            .find(|r| r["id"] == id)
            .unwrap_or_else(|| panic!("a row with id {id:?} should be listed; rows={rows:?}"));
        (
            row["language"].as_str().unwrap().to_string(),
            row["title"].as_str().unwrap().to_string(),
        )
    };
    assert_eq!(
        triple("add-two"),
        ("r".to_string(), "Add Two Numbers".to_string())
    );
    assert_eq!(
        triple("greet"),
        ("python".to_string(), "Greet Someone".to_string())
    );
}
