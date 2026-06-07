//! Integration tests for `blendtutor init`.
//!
//! Exercises course scaffolding end to end via the built binary (`assert_cmd`):
//! run `init` against a directory, then observe both the files it writes and that
//! the freshly scaffolded course is one `list` immediately accepts — the slice's
//! "when I run init, I get a ready-to-edit course" behavior. The pure plan and
//! the refuse-on-nonempty guard are unit-tested in `blendtutor-core`.

use std::path::Path;

use assert_cmd::Command;
use serde_json::Value;

/// The filenames directly under `dir`, for asserting which scaffold artifacts
/// were (or were not) written without depending on directory order.
fn entry_names(dir: &Path) -> Vec<String> {
    std::fs::read_dir(dir)
        .unwrap()
        .map(|entry| entry.unwrap().file_name().to_string_lossy().into_owned())
        .collect()
}

#[test]
fn init_scaffolds_a_ready_course_that_list_accepts() {
    let dir = tempfile::tempdir().unwrap();
    let course = dir.path();

    Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("init")
        .arg(course)
        .assert()
        .success();

    // All five artifact kinds are present: manifest, an example lesson yaml (not
    // an eval), an eval suite, a README, and a key-ignoring gitignore. Asserting
    // the whole set kills the "mkdir / one-placeholder" stub the spec calls out.
    let names = entry_names(course);
    assert!(
        names.iter().any(|n| n == "blendtutor.toml"),
        "a manifest is scaffolded; got {names:?}"
    );
    assert!(
        names.iter().any(|n| n == ".gitignore"),
        "a key-ignoring .gitignore is scaffolded; got {names:?}"
    );
    assert!(
        names
            .iter()
            .any(|n| n.starts_with("eval_") && n.ends_with(".yaml")),
        "an example eval suite is scaffolded; got {names:?}"
    );
    assert!(
        names
            .iter()
            .any(|n| n.contains("lesson") && n.ends_with(".yaml") && !n.starts_with("eval_")),
        "an example lesson is scaffolded; got {names:?}"
    );
    assert!(
        names.iter().any(|n| n.to_lowercase().starts_with("readme")),
        "a README is scaffolded; got {names:?}"
    );

    // The scaffolded course is not merely non-empty: `list` opens it and reports
    // at least one successfully-discovered lesson (error == null). This fails a
    // stub whose manifest points at a lesson that does not parse.
    let listing = Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("list")
        .arg(course)
        .arg("--format")
        .arg("json")
        .output()
        .unwrap();
    assert!(
        listing.status.success(),
        "`list` should accept the scaffolded course, got {:?}; stderr={:?}",
        listing.status,
        String::from_utf8_lossy(&listing.stderr)
    );
    let rows: Vec<Value> = serde_json::from_str(&String::from_utf8_lossy(&listing.stdout))
        .expect("list --format json emits a JSON array");
    let discovered = rows.iter().filter(|row| row["error"].is_null()).count();
    assert!(
        discovered >= 1,
        "the scaffolded course lists at least one valid lesson; rows={rows:?}"
    );
}

#[test]
fn init_refuses_a_nonempty_target_leaving_it_untouched() {
    let dir = tempfile::tempdir().unwrap();
    let course = dir.path();
    let sentinel = course.join("sentinel.txt");
    std::fs::write(&sentinel, "KEEP").unwrap();

    let output = Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("init")
        .arg(course)
        .output()
        .unwrap();

    // Refuses rather than overwriting: nonzero exit with a clear message that the
    // target is not empty.
    assert!(
        !output.status.success(),
        "init must refuse a non-empty target, got {:?}; stderr={:?}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        String::from_utf8_lossy(&output.stderr)
            .to_lowercase()
            .contains("not empty"),
        "the refusal should explain the target is not empty; stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    // The pre-existing file is byte-for-byte untouched.
    assert_eq!(
        std::fs::read_to_string(&sentinel).unwrap(),
        "KEEP",
        "the guard must fire before any write, leaving existing files intact"
    );

    // Nothing else was created: the guard precedes every write, so a
    // partial-clobber (write some files, then notice the dir is non-empty) cannot
    // happen. The directory still holds only its pre-existing file.
    let names = entry_names(course);
    assert_eq!(
        names,
        vec!["sentinel.txt".to_string()],
        "no scaffold artifact should appear in a refused target; got {names:?}"
    );
}
