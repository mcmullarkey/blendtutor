//! Integration tests for `blendtutor new lesson --lang <r|python> <id>`.
//!
//! Exercises lesson authoring end to end via the built binary (`assert_cmd`):
//! scaffold a course with `init`, add a lesson with `new lesson --lang`, then
//! observe that the lesson file lands, `validate` accepts it, and it shows up in
//! `list` with the right language — the slice's "when I run new lesson, I get a
//! language-appropriate lesson registered in the course" behavior. The no-clobber
//! refusal is the second slice. Pure template selection and the boundary guards
//! are unit-tested in `blendtutor-core`.

use assert_cmd::Command;
use serde_json::Value;

/// Scaffold a fresh course into a temp dir and return the handle. The course has
/// no lesson `greet`, so a `new lesson greet` lands cleanly (AC1's `fresh_init_course`).
fn fresh_init_course() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("init")
        .arg(dir.path())
        .assert()
        .success();
    dir
}

#[test]
fn new_lesson_python_creates_a_lesson_that_validates_and_lists() {
    let course = fresh_init_course();

    // `new lesson --lang python greet`, run *inside* the course (the command
    // operates on the current directory, as the AC1 probe does).
    Command::cargo_bin("blendtutor")
        .unwrap()
        .current_dir(course.path())
        .args(["new", "lesson", "--lang", "python", "greet"])
        .assert()
        .success();

    // The lesson file lands under `lessons/<id>.yaml`.
    let lesson_path = course.path().join("lessons").join("greet.yaml");
    assert!(
        lesson_path.is_file(),
        "new lesson should write lessons/greet.yaml; missing at {lesson_path:?}"
    );

    // `validate` accepts the generated lesson: a template that hardcodes a broken
    // schema would fail this conjunct.
    Command::cargo_bin("blendtutor")
        .unwrap()
        .current_dir(course.path())
        .args(["validate", "lessons/greet.yaml"])
        .assert()
        .success();

    // `list` discovers a `greet` row whose language is `python` (the lowercase
    // wire form). A template hardcoding R, or a lesson written but never
    // registered in the manifest, each breaks a distinct conjunct here.
    let listing = Command::cargo_bin("blendtutor")
        .unwrap()
        .args(["list"])
        .arg(course.path())
        .args(["--format", "json"])
        .output()
        .unwrap();
    assert!(
        listing.status.success(),
        "`list` should accept the course after new lesson, got {:?}; stderr={:?}",
        listing.status,
        String::from_utf8_lossy(&listing.stderr)
    );
    let rows: Vec<Value> = serde_json::from_str(&String::from_utf8_lossy(&listing.stdout))
        .expect("list --format json emits a JSON array");
    let greet = rows
        .iter()
        .find(|row| row["id"] == "greet")
        .unwrap_or_else(|| panic!("list should show a `greet` row; rows={rows:?}"));
    assert!(
        greet["error"].is_null(),
        "the greet row must be a discovered lesson, not an error row; got {greet:?}"
    );
    assert_eq!(
        greet["language"], "python",
        "the new lesson's language should be python; got {greet:?}"
    );
}

#[test]
fn new_lesson_r_creates_a_lesson_that_validates_and_lists() {
    // The symmetric twin of the python happy path: --lang r drives the R template
    // end to end, so the language selection is exercised on both branches at the
    // CLI boundary, not just python.
    let course = fresh_init_course();

    Command::cargo_bin("blendtutor")
        .unwrap()
        .current_dir(course.path())
        .args(["new", "lesson", "--lang", "r", "tally"])
        .assert()
        .success();

    Command::cargo_bin("blendtutor")
        .unwrap()
        .current_dir(course.path())
        .args(["validate", "lessons/tally.yaml"])
        .assert()
        .success();

    let listing = Command::cargo_bin("blendtutor")
        .unwrap()
        .args(["list"])
        .arg(course.path())
        .args(["--format", "json"])
        .output()
        .unwrap();
    assert!(listing.status.success());
    let rows: Vec<Value> = serde_json::from_str(&String::from_utf8_lossy(&listing.stdout))
        .expect("list --format json emits a JSON array");
    let tally = rows
        .iter()
        .find(|row| row["id"] == "tally")
        .unwrap_or_else(|| panic!("list should show a `tally` row; rows={rows:?}"));
    assert!(
        tally["error"].is_null(),
        "the tally row must be discovered; got {tally:?}"
    );
    assert_eq!(
        tally["language"], "r",
        "the new lesson's language should be r; got {tally:?}"
    );
}

#[test]
fn new_lesson_refuses_a_duplicate_id_without_clobbering() {
    let course = fresh_init_course();

    // Create lesson `dup` as an R lesson, then snapshot its bytes and the manifest.
    Command::cargo_bin("blendtutor")
        .unwrap()
        .current_dir(course.path())
        .args(["new", "lesson", "--lang", "r", "dup"])
        .assert()
        .success();
    let lesson_path = course.path().join("lessons").join("dup.yaml");
    let lesson_before = std::fs::read(&lesson_path).expect("the first lesson is written");
    let manifest_before = std::fs::read(course.path().join("blendtutor.toml")).unwrap();

    // A second `new lesson` with the same id but a different language must refuse.
    Command::cargo_bin("blendtutor")
        .unwrap()
        .current_dir(course.path())
        .args(["new", "lesson", "--lang", "python", "dup"])
        .assert()
        .failure();

    // The original lesson is byte-for-byte untouched (still the R template), and
    // the manifest gained no duplicate entry: the guard fires before any write.
    assert_eq!(
        std::fs::read(&lesson_path).unwrap(),
        lesson_before,
        "a refused duplicate must not change the existing lesson's bytes"
    );
    assert_eq!(
        std::fs::read(course.path().join("blendtutor.toml")).unwrap(),
        manifest_before,
        "a refused duplicate must not append a manifest entry"
    );
}
