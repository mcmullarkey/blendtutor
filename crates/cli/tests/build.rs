//! Integration tests for `blendtutor build --target webr`.
//!
//! Exercises static-site assembly end to end via the built binary: point
//! `build` at an R course and inspect the directory it emits — the slice's
//! "when I build, I get a deployable webR site" behavior. The pure assembly
//! rules (`plan_site`) are unit-tested in `blendtutor-core`; here we observe the
//! files that actually land on disk.

use std::path::Path;

use assert_cmd::Command;
use serde_json::Value;

/// An R-only course: a manifest and two valid R lessons, each carrying checks
/// and a reference solution. Lives under `core`'s fixtures (the schema's home)
/// and is referenced cross-crate by path.
const R_COURSE: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../core/tests/fixtures/r-course");

/// Run `build --target <target> <course> -o <out>` via the built binary.
fn build(target: &str, course: &str, out: &Path) -> std::process::Output {
    Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("build")
        .arg("--target")
        .arg(target)
        .arg(course)
        .arg("-o")
        .arg(out)
        .output()
        .unwrap()
}

#[test]
fn build_webr_emits_a_deployable_r_lesson_site() {
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build("webr", R_COURSE, &out);
    assert!(
        output.status.success(),
        "`build --target webr` on an R course should exit 0, got {:?}; stderr={:?}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    // The page shell, the in-browser runner (which boots webR), and the COOP/COEP
    // shim all land.
    for name in ["index.html", "lesson-runner.js", "coi-serviceworker.js"] {
        assert!(out.join(name).is_file(), "the built site is missing {name}");
    }

    // The shim is *referenced* from index.html — present-but-dead breaks webR on
    // GitHub Pages (no SharedArrayBuffer), so a bare file on disk is not enough.
    let index = std::fs::read_to_string(out.join("index.html")).unwrap();
    assert!(
        index.contains("coi-serviceworker.js"),
        "index.html must reference the coi-serviceworker shim; index.html={index}"
    );

    // The webR boot is wired into the runner (not a separate file, per the spec).
    let runner = std::fs::read_to_string(out.join("lesson-runner.js")).unwrap();
    assert!(
        runner.to_lowercase().contains("webr"),
        "the lesson runner must boot webR; runner={runner}"
    );

    // One JSON per manifest entry — count matches the course, neither dropped nor
    // duplicated — and each carries the fields the JS contract consumes.
    let mut ids: Vec<String> = std::fs::read_dir(out.join("lessons"))
        .expect("the site has a lessons/ directory")
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "json"))
        .map(|path| {
            let text = std::fs::read_to_string(&path).unwrap();
            let lesson: Value = serde_json::from_str(&text)
                .unwrap_or_else(|e| panic!("lesson JSON {path:?} did not parse: {e}"));
            assert!(lesson["title"].is_string(), "lesson JSON needs a title: {lesson}");
            assert!(lesson["checks"].is_array(), "lesson JSON needs checks: {lesson}");
            assert!(
                lesson["solution"].is_string(),
                "lesson JSON needs a solution the runner can submit: {lesson}"
            );
            lesson["id"]
                .as_str()
                .expect("lesson JSON needs an id")
                .to_string()
        })
        .collect();
    ids.sort();
    assert_eq!(
        ids,
        vec!["add-two".to_string(), "square".to_string()],
        "exactly the course's two lessons are serialized, by slug"
    );
}

#[test]
fn build_refuses_a_language_target_mismatch_before_writing_anything() {
    // An R course built for the Pyodide (Python) target is a language/target
    // mismatch: it must be refused at the boundary (§1.3.1) and emit *nothing*,
    // so a half-written, broken site never ships.
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build("pyodide", R_COURSE, &out);
    assert!(
        !output.status.success(),
        "an R course built for the pyodide target must fail; stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    // The failure names the language/target mismatch — not a generic parse error
    // — so this stays red until the *refusal* exists, not merely until the args
    // are rejected.
    let stderr = String::from_utf8_lossy(&output.stderr).to_lowercase();
    assert!(
        stderr.contains("language") || stderr.contains("does not match"),
        "the failure should name the language/target mismatch; stderr={stderr}"
    );

    // Nothing was written: plan refused before write_site ran.
    assert!(
        !out.exists(),
        "a refused build must not create the output directory"
    );
}
