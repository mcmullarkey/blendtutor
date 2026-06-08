//! Spec test for the cutover README (Slice 20, issue #20).
//!
//! The repo's top-level README is the "what this is" surface (§4.1). After the
//! R-package → Rust-CLI cutover it must document the binary install and the full
//! `init → new → validate → run → eval → build` authoring/run/eval/deploy
//! workflow, including the GitHub Pages + COOP/COEP cross-origin-isolation note a
//! built webR/Pyodide site needs (see `docs/agent-notes/site-build.md`). This
//! pins each required token so a README that drops a command, the install
//! section, or the deploy note fails the build — the same set the issue's AC1
//! probe greps, anchored here so CI (nextest) gates it. It does not judge prose
//! quality; that is a manual readability skim.

use std::path::Path;

/// The repo-root README, read at run time relative to this crate's manifest dir
/// so the assertion always reflects the file on disk and is independent of the
/// process working directory.
fn readme() -> String {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../README.md");
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("read repo-root README at {}: {e}", path.display()))
}

#[test]
fn readme_documents_the_cli_install_and_workflow() {
    let haystack = readme().to_lowercase();

    // (label shown on failure, needle that must appear). The six workflow
    // commands are pinned as their literal `blendtutor <cmd>` invocations — a
    // stronger predicate than a bare word, so prose that merely uses "run" or
    // "new" in passing cannot satisfy it.
    let required: &[(&str, &str)] = &[
        ("binary install", "cargo install"),
        ("init command", "blendtutor init"),
        ("new command", "blendtutor new"),
        ("validate command", "blendtutor validate"),
        ("run command", "blendtutor run"),
        ("eval command", "blendtutor eval"),
        ("build command", "blendtutor build"),
        ("GitHub Pages deploy", "github pages"),
    ];

    let mut missing: Vec<&str> = required
        .iter()
        .filter(|(_, needle)| !haystack.contains(needle))
        .map(|(label, _)| *label)
        .collect();

    // Cross-origin isolation is documented under any of its conventional
    // spellings — the headers (COOP/COEP) or the concept they enable.
    if !["coop", "coep", "cross-origin"]
        .iter()
        .any(|needle| haystack.contains(needle))
    {
        missing.push("COOP/COEP cross-origin-isolation note");
    }

    assert!(
        missing.is_empty(),
        "README.md must document the Rust CLI cutover; missing: {missing:?}"
    );
}
