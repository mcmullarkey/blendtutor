//! Spec test for the R-package cutover (Slice 20, issue #20, AC2).
//!
//! Once the Rust CLI covers authoring + run + eval + build, the obsolete R
//! package is retired so the active tree carries no dual R/Rust ambiguity
//! (§4.2). This pins the two invariants the issue's AC2 probe asserts:
//!   1. the R package's defining artifacts (`R/`, `NAMESPACE`, `DESCRIPTION`)
//!      are absent from the repo root; and
//!   2. no active (tracked, non-`legacy-r/`, non-lock) file references the R
//!      package's identifiers.
//! The R sources are preserved in git history — the `main` branch *is* the R
//! package — so retiring them on the integration branch loses nothing.
//!
//! Both checks need the git work tree, so they skip-with-notice when run outside
//! a checkout (mirroring the `Rscript`-absent convention in `tests/common`).

use std::path::{Path, PathBuf};
use std::process::Command;

/// Repo root via `git rev-parse`, or `None` (after a notice) when this is not a
/// git work tree — so a build from an unpacked tarball skips rather than fails.
fn repo_root() -> Option<PathBuf> {
    let out = Command::new("git")
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(["rev-parse", "--show-toplevel"])
        .output()
        .ok()?;
    if !out.status.success() {
        eprintln!("SKIP: not a git work tree — skipping R-cutover assertions");
        return None;
    }
    let root = String::from_utf8(out.stdout).ok()?;
    Some(PathBuf::from(root.trim()))
}

#[test]
fn r_package_artifacts_are_absent_from_root() {
    let Some(root) = repo_root() else { return };
    for artifact in ["R", "NAMESPACE", "DESCRIPTION"] {
        let path = root.join(artifact);
        assert!(
            !path.exists(),
            "the R package's {artifact} must be retired from the repo root, found {}",
            path.display()
        );
    }
}

#[test]
fn no_active_file_references_the_r_package() {
    let Some(root) = repo_root() else { return };

    // The R-package identifiers the cutover removes. `git grep` searches tracked
    // working-tree files; the exclusions mirror the AC2 probe — a `legacy-r/`
    // relocation path, lockfiles, and this test itself (which necessarily spells
    // the very patterns it forbids, so it must not match its own search).
    let output = Command::new("git")
        .current_dir(&root)
        .args([
            "grep",
            "-nE",
            "-e",
            "inst/lessons|fireworks_integration|Rscript.*educator|pak::pak|devtools::install",
            "--",
            ":!legacy-r/",
            ":!*.lock",
            ":!crates/cli/tests/cutover.rs",
        ])
        .output()
        .expect("run git grep");

    let hits = String::from_utf8_lossy(&output.stdout);
    // git grep exit status: 1 = no matches (clean), 0 = at least one match (a
    // live reference survived), anything else = grep itself errored. Match on the
    // code so an error can never be silently read as "clean".
    match output.status.code() {
        Some(1) => {}
        Some(0) => panic!("no active file may reference the retired R package; found:\n{hits}"),
        other => panic!(
            "git grep failed (exit {other:?}); stderr:\n{}",
            String::from_utf8_lossy(&output.stderr)
        ),
    }
}
