//! Spec test for the R-package cutover (Slice 20, issue #20, AC2).
//!
//! Once the Rust CLI covers authoring + run + eval + build, the obsolete R
//! package is retired so the active tree carries no dual R/Rust ambiguity
//! (§4.2). This pins the *structural* half of AC2 — the R package's source
//! artifacts are gone from the tracked tree:
//!   1. `R/`, `NAMESPACE`, `DESCRIPTION` are absent from the repo root; and
//!   2. no R-package source file (`R/`, `man/`, a `.R`/`.Rd`/`.Rproj`, or the
//!      build configs) is still tracked — only the Rust crates' `.R` student-code
//!      fixtures under `crates/` remain.
//! The R sources are preserved in git history — the `main` branch *is* the R
//! package — so retiring them on the integration branch loses nothing.
//!
//! The *content* half (no active file mentions an R-package identifier such as
//! `pak`/`devtools` install incantations or the old lesson directory) is the
//! issue's AC2 grep probe, run at slice verification. It is deliberately not
//! reproduced here: a committed test that spelled those literals would itself be
//! a match the probe must then special-case — the probe avoids self-matching
//! only because it lives outside the tracked tree. So this test gates the file
//! shapes (which carry no forbidden token) and leaves the literal grep to the
//! probe.
//!
//! Both checks need the git work tree, so they skip-with-notice when run outside
//! a checkout (mirroring the `Rscript`-absent convention in `tests/common`).

use std::path::PathBuf;
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

/// Every tracked path in the work tree, one per line.
fn tracked_files(root: &PathBuf) -> Vec<String> {
    let out = Command::new("git")
        .current_dir(root)
        .args(["ls-files"])
        .output()
        .expect("run git ls-files");
    assert!(out.status.success(), "git ls-files failed");
    String::from_utf8_lossy(&out.stdout)
        .lines()
        .map(str::to_owned)
        .collect()
}

/// True for a path that belongs to the retired R package, as opposed to the Rust
/// crates' `.R` student-code fixtures (kept under `crates/`).
fn is_retired_r_package_path(path: &str) -> bool {
    if path.starts_with("crates/") {
        return false;
    }
    path.starts_with("R/")
        || path.starts_with("man/")
        || path.ends_with(".R")
        || path.ends_with(".Rd")
        || path.ends_with(".Rproj")
        || path.ends_with(".Rprofile")
        || path.ends_with(".Rbuildignore")
        || matches!(path, "NAMESPACE" | "DESCRIPTION")
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
fn no_r_package_sources_are_tracked() {
    let Some(root) = repo_root() else { return };
    let offenders: Vec<String> = tracked_files(&root)
        .into_iter()
        .filter(|path| is_retired_r_package_path(path))
        .collect();
    assert!(
        offenders.is_empty(),
        "the R package sources must be retired (only crates/ .R fixtures may \
         remain); still tracked: {offenders:?}"
    );
}
