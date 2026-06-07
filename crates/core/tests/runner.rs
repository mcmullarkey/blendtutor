//! Integration tests for the language-runner seam, exercised against a real
//! `Rscript`.
//!
//! These spawn the real interpreter, so they skip-with-notice when `Rscript` is
//! absent rather than fail spuriously — mirroring the Slice-1 convention that a
//! missing external tool is a skip, not a failure (see
//! `docs/agent-notes/workspace-and-ci.md`).

use std::time::Duration;

use blendtutor_core::runner::{ExecutionResult, RRunner, Runner, Timeout};

/// True (after printing a notice) when `Rscript` is not on `PATH`, so a test can
/// `return` early instead of failing on machines without R installed.
fn rscript_absent() -> bool {
    let present = std::process::Command::new("Rscript")
        .arg("--version")
        .output()
        .map(|out| out.status.success())
        .unwrap_or(false);
    if !present {
        eprintln!("SKIP: Rscript absent — skipping real-interpreter runner test");
    }
    !present
}

/// AC1 — stdout, stderr, and exit status land in three distinct fields of a
/// normalized [`ExecutionResult`]; the streams do not bleed into one another.
#[tokio::test]
async fn captures_stdout_stderr_exit_separately() {
    if rscript_absent() {
        return;
    }

    let runner = RRunner::new(Timeout(Duration::from_secs(30)));
    // `cat` writes to stdout; `message` writes to stderr; the program exits 0.
    // `execute` is fallible: an `Err` means the interpreter never ran (spawn/IO
    // failure), which is categorically distinct from R writing to stderr, so the
    // two never collapse into one buffer.
    let result: ExecutionResult = runner
        .execute("cat('OUT'); message('ERR')", &[])
        .await
        .expect("Rscript should spawn and run to completion");

    assert!(
        result.stdout.contains("OUT"),
        "stdout should carry cat() output, got {:?}",
        result.stdout
    );
    assert!(
        result.stderr.contains("ERR"),
        "stderr should carry message() output, got {:?}",
        result.stderr
    );
    assert!(
        !result.stdout.contains("ERR"),
        "stderr must not bleed into stdout, got {:?}",
        result.stdout
    );
    assert_eq!(result.exit, Some(0), "a clean exit is Some(0), not a blob");
    assert!(!result.timed_out, "a fast snippet must not be marked timed out");
}
