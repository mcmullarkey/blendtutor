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
    assert!(
        !result.timed_out,
        "a fast snippet must not be marked timed out"
    );
}

/// AC2 — when a run exceeds its timeout, the whole process tree (not just the
/// leader) is killed and a timeout result is returned, rather than the call
/// hanging for the run's natural duration. The grandchild's sentinel must stop
/// growing, which only holds if the kill reached the process *group*.
#[tokio::test]
async fn timeout_kills_process_tree_under_natural_runtime() {
    if rscript_absent() {
        return;
    }

    // A persistent dir — NOT the runner's temp CWD — for the grandchild's script
    // and the sentinel it appends to, both referenced by absolute path so they
    // survive the run dir being cleaned up.
    let dir = tempfile::tempdir().expect("create test dir");
    let sentinel = dir.path().join("sentinel.txt");
    let child_script = dir.path().join("child.R");
    std::fs::write(&sentinel, b"").expect("seed sentinel");
    std::fs::write(
        &child_script,
        format!(
            "repeat {{\n  cat('.', file = '{}', append = TRUE)\n  Sys.sleep(0.02)\n}}\n",
            sentinel.display()
        ),
    )
    .expect("write child script");

    // The parent spawns the grandchild (detached from our pipes so a leak shows
    // as a growing sentinel, not a hung read) then sleeps far past the timeout.
    let code = format!(
        "system2('Rscript', c('--vanilla', '{}'), wait = FALSE, stdout = FALSE, stderr = FALSE)\n\
         Sys.sleep(60)\n",
        child_script.display()
    );

    let runner = RRunner::new(Timeout(Duration::from_secs(2)));
    let start = std::time::Instant::now();
    let result = runner
        .execute(&code, &[])
        .await
        .expect("a runaway run is a timeout, not a runner error");
    let elapsed = start.elapsed();

    assert!(
        result.timed_out,
        "a run past its timeout must be marked timed out"
    );
    assert!(
        elapsed < Duration::from_secs(5),
        "execute must kill the tree near the 2s timeout, not wait out the 60s sleep; took {elapsed:?}"
    );

    // Give any un-reaped grandchild a full second to keep growing the sentinel.
    let size_at_return = std::fs::metadata(&sentinel).expect("stat sentinel").len();
    assert!(
        size_at_return > 0,
        "the grandchild never wrote to the sentinel — the test cannot prove a kill \
         reached the group, so a frozen size would be a vacuous pass"
    );
    tokio::time::sleep(Duration::from_secs(1)).await;
    let size_after = std::fs::metadata(&sentinel).expect("stat sentinel").len();
    assert_eq!(
        size_at_return, size_after,
        "the grandchild kept writing after the timeout — the process group was not \
         killed (sentinel grew {size_at_return} -> {size_after})"
    );
}
