//! Integration tests for the language-runner seam, exercised against a real
//! `Rscript`.
//!
//! These spawn the real interpreter, so they skip-with-notice when `Rscript` is
//! absent rather than fail spuriously — mirroring the Slice-1 convention that a
//! missing external tool is a skip, not a failure (see
//! `docs/agent-notes/workspace-and-ci.md`).

use std::path::PathBuf;
use std::time::Duration;

use blendtutor_core::runner::{ExecutionResult, PythonRunner, RRunner, Runner, Timeout};

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

/// AC3 — file-writing code runs in an isolated temp working directory that is
/// removed when the run returns, and the caller's own directory is left
/// untouched: a relative write lands in the temp dir, not back in the CWD.
#[tokio::test]
async fn runs_in_isolated_temp_dir_cleaned_up() {
    if rscript_absent() {
        return;
    }

    // Treat a fresh dir as the caller's CWD, with a pre-existing marker. nextest
    // runs each test in its own process, so changing the process CWD here cannot
    // disturb sibling tests — and a non-isolating regression would drop its
    // stray `out.txt` here rather than in the source tree.
    let outer = tempfile::tempdir().expect("create outer dir");
    let marker = outer.path().join("marker.txt");
    std::fs::write(&marker, b"keep me").expect("seed marker");
    std::env::set_current_dir(outer.path()).expect("cd into outer dir");

    // R reports its working directory (so the test can locate the run dir) then
    // writes a file by *relative* path — which must land in the isolated run
    // dir, not back in `outer`.
    let runner = RRunner::default();
    let result = runner
        .execute("cat(getwd()); writeLines('x', 'out.txt')", &[])
        .await
        .expect("Rscript should spawn and run to completion");

    let run_dir = PathBuf::from(result.stdout.trim());
    // `getwd()` resolves symlinks (macOS /var -> /private/var), so canonicalize
    // the temp root before the prefix check.
    let temp_root = std::fs::canonicalize(std::env::temp_dir()).expect("canonicalize temp dir");
    assert!(
        run_dir.starts_with(&temp_root),
        "run dir must be under the system temp dir {temp_root:?}, got {run_dir:?}"
    );
    assert!(
        !run_dir.exists(),
        "the per-run temp dir must be removed when execute returns, but {run_dir:?} survived"
    );
    assert!(
        !outer.path().join("out.txt").exists(),
        "a relative write escaped the isolated run dir into the caller's directory"
    );
    assert!(
        marker.exists(),
        "the caller's pre-existing marker was disturbed by the run"
    );
}

/// True (after printing a notice) when `uv run python` cannot run here, so a
/// Python-runner test can `return` early instead of failing on machines without
/// `uv` (or without a Python it can resolve). The `PythonRunner` spawns the
/// interpreter through `uv` (per the project's always-`uv` rule), so this guards
/// on exactly what the runner needs — not on a bare `python` that may be absent
/// even where `uv run python` works.
fn uv_python_absent() -> bool {
    let present = std::process::Command::new("uv")
        .args(["run", "--no-project", "--quiet", "python", "--version"])
        .output()
        .map(|out| out.status.success())
        .unwrap_or(false);
    if !present {
        eprintln!(
            "SKIP: `uv run python` unavailable — skipping real-interpreter python runner test"
        );
    }
    !present
}

/// AC1 — a Python snippet's stdout, stderr, and exit status land in three
/// distinct fields of a normalized [`ExecutionResult`], captured identically to
/// the R runner through the same [`Runner`] trait; the streams do not bleed into
/// one another.
#[tokio::test]
async fn python_captures_streams_distinctly() {
    if uv_python_absent() {
        return;
    }

    let runner = PythonRunner::new(Timeout(Duration::from_secs(30)));
    // Write "OUT" to stdout and "ERR" to stderr, then exit 0. `execute` is
    // fallible: an `Err` means the interpreter never ran (spawn/IO failure),
    // categorically distinct from Python writing to stderr, so the two never
    // collapse into one buffer.
    let result: ExecutionResult = runner
        .execute(
            "import sys; sys.stdout.write('OUT'); sys.stderr.write('ERR')",
            &[],
        )
        .await
        .expect("python should spawn and run to completion");

    assert!(
        result.stdout.contains("OUT"),
        "stdout should carry the stdout write, got {:?}",
        result.stdout
    );
    assert!(
        result.stderr.contains("ERR"),
        "stderr should carry the stderr write, got {:?}",
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
