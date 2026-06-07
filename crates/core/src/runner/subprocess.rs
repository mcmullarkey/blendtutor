//! Shared subprocess mechanics for every language runner: spawn an interpreter
//! in an isolated temp working directory, drain its streams without deadlocking,
//! bound it with a timeout, and reap the whole process group on overrun.
//!
//! The language-specific part — which program to run and which flags precede the
//! learner code — is the caller's, expressed as an [`Interpreter`]. This module
//! owns the spawn/timeout/temp-cwd core so each `Runner` impl is a small
//! descriptor over it rather than a per-language copy of the dance (§4.2 —
//! factored, and deliberately not a "utils" grab-bag: it does one thing).

use std::process::Stdio;

use command_group::AsyncCommandGroup;
use tokio::io::AsyncReadExt;
use tokio::process::Command;
use tokio::task::JoinHandle;
use tokio::time::timeout;

use super::{ExecutionResult, RunnerError, Timeout};

/// How to launch one language's interpreter for a single run.
///
/// The only thing that differs between the R and Python runners: the program
/// and the arguments that precede the code. Everything else (isolation, capture,
/// timeout, process-group kill) is identical and lives in [`run`].
pub(super) struct Interpreter {
    /// The program to spawn, e.g. `"Rscript"` or `"uv"`.
    pub program: &'static str,
    /// The arguments that precede the learner code — e.g. `["--vanilla", "-e"]`
    /// for R, or `["run", "--no-project", "--quiet", "python", "-I", "-c"]` for
    /// Python via uv. The code itself is appended as the final argument.
    pub code_args: &'static [&'static str],
}

/// Run `code` under `interpreter`, bounded by `bound`, and normalize what it did
/// into an [`ExecutionResult`].
///
/// The whole effectful dance lives here: a per-run [`tempfile::TempDir`] as CWD
/// (file-writing code is isolated to it and it is removed on return),
/// `group_spawn` into a fresh process group, concurrent pipe drains so a chatty
/// program cannot deadlock the wait by filling a pipe buffer, and a
/// process-group SIGKILL on overrun so a child the interpreter spawned is reaped
/// rather than leaked. An [`Err`] means the interpreter never ran.
pub(super) async fn run(
    interpreter: &Interpreter,
    code: &str,
    bound: Timeout,
) -> Result<ExecutionResult, RunnerError> {
    let run_dir =
        tempfile::TempDir::new().map_err(|e| RunnerError::new("create run directory", e))?;

    let mut child = Command::new(interpreter.program)
        .args(interpreter.code_args)
        .arg(code)
        .current_dir(run_dir.path())
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .group_spawn()
        .map_err(|e| RunnerError::new(format!("spawn {}", interpreter.program), e))?;

    // Drain both pipes concurrently so a chatty program cannot deadlock the wait
    // by filling a pipe buffer; the reads make progress while we await the
    // process below.
    let mut stdout = child.inner().stdout.take().expect("stdout piped above");
    let mut stderr = child.inner().stderr.take().expect("stderr piped above");
    let out_task: JoinHandle<std::io::Result<Vec<u8>>> = tokio::spawn(async move {
        let mut buf = Vec::new();
        stdout.read_to_end(&mut buf).await.map(|_| buf)
    });
    let err_task: JoinHandle<std::io::Result<Vec<u8>>> = tokio::spawn(async move {
        let mut buf = Vec::new();
        stderr.read_to_end(&mut buf).await.map(|_| buf)
    });

    let (exit, timed_out) = match timeout(bound.0, child.wait()).await {
        Ok(status) => {
            let status = status
                .map_err(|e| RunnerError::new(format!("wait for {}", interpreter.program), e))?;
            (status.code(), false)
        }
        Err(_elapsed) => {
            // SIGKILL the whole process group so a child the interpreter spawned
            // is reaped, not leaked; then reap the leader.
            child
                .kill()
                .await
                .map_err(|e| RunnerError::new(format!("kill {} group", interpreter.program), e))?;
            let status = child
                .wait()
                .await
                .map_err(|e| RunnerError::new(format!("reap {} group", interpreter.program), e))?;
            (status.code(), true)
        }
    };

    // Every group process is gone, so both pipes have closed and the readers have
    // finished; collect their bytes.
    let stdout = join_capture(out_task, "collect stdout").await?;
    let stderr = join_capture(err_task, "collect stderr").await?;

    Ok(ExecutionResult::from_capture(
        &stdout, &stderr, exit, timed_out,
    ))
    // `run_dir` drops here → the temp directory is removed.
}

/// Await a stream-reader task, flattening its join error and IO error into a
/// [`RunnerError`].
async fn join_capture(
    task: JoinHandle<std::io::Result<Vec<u8>>>,
    context: &'static str,
) -> Result<Vec<u8>, RunnerError> {
    match task.await {
        Ok(Ok(bytes)) => Ok(bytes),
        Ok(Err(io_err)) => Err(RunnerError::new(context, io_err)),
        Err(join_err) => Err(RunnerError::new(context, std::io::Error::other(join_err))),
    }
}
