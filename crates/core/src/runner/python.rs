//! Python execution mechanics: spawn the interpreter through `uv`, capture its
//! streams separately, bound it with a timeout, and reap a runaway process tree.
//!
//! This module owns *only* the Python subprocess details (ADR-0005, §4.1);
//! callers depend on the [`Runner`](super::Runner) trait, not on
//! [`PythonRunner`]. It mirrors the R runner; the shared spawn/timeout/temp-cwd
//! core is factored out of both in this slice's refactor step (§4.2).

use std::process::Stdio;
use std::time::Duration;

use command_group::AsyncCommandGroup;
use tokio::io::AsyncReadExt;
use tokio::process::Command;
use tokio::task::JoinHandle;
use tokio::time::timeout;

use super::{ExecutionResult, Runner, RunnerError, Timeout};

/// A [`Runner`] backed by a real Python subprocess spawned through `uv`.
#[derive(Debug, Clone)]
pub struct PythonRunner {
    timeout: Timeout,
}

impl PythonRunner {
    /// Build a runner that kills any execution exceeding `timeout`.
    pub fn new(timeout: Timeout) -> Self {
        Self { timeout }
    }
}

impl Default for PythonRunner {
    /// A 30-second bound — generous for a lesson exercise, finite for a runaway.
    fn default() -> Self {
        Self::new(Timeout(Duration::from_secs(30)))
    }
}

impl Runner for PythonRunner {
    async fn execute(
        &self,
        code: &str,
        _checks: &[String],
    ) -> Result<ExecutionResult, RunnerError> {
        // Per-execute temp dir as CWD: file-writing code is isolated to it and
        // the directory is removed when `run_dir` drops at the end of this fn.
        let run_dir =
            tempfile::TempDir::new().map_err(|e| RunnerError::new("create run directory", e))?;

        // Spawn Python through `uv` (the project's always-uv rule). `--no-project`
        // ignores any surrounding pyproject so capture is reproducible; `--quiet`
        // keeps uv's own chatter out of the streams; `-I` runs Python isolated
        // (no user site, no `PYTHON*` env), the analog of R's `--vanilla`.
        // `group_spawn` puts the tree in its own process group so a timeout kills
        // `uv` *and* the python child it launches, not just the leader.
        let mut child = Command::new("uv")
            .arg("run")
            .arg("--no-project")
            .arg("--quiet")
            .arg("python")
            .arg("-I")
            .arg("-c")
            .arg(code)
            .current_dir(run_dir.path())
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .group_spawn()
            .map_err(|e| RunnerError::new("spawn uv", e))?;

        // Drain both pipes concurrently so a chatty program cannot deadlock the
        // wait by filling a pipe buffer; the reads make progress while we await
        // the process below.
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

        let (exit, timed_out) = match timeout(self.timeout.0, child.wait()).await {
            Ok(status) => {
                let status = status.map_err(|e| RunnerError::new("wait for uv", e))?;
                (status.code(), false)
            }
            Err(_elapsed) => {
                // SIGKILL the whole process group so the python child uv launched
                // is reaped, not leaked; then reap the leader.
                child
                    .kill()
                    .await
                    .map_err(|e| RunnerError::new("kill uv group", e))?;
                let status = child
                    .wait()
                    .await
                    .map_err(|e| RunnerError::new("reap uv group", e))?;
                (status.code(), true)
            }
        };

        // Every group process is gone, so both pipes have closed and the readers
        // have finished; collect their bytes.
        let stdout = join_capture(out_task, "collect stdout").await?;
        let stderr = join_capture(err_task, "collect stderr").await?;

        Ok(ExecutionResult::from_capture(
            &stdout, &stderr, exit, timed_out,
        ))
        // `run_dir` drops here → the temp directory is removed.
    }
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
