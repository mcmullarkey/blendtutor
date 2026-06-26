---
topic: runner
created: 2026-06-06
slices: [7, 8]
---

How `core::runner` runs learner code: the language-neutral seam and the R
subprocess mechanics. The Python runner (Slice 8) mirrors this design.

- 2026-06-06 (#7): The seam is the `Runner` trait in `crates/core/src/runner/mod.rs`
  (`execute(&self, code, checks) -> Result<ExecutionResult, RunnerError>`).
  Consumers (grading, CLI, Slice 8) depend on the trait, never on the concrete
  `RRunner` (§3.4, ADR-0005). The R-specific code lives in the private `r`
  submodule and is re-exported as `runner::RRunner`.
- 2026-06-06 (#7): The trait method is declared returning
  `impl Future<Output = …> + Send`, NOT `async fn`. A public trait with `async fn`
  trips clippy's `async_fn_in_trait`, which is an error under CI's `-D warnings`.
  The impl block still writes a plain `async fn execute` — that satisfies the
  `impl Future` RPITIT. Mirror this for the Python runner.
- 2026-06-06 (#7): `execute` is fallible on purpose. `Err(RunnerError)` means the
  interpreter never ran (spawn/IO failure); a program that ran and wrote to stderr
  is `Ok` with a populated `stderr`. So a launch failure can never masquerade as a
  learner diagnostic (§1.3). `RunnerError` is a typed error (context string + the
  io::Error source) so `core` stays `anyhow`-free (ADR-0001).
- 2026-06-06 (#7): `ExecutionResult` keeps `stdout`/`stderr`/`exit`/`final_value`/
  `timed_out` as five distinct fields (§1.2). `final_value` is a reserved forward
  shape — always `None` until grading (Slice 9) supplies its producer. `checks`
  is likewise reserved (`&[String]`, unused in v1); the grading slice refines the
  element type.
- 2026-06-06 (#7): **command-group feature flag is `with-tokio`, not `tokio`.**
  Selecting `features = ["tokio"]` only turns on the optional tokio dependency and
  silently leaves `AsyncCommandGroup`/`AsyncGroupChild` un-exported — you get
  "no method named `group_spawn`". Use `command-group = { features = ["with-tokio"] }`.
- 2026-06-06 (#7): **Process-group kill is load-bearing for the timeout.** Spawn
  via `AsyncCommandGroup::group_spawn` (new process group), and on timeout call
  `AsyncGroupChild::kill().await` (SIGKILL to the whole group). The tokio
  `Child::kill` (reachable via `child.inner().kill()`) kills only the leader — an
  R child that spawned its own children leaks. AC2's negative control proved this:
  leader-only kill let the grandchild keep growing its sentinel.
- 2026-06-06 (#7): **Drain stdout/stderr concurrently or `wait` deadlocks.** A
  program that fills a ~64 KB pipe buffer blocks on write while `wait` blocks on
  exit. Take the piped handles off `child.inner()` and read them in `tokio::spawn`
  tasks before awaiting the process, then join the tasks after. When a test has R
  spawn a background child, detach the child's stdio (`system2(..., stdout = FALSE,
  stderr = FALSE)`) so a not-yet-reaped grandchild holding the inherited pipe open
  can't hang the read — a leak then shows as a growing file, not a hang.
- 2026-06-06 (#7): Spawn `Rscript --vanilla -e <code>`. `--vanilla` skips user/site
  `.Rprofile`/`.Renviron`, so capture is reproducible across machines and no
  profile output bleeds into stdout. The repo root carries a `.Rprofile`/`.Renviron`;
  the runner's temp CWD avoids them anyway, but `--vanilla` also blocks the user's
  `~/.Rprofile`.
- 2026-06-06 (#7): Each `execute` runs in a per-call `tempfile::TempDir` set as the
  subprocess CWD, dropped on return (isolation + cleanup, AC3). `tempfile` is a
  normal `core` dependency now (not dev-only) because production code uses it.
- 2026-06-06 (#7): Real-`Rscript` tests live in `crates/core/tests/runner.rs` and
  guard with `rscript_absent()` (skip-with-notice when R is missing) per the
  Slice-1 convention. Two test gotchas worth reusing for Slice 8:
  - **macOS temp-dir symlink:** `getwd()` (and any interpreter cwd report)
    canonicalizes `/var` → `/private/var`, so `env::temp_dir()` must be
    `fs::canonicalize`d before a `starts_with` prefix assertion.
  - **`set_current_dir` is safe here** only because `cargo nextest run` executes
    each test in its own process; a plain `cargo test` (threads) would race.
- 2026-06-06 (#7): Integration tests (`tests/*.rs`) see the crate's normal
  `[dependencies]` — do **not** duplicate `tokio`/`tempfile` into
  `[dev-dependencies]` (roborev caught this on #7). Only test-exclusive crates
  (e.g. `serde_json`) belong in `[dev-dependencies]`.
- 2026-06-06 (#8): **Python runs through `uv`, not a bare `python`.** Per the
  project's always-`uv` rule, `PythonRunner` spawns
  `uv run --no-project --quiet python -I -c <code>`. `--no-project` ignores any
  surrounding pyproject so capture is reproducible; `--quiet` keeps uv's own
  env-setup chatter out of the captured streams (without it, first-run messages
  bleed into stdout/stderr); `-I` runs Python isolated (no user site, no
  `PYTHON*` env) — the analog of R's `--vanilla`. The dev machine (and macOS
  generally) has `python3` but no `python`, so the test skip-guard checks
  `uv run --no-project --quiet python --version`, not a bare `python` — it gates
  on exactly what the runner needs.
- 2026-06-06 (#8): **uv adds a process layer the group-kill already handles.**
  `uv run python` makes `uv` the group leader and `python` its child, so the
  Slice-7 process-group SIGKILL reaps both — no extra work. AC2 (`while True:
  pass` under a 500 ms `Timeout`) confirmed elapsed ≈ the timeout, i.e. the
  python child is killed, not just uv. It is the same child-tree shape the group
  kill was built for.
- 2026-06-06 (#8): **Shared core factored into `runner::subprocess`.** The
  spawn/drain/timeout/kill/temp-cwd dance is identical across languages; it lives
  in the private `subprocess::run(&Interpreter, code, Timeout)`. A `Runner` impl
  is now just an `Interpreter { program, code_args }` const (R: `Rscript` +
  `["--vanilla", "-e"]`; Python: `uv` +
  `["run", "--no-project", "--quiet", "python", "-I", "-c"]`) — adding a language
  is a descriptor, not a re-implementation (§4.2). The core appends `code` as the
  final arg.
- 2026-06-06 (#8): `RunnerError`'s context is now `String` (was `&'static str`),
  built via `impl Into<String>`, so the shared core can name the failing
  interpreter — `format!("spawn {program}")` yields "spawn Rscript" / "spawn uv"
  rather than a generic "spawn interpreter". Existing `&str` literal call sites
  coerce unchanged.
