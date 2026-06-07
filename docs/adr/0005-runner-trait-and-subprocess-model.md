# ADR-0005: Runner trait and the R subprocess execution model

- Status: Accepted
- Date: 2026-06-06

(The plan and issue #7 refer to this as "ADR-0003" — that number was assigned
to the lesson schema before this slice; the runner ADR is 0005.)

## Context

Issue #7 ports the R package's code execution (the `submit_code()` / student
loop in `R/educator_tools.R`, which shells out to run learner R code) into the
Rust `core`. Every later slice — Python execution (Slice 8), grading (Slice 9),
the CLI grade command — depends on *running learner code and observing what it
did*. The R side runs code and conflates its observations: stdout, messages, and
errors arrive interleaved, and a runaway loop has no bound.

We need the language-neutral seam those slices depend on, plus its first concrete
implementation (R via real `Rscript`). The decisions: where the boundary sits,
how an execution's observations are typed, and how a subprocess is bounded so a
child that spawns its own children cannot outlive the call.

## Options

1. **Concrete `RRunner` only, no trait.** Grading and the CLI call the R runner
   directly. Simplest now, but welds every consumer to R: adding the Python
   runner (Slice 8) or swapping to webR/Pyodide for the browser build forces an
   edit at each call site, and `core::grading` would import R subprocess
   mechanics it has no business knowing (§3.4, §4.1 violations).
2. **`Runner` trait + `RRunner` impl.** Consumers depend on
   `Runner::execute`; `core::runner::r` owns the R subprocess mechanics alone.
   A second language or a hosted backend is a new `impl Runner`, invisible to
   callers (§3.4). The cost is one indirection and a provisional `checks`
   parameter whose element type the grading slice will refine.

## Decision

Option 2.

- **Seam (§3.4).** `Runner::execute(&self, code, checks) -> Result<ExecutionResult, RunnerError>`.
  The method is declared returning `impl Future<Output = …> + Send` rather than
  with `async fn`, so the public trait stays clear of the `async_fn_in_trait`
  lint under CI's `-D warnings`. `checks: &[String]` is provisional — reserved
  for the grading slice, which refines the element type; v1 execution does not
  yet consume it.
- **Fallibility (§1.3).** `execute` is fallible. An `Err(RunnerError)` means the
  interpreter never ran (spawn failure, pipe/IO error) — categorically distinct
  from R running and writing to stderr. So `ExecutionResult` only ever represents
  a process that actually ran, and a launch failure can never masquerade as a
  learner diagnostic in `stderr`.
- **Observations (§1.2).** `ExecutionResult { stdout, stderr, exit: Option<i32>,
  final_value: Option<String>, timed_out: bool }` — five distinct fields, not one
  blob. `final_value` is reserved for the grading prompt and is `None` in this
  slice (its producer arrives with grading, Slice 9); the field is part of the
  agreed shape so the type does not churn when that slice lands. Building the
  result from captured bytes is a pure `ExecutionResult::from_capture` (§2.3,
  §5.3), separate from the effectful spawn.
- **Timeout (§1.4).** A `Timeout(Duration)` newtype, not a magic integer.
- **Subprocess model.** `tokio::process` spawns `Rscript -e <code>` with piped
  stdout/stderr, in a **new process group** via `command-group`. On timeout the
  whole group is SIGKILLed, so an R child that spawned grandchildren is reaped
  rather than leaked. `command-group` is chosen over hand-rolled
  `nix::setpgid` + `killpg` (less `unsafe`, maintained, cross-platform).
- **Isolation.** Each `execute` runs in a per-call `tempfile::TempDir` as the
  subprocess CWD, dropped on return — file-writing code is contained and cleaned
  up, and the caller's CWD is untouched.

**Sandbox posture (trusted-local, v1).** Student code runs in a local subprocess
bounded only by the timeout and the temp CWD. There is **no** security sandbox:
the model is a single trusted user grading their own learners locally. A hosted,
multi-tenant sandbox is a separate effort (issue #7 Out of Scope) and would be a
new `impl Runner`, not a change to this one.

## Consequences

- The Python runner, grading, and the CLI depend on `Runner`, never on `RRunner`
  or `Rscript`; a representation change to R execution stays inside
  `core::runner::r` (§3.2). `core::runner::r` never knows about lessons or LLMs
  (§4.1).
- Process-group kill adds the `command-group` dependency and means timeout
  behavior is only meaningfully testable against a real interpreter that spawns a
  child — so AC2 is a real-`Rscript` integration test, skipped-with-notice when R
  is absent (Slice-1 convention), not a unit test.
- `tokio` enters the workspace here. `core` gains an async, effectful surface for
  the first time; the pure lesson/course code is unaffected.
- `final_value` ships as a typed `None` ahead of its producer. This is a
  deliberate forward-shape commitment (the grading slice fills it), called out so
  an always-`None` field is not mistaken for a defect before Slice 9.
