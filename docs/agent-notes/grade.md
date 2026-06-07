---
topic: grade
created: 2026-06-06
slices: [9]
---

How student submissions are graded against a lesson's checks: the `core::grade`
join between `lesson` and `runner`, and how a per-check verdict is produced.

- 2026-06-06 (#9): `core::grade` is the lesson↔runner join (§3.1). It exposes two
  functions: `select_runner(&Language) -> RunnerKind` (pure — maps a language to a
  runner, §2.1) and `async run_checks(impl Runner, submission, &[String]) ->
  Vec<CheckOutcome>` (effectful, §2.2). It depends on `lesson` and `runner` only
  through their public types and does NOT build prompts or call LLMs (§4.1) — that
  is Slice 10's provider layer.
- 2026-06-06 (#9): `CheckOutcome` is a sum type `{ Pass, Fail{detail}, NotRun{reason} }`,
  one per check, never folded into an aggregate bool (§1.2). `NotRun` is the
  load-bearing variant: a submission that could not run at all is kept distinct
  from a check the submission ran and violated (§3.3), so "errored before checks"
  never masquerades as a `Fail`.
- 2026-06-06 (#9): **The submission is run on its own first (the gate).** A check
  is graded by running `submission + "\n" + check` and reading the exit code
  (0 = `Pass`, non-zero = `Fail{stderr}`). But to tell `NotRun` from `Fail` you
  MUST execute the submission *alone* before any check: a non-zero exit or a
  launch failure there makes every check `NotRun`. Concatenating submission+check
  and reading the combined exit cannot distinguish them — and R in particular
  splices an unterminated submission onto the check line (`x <-` + a check parses
  as `x <- stopifnot(...)` and fails as "could not find function", a Fail-shaped
  error). Verified by AC3 against real Rscript.
- 2026-06-06 (#9): `run_checks` short-circuits to `vec![]` when `checks` is empty,
  before the gate's subprocess (§1.3.1). This is the common path: an LLM-only
  lesson (the R package's model) has no code-checks, so it must not pay for a
  submission spawn.
- 2026-06-06 (#9): **`select_runner` returns a `RunnerKind` enum, not a
  `Box<dyn Runner>`.** `Runner::execute` returns `impl Future` (an RPITIT, see
  `runner.md`), so the trait is not object-safe and a runtime-chosen runner cannot
  be boxed. `RunnerKind { R(RRunner), Python(PythonRunner) }` impls `Runner` by
  delegating, so consumers still depend on the trait (`run_checks` takes
  `impl Runner`, never names a concrete runner — §3.4). The `match` over
  `Language` is exhaustive with no `_` arm, so a new language forces a new dispatch
  arm at compile time.
- 2026-06-06 (#9): Checks live on the lesson as `Lesson.checks: Vec<String>`
  (`#[serde(default)]`, see `lesson-model.md`) — code-strings in the lesson's
  language. Empty by default so every existing (checkless, LLM-graded) lesson stays
  valid; the field is a `Vec`, not an `Option<Vec>`, so "no checks" is just the
  empty list (§1.1).
- 2026-06-06 (#9): Unit tests drive the classification with a `#[cfg(test)]`
  `ScriptedRunner` that impls the real `Runner` trait and replays a fixed script of
  results in call order — so `run_checks`'s exit-code→outcome mapping, the
  spawn-failure (`Err`) arm a real interpreter never reaches, and element-wise
  ordering are all pinned with no `Rscript`/`uv` on `PATH`. Its call cursor is an
  `AtomicUsize` (NOT a `Cell`) so `&self` stays `Sync` and the `execute` future
  stays `Send` under the RPITIT `+ Send` bound. Real-interpreter behaviour is the
  separate skip-guarded integration test in `tests/grade.rs`.
- 2026-06-06 (#9): **Known gap — timeouts produce an empty message.** `run_checks`
  classifies a timed-out submission as `NotRun` and a timed-out check as `Fail`
  (both defensible), but neither it nor `submission_run_failure` inspects
  `ExecutionResult.timed_out`, so the `reason`/`detail` is the empty post-SIGKILL
  stderr. Giving timeouts an informative message (or a dedicated outcome), plus a
  `ScriptedRunner::TimedOut` reply to pin it, is deferred — surfaced by the
  adversarial review on PR #34; no AC in #9 covers it. Grep `timed_out` here when
  that slice starts.
