---
topic: run
created: 2026-06-07
slices: [11]
---

The `run` command and `core::run` — the headline student loop that composes
execute → grade → LLM feedback into a typed report. See [[llm]], [[grade]],
[[runner]], [[output]] for the layers it joins.

- 2026-06-07 (#11): `core::run::run_lesson` is the effect-shell (§2.4, §3.3): it
  `select_runner` → `execute` (capture output) → `run_checks` → `build_prompt` →
  `request_feedback`, composing the runner/grade/llm layers through their public
  types only. It adds no domain logic, renders nothing, and maps no exit codes.
  Short and linear — the only branch is error propagation (`RunError::{Run,
  Feedback}`, anyhow-free per ADR-0001).
- 2026-06-07 (#11): **`RunReport` is typed but owns its canonical JSON.** It holds
  the `Verdict` sum type (message included, so "correct with no feedback" is
  unrepresentable), `Vec<CheckOutcome>`, and the captured `output: String`. The
  flat wire shape `{verdict, feedback, checks, output}` is a **private
  `RunDocument`** wired via `#[serde(into = "RunDocument", from = "RunDocument")]`
  — the two `From` impls are total inverses, so a report round-trips losslessly.
  `verdict` serializes to a lowercase string tag (`VerdictTag`) with the message
  split into `feedback`. core owns the report's JSON the way it owns `Lesson`'s;
  the cli only chooses format + stream. `CheckOutcome` gained Serialize/Deserialize
  for the `checks` array.
- 2026-06-07 (#11): **Exit codes 0/2/1 are disjoint and mapped at the cli edge.**
  correct = 0, incorrect = 2 (reserved "not yet correct", const `NOT_YET_CORRECT`
  in `cli::commands::run`), error = 1. The error code is *not* part of the verdict
  map — it comes from `main` returning `Err` (a failed load/read/feedback), so it
  can never collide with the verdict codes. `verdict_exit_code` reads the typed
  `report.verdict()`; it lives in the cli (not on `RunReport`) so `core` stays
  free of `std::process::ExitCode`, which a future GUI would not want (§3.1).
- 2026-06-07 (#11): **`--code` is optional.** Given → read the file (a missing/
  unreadable file is an IO error → exit 1, distinct from a "not yet correct"
  verdict — so a bad input never leaks the retry code). Omitted → read piped
  stdin, falling back to an empty submission on a TTY (`io::stdin().is_terminal()`)
  rather than blocking. This reconciles AC2 (missing `--code` → 1) with AC3
  (omitted flag still runs) without a clap-level required arg (clap's own missing-
  arg exit is 2, which would collide with the retry code).
- 2026-06-07 (#11): **The binary owns the async runtime; `core` stays a library.**
  `cli::commands::run` builds a `tokio::runtime::Builder::new_current_thread()
  .enable_all()` runtime and `block_on`s `run_lesson`. current_thread + enable_all
  drives both the runner subprocess (tokio::process, drained on spawned tasks) and
  the rig/reqwest HTTP call — the same flavor `#[tokio::test]` uses for the runner
  tests. cli gained `tokio` as a direct dep for this.
- 2026-06-07 (#11): **Known double-run.** `run_lesson` executes the submission on
  its own to capture `output`, and `run_checks` runs it *again* as its grade gate
  when the lesson has checks — so a side-effecting/nondeterministic submission
  could diverge between the captured output and what checks graded. Benign under
  v0's trusted-local deterministic model, and the common LLM-only lesson has no
  checks (one run). Deduping needs `grade::run_checks` to surface the gate's
  output — a `grade`-layer change deferred to a later slice.
- 2026-06-07 (#11): **CLI integration test pattern (reusable).** Point the spawned
  binary at an in-test `wiremock::MockServer` (a real localhost port) via the
  `BLENDTUTOR_PROVIDER_URL` base-URL override. `assert_cmd::Command` is
  synchronous, so run it inside `tokio::task::spawn_blocking`: that frees the
  current_thread runtime to keep serving the mock while the child's request is in
  flight (a direct blocking call deadlocks). The provider key is set on the
  **child** (`.env`), never the test process, so these tests need no `#[serial]`
  (unlike the env-mutating `core/tests/llm.rs`). They run the real `Rscript`, so
  they skip-with-notice via `rscript_absent()`. Helpers live in
  `crates/cli/tests/common/mod.rs` (`mount_feedback`, `blendtutor_output`,
  `dead_provider_url`).
- 2026-06-07 (#11): **Spec reconciliation — no `blendtutor-test-support`/`mockd`
  crate.** The issue planned a separate `mockd` binary; in-test wiremock (above)
  covers the same `BLENDTUTOR_PROVIDER_URL` seam with far less infrastructure and
  reuses the proven OpenAI tool-call envelope from `core/tests/llm.rs` (tool name
  `submit`, `arguments` as a JSON-encoded string). No new workspace crate; cli
  gained only `wiremock` + `tokio` dev-deps. The real-provider boundary keeps its
  `live`-profile coverage from Slice 10.
- 2026-06-07 (#11): **Diagnostics on stderr, machine doc on stdout.** `emit_run`
  writes both human and json to stdout (both are the command's data); a failed
  load/provider call is an error that reaches stderr via `main`. No `tracing`
  subscriber is installed, so `RUST_LOG` produces no stdout bytes — the json
  document stays a clean single parse even with logging requested.
