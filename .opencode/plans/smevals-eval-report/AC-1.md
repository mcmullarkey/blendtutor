---
ac: 1
depends_on: none
risk: medium
status: complete
---

## Final Spec (resolver-merged)

## AC-1
Extend `blendtutor eval --format json` to carry per-case `feedback_message` + `--case N` single-case selection (1-based) on the existing `Eval` subcommand. Out-of-range N → exit 1 naming suite size. No change to exit-code semantics (still exit 0 on low accuracy). No change to eval-report.json shape consumed by build.

### Executable Spec
- **predicate:**
  1. `eval <lesson> --format json` on 3-case demo suite → every `cases[i].feedback_message` present, String-typed (never null/omitted), verbatim-equal to `Verdict::message()` incl. Correct verdicts.
  2. No `skip_serializing_if` — unit test on empty-message Verdict asserts `"feedback_message":""` key present.
  3. `--case 2` → `cases.len()==1` AND `cases[0].feedback_message=="beta is off"` AND `matched==true` AND wiremock `received_requests==1` (catches run-all-then-filter).
  4. `--case 1` → alpha case (1-based indexing verified).
  5. `--case 0` AND `--case 4` → exit 1 AND stderr contains suite size `"3"`.
  6. `--case 3` (mismatch) → exit 0 (exit-code semantics unchanged on low accuracy).
  7. Backward compat: bare run yields accuracy 2/3, matched `[true,true,false]`; `ExpectedVerdict` stays polarity-only (existing test holds).
- **probe:**
  ```
  uv run cargo nextest run -p blendtutor-core eval_feedback_message
  uv run cargo nextest run -p blendtutor-core eval_no_skip_serializing_if
  uv run cargo nextest run -p blendtutor-cli eval_case_selection -- --nocapture   # wiremock mount_three_case_provider, received_requests==1
  uv run cargo nextest run -p blendtutor-cli eval_case_out_of_range                # N=0, N=4 → exit 1, stderr names "3"
  uv run cargo nextest run -p blendtutor-cli eval_case_mismatch_exit0
  ```
- **negative:** (a) message empty/omitted/constant-across-cases; (b) `--case` silently ignored; (c) clamped to first case; (d) out-of-range clamped; (e) exit 1 without naming suite size; (f) mismatch case exiting nonzero; (g) run-all-then-filter (wiremock request count catches, 3× cost); (h) message captured only for Incorrect verdicts; (i) `skip_serializing_if` dropping empty string; (j) message stuffed into `ExpectedVerdict`; (k) error index reported post-filter (wrong case number under `--case N`).
- **verification:** code · cargo nextest (unit + wiremock integration)
- **fixture status:** existing `eval_command/demo_lesson.yaml` (3 cases) — no new fixtures
- **rubric anchor:** §1 (illegal states unrepresentable: `feedback_message: String`, never Option/null), §2 (pure `CaseResult::score` + thin CLI edge), §5 (`run_eval` case filter testable without patches)

### Design Intent
- **Types / interfaces (§1):** `CaseResult` gains `feedback_message: String` (both Verdict variants carry `message` — feedback.rs:30-38). Serialize-only derive (no Deserialize → no fixture break, additive field safe).
- **Pure / effectful (§2):** `CaseResult::score` pure — body adds `match` on actual verdict, captures `verdict.message()`. CLI edge = thin: clap parse + exit-code mapping only.
- **Boundary cuts (§3):** Core owns selection + validation; CLI owns arg parsing + process exit. New `EvalRunError::CaseOutOfRange { requested, suite_size }` variant keeps contract in one place.
- **Module responsibility (§4):** eval.rs = suite execution incl. case selection; main.rs = arg wiring + exit codes; feedback.rs untouched (message source).
- **Function discipline (§5):** `run_eval` gains `case: Option<usize>` param — performs range check + filter, returns `EvalRunError::CaseOutOfRange` on out-of-bounds. One function, one validation point, unit-testable without CLI harness.

### Decisions on Flagged Divergences
- **feedback_message type:** String, confirmed. B's correction (both variants carry message) accepted.
- **Error reporting under `--case N`:** thread ORIGINAL 1-based suite index through filter — preserve `(original_index, case)` pair during selection so `EvalRunError` reports user's requested number, not post-filter index 0. Predicate negative (k) covers sneaky-pass.
- **Range-check location:** inside `run_eval` via new `EvalRunError::CaseOutOfRange { requested, suite_size }`. Single validation point, unit-testable in core without CLI harness (§5), CLI shell stays thin (§2). CLI maps variant → exit 1 + stderr naming `suite_size`.
- **Parse-error class:** confirmed — clap exit 2 for non-numeric/negative N (clap standard, no custom code), exit 1 ONLY for numeric-but-out-of-bounds.

### Technical Context
- **Files likely touched:** `crates/cli/src/main.rs` (Eval args: `--case: Option<usize>`; exit-code mapping), `crates/core/src/eval.rs` (`CaseResult` + `feedback_message`, `run_eval` filter/range check, `EvalRunError::CaseOutOfRange`), `crates/core/src/llm/feedback.rs` (read-only reference for `Verdict::message()`), `eval_command/demo_lesson.yaml` (fixture, read-only).
- **Architecture notes:** Human render UNCHANGED (snapshot stays valid). `eval-course.sh:158` writes `{"accuracy"}` via jq — additive JSON field doubly safe. Site `eval_summary_from_report_json` has no `deny_unknown_fields`. Empty-suite edge: untested, out of scope (no empty suites in tree). Verbatim multi-line messages safe via serde_json escaping.

### Dependencies
- **Depends on:** none (batch [1,2,6])
- **Blocks:** AC-3 (runner shells `--case N --format json`), AC-4 (verdict parsing of stdout header)
- **Conflict set:** `crates/cli/src/main.rs` (shared with AC-5 — serialized across batches), `crates/core/src/eval.rs`
- **Risk level:** medium (load-bearing JSON contract for entire harness; mitigated by Serialize-only derive + additive field)

Resolver note: disagreement=minor — diverged on error-index-under-filter + range-check location; resolved by design rules (§2/§5), no user input needed.

### Progress
- [x] Speculators A+B returned (2026-08-07)
- [x] Resolver merged (2026-08-07)
- [x] Implementation (2026-08-07, branch 194-eval-feedback-message)
  - test(red) 200e08e: probe tests (core unit + CLI wiremock integration) fail pre-impl
  - feat 3bc8d3f: feedback_message on CaseResult + --case N + EvalRunError::CaseOutOfRange; all green
  - Evidence: docs/evidence/194/ (run.log E2E + test-suite.log probes)
- [ ] PR review (Director dispatch)

### Decision Log
- 2026-08-07 — feedback_message: String (not Option): both Verdict variants carry message; never null/skipped
- 2026-08-07 — Original suite index preserved through filter for error reporting
- 2026-08-07 — Range check inside run_eval via EvalRunError::CaseOutOfRange
- 2026-08-07 — clap exit 2 for parse errors; exit 1 only for out-of-range
- 2026-08-07 — EvalRunError struct → enum (Run | CaseOutOfRange); existing Run-variant test updated in same commit

### Surprises & Discoveries
- 2026-08-07 — `Verdict` has NO public `message()` accessor (spec referenced one); message read inline in `CaseResult::score` via exhaustive match on both variants — feedback.rs stays untouched per §4. Test names `eval_feedback_message`/`eval_no_skip_serializing_if` were pinned verbatim.
- 2026-08-07 — `RunError` lacks `PartialEq`, so `EvalRunError` cannot derive it; index-mapping tests use `matches!` instead of `assert_eq!`.
- 2026-08-07 — Probe filter `eval_case_mismatch_exit0` (no trailing 's') must match a test name exactly; initial `eval_case_mismatch_exits_0` matched 0 tests → nextest would fail. Renamed to match probe.
- 2026-08-07 — nextest not installed locally; spec probes run via `cargo test` (same name filtering); CI gate runs `cargo nextest run --locked`.
- 2026-08-07 — clippy `collapsible_if` required let-chain (`if let Some(i) = sel && i != idx`) in the run_eval filter.

### Idempotence & Recovery
- Safe retry: re-run nextest probes; additive field + unchanged constructor → no migration
- Rollback: revert eval.rs + main.rs changes; JSON field removal is additive-safe for jq consumers