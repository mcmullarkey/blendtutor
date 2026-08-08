# Issue #194 — E2E Evidence

eval AC-1: extend `blendtutor eval --format json` to carry per-case
`feedback_message` (String, always present, verbatim) + `--case N` single-case
selection (1-based). Out-of-range N → exit 1 naming suite size. Exit-code
semantics unchanged (low accuracy still exits 0). eval-report.json shape
consumed by build unchanged (additive field only).

Verification: code · cargo nextest (unit + wiremock integration). nextest is
not installed locally; the spec probe filters were run with `cargo test` (same
test-name matching). CI (ci.yml) runs `cargo nextest run --locked` as the gate.

## What changed

1. **`feedback_message` on every `CaseResult`** — captured at score time from
   the runtime `Verdict` for BOTH variants (`Correct` and `Incorrect`), stored
   as a plain `String` (never `Option`), serialized with no `skip_serializing_if`
   so an empty message still emits the `"feedback_message":""` key.
2. **`--case N` single-case selection** — `run_eval` gains `case: Option<usize>`
   (1-based). A single pure validation point (`select_case_index`) rejects `0`
   and past-the-end values via `EvalRunError::CaseOutOfRange { requested,
   suite_size }`; the CLI propagates it as exit 1 with stderr naming the suite
   size. Non-numeric `--case` stays a clap parse error (exit 2).
3. **`EvalRunError` struct → enum** (`Run` | `CaseOutOfRange`).

## Evidence artifacts

| Artifact | Contents |
|----------|----------|
| `run.log` | Real binary against a live local mock provider: bare JSON run, `--case 1/2/3` JSON, `--case 2` human, `--case 0`/`--case 4` (exit 1 + stderr naming `3`), `--case abc` (exit 2). Mock request log proves exactly one provider request per scored case (7 requests for 7 scored cases; zero for rejected selections — not run-all-then-filter) |
| `test-suite.log` | All five spec probe filters: `eval_feedback_message` (2), `eval_no_skip_serializing_if` (1), `eval_case_selection` (2), `eval_case_out_of_range` (1), `eval_case_mismatch_exit0` (1) — all ok |

## Probe runs

| Command | Result |
|---------|--------|
| `cargo test -p blendtutor-core --lib eval_feedback_message` | ok (2 passed) |
| `cargo test -p blendtutor-core --lib eval_no_skip_serializing_if` | ok (1 passed) |
| `cargo test -p blendtutor-cli --test eval eval_case_selection` | ok (2 passed) |
| `cargo test -p blendtutor-cli --test eval eval_case_out_of_range` | ok (1 passed) |
| `cargo test -p blendtutor-cli --test eval eval_case_mismatch_exit0` | ok (1 passed) |
| `cargo test --workspace` | all green (183 core lib tests + full cli suite, 0 failed) |
| `cargo clippy --all-targets --locked -- -D warnings` | clean |
| `cargo fmt --all --check` | clean |

## Notes

- `crates/core/src/llm/feedback.rs` untouched — the message is read inline in
  `CaseResult::score` via an exhaustive match on `Verdict` (design intent §4).
- `scripts/eval-course.sh` reads `.cases[].matched` + `.accuracy` via jq —
  additive `feedback_message` field is safe; no eval-report.json shape change.
- `ExpectedVerdict` stays polarity-only (existing
  `verdict_polarity_drops_the_feedback_message` test unchanged and green).
- Human render unchanged (snapshot stays valid).
