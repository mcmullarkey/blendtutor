# Issue #196 — E2E Evidence

AC-3: shared smevals runner script (`scripts/smevals/run.sh`) + deterministic
polarity checker (`scripts/smevals/check_polarity.sh`) wired into the
AC-2-generated `graders/default.yaml` template (`required: true`). Verification:
code — stub-based shell BDD + real-tool E2E.

## What changed

1. **`scripts/smevals/run.sh`** — the smevals runner. Reads the smevals task
   contract env (all five vars verified set by real smevals 0.2.0:
   `SMEVALS_MODEL`, `SMEVALS_PROMPT`, `SMEVALS_TASK_LESSON`,
   `SMEVALS_TASK_CASE`, `SMEVALS_TASK_EXPECTED`), invokes
   `blendtutor eval <lesson> --case N --format json` (AC-1) under a per-call
   `timeout "${SMEVALS_TIMEOUT:-120}"`, and emits the verdict header + verbatim
   feedback to stdout (smevals saves it as `output.txt`). Verdict comes from the
   JSON `.cases[0].actual` token — the source of truth, never `.matched`.
   Bounded transient-only retry: 3 attempts, 2s backoff, retrying exit-1,
   timeout-124, empty stdout, and malformed JSON; a well-formed run never
   retries. No env scrubbing (FIREWORKS_API_KEY reaches blendtutor). jq guard.
2. **`scripts/smevals/check_polarity.sh`** — the polarity checker. Reads the
   output file (argv 1, else `${SMEVALS_RUN_DIR}/output.txt`), takes line 1
   ONLY (never a grep scan), matches the full-line grammar
   `verdict: correct|incorrect` exactly (anything else fails closed), compares
   against `SMEVALS_TASK_EXPECTED` exactly, emits the smevals grader JSON
   contract and exits 0/1. Wired `required: true` in `graders/default.yaml` by
   AC-2's template — a mismatch halts grading.
3. **`scripts/tests/test_smevals_runner.sh`** — stub-based shell BDD test
   (48 assertions, all 17 predicates + the sneaky-pass negative). Stub
   blendtutor asserts the argv contract and records calls to a counter file; a
   GNU-compatible `timeout` shim on PATH keeps the timeout path deterministic
   on macOS (no coreutils) and CI ubuntu alike.
4. **`.github/workflows/ci.yml`** — `smevals runner test` step added to the
   `check` job (`run: bash scripts/tests/test_smevals_runner.sh`).

## Evidence

| File | Proves |
|------|--------|
| `test-suite.log` | 48/48 shell BDD assertions green: env→argv wiring (stub counter proves invocation), header contract (line-1 `verdict: correct\|incorrect` + byte-exact multi-line message), `.actual` source-of-truth (inconsistent JSON `.actual=incorrect .matched=true` → `verdict: incorrect`), four polarity combos, line-1-only parsing, fail-closed malformed headers, case-sensitivity, transient-only retry (3 attempts / no-retry-on-valid-mismatch / ≥4s backoff / timeout-124 retry), missing-env naming, FIREWORKS_API_KEY propagation, empty-message header-only, jq guard, `set -euo pipefail` hygiene, generator-template↔script path contract |
| `probe-real-smevals.log` | **Real-tool round trip**: `uvx smevals==0.2.0 run -g` against the AC-2-generated eval dir consuming **my real run.sh + check_polarity.sh** (only the LLM call stubbed). Run A (polarities match): 3/3 `grade: pass`, run exit 0, observed argv `eval demo_lesson --case N --format json`. Run B (deliberate mismatch): 3/3 `grade: fail`, run exit 1 — the `required: true` checker halts grading; `grade.yaml` shows `ok: false / score: 0.0 / notes: polarity mismatch` with the pinned checker path `../../../../scripts/smevals/check_polarity.sh` |

## Why unit tests alone were insufficient

The runner/checker contract is an external one — smevals 0.2.0 sets the
`SMEVALS_*` env and captures stdout as `output.txt`, and the AC-2 generator
emits the `runner:`/`checker:` paths. The real-tool probe proves the env wiring,
the stdout→output.txt capture, the line-1 header parse, and the
`required: true` grader halt all work against the actual tool, not just the
stub harness. The stub harness alone proves the runner/checker logic;
the probe proves the tool boundary.

## Notes

- **GNU `timeout` dependency**: the binding decision wraps the blendtutor call
  in `timeout "${SMEVALS_TIMEOUT:-120}"`. macOS without coreutils lacks
  `timeout`; CI ubuntu ships it. The test ships a GNU-compatible `timeout` shim
  on PATH so the timeout path is exercised deterministically everywhere; macOS
  users running smevals need GNU coreutils (or a `timeout` shim on PATH).
- The runner requires all five task-contract env vars fail-closed (first
  missing named on stderr); `SMEVALS_MODEL`/`SMEVALS_PROMPT`/
  `SMEVALS_TASK_EXPECTED` are validated but not consumed — `blendtutor eval`
  derives the model from the provider default (single-sourced to the same value
  `configs/default.yaml` names) and the submission from the eval suite file.

## Status

All probes pass. `bash scripts/tests/test_smevals_runner.sh` → 48 passed,
0 failed. Rust suite untouched (no `crates/` changes) — `cargo test` re-run to
confirm zero regressions.
