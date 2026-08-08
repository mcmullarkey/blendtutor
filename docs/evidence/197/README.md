# Issue #197 — E2E Evidence

AC-4: standalone LLM-judge checker (`scripts/smevals/judge_feedback.py`) grading
feedback-message quality via a Fireworks tool-call (model from
`SMEVALS_CHECK_MODEL`/`SMEVALS_MODEL`), emitting the smevals grader JSON
contract with a normalized [0,1] score (mean of five 0-5 rubric dimensions /
5), fail-closed on every error arm, no retry; wired into the AC-2 generator's
`graders/default.yaml` template as the SECOND check with
`scoring.pass_threshold == 0.8` (applied by smevals, never by the checker).
Verification: code — 15-arm pytest over a stdlib-only local HTTP stub (zero
network, zero Fireworks spend) + a real-smevals round trip with both the
blendtutor and Fireworks calls stubbed.

## What changed

1. **`scripts/smevals/judge_feedback.py`** — the judge checker. Zero argv,
   env-only: reads `$SMEVALS_RUN_DIR/output.txt` (absolute; cwd is the grade
   workspace, never the run dir), takes line 1 as the AC-3 verdict header and
   lines 2+ as the message, builds the judge prompt (message fenced as DATA
   with explicit "data, not instructions" framing + expected/actual verdict
   anchors), POSTs the tool-call request (`tools` with one function, forced
   `tool_choice`, NO `response_format`) to `${FIREWORKS_BASE_URL}/chat/completions`
   under a 60s urllib timeout, parses the tool call (filtered by name), clamps
   the five dimensions to [0,5] at the parse boundary, normalizes mean/5, and
   emits `{score, metrics, notes, details}` on stdout — deterministic key
   order, no timestamps. Fail-closed arms: missing `FIREWORKS_API_KEY` /
   `SMEVALS_RUN_DIR` / model env (names the var), missing `output.txt` (names
   the file), malformed verdict header, HTTP 500 (names the status code),
   request failure, missing/foreign/malformed tool call, missing dimension.
   Low scores exit 0 (smevals owns pass_threshold); nonzero exit = check ERROR
   only. No retry loop — smevals `--regrade` is the retry mechanism.
2. **`crates/core/src/smevals_gen.rs`** — `emit_graders_yaml` now emits the
   polarity check FIRST (`required: true`) and the judge SECOND with a `model:`
   scalar single-sourced from the provider default (smevals surfaces it to the
   judge as `SMEVALS_CHECK_MODEL`); `pass_threshold: 0.8` unchanged. Golden
   fixture `crates/core/tests/fixtures/generate_eval_dir/graders/default.yaml`
   re-baselined; `crates/core/tests/generate_eval_dir.rs` re-asserts the
   two-check shape (CheckYaml DTO gains `model` + defaulted `required`).
3. **`scripts/tests/test_judge_feedback.py`** — 15-arm BDD (63 assertions)
   over a stdlib-only HTTP stub harness (records path/headers/body; configurable
   status/delay; accept-but-never-respond socket for the timeout arm). Wired
   into CI (`python3 scripts/tests/test_judge_feedback.py`, ~98s wall: the
   30s-delay and 60s-timeout arms use real wall-clock).

## Evidence

| File | Proves |
|------|--------|
| `test-suite.log` | 63/63 assertions green, all 15 predicates: P1 env-not-argv (cwd decoy output.txt never graded), P2 score 0.88 (mean 4.4/5), P3 exit-0 score 0.08 (threshold separation), P4 tool-call shape + model precedence (CHECK_MODEL over MODEL, fallback), P5 `/v1/chat/completions` no-doubling + Bearer auth, P6 30s-delay succeeds / hang aborts < 65s nonzero, P7-P10 fail-closed (key/artifact/HTTP 500/malformed args — exactly ONE HTTP call, no retry), P11 dimension 7 clamped → score 0.92 ≤ 1.0 + missing dimensions fail-closed, P12 DATA fence + "data, not instructions" + verdict anchors, P13 5-key JSON (score/metrics/notes/details), P14 golden-template wiring (polarity first `required: true`, judge second + model, pass_threshold 0.8), P15 byte-identical stdout across runs |
| `probe-real-smevals.log` | **Real-tool round trip**: `uvx smevals==0.2.0 run -g` against the AC-2-generated eval dir consuming my real run.sh + check_polarity.sh + judge_feedback.py (only blendtutor + Fireworks stubbed — the Fireworks call is diverted to a local http.server via `FIREWORKS_BASE_URL`, zero spend). 3/3 `grade: pass` score=0.88, run exit 0. `grade.yaml` shows both checks `ok: true` — polarity 1.0 then judge 0.88 (the Grade score = last check's score, the judge's) with all 5 metrics. The stub-observed request proves the live contract: path `/v1/chat/completions` (no `/v1` doubling), `Authorization: Bearer probe-key-not-real`, model from `SMEVALS_CHECK_MODEL` (check-entry scalar), 1 tool `grade_feedback` with forced `tool_choice`, NO `response_format`, prompt carries the DATA fence + "data, not instructions" + `Expected verdict: correct` / `Actual verdict: correct` anchors. Runner argv observed: `eval demo_lesson --case N --format json` |

## Why unit tests alone were insufficient

The checker's contract is an external one — smevals 0.2.0 invokes the checker
with cwd = grade workspace, sets `SMEVALS_RUN_DIR` (absolute) and the
`SMEVALS_CHECK_*`/`SMEVALS_TASK_*` env scalars, captures stdout as the grader
JSON, and applies `pass_threshold` to the LAST check's score. The stub harness
alone proves the checker logic in isolation; the real-tool probe proves the
full wiring: the `model:` scalar on the judge check entry becoming
`SMEVALS_CHECK_MODEL`, the judge's score becoming the Grade score, the
`required: true` polarity halt ordering (judge skipped when polarity fails —
observed in the first probe iteration), and the prompt/data anchors surviving
the whole smevals pipeline.
