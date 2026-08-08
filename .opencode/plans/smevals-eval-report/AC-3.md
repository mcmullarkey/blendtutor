---
ac: 3
depends_on: AC-1, AC-2
risk: low
status: complete
---

## AC spec: smevals runner script (run.sh) + deterministic polarity checker (check_polarity.sh)

### Executable Spec
- **predicate:** 17 clauses, all must hold:
  1. **Env→argv wiring:** stub blendtutor on PATH asserts it receives `eval <lesson> --case N --format json` built from SMEVALS_TASK_LESSON/SMEVALS_TASK_CASE; stub counter file proves the stub was invoked (no canned output).
  2. **No cargo run:** `grep -c 'cargo run' scripts/smevals/*.sh == 0` — runner resolves blendtutor from PATH.
  3. **Header contract:** on success, runner stdout line 1 == `verdict: correct|incorrect` (exact, lowercase); lines 2+ == feedback_message byte-exact, multi-line preserved (jq -r '.cases[0].feedback_message').
  4. **Source-of-truth = .actual, NOT .matched:** inconsistent-JSON stub (.cases[0].actual=="incorrect", .matched==true) MUST produce `verdict: incorrect`.
  5. **Four polarity combinations:** expected x actual in {correct, incorrect}² → checker exit 0 on the two matches, non-zero on the two mismatches.
  6. **Line-1-only parsing:** output file with `verdict: ` on line 2+ MUST NOT confuse the checker — parse head -1 / read -r line 1 only, no grep.
  7. **Fail-closed on malformed header:** `verdict: maybe`, `verdict:` (empty), `verdict:correct` (no space), 0-byte output → checker non-zero.
  8. **Case-sensitive token:** `verdict: Correct` → checker fails closed.
  9. **Bounded retry, transient only:** retry (max 3 attempts, sleep 2 backoff) on blendtutor exit-1, empty stdout, or malformed JSON; NO retry on well-formed mismatch verdict — stub counter shows exactly 1 call on valid JSON with mismatched verdict.
  10. **Backoff wall-time:** retry-then-fail scenario (stub always exits 1) takes >= 4s wall-clock.
  11. **Per-call timeout:** invocation wrapped in `timeout "${SMEVALS_TIMEOUT:-120}"`; exit 124 treated as transient → retry. Test overrides SMEVALS_TIMEOUT=2 with a sleep 5 stub (proves timeout path in ~10s, not 2-min waits).
  12. **Missing env → usage error:** unset SMEVALS_TASK_LESSON/SMEVALS_TASK_CASE → non-zero exit naming the missing variable on stderr.
  13. **FIREWORKS_API_KEY propagation:** no env -i / env scrubbing in runner — stub asserts the var is visible in its environment when set by the caller.
  14. **Empty feedback_message:** valid JSON with feedback_message: "" → header line emitted, zero following lines, runner exit 0.
  15. **command -v jq guard:** jq absent from PATH → runner exits non-zero naming jq.
  16. **Hygiene:** both scripts start with `set -euo pipefail`.
  17. **required:true path contract:** the checker path recorded in the AC-2-generated graders/default.yaml template == the actual checker script path (cross-file assert: grep -F of the literal path against the generator source and the real file).
- **probe:** `bash scripts/tests/test_smevals_runner.sh` (shell BDD, stub-on-PATH + counter-file pattern per scripts/tests/eval-course.sh:83-137; ok/ko/assert_eq helpers; mktemp -d + trap rm -rf; cd "$(git rev-parse --show-toplevel)"; set -euo pipefail). CI: step added to `.github/workflows/ci.yml` (`run: bash scripts/tests/test_smevals_runner.sh`).
- **negative:** Stub emitting inconsistent JSON (.actual=incorrect, .matched=true) with message body containing `verdict: correct` on line 3 — a sneaky-pass runner reading .matched or grep-scanning for verdict: produces the wrong polarity and the checker wrongly exits 0. Correct: verdict = incorrect from line-1 header derived from .actual; checker compares against SMEVALS_TASK_EXPECTED exactly.
- **verification:** code · shell BDD test with stub-on-PATH pattern
- **fixture status:** NEW — scripts/smevals/run.sh (runner), scripts/smevals/check_polarity.sh (checker), scripts/tests/test_smevals_runner.sh (test). Conventions anchor: scripts/tests/eval-course.sh:83-137 (existing). NOTE: script names pinned run.sh + check_polarity.sh — AC-2's generator template (configs/default.yaml runner path + graders/default.yaml checker path) emits these exact relative paths.
- **rubric anchor:** §2 (pure polarity-checker logic shell-testable; effectful LLM call isolated in runner), §5 (single-responsibility scripts)

### Design Intent
- **Types (§1):** verdict channel is a two-token contract — line-1 header `verdict: correct|incorrect`, body verbatim; malformed header unrepresentable to checker (fail-closed).
- **Pure / effectful (§2):** checker = pure comparator (output.txt x SMEVALS_TASK_EXPECTED → exit code); runner = thin effectful shell (blendtutor invocation + retry loop + timeout).
- **Boundary cuts (§3):** scripts/smevals/ owns the smevals-facing boundary; core eval logic stays in Rust; jq is the only JSON boundary tool.
- **Module responsibility (§4):** runner: env→argv, timeout, retry-on-transient, header emission. Checker: line-1 parse, exact case-sensitive comparison, required:true.
- **Function discipline (§5):** one concern per script; the runner's retry loop and the checker's line-1 parse are each directly stub-testable.

### Technical Context
- **Files touched:**
  - `scripts/smevals/run.sh` — NEW. Reads the smevals task contract env (verified against real smevals 0.2.0: SMEVALS_MODEL, SMEVALS_PROMPT, SMEVALS_TASK_LESSON, SMEVALS_TASK_CASE, SMEVALS_TASK_EXPECTED all set; SMEVALS_RUN_DIR set for checker), invokes `blendtutor eval <lesson> --case N --format json` under `timeout "${SMEVALS_TIMEOUT:-120}"`, parses `.cases[0].actual` (source of truth) + `.cases[0].feedback_message` via jq, emits `verdict: <actual>` + verbatim message. Retries only exit-1 / 124 / empty-stdout / malformed-JSON, 3 attempts, 2s backoff. Requires all five task env vars fail-closed (missing → named on stderr). jq guard first.
  - `scripts/smevals/check_polarity.sh` — NEW. Reads output file (argv 1, else `${SMEVALS_RUN_DIR:-}/output.txt`), takes line 1 ONLY, matches the full-line grammar `verdict: correct|incorrect` exactly (anything else fails closed), compares against SMEVALS_TASK_EXPECTED, emits `{"score": 1.0|0.0, "notes": ...}` and exits 0/1 (smevals grader contract, `required: true` in graders/default.yaml).
  - `scripts/tests/test_smevals_runner.sh` — NEW shell BDD test. Stub blendtutor (asserts argv contract + counter file) + GNU-compatible `timeout` shim on PATH (macOS lacks coreutils timeout; CI ubuntu has real one — shim shadows it deterministically in tests).
  - `.github/workflows/ci.yml` — add `smevals runner test` step to the `check` job (bash + jq + GNU timeout on ubuntu-latest; the test needs no quarto).
  - `.opencode/plans/smevals-eval-report/AC-3.md` — this plan.
  - `docs/evidence/196/` — E2E evidence.
- **Env contract provenance:** real-smevals probe (AC-2 eval dir + env-dumping stub runner) confirmed smevals 0.2.0 sets SMEVALS_MODEL / SMEVALS_PROMPT / SMEVALS_TASK_LESSON / SMEVALS_TASK_CASE / SMEVALS_TASK_EXPECTED / SMEVALS_RUN_DIR (absolute), and passes FIREWORKS_API_KEY through unchanged.
- **JSON shape:** `blendtutor eval <lesson> --case N --format json` → `{"cases":[{"expected","actual","matched","feedback_message"}],"accuracy"}` (crates/core/src/eval.rs:240-258; actual = lowercase token via ExpectedVerdict::token()). matched is derived via score_case, never independently set.
- **Test migration:** 0 files — new scripts + new test; no existing symbol changed.

### Dependencies
- **Depends on:** AC-1 (#194 — `eval --case N --format json` + feedback_message + .actual token), AC-2 (#195 — generated .smevals/ layout, graders/default.yaml + configs/default.yaml templates with exact script paths).
- **Blocks:** LLM-judge (AC-4, slots into same graders/default.yaml), eval-report (AC-5).
- **Conflict set:** scripts/smevals/, scripts/tests/test_smevals_runner.sh, AC-2's graders/configs templates (path contract — read-only for AC-3).
- **Risk level:** low — shell-only, no Rust changes; cross-AC path contract is pinned by predicate 17 and AC-2's own integration tests.

### Progress
- [x] spec written (Director) — 2026-08-08
- [x] red test (test_smevals_runner.sh — runner missing → 127) — 2026-08-08
- [x] green: run.sh + check_polarity.sh — 48/48 assertions — 2026-08-08
- [x] CI step added to .github/workflows/ci.yml (check job) — 2026-08-08
- [x] E2E evidence docs/evidence/196/ (test-suite.log + real-smevals probe) — 2026-08-08
- [x] full workspace cargo test green (no crates/ changes) — 2026-08-08

### Decision Log
- 2026-08-08 — required env set: all five task-contract vars (LESSON/CASE/MODEL/PROMPT/EXPECTED) required fail-closed, first missing named on stderr. Predicate 12 pins LESSON/CASE naming; the rest are the full "reads" contract from the issue summary, and the real-smevals probe proved all five are always set.
- 2026-08-08 — MODEL/PROMPT/EXPECTED validated but not consumed: blendtutor eval derives the model from the provider default (single-sourced to the same value as configs/default.yaml) and the submission from the eval suite file — no drift possible, so forwarding them into argv would be wrong.
- 2026-08-08 — timeout shim: local macOS lacks GNU `timeout`; the test ships a GNU-compatible shim (SIGTERM after N s → exit 124) on PATH FIRST so behavior is deterministic on both macOS and CI ubuntu (which shadows with the real coreutils timeout).
- 2026-08-08 — checker output-file resolution: argv 1 if given, else `${SMEVALS_RUN_DIR:-}/output.txt` — argv keeps tests hermetic; SMEVALS_RUN_DIR is the real-smevals contract (proven by the AC-2 smoke).

### Surprises & Discoveries
- 2026-08-08 — Local macOS has NO GNU `timeout` (CI ubuntu does), yet the binding decision wraps blendtutor in `timeout "${SMEVALS_TIMEOUT:-120}"`. The test ships a GNU-compatible `timeout` shim on PATH FIRST so the timeout path is deterministic on both platforms. First shim draft HUNG the suite: `kill $pid` on the stub orphans its inner `sleep 5` (bash doesn't exec), and the orphan keeps the runner's command-substitution stdout pipe open → each attempt cost ~5s instead of ~2s (19s observed vs 10s expected). Fix: `set -m` + process-group kill `kill -TERM -$pid` takes the whole tree down together; the killer subshell also redirects its fds so it can never hold the pipe.
- 2026-08-08 — The smevals 0.2.0 env contract is now empirically pinned, not assumed: an env-dumping stub runner under real `uvx smevals==0.2.0 run` showed SMEVALS_MODEL / SMEVALS_PROMPT / SMEVALS_TASK_LESSON / SMEVALS_TASK_CASE / SMEVALS_TASK_EXPECTED / SMEVALS_RUN_DIR (absolute) all set, and FIREWORKS_API_KEY passed through unscrubbed. This de-risked requiring all five in the runner (fail-closed, first-missing named).
- 2026-08-08 — `grep -c` exits 1 on zero matches. With `set -o pipefail` in the test, the predicate-2 count assertion silently killed the whole test script (no FAIL line — just a dying `$(...)`). Fixed with `{ grep ... || true; } | awk`; naive `| ... || echo 0` double-prints and failed the assert with a stray newline.
- 2026-08-08 — Predicate-2 self-trap: the runner's own docstring contained the literal string "cargo run" ("…resolving blendtutor from PATH (never cargo run)") — the spec's `grep -c 'cargo run' scripts/smevals/*.sh == 0` matched it. Reworded to "never an in-tree build invocation".
- 2026-08-08 — `smevals run -g <path>`: `-g` consumes the next arg as its grader value, so the eval path MUST precede `-g` (`smevals run <path> -g`). Harmless CLI quirk that cost a probe cycle.
- 2026-08-08 — The AC-2 smoke temp dir (`/var/folders/.../smevals-e2e-76934/repo`) survived and was reused for the real-tool E2E probe: real smevals 0.2.0 + AC-2-generated templates + MY real run.sh/check_polarity.sh, with only the blendtutor LLM call stubbed. Run A (match) → 3/3 pass exit 0; Run B (mismatch) → 3/3 fail exit 1, `grade.yaml` shows `ok: false / notes: polarity mismatch` — the `required: true` halt proven end-to-end. Worth noting the generator's `lesson:` value is the slug (`demo_lesson`), which the runner forwards verbatim as the eval arg.

### Idempotence & Recovery
- Safe retry: re-run `bash scripts/tests/test_smevals_runner.sh` — hermetic (mktemp + trap, stub-on-PATH). Idempotent.
- Rollback: delete scripts/smevals/run.sh + check_polarity.sh + test + CI step.
