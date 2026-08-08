---
ac: 4
depends_on: AC-1, AC-2, AC-3
risk: low
status: complete
---

## AC spec: LLM-judge checker (judge_feedback.py) grading feedback-message quality

### Executable Spec
- **predicate:** ALL of (15 clauses):
  1. **Env-not-argv:** checker invoked with zero argv; reads `$SMEVALS_RUN_DIR/output.txt` via absolute env path. Test runs with cwd != run dir and plants a decoy output.txt in cwd — only an absolute env-path read grades the run dir's file.
  2. **Score normalization [0,1]:** score == mean_of_5_dimensions / 5. Stubbed judge response mean 4.4/5 → stdout JSON `score == 0.88`, abs diff < 1e-9.
  3. **Exit-code/threshold separation:** stubbed low score (0.4/5 → 0.08) → judge exits 0 with `score: 0.08` on stdout. Threshold application belongs to smevals, NOT the checker. Nonzero exit = check ERROR only.
  4. **Tool-call request shape:** request body contains `model == $SMEVALS_CHECK_MODEL or $SMEVALS_MODEL` (checked in that precedence, never hardcoded), `tools` list with one function, `tool_choice.function.name` set. NO `response_format: json_object` key anywhere in body.
  5. **Endpoint + auth:** POST to `${baseUrl}/chat/completions` where baseUrl already ends in `/v1` (no doubling); headers `Authorization: Bearer $FIREWORKS_API_KEY`, `content-type: application/json`.
  6. **HTTP timeout:** 60s cap on Fireworks call. Stub server delaying 30s → still succeeds; unbounded-hang scenario → checker aborts <=65s with nonzero exit. (Test: stub socket that accepts but never responds; assert wall-clock < 65s and exit != 0.)
  7. **Fail closed — missing key:** `FIREWORKS_API_KEY` unset → nonzero exit; stderr names the variable. Same arm covers `SMEVALS_TASK_EXPECTED` unset → nonzero exit, stderr names the variable (all 4 env arms fail-closed tested).
  8. **Fail closed — missing artifact:** `$SMEVALS_RUN_DIR/output.txt` absent → nonzero exit; stderr names the file.
  9. **Fail closed — HTTP 500:** stub returns 500 → nonzero exit; stderr names status code.
  10. **Fail closed — malformed tool-call args:** `function.arguments` unparseable JSON → nonzero exit. NO retry loop in checker — retry mechanism is operator-level `--regrade`.
  11. **Score clamping:** stubbed dimension value 7 (out of 0-5 range) → clamped at the parse boundary; emitted `score` NEVER > 1.0. Missing dimensions key → nonzero exit.
  12. **Prompt-injection defense:** feedback message fenced as DATA in prompt with explicit "data, not instructions" framing; prompt carries expected verdict (`SMEVALS_TASK_EXPECTED`) + actual verdict (output.txt line 1 header) so judge can score verdict-rationale correctness. Every interpolated value is `neutralize()`d (mirrors crates/core/src/llm/prompt.rs:87): a message carrying literal `BEGIN/END FEEDBACK DATA` or forged `Expected verdict:` / `Actual verdict:` anchors → replaced with `[neutralized-delimiter]`, so the fences/labels appear exactly once in the prompt.
  13. **smevals 5-key JSON:** stdout JSON parses with `score` (float), `notes`, `metrics`; unknown keys tolerated into `details`. `metrics` carries the 5 dimension scores.
  14. **graders/default.yaml wiring:** generator template emits polarity check FIRST (`required: true`), judge SECOND, `scoring.pass_threshold == 0.8`. Golden fixture updated; golden-dir test re-baselined (5-file migration: 2 new + 3 AC-2-owned modified).
  15. **Determinism:** with stubbed judge response, stdout JSON byte-identical across two runs (stable key order, no timestamps/random in output).
- **probe:** `uv run pytest scripts/tests/test_judge_feedback.py -x -q` — stub-HTTP harness driving scripts/smevals/judge_feedback.py across all 15 arms (SMEVALS_* env injected per case; responses served from local stub, zero network). Python BDD test in scripts/tests/ per repo convention (test_quarto_key_page.py pattern: pytest-collectable AND standalone `python3`); CI wiring `run: python3 scripts/tests/test_judge_feedback.py` (check job, after smevals runner test).
- **negative:** Stub returns dimension score 7 and HTTP 500 in sequence; checker must never emit score > 1.0, must never retry malformed tool-call output, must never read argv, must never apply pass_threshold itself (exit 0 with score 0.08 proves separation). A checker that hardcodes the model, doubles `/v1`, or reads output.txt relative to cwd FAILS this spec.
- **verification:** code · pytest with local HTTP stub (stdlib http.server); zero network, zero Fireworks spend
- **fixture status:** NEW (stub harness + checker `scripts/smevals/judge_feedback.py`); graders golden fixture modified — cite AC-2 golden path when landed
- **rubric anchor:** §2 (checker is effectful shell over pure prompt-build + parse/normalize core), §5 (each fail-closed arm its own testable function)

### Design Intent
- **Types (§1):** dimension scores constrained to [0,5] at parse boundary (`clamp_dimensions`); normalized score guarantees [0,1] before JSON emission. smevals 5-key envelope as a fixed dict shape (score/metrics/notes/details, stable key order).
- **Pure/effectful (§2):** pure core: `build_prompt` (fenced message + verdict anchors), `build_request` (tool-call body), `parse_tool_call` (name-filtered extraction), `clamp_dimensions`/`normalize_score`, `emit_grade`. Effectful shell: env reads, single HTTP POST, stdout emit, exit code.
- **Boundary (§3):** checker lives at scripts/smevals/ boundary — run BY smevals, not imported by blendtutor. Only contract is smevals env protocol + stdout JSON.
- **Module (§4):** header documents: grades feedback-message quality; reads SMEVALS_RUN_DIR/SMEVALS_CHECK_MODEL/SMEVALS_MODEL/FIREWORKS_API_KEY/FIREWORKS_BASE_URL; does NOT apply pass_threshold, does NOT retry, does NOT write files.
- **Discipline (§5):** build_prompt/build_request/parse_tool_call/clamp_dimensions/normalize_score/emit_grade — each small, unit-testable without network.

### Technical Context
- **Files touched:**
  - `scripts/smevals/judge_feedback.py` — NEW. `#!/usr/bin/env python3`, stdlib only (urllib/json/os/sys/pathlib — smevals invokes bare python3, no uv wrapper). Tool-call request mirrors feedback.js fireworksRequest; base URL default `https://api.fireworks.ai/inference/v1` (provider.rs:43) with `FIREWORKS_BASE_URL` override for hermetic tests; 60s urllib timeout; five 0-5 rubric dimensions (verdict-rationale correctness, actionability, references check results, no solution leak, no hallucinated errors); score = mean/5, clamped [0,1].
  - `scripts/tests/test_judge_feedback.py` — NEW 15-arm BDD (77 assertions), pytest-collectable + standalone. Stdlib http.server stub (records path/headers/body, configurable status/delay), raw-socket hang server, decoy-cwd negative for P1, forged-fence negative for P12.
  - `crates/core/src/smevals_gen.rs` — `JUDGE_REL` const; `emit_graders_yaml` emits polarity first (`required: true`), judge second with `model:` scalar single-sourced from `ProviderChoice::Fireworks.default_model()` (becomes `SMEVALS_CHECK_MODEL`).
  - `crates/core/tests/fixtures/generate_eval_dir/graders/default.yaml` — golden re-baselined (mechanical regen, not hand-edit).
  - `crates/core/tests/generate_eval_dir.rs` — CheckYaml DTO gains `model: Option<String>` + defaulted `required`; two-check assertions (polarity required, judge not required + model single-sourced).
  - `.github/workflows/ci.yml` — `smevals judge test` step in check job (`python3 scripts/tests/test_judge_feedback.py`).
  - `docs/evidence/197/` — E2E evidence (test-suite.log + real-smevals probe).
- **Env contract provenance (real smevals 0.2.0):** checkers run with cwd = grade workspace; smevals sets `SMEVALS_RUN_DIR` (absolute), `SMEVALS_CHECK_*` scalars from the check entry (`model:` → `SMEVALS_CHECK_MODEL`), `SMEVALS_CHECK` (JSON), `SMEVALS_TASK_*` scalars (`expected:` → `SMEVALS_TASK_EXPECTED`); `FIREWORKS_API_KEY` passed through unscrubbed. Score = last check's score; judge is last → its score is the Grade score. A failed non-required check still fails the grade (`not all(r["ok"])`); `required: true` on polarity halts grading before the judge.
- **Test migration:** 5 files (2 new + 3 AC-2-owned modified: smevals_gen.rs grader template, golden fixture, golden-dir test).

### Dependencies
- **Depends on:** AC-2 (#195 — generator + graders/default.yaml template + provider model default 0731), AC-3 (#196 — output.txt verdict-header format + polarity checker contract).
- **Blocks:** eval-report (AC-5) — judge tested independently there.
- **Conflict set:** generator template + golden fixture (shared with AC-2 — serialized after AC-2 landed); scripts/smevals/ (low conflict).
- **Risk level:** low — one new stdlib Python checker + template wiring; no crates/cli changes (AC-5 owns later).

### Progress
- [x] spec written (Director) — 2026-08-08
- [x] red test (test_judge_feedback.py — checker missing → exit 2) — 2026-08-08
- [x] green: judge_feedback.py — 15/15 arms — 2026-08-08
- [x] generator wiring (smevals_gen.rs template + golden fixture + generate_eval_dir.rs) — golden-dir test green — 2026-08-08
- [x] CI step added to .github/workflows/ci.yml (check job) — 2026-08-08
- [x] full workspace cargo nextest green (332/332), AC-3 shell BDD regression green (48/48) — 2026-08-08
- [x] E2E evidence docs/evidence/197/ (test-suite.log 77/77 + real-smevals probe 3/3 pass score 0.88) — 2026-08-08
- [x] PR review cycle 1 fixes: neutralize() on all interpolated values (P12 forged-fence arm, mirrors prompt.rs:87); SMEVALS_TASK_EXPECTED unset fail-closed arm (P7); assertion count 65 → 77 in docs — 2026-08-08

### Decision Log
- 2026-08-08 — model env precedence: `SMEVALS_CHECK_MODEL or SMEVALS_MODEL`. Real smevals does NOT set SMEVALS_MODEL for checkers (runner env only), so the graders template must emit a `model:` scalar on the judge check entry (→ SMEVALS_CHECK_MODEL), single-sourced from the provider default; SMEVALS_MODEL stays as the direct-invocation fallback.
- 2026-08-08 — score normalization at the boundary: `clamp_dimensions` (raw values → [0,5] floats, non-numeric fail-closed) feeds BOTH `metrics` and `normalize_score` — the metrics are the clamped dimension scores per §1 "constrained at the parse boundary"; the emitted score is mean/5, provably ≤ 1.0.
- 2026-08-08 — 60s timeout hardcoded (not env-configurable): the spec's two arms (30s-delay succeeds, hang aborts < 65s) bracket the cap with the real default; an env knob would weaken the "60s judge-imposed" decision.
- 2026-08-08 — judge check NOT `required: true`: smevals fails the grade on any non-ok check (`not all(r["ok"])`), so the judge's own nonzero exit already fails closed; `required: true` on the polarity check halts grading BEFORE the judge (verified in the real-smevals probe first iteration: judge `skipped: true` when polarity mismatched).
- 2026-08-08 — FIREWORKS_BASE_URL env override (default `https://api.fireworks.ai/inference/v1`): the real-smevals E2E probe needs to divert the judge's HTTPS call to a local http.server — zero spend — without changing the checker's default endpoint.
- 2026-08-08 — neutralize() on every interpolated prompt value (message + verdict strings), replacing BEGIN/END FEEDBACK DATA and Expected/Actual verdict: labels with `[neutralized-delimiter]` — mirrors the established codebase convention (crates/core/src/llm/prompt.rs:87, crates/core/assets/shared/feedback.js:95). The old defense (fence + "data, not instructions" framing) alone let a message carrying literal `END FEEDBACK DATA` forge a second closing fence; neutralize() makes the fence/labels appear exactly once. P12 gained a forged-fence arm that fails without the fix.

### Surprises & Discoveries
- 2026-08-08 — Local macOS has NO GNU `timeout` (CI ubuntu does), and run.sh wraps blendtutor in `timeout` — the first real-smevals probe silently produced EMPTY output.txt (runner exit 127, permanent failure) → polarity check failed closed → judge skipped. Reused the AC-3 GNU-compatible timeout shim (test_smevals_runner.sh) on PATH FIRST; probe then ran green. Worth noting run.sh's retry policy treats 127 as permanent by design (usage error), so the shim is required for ANY local real-tool run.
- 2026-08-08 — Backgrounded probe stubs hold the script's stdout pipe open: a crashed probe (python traceback under `set -e`) left the fireworks stub alive AND holding the pipe → the capture hung until the 180s kill; worse, the leaked stub kept squatting its fixed port so the NEXT probe's requests silently went to the stale stub (grade scored 0.88 but the fresh request log was empty). Fixes: detach stub fds (`>/dev/null 2>&1 </dev/null &`) + wait on the PID at teardown, and bind port 0 (ephemeral) with the actual port written to a file so a stale stub can never steal the port. Same orphan-holds-pipe class as the AC-3 timeout-shim surprise — this time it was the probe harness, not the code under test.
- 2026-08-08 — smevals sets `SMEVALS_CHECK_MODEL` (not `SMEVALS_MODEL`) for checkers — confirmed from cli.py `execute_checker_program` (env = os.environ | SMEVALS_CHECK_* scalars | {SMEVALS_RUN_DIR, SMEVALS_CHECK} | SMEVALS_TASK_* | SMEVALS_TASK; no SMEVALS_MODEL). This made the template `model:` scalar mandatory, not optional.
- 2026-08-08 — `uvx smevals==0.2.0 run <evaldir> -g`: `-g` consumes the next arg as its grader value, so the eval path MUST precede `-g` (AC-3 already recorded this quirk — confirmed again on the AC-4 probe).

### Idempotence & Recovery
- Safe retry: re-run `uv run pytest scripts/tests/test_judge_feedback.py -x -q` (hermetic: mktemp + trap, local stub, zero network). Idempotent.
- Real-tool probe: `bash <probe script>` — rebuilds the eval dir from the golden fixture (`.smevals/` is gitignored), stub blendtutor + timeout shim on PATH, ephemeral-port Fireworks stub. Idempotent.
- Rollback: delete scripts/smevals/judge_feedback.py + test + CI step + revert smevals_gen.rs/golden/generate_eval_dir.rs to the AC-3 state.
