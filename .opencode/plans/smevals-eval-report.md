# Plan: smevals-eval-report

## Feature Goal

Make blendtutor's "run evals before you ship a lesson" selling point turn-key: a lesson creator writes `lesson.yaml` + sibling `eval_<lesson>.yaml` (existing convention, unchanged), runs one command (`blendtutor eval-report <lesson>`), and gets a self-contained smevals HTML report that grades BOTH verdict polarity (deterministic, reusing existing expected verdicts per ADR-0007) AND feedback-message quality (LLM-judge, DeepSeek V4 Flash 0731 on Fireworks — same model as the feedback under test). The report is committed under `docs/evals/<lesson>/` and auto-published to GitHub Pages via the existing docs.yml assemble pattern, so authors can link to it as evidence. The existing `blendtutor eval` / `eval-report.json` / build-pipeline consumption stay byte-identical (no breaking changes); the smevals report is a superset layer on top.

## ACs

### AC-1
Resolved: 2026-08-07 — feedback_message: String (not Option): both Verdict variants carry message; never null/skipped

**depends_on:** none

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

### Decisions on Flagged Divergences
- **feedback_message type:** String, confirmed. B's correction (both variants carry message) accepted.
- **Error reporting under `--case N`:** thread ORIGINAL 1-based suite index through filter — preserve `(original_index, case)` pair during selection so `EvalRunError` reports user's requested number, not post-filter index 0. Predicate negative (k) covers sneaky-pass.
- **Range-check location:** inside `run_eval` via new `EvalRunError::CaseOutOfRange { requested, suite_size }`. Single validation point, unit-testable in core without CLI harness (§5), CLI shell stays thin (§2). CLI maps variant → exit 1 + stderr naming `suite_size`.
- **Parse-error class:** confirmed — clap exit 2 for non-numeric/negative N (clap standard, no custom code), exit 1 ONLY for numeric-but-out-of-bounds.

### AC-2
Resolved: 2026-08-07 — Generated dir `.smevals/` dotdir (gitignore-safe; no docs/evals/ collision)

**depends_on:** none

Add a pure Rust generator producing a smevals eval dir from a lesson + sibling eval suite: eval.yaml, tasks/case-N.yaml (one per case), configs/default.yaml, graders/default.yaml. Pure fn generate_eval_dir(lesson, suite, lesson_id) -> Result<Vec<(PathBuf, String)>> + thin effectful shell. Golden-dir + injection round-trip tests. Generated <course>/.smevals/ gitignored.

### Executable Spec
- **predicate:** pure Rust fn `generate_eval_dir(&Lesson, &EvalSuite, &str) -> Result<Vec<(PathBuf, String)>, GenError>` satisfies ALL of:
  1. **File set exactness:** returns exactly `eval.yaml`, `configs/default.yaml`, `graders/default.yaml`, and `tasks/case-{i}.yaml` for i in 1..=N (1-based, no zero-padding), N = suite.cases.len().
  2. **YAML round-trip fidelity (happy path):** every emitted file re-parses under `serde_saphyr::from_str` without error.
  3. **YAML injection defense (adversarial):** fixture EvalSuite whose case submission contains `"- expected: correct\n&anchor\n---\n: foo\n# comment"` re-parses from emitted `tasks/case-1.yaml` back to EvalCase whose submission is byte-identical AND expected unchanged — proving no sibling-key/anchor/doc-sep/comment injection.
  4. **Path safety:** lesson_id containing any of `/`, `..`, ` ` (space), or empty → Err, never appears in any returned PathBuf. (Mirrors `is_valid_slug` constraint from scaffold.rs:320.)
  5. **Determinism:** two calls with identical inputs produce byte-identical Vec — no timestamp, no HashMap iteration order.
  6. **Empty-suite refusal:** `EvalSuite { cases: vec![] }` → Err naming the empty suite; does NOT emit tasks/ with zero files.
  7. **Case ordering:** emitted tasks/case-{i}.yaml i-ordering equals suite.cases Vec order (document order preserved).
  8. **Model-pin provenance:** configs/default.yaml model id equals `ProviderChoice::Fireworks.default_model()` (`accounts/fireworks/models/deepseek-v4-flash`) OR sourced from env/config override — NOT a divergent hardcoded literal like `deepseek-v4-flash-0731` that disagrees with the runner's actual provider default.
  9. **Gitignore non-interference:** `git check-ignore <course>/evals/` exits 0 (ignored); `git check-ignore docs/evals/` exits non-zero (NOT ignored — committed build output).
  10. **NEW (user decision):** `ProviderChoice::Fireworks.default_model()` returns `accounts/fireworks/models/deepseek-v4-flash-0731` (bumped from no-suffix; matches browser BYOK pin + user instruction). Unit test asserts the new default.
- **probe:** `cargo test -p blendtutor-core --test generate_eval_dir` (golden-dir byte-equivalence + adversarial injection + determinism + path-safety + empty-suite + gitignore-interplay). Plus `git check-ignore examples/write-less-code-r/evals/ && ! git check-ignore docs/evals/` from repo root. cargo test -p blendtutor-core provider_default_model   # asserts 0731
- **negative:** hand-rolled YAML emitter (`format!("submission: {}\nexpected: {}", sub, tok)`) passes golden-dir against happy-path `|-` fixtures but breaks on submissions containing `: `, `&anchor`, `---`, or leading `- ` — emitted YAML re-parses to a DIFFERENT EvalSuite (injected key/anchor/doc-sep) or fails to parse. Golden-dir alone does NOT catch this.
- **verification:** code
- **fixture status:** NEW — `crates/core/tests/generate_eval_dir.rs` + `crates/core/tests/fixtures/generate_eval_dir/` (golden dir + adversarial injection fixture). Existing eval fixtures are generator INPUTS, reused.
- **rubric anchor:** §2.1 (pure core), §1.3.1 (reject invalid at boundary: non-slug lesson_id, empty suite, YAML-injection content), §5.1 (disciplined fn)

### Decisions on Flagged Divergences
- **Model pin (USER DECISION):** bump `ProviderChoice::Fireworks.default_model()` → `accounts/fireworks/models/deepseek-v4-flash-0731`. configs/default.yaml single-sources from it. Runtime match guaranteed (runner shells `blendtutor eval` which uses provider default). Existing drift (browser 0731 vs Rust no-suffix) fixed. All tests wiremock — no real model, no test impact.
- **Empty suite:** Err naming the empty suite (adversarial wins — vacuous 100% pass is sneaky-pass).
- **Generated dir:** `<course>/evals/` RENAMED → `<course>/.smevals/` (dotdir), ignored via `**/.smevals/` — robust for courses anywhere, provably does NOT collide with docs/evals/ (committed) or crates/*/tests/fixtures/evals/. Existing `.gitignore:5` `/evals/` untouched.
- **YAML emission:** serde-saphyr 0.0.27 is parse-only — hand-rolled block-scalar discipline pinned by injection round-trip test (first YAML serialization in codebase; #1 sneaky-pass vector).
- **Test shape:** golden-dir + parameterized round-trip over all 12 fixtures + adversarial injection — all three confirmed.

### AC-3
Resolved: 2026-08-07 — Script names pinned: run.sh + check_polarity.sh (AC-2 template must emit these paths)

**depends_on:** AC-1, AC-2

Shared smevals Runner script + deterministic polarity checker. Runner reads SMEVALS_MODEL/SMEVALS_PROMPT/SMEVALS_TASK_LESSON/SMEVALS_TASK_CASE/SMEVALS_TASK_EXPECTED env vars, invokes `blendtutor eval <lesson> --case N --format json` (AC-1), prints feedback_message to stdout (smevals saves as output.txt). Bounded transient-only retry. Polarity checker: exact expected-vs-actual verdict comparison, required: true in graders/default.yaml.

### Executable Spec
- **predicate:**
  1. Env→argv wiring: stub blendtutor on PATH asserts it receives `eval <lesson> --case N --format json` built from SMEVALS_TASK_LESSON/SMEVALS_TASK_CASE; stub counter file proves the stub was invoked (no canned output).
  2. No cargo run: grep -c 'cargo run' scripts/smevals/*.sh == 0 — runner resolves blendtutor from PATH.
  3. Header contract: on success, runner stdout line 1 == `verdict: correct` or `verdict: incorrect` (exact, lowercase); lines 2+ == feedback_message byte-exact, multi-line preserved (jq -r '.cases[0].feedback_message').
  4. Source-of-truth = .actual token, NOT .matched: inconsistent-JSON stub emitting .cases[0].actual == "incorrect" with .matched == true MUST produce `verdict: incorrect`.
  5. All four polarity combinations: expected in {correct, incorrect} x actual in {correct, incorrect} → checker exit 0 on the two matches, non-zero on the two mismatches.
  6. Line-1-only parsing: output file with `verdict: ` on line 2+ (message body contains it) MUST NOT confuse the checker — no grep verdict:; parse head -1 / read -r line 1 only.
  7. Fail-closed on malformed header: `verdict: maybe`, `verdict:` (empty), `verdict:correct` (no space), 0-byte output file → checker non-zero exit.
  8. Case-sensitive token: `verdict: Correct` → checker fails closed.
  9. Bounded retry, transient only: retry (max 3 attempts, sleep 2 backoff) on blendtutor exit-1, empty stdout, or malformed JSON; NO retry on well-formed mismatch verdict — stub counter shows exactly 1 call when valid JSON with mismatched verdict returned.
  10. Backoff wall-time: retry-then-fail scenario (stub always exits 1) takes >= 4s wall-clock.
  11. Per-call timeout: blendtutor invocation wrapped in `timeout "${SMEVALS_TIMEOUT:-120}"`; timeout-hit (exit 124) treated as transient → retry. Test overrides SMEVALS_TIMEOUT=2 with a sleep 5 stub to prove timeout path without 2-min waits.
  12. Missing env → usage error: unset SMEVALS_TASK_LESSON/SMEVALS_TASK_CASE → non-zero exit naming the missing variable on stderr.
  13. FIREWORKS_API_KEY propagation: no env -i / env scrubbing in runner — stub asserts the var is visible in its environment when set by the caller.
  14. Empty feedback_message: valid JSON with feedback_message: "" → header line emitted, zero following lines, runner exit 0.
  15. command -v jq guard: jq absent from PATH → runner exits non-zero naming jq.
  16. Hygiene: both scripts start with `set -euo pipefail`.
  17. required:true path contract: the checker path recorded in the AC-2-generated graders/default.yaml template == the actual checker script path (cross-file assert, e.g. grep -F of the literal path against both files).
- **probe:** bash scripts/tests/test_smevals_runner.sh (shell BDD, stub-on-PATH + counter-file pattern per scripts/tests/eval-course.sh:83-137; ok/ko/assert_eq/assert_contains helpers; mktemp -d + trap rm -rf; cd "$(git rev-parse --show-toplevel)"; set -euo pipefail)
- **negative:** Stub emitting inconsistent JSON (.actual=incorrect, .matched=true) with message body containing `verdict: correct` on line 3 — a sneaky-pass runner reading .matched or grep-scanning for verdict: produces the wrong polarity and the checker wrongly exits 0. Correct: verdict = incorrect from line-1 header derived from .actual; checker compares against SMEVALS_TASK_EXPECTED exactly.
- **verification:** code · shell BDD test with stub-on-PATH pattern
- **fixture status:** NEW — scripts/smevals/run.sh (runner), scripts/smevals/check_polarity.sh (checker), scripts/tests/test_smevals_runner.sh (test). Conventions anchor: scripts/tests/eval-course.sh:83-137 (existing). NOTE: script names pinned run.sh + check_polarity.sh — AC-2's generator template (configs/default.yaml runner path + graders/default.yaml checker path) MUST emit these exact relative paths.
- **rubric anchor:** §2 (pure polarity-checker logic shell-testable; effectful LLM call isolated in runner), §5 (single-responsibility scripts)

### Decisions on Flagged Divergences
- **Per-call timeout (B's needs-clarification):** resolved — `timeout "${SMEVALS_TIMEOUT:-120}"` wrapper on blendtutor invocation. Default 120s exceeds typical LLM latency (5-30s) with headroom; env-overridable so stub test exercises timeout path in ~2s (SMEVALS_TIMEOUT=2 + sleep 5 stub → exit 124 → transient retry). No user clarification needed.
- **Source-of-truth:** .actual token (adversarial wins) — matched derived via score_case (eval.rs:250-258), never independently set; inconsistent-JSON stub proves it.
- **Header contract:** line-1-only + fail-closed (adversarial wins).
- **Retry:** transient-only (exit-1/empty-stdout/malformed-JSON/timeout-124), never on mismatch verdict; 3 attempts, sleep 2 backoff.
- **Script names:** run.sh + check_polarity.sh (resolver pin) — cross-AC contract with AC-2's generator template.
- **Env propagation:** no env -i (FIREWORKS_API_KEY must reach blendtutor subprocess).

### AC-4
Resolved: 2026-08-07 — Score [0,1] normalized (smevals protocol); threshold applied by smevals, not checker

**depends_on:** AC-2, AC-3

LLM-judge checker: standalone smevals checker grading feedback-message quality via Fireworks tool-call (DeepSeek V4 Flash 0731), normalized [0,1] score, fail-closed, wired as second graders/default.yaml check with pass_threshold 0.8.

### Executable Spec
- **predicate:** ALL of:
  1. **Env-not-argv:** checker invoked with zero argv; reads `$SMEVALS_RUN_DIR/output.txt` via absolute env path. Test runs with cwd != run dir to prove no cwd-relative reads.
  2. **Score normalization [0,1]:** score == mean_of_5_dimensions / 5. Stubbed judge response mean 4.4/5 → stdout JSON `score == 0.88`, abs diff < 1e-9.
  3. **Exit-code/threshold separation:** stubbed low score (0.4/5 → 0.08) → judge exits 0 with `score: 0.08` on stdout. Threshold application belongs to smevals, NOT the checker. Nonzero exit = check ERROR only.
  4. **Tool-call request shape:** request body contains `model == $SMEVALS_CHECK_MODEL or $SMEVALS_MODEL` (checked in that precedence, never hardcoded), `tools` list with one function, `tool_choice.function.name` set. NO `response_format: json_object` key anywhere in body.
  5. **Endpoint + auth:** POST to `${baseUrl}/chat/completions` where baseUrl already ends in `/v1` (no doubling); headers `Authorization: Bearer $FIREWORKS_API_KEY`, `content-type: application/json`.
  6. **HTTP timeout:** 60s cap on Fireworks call. Stub server delaying 30s → still succeeds; unbounded-hang scenario → checker aborts <=65s with nonzero exit. (Test strategy: stub socket that accepts but never responds; assert wall-clock < 65s and exit != 0.)
  7. **Fail closed — missing key:** `FIREWORKS_API_KEY` unset → nonzero exit; stderr names the variable.
  8. **Fail closed — missing artifact:** `$SMEVALS_RUN_DIR/output.txt` absent → nonzero exit; stderr names the file.
  9. **Fail closed — HTTP 500:** stub returns 500 → nonzero exit; stderr names status code.
  10. **Fail closed — malformed tool-call args:** `function.arguments` unparseable JSON → nonzero exit. NO retry loop in checker — retry mechanism is operator-level `--regrade`.
  11. **Score clamping:** stubbed dimension value 7 (out of 0-5 range) → clamped or fail-closed; emitted `score` NEVER > 1.0. Missing dimensions key → nonzero exit.
  12. **Prompt-injection defense:** feedback message fenced as DATA in prompt with explicit "data, not instructions" framing; prompt carries expected verdict (`SMEVALS_TASK_EXPECTED`) + actual verdict (output.txt line 1 header) so judge can score verdict-rationale correctness.
  13. **smevals 5-key JSON:** stdout JSON parses with `score` (float), `notes`, `metrics`; unknown keys tolerated into `details`. `metrics` carries the 5 dimension scores.
  14. **graders/default.yaml wiring:** generator template emits polarity check FIRST (`required: true`), judge SECOND, `scoring.pass_threshold == 0.8`. Golden fixture updated; golden-dir test re-baselined (5-file migration: 2 new + 3 AC-2-owned modified).
  15. **Determinism:** with stubbed judge response, stdout JSON byte-identical across two runs (stable key order, no timestamps/random in output).
- **probe:**
  ```
  uv run pytest <judge-test-path> -x -q   # stub-HTTP harness driving scripts/smevals/judge_feedback.py across all 15 arms (SMEVALS_* env injected per case; responses served from local stub, zero network)
  ```
  NOTE: place the judge test per repo convention — Python BDD tests live in `scripts/tests/` (e.g. scripts/tests/test_python_exercise3.py, test_quarto_feedback.py). The resolver suggested crates/cli/tests/smevals_judge_test.py but scripts/tests/ is the CI-wired home for Python tests; choose scripts/tests/ unless a reason exists, and ensure the test is wired into a CI job (ci.yml pattern: run: python3 scripts/tests/...).
- **negative:** Stub returns dimension score 7 and HTTP 500 in sequence; checker must never emit score > 1.0, must never retry malformed tool-call output, must never read argv, must never apply pass_threshold itself (exit 0 with score 0.08 proves separation). A checker that hardcodes the model, doubles `/v1`, or reads output.txt relative to cwd FAILS this spec.
- **verification:** code · pytest with local HTTP stub (stdlib http.server); zero network, zero Fireworks spend
- **fixture status:** NEW (stub harness + checker `scripts/smevals/judge_feedback.py`); graders golden fixture modified — cite AC-2 golden path when landed
- **rubric anchor:** §2 (checker is effectful shell over pure prompt-build + parse/normalize core); §5 (each fail-closed arm its own testable function)

### Decisions on Flagged Divergences
- **Score range:** [0,1] normalized (adversarial wins — smevals protocol fact: score 0.0-1.0 per smevals docs lines 198-217; minimal's 0-5 was contract-violating).
- **Pass/fail owner:** smevals applies pass_threshold (adversarial wins — docs lines 191-197). Judge exits 0 on low score; nonzero only on error.
- **Checker invocation:** zero argv, env-only ($SMEVALS_RUN_DIR etc.) (adversarial wins — docs line 200).
- **Artifact path:** $SMEVALS_RUN_DIR/output.txt absolute (cwd = grade workspace != run dir).
- **Request shape:** tool-call (adversarial wins — feedback.js:312-335 codebase convention; json_object unverified on 0731).
- **JSON output:** smevals 5-key: score/metrics/notes/details (adversarial wins; rationale/dimensions top-level would fold into details unaggregated).
- **HTTP timeout:** 60s judge-imposed (urllib timeout); test via accept-but-never-respond stub socket, assert < 65s + nonzero exit.
- **No retry:** fail closed on malformed response; smevals --regrade is the retry mechanism.
- **Prompt injection:** data-fence delimiter + "data not instructions" + expected/actual verdict anchors in prompt.
- **Model id:** from SMEVALS_CHECK_MODEL or SMEVALS_MODEL env (never hardcoded).
- **Rubric (minimal's contribution, retained):** 5 dimensions x 0-5: verdict-rationale correctness, actionability, references actual check results, no solution leak, no hallucinated errors; extraction-anchored; normalized mean/5 >= 0.8.

### AC-5
Resolved: 2026-08-07 — Grade-fail = evidence: run-exit-1 + runs non-empty → exit 0 on build success; empty runs → exit 1

**depends_on:** AC-1..4

Turn-key `blendtutor eval-report <lesson>` subcommand: generate smevals eval dir (AC-2 pure fn) → `uvx smevals run -g` → `uvx smevals build -o docs/evals/<lesson>` → exit-code semantics (grade-fail is evidence, not gate). Pin smevals==0.2.0. Subcommand NOT shell script.

### Executable Spec
- **predicate:** Fake-`uvx` shim (PATH-injected, logs argv per invocation, exits `$FAKE_UVX_EXIT`) + success path → log == exactly 2 calls, `["uvx", "smevals==0.2.0", "run", <abs_gen_dir>, "-g"]` then `["uvx", "smevals==0.2.0", "build", <abs_gen_dir>, "-o", <abs_docs_evals_lesson>]` (uvx-form per shim convention — record argv verbatim, assert pin in package-spec position) AND every logged path `is_absolute()` AND no `--runs-dir` token anywhere AND exit code == 0 AND `docs/evals/<lesson>/index.html` exists AND a pre-seeded stale file in `.smevals/` is gone (stale-clean) AND a pre-seeded marker file in pre-existing `docs/evals/<lesson>/` survives when build fails. Failure paths: generator `Err` → exit 1, stderr names `generate`, shim log empty; run-exit-1-with-artifacts → build still invoked, exit 0; run-exit-1-with-empty-runs → exit 1 names `run`; build-exit-1 → exit 1 names `build`; uvx absent → clean stage-named error, no panic (assert stderr contains `uvx`, no `panicked at`).
- **probe:**
  ```
  uv run cargo test -p blendtutor-cli --test eval_report_cli
  ```
  (integration test builds `Command::cargo_bin("blendtutor")`, PATH-injects tempdir shim `uvx`; `blendtutor_output_env` helper can't hold dynamic PATH — test constructs its own env.)
  Secondary: `uv run cargo test -p blendtutor-cli` full suite (cli.rs PLANNED_SUBCOMMANDS 8→9 regression).
  Manual smoke (USER DECISION — not automated; builder runs locally with real key, commits docs/evals/<lesson>/):
  ```
  blendtutor eval-report examples/write-less-code-r/lessons/01_seed_data.yaml
  git add docs/evals/
  ```
- **negative:** (a) generator `Err` (missing sibling suite / bad slug) → uvx never invoked, exit 1 names `generate`; (b) run exits 1 but produced runs → still exit 0 (grade-fail is evidence, not gate); (c) build fails → exit 1 names `build` AND prior committed report untouched; (d) relative gen_dir would pass shim-log equality on naive strings — `is_absolute()` assertion catches CWD-relative regression; (e) `--runs-dir` sneaky-pass would strand runs outside eval dir making `build` (which lacks `--runs-dir`) build an empty/stale report — token-scan catches it; (f) panics on missing uvx (unwrap) — assert no `panicked at`.
- **verification:** code · cargo test integration against fake-uvx shim + manual real-key smoke (committed docs/evals/<lesson>/ per user decision)
- **fixture status:** NEW — `crates/cli/tests/eval_report_cli.rs`, shim generated per-test in tempdir. Reuses existing fixture lesson + sibling eval suite (e.g. crates/core/tests/fixtures/eval_command/ pair, or examples/write-less-code-r/ lesson + eval). "No sibling suite" case reuses LESSON const (add_two_numbers.yaml).
- **rubric anchor:** §1.2 (command set as checked sum type — Commands::EvalReport variant), §2.3 (pure generator in core, thin effectful shell in cli), §5 (single orchestration fn, stage-named anyhow::Context)

### Decisions on Flagged Divergences
- **Grade-fail semantics (#1 semantic trap):** adversarial wins WITH empty-runs guard. smevals run -g exits nonzero on grade-fail (low quality) OR process fail. eval-report: run exit code is INFORMATIONAL. run exits 1 + runs/ non-empty → proceed, exit 0 on build success (grade-fail = low-accuracy evidence; matches eval.rs "command always succeeds producing a report" philosophy + USER DECISION to commit real reports). run exits 1 + runs/ EMPTY (no usable artifacts) → exit 1 naming `run` (nothing committable; empty report is NOT evidence). Build failure → exit 1 naming `build` regardless of run outcome. Both fail → exit 1 naming `build` with run's exit code in error context. Shim test asserts each branch via per-call FAKE_UVX_EXIT sequencing.
- **lesson_id provenance:** RESOLVED — file stem (canonical per `new lesson` convention: id == file stem under lessons/), validated against manifest/sibling suite by AC-2 generator (Err on bad slug). Single-sourced via ONE shared helper (course-root walk-up + stem) used by BOTH AC-2's effectful shell and AC-5 — same derivation, no drift. 3 consumers (task names, docs/evals/<id>/, AC-6 URL /evals/<id>/) all read the one helper. Manifest-slug alternative rejected (dual-derivation risk).
- **.smevals/ location:** CONFIRMED course root via walk-up to blendtutor.toml (NOT lesson_path.parent()), cleaned before each generate (stale runs = false evidence).
- **Overwrite strategy:** build-into-temp + atomic rename. `smevals build -o docs/evals/.<lesson>.tmp/` then on success rm old dir + rename; on failure remove temp, prior committed report survives. docs.yml rm -rf precedent REJECTED (CI rebuilds from clean checkout; local re-run with committed prior report loses data on build failure otherwise).
- **gen_dir absolute + no --runs-dir + pin in argv[0] + uvx-not-found no-panic + cli.rs migration 8→9 + subcommand-not-script + no-CI-rerun:** confirmed adversarial clauses.
- **uvx-not-found:** check Command::spawn error (ErrorKind::NotFound) → anyhow error naming uvx/install hint; never .unwrap().
- **External contract:** smevals argv shape pinned but round-trip unverifiable in-repo — builder MUST smoke-test `uvx smevals==0.2.0 run -g` + `build` once against a real generated eval dir before merging; the committed docs/evals/<lesson>/ artifact (USER DECISION) doubles as that smoke-test evidence.

### AC-6
Resolved: 2026-08-07 — Guard form if/then/fi (set -e safe); && shorthand pinned absent

**depends_on:** none (guard keeps job green pre-AC-5)

Extend .github/workflows/docs.yml to assemble committed docs/evals/ into the GitHub Pages artifact at /evals/ following the existing cp-into-docs/book/book pattern, guarded so the job stays green before any report exists. Preserve all current paths. Extend scripts/tests/test_docs_pages_artifact.sh to assert the evals nest.

### Executable Spec
- predicate clause 1: "docs.yml build job contains a step with guard form `if [ -d docs/evals ]; then … fi` (literal `if [ -d docs/evals ]` present) AND the shorthand `[ -d docs/evals ] &&` is ABSENT anywhere in the workflow (Actions runs steps under `bash -eo pipefail`; a trailing && chain exits 1 when the dir is missing → CI red before AC-5 lands)."
- predicate clause 2: "Within that step, line order is: guard < `rm -rf docs/book/book/evals` < `mkdir -p docs/book/book/evals` < literal dot-copy `cp -R docs/evals/. docs/book/book/evals/` (trailing `/.`; bare cp double-nests to `/evals/evals/<lesson>/` → 404)."
- predicate clause 3: "Step ordering in build block: demo-standalone assemble < evals guard < `.nojekyll` < `actions/upload-pages-artifact@v5` (evals content lands before single-artifact upload; all current paths preserved)."
- predicate clause 4: "No `|| true` / `continue-on-error` on or around the evals step."
- predicate clause 5: "Deploy-leak: needles `evals`, `docs/evals` absent from the deploy job block (deploy has no checkout)."
- predicate clause 6: "Mirror contract: `scripts/check-docs.sh` (under `set -euo pipefail`, line 14) contains the same guarded assemble against `$book_out` — `if [ -d docs/evals ]`, `rm -rf \"$book_out/evals\"`, `mkdir -p \"$book_out/evals\"`, `cp -R docs/evals/. \"$book_out/evals/\"` — plus a double-nest assert `! [ -e \"$book_out/evals/evals\" ]` inside the guard."
- predicate clause 7: "Test Phase 1 extends: L_EVALS_GUARD/L_EVALS_CP block_line needles, ordering chain (clause 3), `&&`-shorthand absence pin (clause 1), MIRROR_OK needle list +3 → count 8→11, deploy-leak list +`evals`."
- predicate clause 8: "Functional fixture sub-phase: temp fixture `docs/evals/evals-fixture/index.html`; run the guarded assemble snippet under `bash -euo pipefail` against a scratch book_out → fixture HTML lands at `<scratch>/evals/evals-fixture/index.html`, byte-identical, AND `! [ -e <scratch>/evals/evals ]`; then with fixture removed, same snippet under `bash -e` exits 0 and creates no `evals/` dir. Fixture + scratch cleaned up (trap)."
- predicate clause 9: "No regression: existing Phase 1/2 survival asserts stay green; no `rm -rf docs/book/book/*` clobber introduced."
- probe: "bash scripts/tests/test_docs_pages_artifact.sh (wired in ci.yml quarto-render job at ci.yml:128; ci.yml itself NOT modified by this AC). Secondary: bash scripts/check-docs.sh (local mirror; if-guard skips evals when docs/evals absent)."
- negative: "(a) docs/evals missing → guard snippet exits 0 under bash -e, no evals nest, CI stays green (functional, clause 8); (b) && shorthand instead of if/then/fi → structural absence pin fails; (c) || true on the step → fail; (d) bare `cp -R docs/evals docs/book/book/evals/` (no `/.`) → functional double-nest assert fails; (e) evals step after .nojekyll or after upload → ordering chain fails; (f) check-docs.sh not mirrored → MIRROR_OK 11-count fails; (g) `evals` needle in deploy block → deploy-leak assert fails; (h) mkdir outside guard → empty /evals/ nest published pre-AC-5 → within-step order pin fails."
- verification: "code · shell test (structural pins + functional fixture sub-phase), CI-enforced via quarto-render job"
- fixture status: "NEW — runtime temp fixture (mkdir -p docs/evals/evals-fixture + echo HTML), removed via trap after assertion"
- rubric anchor: "§4.1 mirror contract (check-docs.sh mirrors docs.yml build steps); §5.1 guarded single-responsibility step"

### Decisions on Flagged Divergences
- **mkdir placement:** inside guard (adversarial wins — no empty /evals/ nest published pre-AC-5).
- **Stale-content removal:** `rm -rf docs/book/book/evals` inside guard, before mkdir (api precedent docs.yml:67 — docs/evals is committed source; lesson deletion is a committed deletion; cp-only never removes stale reports).
- **Guard form:** if/then/fi only; `&&` shorthand pinned absent (verified: Actions run: = bash -eo pipefail).
- **Fixture sub-phase:** inline functional snippet in test (catches cp semantics) coupled to docs.yml via literal needle pins (catches drift between replicated snippet and workflow).

### AC-7
Resolved: 2026-08-07 — readme.rs pin in scope (1-file migration); substring gotcha: "blendtutor eval" ⊂ "blendtutor eval-report"

**depends_on:** AC-1, AC-2, AC-5, AC-6

Document the turn-key eval-report flow: creating-lessons Step 9 + README + agent-notes + readme.rs pin. Run scripts/check-docs.sh as structural probe.

### Executable Spec
- **predicate:** ALL of —
  1. `grep -c '^## Step 9 — Generate the eval report' docs/book/src/creating-lessons.md` == 1
  2. `grep -c '^## Step 10 — Build a browser site' docs/book/src/creating-lessons.md` == 1 (old Step 9 at line 325 renumbered)
  3. `grep -c '^## Step 9' docs/book/src/creating-lessons.md` == 1 (no duplicate Step 9)
  4. New Step 9 section sits between old Step 8 (line 307 "Score the grading prompt") and Step 10
  5. `sed -n '/^## Step 9 — Generate/,/^## Step 10/p' docs/book/src/creating-lessons.md | grep -c 'eval-report\.json'` == 0 — **anti-conflation**: new step must NOT reference the OLD Slice-13 eval-report.json site-build artifact (6+ live refs: creating-lessons.md:344/362/365, README.md:121, site-build.md:98/110, build.rs:14 — copy-paste trap)
  6. Step 9 content greps (one grep each on the Step 9 section): `blendtutor eval-report` present; cost warning present (grep -i 'cost\|paid\|spend'); `FIREWORKS_API_KEY` present; local-only note present; grade-fail-is-evidence (exit 0 on completed-but-low-quality) present; `.smevals/` vs `docs/evals/<lesson>/` distinction present (both tokens); `git add docs/evals` scoping present; `uv` prerequisite present; `smevals` named
  7. `grep 'eval-report' README.md` >= 1 AND README authoring-workflow string (line 56) includes eval-report between eval and build (grep 'eval.*eval-report.*build' README.md)
  8. README "What is blendtutor?" bullet (lines 19-21) gains one-line pointer to docs/evals/<lesson>/ eval evidence
  9. docs/agent-notes/eval.md gains dated entries for AC-1/2/3/5 decisions tagged `(#AC-N)`, frontmatter gains `acs: [1, 2, 3, 5]`
  10. crates/cli/tests/readme.rs required array gains `("eval-report command", "blendtutor eval-report")` and the file docstring's workflow mention updated
  11. `cargo test -p blendtutor --test readme` exits 0
- **probe:**
  ```bash
  bash scripts/check-docs.sh && \
  grep -q '^## Step 9 — Generate the eval report' docs/book/src/creating-lessons.md && \
  grep -q '^## Step 10 — Build a browser site' docs/book/src/creating-lessons.md && \
  test "$(grep -c '^## Step 9' docs/book/src/creating-lessons.md)" -eq 1 && \
  test "$(sed -n '/^## Step 9 — Generate/,/^## Step 10/p' docs/book/src/creating-lessons.md | grep -c 'eval-report\.json')" -eq 0 && \
  grep -q 'blendtutor eval-report' README.md && \
  grep -qE 'eval.*eval-report.*build' README.md && \
  grep -q 'AC-1' docs/agent-notes/eval.md && grep -q '^acs:' docs/agent-notes/eval.md && \
  cargo test -p blendtutor --test readme
  ```
  NOTE: check-docs.sh is necessary but NOT sufficient (builds mdBook, rustdoc -D warnings, example sites, README example links, SUMMARY.md — checks NO prose/step/README-pointer content). The content greps are the real probe. check-docs.sh is a local mirror, NOT run in CI.
- **negative:** (A) Renumbering miss — Step 9 inserted but old "Step 9 — Build" not renumbered → duplicate Step 9 (#3); or Step 9 missing while Step 10 exists (#1). (B) Naming conflation — Step 9 references eval-report.json (OLD artifact), misleading authors; check-docs.sh + heading greps pass → #5 catches. (C) Missing cost warning / git-scoping / .smevals-vs-committed distinction → author incurs paid spend unknowingly or commits megabytes of ephemeral <course>/.smevals/ LLM output → #6 greps. (D) README bullet added but workflow string (line 56) untouched → author's mental model incomplete → #7.
- **verification:** code · bash (check-docs.sh + content greps + readme.rs test). Prose quality/tone is a manual skim — not gated, not load-bearing.
- **fixture status:** docs/book/src/creating-lessons.md:307-325 · README.md:19-21,56 · docs/agent-notes/eval.md:4,92 · crates/cli/tests/readme.rs:32-41 · scripts/check-docs.sh (exists)
- **rubric anchor:** — (no §N.M signal exercised by a docs AC)

### Decisions on Flagged Divergences
- **Test migration:** readme.rs pin IN scope (1 file). Resolver confirmed B right, A's "0 test migration" wrong. readme.rs substring gotcha: needle `"blendtutor eval"` is substring of `"blendtutor eval-report"` — existing eval pin cannot detect a missing eval-report pin; explicit tuple required.
- **agent-notes tag convention (was needs-clarification):** resolved — `(#AC-N)` tags + new `acs: [1, 2, 3, 5]` frontmatter field. Rationale: feature's natural unit is the AC (no slice numbers exist; issue numbers unknown at spec time); dated-entry convention preserved; acs: frontmatter additive, cannot collide with existing slices: [12, 13].
- **README scope:** bullet (lines 19-21) + workflow string update (line 56, eval → eval-report → build) — confirmed.
- **Anti-conflation scope:** new Step 9 must not reference eval-report.json (grep #5). Clarifying OLD refs (344/362/365, README:121) is OUT of scope — they correctly describe the real site-build artifact; optional follow-up, not a mandate.
- **Content depth:** full adversarial content greps retained (cost warning, FIREWORKS_API_KEY, local-only, grade-fail-is-evidence, .smevals vs docs/evals, git add docs/evals, uv prerequisite, smevals named).
- **Step mislabel correction:** AC text says "after existing Step 8"; existing Step 8 = "Score the grading prompt" (line 307, the blendtutor eval step). Step 7 = "Write an eval suite" (line 266). Insertion point = after line 323 regardless of label.

## Batch Schedule

- **Batch 1 (parallel): AC-1, AC-2, AC-6**
  - AC-1: Rust core+cli, own files except main.rs (sole Batch-1 main.rs writer).
  - AC-2: new generator module/scripts — disjoint from AC-1.
  - AC-6: workflow YAML only — disjoint. Include `[ -d docs/evals ]` guard so it merges green before any report exists.
- **Batch 2 (sequential chain): AC-3 → AC-4** (both depend on Batch 1; serialized against each other due to shared grader-template wiring)
- **Batch 3 (sequential): AC-5** (sole main.rs writer in this batch; integrates everything)
- **Batch 4 (sequential): AC-7** (documents the final, working UX + Pages URL shape)

## Open Questions

All decomposition open questions resolved via user decisions: (1) judge rubric draft + threshold 0.8 approved; (2) first real eval report INLINE in AC-5's PR; (3) course-level roll-up out of scope; (4) provider default bumped to deepseek-v4-flash-0731. No remaining open questions.

## Cross-AC Contracts

- Output.txt header contract: line 1 `verdict: correct|incorrect`, message lines 2+ (AC-3 defines, AC-4 consumes)
- Generated dir `<course>/.smevals/` gitignored via `**/.smevals/` (AC-2 owns; AC-5 writes into it)
- Script paths: `scripts/smevals/run.sh` + `check_polarity.sh` (AC-3) + `judge_feedback.py` (AC-4) — AC-2's generator template MUST emit these exact relative paths in configs/default.yaml + graders/default.yaml
- Model single-source: ProviderChoice::Fireworks.default_model() = `accounts/fireworks/models/deepseek-v4-flash-0731` (AC-2 bumps provider; configs/default.yaml + judge read from it / SMEVALS_* env)
- lesson_id = lesson file stem via shared course-root walk-up helper (AC-2 effectful shell + AC-5 agree; 3 consumers: task names, docs/evals/<id>/, /evals/<id>/ URL)
- graders/default.yaml: polarity check (required: true) FIRST, judge SECOND, pass_threshold 0.8 (AC-2 template, AC-3 wires polarity, AC-4 appends judge)
- `docs/evals/` committed artifact (AC-5 produces, AC-6 publishes, AC-7 documents)
- crates/cli/src/main.rs HOT FILE: AC-1 (Eval --case flag, Batch 1) and AC-5 (EvalReport variant, Batch 3) — serialized, never parallel