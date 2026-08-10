# Decomposition: smevals-eval-report

## Feature Goal

Make blendtutor's "run evals before you ship a lesson" selling point turn-key: a lesson creator writes `lesson.yaml` + sibling `eval_<lesson>.yaml` (existing convention, unchanged), runs one command (`blendtutor eval-report <lesson>`), and gets a self-contained smevals HTML report that grades BOTH verdict polarity (deterministic, reusing existing expected verdicts per ADR-0007) AND feedback-message quality (LLM-judge, DeepSeek V4 Flash 0731 on Fireworks — same model as the feedback under test). The report is committed under `docs/evals/<lesson>/` and auto-published to GitHub Pages via the existing docs.yml assemble pattern, so authors can link to it as evidence. The existing `blendtutor eval` / `eval-report.json` / build-pipeline consumption stay byte-identical (no breaking changes); the smevals report is a superset layer on top.

## Verified Codebase Facts (checked against source, 2026-08-07)

- `crates/cli/src/commands/eval.rs`: thin shell — `sibling_suite_path`, drives `run_eval`, calls `output::emit_eval(&report, format)`. No single-case mode exists.
- `crates/core/src/eval.rs`: `CaseResult { expected, actual, matched }` — **`feedback_message` is dropped** in `ExpectedVerdict::from(&Verdict)` (line 86-96). `CaseResult::score` is the ONLY constructor. `EvalReport` serializes `{cases, accuracy}`.
- `crates/cli/src/output.rs:412` `emit_eval` — JSON = `serde_json::to_string(report)`; human = accuracy headline + per-case rows. Adding a field to `CaseResult` changes BOTH JSON and (potentially) human render.
- `crates/cli/src/main.rs`: clap `Commands` enum — `Eval { lesson, format }`. New subcommand = new variant + match arm + `commands/mod.rs` module line. THIS IS THE HOT FILE.
- Test seam: `BLENDTUTOR_PROVIDER_URL` (`PROVIDER_URL_VAR` in `commands/mod.rs:19`) + wiremock `mount_feedback_for(server, needle, …)` routing distinct scripted verdicts by submission needle. No real API key needed in tests.
- `scripts/eval-course.sh` consumes `blendtutor eval --format json` via jq `.cases[] | .matched` — **adding a JSON field is safe (jq selects), renaming/removing is not.**
- `.github/workflows/docs.yml`: single-path artifact `docs/book/book` via `upload-pages-artifact@v5`, with an established **assemble pattern**: `cp -R` into `docs/book/book/{api,examples,demo}` before upload. Evals nest the same way (`docs/book/book/evals/`) — **multi-path artifact support is NOT needed**; the open question from research is resolved by following the existing pattern. `test_docs_pages_artifact.sh` exists as the test harness for this job.
- `.gitignore`: `/evals/` anchored (top-level R working dir); a generated `<course>/evals/` tree is NOT currently ignored.
- `docs/book/src/creating-lessons.md`: 9-step guide; Step 7 = write eval suite, Step 8 = score grading prompt (`blendtutor eval`). New step slots after Step 8.
- Tests: `cargo nextest run`; eval integration tests skip without `Rscript` (`rscript_absent()`); demo-lesson eval cases must be cleanly-running code or a launch failure becomes `EvalRunError`.

## AC Table

| AC | Description | Dependencies | Conflict Set | Risk | Medium |
|----|-------------|--------------|--------------|------|--------|
| 1 | Extend `blendtutor eval --format json` to carry per-case `feedback_message` and add `--case N` single-case selection | none | `crates/core/src/eval.rs`, `crates/cli/src/commands/eval.rs`, `crates/cli/src/main.rs`, `crates/cli/src/output.rs`, `crates/core/tests/eval.rs` | med | code |
| 2 | Add generator producing a smevals eval dir (`eval.yaml`, `tasks/case-N.yaml`, `configs/default.yaml`, `graders/default.yaml`) from a lesson + its sibling eval suite | none | new `scripts/smevals/` (or `crates/core/src/smevals_gen.rs` — see design notes), `.gitignore`, tests | med | code |
| 3 | Add shared smevals runner script (reads `SMEVALS_TASK_*` env, invokes single-case eval, prints feedback to stdout, retry wrapper) + deterministic polarity checker | AC-1 | `scripts/smevals/run-llm`, `scripts/smevals/checkers/polarity*`, `scripts/tests/test_smevals_runner.sh` | med | code |
| 4 | Add LLM-judge checker calling Fireworks DeepSeek V4 Flash 0731 with a feedback-quality rubric, wired into `graders/default.yaml` with a `pass_threshold` | AC-2 | `scripts/smevals/checkers/judge*`, generator's `graders/default.yaml` template (AC-2's file), `scripts/tests/test_smevals_judge.*` | med | code |
| 5 | Add turn-key `blendtutor eval-report <lesson>` subcommand: generate dir → `uvx smevals run -g` → `uvx smevals build -o docs/evals/<lesson>` → nonzero exit on failure; pin `smevals==0.2.0` | AC-1, AC-2, AC-3, AC-4 | **`crates/cli/src/main.rs` (HOT — AC-1 also touches)**, `crates/cli/src/commands/mod.rs`, new `crates/cli/src/commands/eval_report.rs`, `scripts/tests/test_eval_report.sh` | high | code |
| 6 | Extend docs.yml to assemble committed `docs/evals/` into the Pages artifact at `/evals/` (existing cp-into-`docs/book/book` pattern), preserving all current paths | none (mechanical); validated end-to-end only after AC-5 | `.github/workflows/docs.yml`, `scripts/tests/test_docs_pages_artifact.sh`, `.gitignore` (decide committed-vs-ignored boundary) | low | code |
| 7 | Document the turn-key eval flow: new step in `creating-lessons.md` (after Step 8) + README pointer + `docs/evals/` evidence convention | AC-5, AC-6 | `docs/book/src/creating-lessons.md`, `README.md`, `docs/agent-notes/eval.md` | low | code (check-docs.sh) + manual (tone) |

## Dependency DAG

```
AC-1 ──────────────┐
                   ├──→ AC-3 ──┐
AC-2 ───────┬──────┘          ├──→ AC-5 ──→ AC-7
            └──→ AC-4 ────────┘          ↗
AC-6 ───────────────────────────────────┘ (AC-7 only; AC-6 itself is independent)
```

- AC-1 and AC-2 are roots — no shared files, fully parallel.
- AC-3 needs AC-1's `--case N` + `feedback_message` JSON (runner shells the CLI).
- AC-4 needs AC-2's directory layout (grader paths are relative to the generated dir).
- AC-5 needs all four (orchestrates generator + runner + checkers + build).
- AC-6 is mechanically independent (workflow edit + test), but its live-URL validation only means something once AC-5 can produce `docs/evals/` content. Mergeable in Batch 1 with a guard (`[ -d docs/evals ]` in the assemble step), re-validated after AC-5.
- AC-7 documents final UX, so it lands last.

## Hot Conflict Files

- **`crates/cli/src/main.rs`**: AC-1 (adds `--case` flag to `Eval` variant) + AC-5 (adds `EvalReport` variant + match arm). Serialize: AC-1 in Batch 1, AC-5 in Batch 3 — never parallel.
- **`scripts/smevals/checkers/` + generator's `graders/default.yaml` template**: AC-3 (polarity checker) and AC-4 (judge checker) both wire into the grader template owned by AC-2. Serialize AC-3 → AC-4 within Batch 2, or split the template into `checks:` list edits that don't overlap (risky — prefer serialize).
- **`.gitignore`**: AC-2 (generated dir ignore rule) + AC-6 (docs/evals boundary). Different lines, low real risk; flag only.
- `docs/book/src/creating-lessons.md`: AC-7 only. No conflict.

## Suggested Batch Schedule

- **Batch 1 (parallel): AC-1, AC-2, AC-6**
  - AC-1: Rust core+cli, own files except main.rs (sole Batch-1 main.rs writer).
  - AC-2: new generator module/scripts — disjoint from AC-1.
  - AC-6: workflow YAML only — disjoint. Include `[ -d docs/evals ]` guard so it merges green before any report exists.
- **Batch 2 (sequential chain): AC-3 → AC-4** (both depend on Batch 1; serialized against each other due to shared grader-template wiring)
- **Batch 3 (sequential): AC-5** (sole main.rs writer in this batch; integrates everything)
- **Batch 4 (sequential): AC-7** (documents the final, working UX + Pages URL shape)

## Design-Intent Notes (per AC)

### AC-1 — expose feedback_message + single-case mode
- **Types (§1):** extend `CaseResult` with `feedback_message: String` captured in `CaseResult::score` (the only constructor — message cannot be inconsistent with the verdict it came from). Serialization adds one key; `eval-course.sh`'s jq (`.matched` selects) is unaffected. Human render: keep unchanged or truncate-preview — decide in spec; JSON is the runner contract.
- **Single-case surface:** smallest clean change = `--case N` (1-based, matching human report numbering per agent-notes/eval.md case-numbering convention) on the existing `Eval` variant, NOT a new subcommand. Out-of-range N = exit 1 naming the suite size.
- **Pure/effectful (§2):** `run_eval` gains a case-filter parameter (pure selection before the effectful loop); per-case pipeline unchanged. Keep `core` filesystem-free.
- **Do NOT** change exit-code semantics (still exit 0 on low accuracy) or `eval-report.json` shape consumed by `build`.

### AC-2 — smevals eval-dir generator
- **Module responsibility (§4):** one module owns "blendtutor eval suite → smevals directory" translation. Pure function: `(Lesson, EvalSuite, lesson_id) → Vec<(relative_path, contents)>`; effectful shell writes files. This makes golden-dir tests trivial (compare emitted tree against `tests/fixtures/` snapshot) with zero LLM.
- **Placement decision for spec phase:** Rust (`crates/core/src/smevals_gen.rs`, driven by AC-5's subcommand) is preferred over a Python script — keeps workspace mono-language, reuses `parse_eval_suite`, testable via nextest. A script would duplicate the eval-YAML parse.
- **Conventions baked in:** `configs/default.yaml` pins `model: accounts/fireworks/models/deepseek-v4-flash-0731` (the browser-pinned id — single source decision belongs here); `tasks/case-N.yaml` carry `lesson:` + `case:` keys (→ `SMEVALS_TASK_LESSON`/`SMEVALS_TASK_CASE`); runner/checker paths relative into `scripts/smevals/`.
- **Git hygiene:** generated `<course>/evals/` + smevals `runs/` gitignored (decision: generated dir is ephemeral build input; `docs/evals/` is the committed artifact — matches docs/evidence/ convention). Beware the anchored `/evals/` rule (agent-notes/eval.md gotcha) — new rule must be anchored per-course or use `**/evals/*/runs/` style patterns deliberately.

### AC-3 — runner + polarity checker
- **Runner contract:** reads `SMEVALS_MODEL`, `SMEVALS_TASK_LESSON`, `SMEVALS_TASK_CASE`; invokes `blendtutor eval <lesson> --case N --format json`; prints `feedback_message` to stdout. Retry wrapper (smevals has none): bounded retries with backoff on transient failure; exhausted retries exit nonzero so `smevals run -g` fails (CI-gateable locally).
- **Verdict channel (spec-phase detail, flagged):** smevals saves only runner stdout as `output.txt`. The polarity checker needs the *actual verdict*. Options: (a) runner emits a machine header (first line `verdict: correct`, message after) — polarity checker parses the header, judge checker sees whole file; (b) sidecar file in the run dir. Recommend (a) — single artifact, no sidecar protocol. **This makes AC-3 and AC-4 coupled to the header format — serialize (already done in schedule).**
- **Polarity checker:** pure script, `expected` passed via `SMEVALS_TASK_EXPECTED` (generator bakes it into the task YAML), exact polarity equality — mirrors `score_case` semantics (no substring slack).
- **Testing:** stub provider via `BLENDTUTOR_PROVIDER_URL` + wiremock needle pattern (existing seam, agent-notes/eval.md); runner test shells the compiled CLI against the stub — no real Fireworks key. Requires `Rscript` guard pattern (`rscript_absent()`) if tests execute submissions.

### AC-4 — LLM-judge checker
- **Rubric (manual-taste input needed at spec):** correctness of verdict rationale, actionability, references actual check results/output, no solution leak, no hallucinated errors. Judge = DeepSeek V4 Flash 0731 via Fireworks OpenAI-compatible endpoint; structured output (score + rationale) so grading is parseable; `pass_threshold` in `graders/default.yaml`.
- **Self-judge caveat:** judge model == model under test. Acceptable per user decision (do not re-litigate) but the rubric should be extraction-anchored (cite check results) to limit self-leniency. Note in spec.
- **Testing:** mock the judge endpoint (httpx-mock / local stub), never real key in CI. Cost: judge call per case per run — local only, documented in AC-7.

### AC-5 — `blendtutor eval-report` subcommand
- **UX decision (resolved):** subcommand, not a shell script — discoverable via `--help`, consistent with init/new/validate/run/eval/build, and the pipeline already lives in Rust. It shells `uvx smevals==0.2.0` (pinned — smevals is young) as a subprocess; no Python API exists.
- **Effectful orchestration (§2.4):** the command is a thin shell — generate (AC-2 pure fn) → run → grade → build. Each stage's failure names the stage and exits nonzero. No scoring logic in cli.
- **Output contract:** `docs/evals/<lesson>/` created/updated; runs dir stays inside the gitignored generated dir. Guard: refuse (or `--force`-overwrite) dirty existing output? Decide in spec — recommend overwrite-with-clean (rm + rebuild) for reproducibility, matching docs.yml's `rm -rf …/api` precedent.
- **Testing:** cli integration test with stub provider + a fake `uvx` shim on PATH (assert argv sequencing + exit-code propagation) — no real smevals/Fireworks in CI. A real-key smoke run is manual (AC-7 documents it).

### AC-6 — Pages publishing
- **Follow the existing assemble pattern** (docs.yml lines 63-111): `mkdir -p docs/book/book/evals && cp -R docs/evals/. docs/book/book/evals/` with a `[ -d docs/evals ]` guard so the job stays green before the first report lands. `.nojekyll` already at artifact root — smevals' underscore paths are safe. Multi-path `upload-pages-artifact` NOT needed (question resolved against source).
- Extend `test_docs_pages_artifact.sh` to assert the evals nest appears in the assembled artifact (fixture HTML under a temp `docs/evals/`).
- Live-URL check (rodney `pages-live.js` pattern) is a **manual/verify-live follow-up** after first real report — do not gate CI on content that requires a paid key.

### AC-7 — docs
- `creating-lessons.md`: new "Step 9 — Generate the eval report" (renumber Build to Step 10) — command, cost warning (real API calls, local only), commit-`docs/evals/` convention, Pages URL shape `https://mcmullarkey.github.io/blendtutor/evals/<lesson>/`.
- README: one-line pointer in the selling-points section (the feature request quotes it).
- `docs/agent-notes/eval.md`: append dated entries for AC-1/2/3 decisions (feedback_message surfacing, header-channel verdict protocol, generator purity) per repo convention.
- Run `scripts/check-docs.sh` (builds book + assembles locally) as the structural probe.

## Open Questions

- [needs-clarification] **Rubric content + pass_threshold for the LLM-judge** (AC-4): I can draft a rubric (correctness / actionability / grounded-in-checks / no-leak / no-hallucination, score ≥ 0.8), but the pass bar is a product-taste call. Default proposal: threshold 0.8, judge prompted for integer 0-5 per dimension. OK to proceed with that and tune later?
- [needs-clarification] **First committed evidence report**: publishing a real `docs/evals/<lesson>/` for an example course requires one local run with a real `FIREWORKS_API_KEY` (cost, non-deterministic). Should the first committed report (e.g. for `examples/write-less-code-python`) be part of AC-5's PR (manual local run, committed) or a follow-up after merge? Recommend: follow-up PR, keeps AC-5's diff deterministic/reviewable.
- [needs-clarification] **Course-level aggregation**: feature is per-lesson. A course-level `eval-report <course>` roll-up (all lessons → one Pages index) is OUT of scope here — confirm, or flag as a fast-follow.
- Non-blocking (resolved during decomposition, listed for the record): multi-path Pages artifact → resolved via existing assemble pattern; subcommand vs script → subcommand; verdict channel to polarity checker → stdout header line (AC-3 note).
