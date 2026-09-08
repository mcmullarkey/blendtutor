# Master Plan: user-friendliness

## Feature Goal

Make blendtutor easy to adopt and easy to read: (1) one-line install via GitHub
releases + a uv-style `curl | sh` installer script (no Rust toolchain required);
(2) mass slimming of user-facing documentation (README + mdBook chapters) to the
concision bar set by `whole-game.md`, plus removal of the redundant
`demo-standalone/` directory (example sites + demo book already cover it);
(3) a more intuitive, first-class evals flow so the native
`blendtutor eval` / `eval-report` path is the obvious way to score grading
prompts, with smevals remaining optional rather than required; and (4) a
user-side interpretation surface: terminal output that shows *why* a case
mismatched (grader's verbatim feedback + next-step hints) and a durable
smevals-free report artifact, so a binary-installed user can run AND
interpret evals without an external Python toolchain.

## Wave Plan

- **Wave 1 (phase-1): AC-1, AC-2, AC-3, AC-4, AC-5** — ships one-line install + slim docs + demo-standalone removal. Revertable per-AC (release workflow, installer, README, book, demo removal are each independently mergeable).
- **Wave 2 (phase-2): AC-6, AC-7, AC-8** — eval UX redesign + user-side interpretation surface (terminal mismatch detail + durable smevals-free artifact). Sequenced after docs are slim so eval docs land on the new concise surfaces; AC-6 → AC-7 → AC-8 serialized on `tests/eval.rs` + whole-game.md eval region. Together they make the native eval loop run-AND-interpretable with no external Python toolchain.

## Dependency DAG

```
AC-1 → AC-2 → AC-3
AC-4 (independent)
AC-5 (independent; README demo-link edit orders with AC-3)
AC-1, AC-2 ─┐
            ├→ (nothing else depends on release chain)
AC-3, AC-4 → AC-6 (docs conflicts: README, creating-lessons.md)
AC-6 → AC-7 (eval command surface + whole-game.md eval region finalized; tests/eval.rs serialization)
AC-7 → AC-8 (tests/eval.rs + whole-game.md serialization; AC-8 docs land on AC-7's new output shape)
```

## Batch Schedule

- Batch 1 (parallel): AC-1, AC-4, AC-5*  *(AC-5's README link edit deferred to AC-3's batch to avoid README race)
- Batch 2 (after AC-1): AC-2
- Batch 3 (after AC-2, parallel): AC-3 (+ AC-5's deferred README demo-link swap folded in)
- Batch 4 (wave 2, after AC-3 + AC-4): AC-6 (spec → build)
- Batch 5 (wave 2, after AC-6): AC-7 (spec-resolved — straight to build)
- Batch 6 (wave 2, after AC-7): AC-8 (speculate → resolve → build)

## Hot Conflict Files

- `README.md`: touched by AC-2, AC-3, AC-5 (demo-link swap), AC-6 — serialize: AC-2 → AC-3 → AC-5's link edit folded into AC-3 or immediately after; AC-6 in wave 2.
- `docs/book/src/creating-lessons.md`: AC-4, AC-6 — wave separation (AC-6 wave 2).
- `.github/workflows/ci.yml`: AC-2 (add install test step), AC-5 (remove demo render step), AC-6 possibly — serialize via wave/batch chaining.
- `.github/workflows/docs.yml`: AC-5 (demo removal), AC-6 maybe — AC-6 wave 2.
- `scripts/tests/test_docs_pages_artifact.sh`: AC-5 only (plus docs.yml mirror contract).
- `crates/cli/tests/eval.rs`: AC-6 (runs pinned tests, unedited), AC-7 (new footer/feedback assertions), AC-8 (new flag assertions) — serialize AC-6 → AC-7 → AC-8.
- `docs/book/src/whole-game.md` (eval region :57-83): AC-6 (eval docs rewrite), AC-7 (new output example), AC-8 (artifact flow) — same wave-2 serialization.
- `crates/cli/src/output.rs` + `crates/cli/src/snapshots/*eval_human*.snap`: AC-7 only.
- `crates/cli/src/commands/eval.rs`: AC-8 only (AC-6's P5 pins its surface but does not edit it; --help grep pin must survive any flag addition).

## Open Questions (all resolved)

- [resolved] demo/ = demo-standalone/ deletion; demo-book/ kept (user confirmed).
- [resolved] verify-live DELETED outright, not repointed (user decision). pages-live suite fate: delete if sole consumer is verify-live; else retarget to /demo-book/.
- [resolved] Windows installer out of scope (user confirmed); linux+macOS only.
- [resolved] evals/*.R legacy scripts pruned in AC-6 if verified dead (user confirmed; speculators must verify call sites).

## Flagged Item (non-blocking)

- `docs/test-strategy/assertion-quality.md` referenced by spec prompts does NOT exist in this repo (glob empty). Non-blocking: builders classify assertions per prompt-implied vocabulary. Director should reconcile the doc path or drop the pointer from spec prompts.

---

# AC Specs

## AC-1 — release workflow

- Spec mode: spec-resolved
- Key files: `.github/workflows/release.yml` (NEW), `Cargo.toml`, `crates/cli/Cargo.toml` (bin name = `blendtutor`; confirm `[[bin]]`/package name)
- Invariants/constraints:
  - Repo convention: least-privilege `permissions:`, toolchain pinned via `rust-toolchain.toml`, no `continue-on-error`/`|| true` anywhere (CI doctrine — see ci.yml comments).
  - Trigger: `v*` tags + `workflow_dispatch`. Artifact contract (consumed by AC-2 — pin it HERE): `blendtutor-<tag>-<target>.tar.gz` + `blendtutor-<tag>-sha256sums.txt`, release name = tag.
  - Precedent test: YAML pin tests use awk job-block extraction (see `scripts/tests/test_verify_live_wiring.sh` Phase 1).
- Prior art: uv's release flow (astral-sh/uv release.yml); repo's own `docs.yml` build-job structure.
- Verification: code (YAML pin test, fail-closed) — verification of an actual release is manual/post-tag.
- Test seam: NEW `scripts/tests/test_release_yml.sh` modeled on `test_verify_live_wiring.sh` Phase 1.
- Literal grep requirement: no model-ID/constant touched. Asset-name literal `blendtutor-<tag>-*` appears only here + AC-2 test — cross-grep these two files at PR time.

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open

## AC-2 — install.sh

- Spec mode: spec-resolved
- Key files: `scripts/install.sh` (NEW), `scripts/tests/test_install_sh.sh` (NEW), `README.md` (install section — final wording lands in AC-3; AC-2 makes only the minimal section edit)
- Invariants/constraints:
  - uv installer contract: `curl -LsSf https://github.com/mcmullarkey/blendtutor/releases/...` — script must work via `curl | sh` (stdin-piped, no `$0` assumptions, POSIX-ish sh).
  - Asset naming must byte-match AC-1's pinned contract.
  - Fail-closed: checksum mismatch → exit nonzero with message (repo doctrine).
  - Install dir default `~/.local/bin`, overridable via env (uv uses `UV_INSTALL_DIR`; mirror as `BLENDTUTOR_INSTALL_DIR`).
- Prior art: astral.sh/uv/install.sh; repo shell-BDD precedent `scripts/tests/test_smevals_runner.sh` (stub-based, env-injection).
- Verification: code
- Test seam: stub-based shell BDD — fake `curl`/`uname` via PATH injection (mirror `test_smevals_runner.sh` pattern). Runs in ci.yml like other script tests → test must also add its ci.yml invocation (hot conflict with CI file — note for batch scheduling).
- Literal grep: `mcmullarkey/blendtutor` base URL literal — grep across `scripts/ tests/ rodney-probes/` at spec-compile to catch existing pins (quarto add tests use the same org/repo string).

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open

## AC-3 — README slim

- Spec mode: spec-resolved
- Key files: `README.md`
- Invariants/constraints:
  - Quality bar = `docs/book/src/whole-game.md` tone: one idea per section, commands first, prose subordinate.
  - Hard pins (must survive): `quarto add mcmullarkey/blendtutor` install command, BYOK mention, min-Quarto requirement, demo link — `scripts/tests/test_quarto_distribution.sh` asserts a 15-clause README predicate; grep its exact clauses before rewriting and keep every pinned string.
  - Ceiling: ≤ ~130 lines (from 373). Install section leads with the `curl | sh` one-liner; `cargo install --path crates/cli` becomes the fallback.
- Prior art: whole-game.md structure.
- Verification: manual (succinctness judgment) + code (`test_quarto_distribution.sh` pins stay green).
- Test seam: existing `test_quarto_distribution.sh` (grep it verbatim first — it is the structural contract).

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open

## AC-4 — book slim

- Spec mode: spec-resolved
- Key files: `docs/book/src/creating-lessons.md` (458 → ≤150 lines), `docs/book/src/introduction.md`, `docs/book/src/examples.md`, `docs/book/src/SUMMARY.md`, `docs/book/src/api-reference.md`
- Invariants/constraints:
  - Do NOT touch `whole-game.md` (the bar), `docs/adr/*` (historical records), `docs/agent-notes/*` (agent-facing, not user docs), `docs/evidence/*` (historical).
  - Move detail OUT, don't delete info that has no other home: architecture/API depth belongs to rustdoc (`/api`) — link, don't inline.
  - `scripts/check-docs.sh` + `test_docs_pages_artifact.sh` build the mdBook — any SUMMARY.md change must keep `mdbook build docs/book` green.
- Prior art: `whole-game.md`.
- Verification: manual + code (mdbook build green in CI).
- Test seam: existing docs build (`scripts/check-docs.sh`).

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open

## AC-5 — demo-standalone removal

- Spec mode: spec-resolved
- Key files (full blast radius from repo-wide grep of `demo-standalone`):
  - `demo-standalone/` (delete dir)
   - `.github/workflows/docs.yml` — remove "Render demo-standalone", "Fix COI service-worker scope", "Assemble demo-standalone (/demo/)" steps; remove verify-live job entirely (user decision) (currently probes `DEPLOYED_URL + demo/`)
  - `.github/workflows/ci.yml` — remove the "Standalone demo render test" step (`test_demo_standalone_render.sh`)
  - `scripts/fix-demo-coi-scope.sh` (delete), `scripts/check-docs.sh` (remove demo legs)
  - `scripts/tests/test_demo_standalone_render.sh` (delete), `scripts/tests/test_docs_pages_artifact.sh` (remove demo pins)
   - `rodney-probes/pages-live.js`, `pages-live-core.js`, `pages-live-core.test.js` (probe targets /demo/) — verify-live job is DELETED per user decision (not repointed). If the pages-live suite's sole consumer is verify-live, delete the suite too; if other consumers exist (e.g. ci.yml PR checks), retarget them to /demo-book/. Builder/spec must verify consumers before choosing.
  - Historical `docs/evidence/150|152|153|170|199|212/` logs reference demo-standalone — DO NOT touch (historical records).
- Invariants/constraints:
  - `check-docs.sh` mirrors docs.yml (mirror-contract pinned by `test_docs_pages_artifact.sh`) — the two must stay in lockstep.
  - Quarto-distribution test pins a "demo link" in README (see AC-3) — that link points at /demo/ today; AC-5 must update it to /demo-book/ or an example site, coordinated with AC-3's README rewrite (README conflict → this AC's README edit orders with AC-3; keep the edit minimal: swap the URL only).
- Prior art: none — pure deletion + re-pointing.
- Verification: code (CI green: remaining tests after pin removal; mirror contract intact).
- Test seam: existing `test_docs_pages_artifact.sh` + `check-docs.sh`.

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open

## AC-6 — eval UX redesign

**Factual dispute verdict:** Speculator A correct. `scaffold_plan()` (scaffold.rs:60-83, init-only via `scaffold_course`) plans EVAL_FILENAME (eval_lesson_hello.yaml) + EVAL_TEMPLATE for the starter lesson. `add_lesson()` (scaffold.rs:346-361, called by `blendtutor new lesson` via commands/new.rs) writes ONLY lessons/<id>.yaml + manifest append — no eval sibling. B conflated init starter-files with new-lesson path. Corollary: evals/*.R untracked+gitignored (.gitignore:5 /evals/ anchored) — "prune" in PR = (1) .gitignore tracked edit, (2) doc reference cleanup, (3) local rm -rf evals/ documented in issue (PR cannot delete untracked files).

**Needs-clarification resolutions:**
1. evals/logs/ — pruned (same local rm -rf evals/).
2. Eval-sibling template — language-aware minimal 1-case skeleton via pure eval_template(Language, id) mirroring lesson_template dispatch.
3. Command-surface change — NO. No doctor, no fold-into-build. "First-class" = (a) scaffolding parity (real gap), (b) prominent README/book eval section naming sibling convention, (c) prune + .gitignore cleanup. Missing-sibling error already names resolved path (eval.rs:40-41) — regression-pin only.

## AC spec: Eval UX first-class — scaffold eval sibling in `new lesson`, prune dead evals/*.R + /evals/ gitignore entry, pin EvalSummary/smevals/CLI non-regression

### Executable Spec
- predicate:
  - F1 (scaffolding parity — the functional gap): Fresh `blendtutor init` course in tempdir; `blendtutor new lesson --lang python tally` → lessons/tally.yaml exists AND lessons/eval_tally.yaml exists; sibling name derived via single eval_ naming convention (shared prefix constant consumed by sibling_suite_path, commands/mod.rs:32-37 — never a second hand-copied format!("eval_{id}.yaml") rule). Sibling content = language-aware minimal 1-case suite emitted by pure eval_template(Language, &str) mirroring lesson_template.
  - F2 (no-clobber, atomic): eval sibling written via existing write_without_clobber (scaffold.rs:369-375, create_new fused check) — second `blendtutor new lesson` on existing id still fails AlreadyExists; existing user-edited eval_tally.yaml never overwritten.
  - P1 (prune completeness): .gitignore:3-5 (/evals/ entry + 2-line anchoring comment) removed from tracked config. No ACTIVE code/CI/shell/doc reference to eval_fireworks_(vitals|sequential).R or eval_evaluate_with_llm — absence grep across crates/*/src, .github/, scripts/, docs/book/src, README.md returns zero hits (historical provenance in docs/adr/, docs/agent-notes/, crates/core/tests/fixtures/evals/*.yaml header comments is archival and exempt). Deletion of untracked evals/*.R + evals/logs/ is local rm -rf evals/, documented in issue body.
  - P2 (smevals optionality, structural): crates/cli/src/commands/eval.rs retains zero references to uvx|smevals|SMEVALS_PIN; eval_report.rs retains uvx smevals==0.2.0 pin (sole integration point). README/book describe smevals as optional for eval-report, never prerequisite for eval.
  - P3 (non-regression — EvalSummary boundary): eval_summary_from_report_json (site/mod.rs:392-415) unchanged: accuracy > 1.0 → AccuracyOutOfRange, malformed JSON → Malformed, missing report → NotValidated. EvalReportError closed variant set unchanged; EvalReport JSON schema ({"cases": [...], "accuracy": f64}) unchanged.
  - P4 (non-regression — model default): ProviderChoice::Fireworks.default_model() still returns accounts/fireworks/models/deepseek-v4-flash-0731; scripts/check-model-alignment.sh passes.
  - P5 (non-regression — CLI surface): blendtutor --help still lists exactly 9 subcommands incl. eval + eval-report (cli.rs PLANNED_SUBCOMMANDS, main.rs:134-166 exhaustive sum) — NO doctor added, NO eval-report folded into build, no subcommand removed/renamed.
  - P6 (non-regression — pinned tokens/harnesses): crates/cli/tests/readme.rs 9 tokens pass (incl. blendtutor eval, blendtutor eval-report); test_quarto_distribution.sh, test_smevals_runner.sh, test_judge_feedback.py pass; cutover.rs (evals/ untracked check) passes.
  - P7 (docs clarity): blendtutor eval --help output contains eval_ (sibling convention); README eval section + creating-lessons.md Step 7 + whole-game.md reference eval_ and never instruct users to run/consult evals/*.R as active. blendtutor new lesson claim at README:80 becomes TRUE (F1) rather than edited weaker.
  - R1 (auto-discovery regression pin): blendtutor eval lessons/tally.yaml --format json against provider-stub harness resolves suite without explicit suite path arg — exit 0, JSON body accuracy ∈ [0,1] (existing behavior at eval.rs:39, pinned, not rebuilt).
- probe:
  cargo test -p blendtutor-cli --test new && cargo test -p blendtutor-cli --test eval && cargo test -p blendtutor-cli --test eval_report_cli && cargo test -p blendtutor-cli --test readme --test cli --test cutover && cargo test -p blendtutor-core && bash scripts/check-model-alignment.sh && bash scripts/tests/test_smevals_runner.sh && python3 scripts/tests/test_judge_feedback.py && bash scripts/tests/test_quarto_distribution.sh && ! rg -n 'eval_fireworks_(vitals|sequential)\.R|eval_evaluate_with_llm' crates/ .github/ scripts/ docs/book/src README.md && ! grep -q '^/evals/$' .gitignore && ! rg -n 'uvx|smevals|SMEVALS_PIN' crates/cli/src/commands/eval.rs && rg -q 'smevals==0.2.0' crates/cli/src/commands/eval_report.rs && cargo run -q -p blendtutor-cli --bin blendtutor -- eval --help | grep -q 'eval_'
- negative:
  - Missing-sibling arm (regression pin): blendtutor eval lessons/tally.yaml with eval_tally.yaml absent → exit 1, stderr names resolved sibling path (eval.rs:40-41).
  - No-clobber arm: re-run blendtutor new lesson --lang python tally on existing course → nonzero AlreadyExists; pre-existing hand-edited eval_tally.yaml untouched (byte-identical); second format!("eval_...") naming rule hand-duplicated anywhere in scaffold.rs/new.rs = invariant violation (absence grep).
  - Prune-incomplete arm: any surviving invocation reference to deleted R scripts in crates/, .github/, scripts/, docs/book/src, README.md, or .gitignore:5 /evals/ entry dangling. NOTE: check-docs.sh/docs.yml "evals" tokens referring to committed docs/evals/ (smevals evidence) + Pages /evals/ mount are NOT dead-R-script references — absence grep patterns anchored to R filenames, not bare "evals".
  - Cheapest-broken arms: (a) prose-shuffle README passing presence greps but failing F1 file-existence; (b) .gitignore entry rm'd without doc cleanup → stale refs caught by P7 grep; (c) touching eval_summary_from_report_json to "simplify" → accuracy boundary weakened → P3 failure; (d) eval sibling added without no-clobber → user config silently clobbered → F2 failure.
- verification: code — cargo integration tests (new, eval, readme, cli, cutover) + shell grep arms (absence/presence) + shell harness scripts; manual residual: docs prose clarity — subjective, human-judged at review.
- fixture status: existing — crates/cli/tests/new.rs (add NEW red test new_scaffolds_a_sibling_eval_suite: tempdir + init + new, assert both paths + re-run non-clobber), crates/cli/tests/eval.rs (7 tests, covers R1 via crates/core/tests/fixtures/eval_command/eval_demo_lesson.yaml), crates/cli/tests/eval_report_cli.rs (fake uvx shim), crates/cli/tests/readme.rs:33-47 (9 tokens), crates/cli/tests/cli.rs (9 subcommands), crates/cli/tests/cutover.rs:75, crates/core/src/scaffold.rs:447 (the_scaffolded_lesson_eval_and_manifest_parse_with_production_parsers — sibling template must parse with production eval-suite parser; extend or mirror for eval_template), .gitignore:3-5 (tracked edit). NEW — eval_template pure fn + unit test in scaffold.rs; NEW red integration test in crates/cli/tests/new.rs.
- rubric anchor: §1.1/§1.3.1 (EvalSummary boundary + accuracy ∈ [0,1] closed validation; atomic create_new no-clobber — no check-then-write window), §2 (eval pure scoring vs eval-report effectful smevals orchestration stay unmerged), §3.1 (sibling convention single source in sibling_suite_path/eval_ prefix constant; prune cuts last joint between retired R package and active Rust CLI), §4 (cli thin shell; core::scaffold owns what new creates; module headers eval.rs vs eval_report.rs stay disjoint), §5.1 (one pure eval_template emitter mirroring lesson_template; no directory-scan fallback — deterministic one-lesson→one-suite naming).
- assertion-quality: observable-effect (file existence of lesson + eval sibling) · exact-equality (derived sibling path, mirrors sibling_suite_path unit tests mod.rs:44-48) · exit-code (no-clobber + missing-sibling negatives) · absence/structural grep (prune arms P1, second-naming-rule grep) — per docs/test-strategy/assertion-quality.md closed vocabulary.

### Design Intent
- Types (§1): No new types. Eval sibling = derived path (pure fn), never stored in manifest — drift unrepresentable; resist [eval] manifest config. EvalReportError closed two-variant failure set untouched. Refusal arms (from B): EvalParseError, EvalRunError::{Run,CaseOutOfRange}, EvalReportError::{Malformed,AccuracyOutOfRange}, GenError::{EmptySuite,NoRepoRoot} — none removed/weakened.
- Pure/effectful (§2): eval_template(Language, &str) -> String pure, mirroring lesson_template (:235-248); add_lesson stays effectful shell — extended to write both files through ONE write_without_clobber path (create_new fused, no check-then-write). parse_eval_suite/eval_summary_from_report_json pure; eval thin shell, eval-report thicker shell — folding eval-report into build rejected (couples build to external uvx smevals).
- Boundaries (§3): sibling naming convention lives in exactly one place — eval_ prefix constant consumed by sibling_suite_path (commands/mod.rs:32-37, doc-pinned :30-31); new scaffolding derives through it (or same constant if core/cli crate boundary requires), never second format! rule. Prune completes dead-R-package joint cut (cutover.rs:75 pins evals/ untracked). smevals stays separate optional integration.
- Module responsibility (§4): core::scaffold owns what new creates — add_lesson becomes "add a lesson and its grading harness" (header comment); README:80 claim becomes true. cli::commands::new stays boundary parser. eval.rs header unchanged, must not grow smevals orchestration. Docs name sibling convention; R scripts historical provenance only.
- Function discipline (§5): add_lesson one thing, no flag/branch on "with eval" — parity unconditional. sibling_suite_path unchanged, NO directory-scan fallback (multi-suite ambiguity). eval_template one pure emitter, not per-language family.

### Technical Context
- Files touched: crates/core/src/scaffold.rs (:235 region NEW eval_template fn + unit test; :346-361 add_lesson extended via write_without_clobber; :416-468 scaffold-plan tests no change needed), crates/cli/tests/new.rs (NEW red test), README.md eval section (~:105-116) + new-lesson claim (:80), docs/book/src/creating-lessons.md (Step 7 ~266-323, Step 9 ~325-360), docs/book/src/whole-game.md (:57-83), .gitignore:3-5, docs/agent-notes/eval.md (:43-58 may annotate "retired"; permitted). Local-only: rm -rf evals/.
- Files NOT touched (non-regression): site/mod.rs:69-78,221-254,392-415; eval.rs:296-318; provider.rs:58-63; commands/eval.rs (zero smevals refs preserved); eval_report.rs:35 (SMEVALS_PIN); commands/mod.rs:32-37; main.rs:134-166; cli.rs:6-16 (PLANNED_SUBCOMMANDS stays 9); readme.rs:33-47; scripts/check-model-alignment.sh; scripts/tests/test_{smevals_runner,judge_feedback,quarto_distribution}.*; scripts/smevals/ (whole tree).
- Architecture notes: planner's "doctor surface" does not exist — real command surface from main.rs:134-166 (exhaustive sum, 9 subcommands). Auto-discovery already shipped (sibling_suite_path, 4 callers, 3 unit tests) — AC reduces to scaffolding parity + prune + docs. EVAL_REPORT_FILE = "eval-report.json" (build.rs:14) untouched. Absence-grep scope: check-docs.sh + docs.yml contain "evals" tokens referring to committed docs/evals/ (smevals evidence) + Pages /evals/ mount — NOT dead R scripts; grep anchors on R filenames. Expected test migration: 6 files reference touched symbols — only new.rs + scaffold.rs in-module tests gain NEW tests; rest regression-pinned unedited.

### Dependencies
- Depends on: AC-3 (README slim), AC-4 (book slim) — wave sequencing only, no functional dependency
- Blocks: nothing functionally; final eval-docs wording
- Conflict set: README.md (HOT — serialized AC-2→AC-3→AC-6), docs/book/src/creating-lessons.md, docs/book/src/whole-game.md, crates/core/src/scaffold.rs, crates/cli/tests/new.rs, .gitignore
- Risk: low (one small effectful extension in well-tested scaffold module + docs/prune; all invariants already pinned)

### Pattern Detectors
- route/ref mismatch: planner named nonexistent "doctor surface" — spec re-derived from main.rs; resolved, no user input.
- dual-consumer pin: deepseek-v4-flash-0731 pinned by check-model-alignment.sh (Rust + JS) — unchanged, P4 verifies.
- sneaky-pass class (prune): PR cannot git rm untracked files — builder editing tracked config/docs but skipping local rm -rf evals/ passes CI yet leaves dead scripts. Mitigation: issue body documents local deletion; .gitignore removal + absence grep are tracked CI-checkable arms.
- archive-vs-active grep scope: absence patterns anchored to R filenames to avoid false-positives on docs/evals/ + Pages /evals/ references.

**Disagreement level: MODERATE** — load-bearing scaffold fact + prune mechanism diverged, resolved by codebase evidence; no user escalation.
**Friction:** planner named nonexistent doctor command — both speculators re-derived from main.rs. Candidate lesson: planner must list command surface from main.rs grep, not memory.

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open

## AC-7 — eval terminal interpretation

- Spec mode: **spec-resolved** (single render-only module, closed design space, invariants found by full-test grep below)
- Key files: `crates/cli/src/output.rs` (`render_eval` :378-402, `emit_eval` :412-420, mod tests :422+), `crates/core/src/eval.rs` (`CaseResult::feedback_message()` :283-286 — accessor ALREADY EXISTS, no core change), `crates/cli/src/snapshots/blendtutor__output__tests__eval_human_matches_snapshot.snap`, `crates/cli/tests/eval.rs`, `docs/book/src/whole-game.md` (eval section :57-83)
- Invariants/constraints:
  - `render_eval` is PURE (`&EvalReport -> String`) — footer text derives from case data only; do NOT thread the lesson path / file name into the render (would break the pure seam, §2).
  - JSON arm non-regression: `output.rs:412-416` — `--format json` shape (`cases`, `accuracy`, per-case `feedback_message` already serialized) byte-unchanged; the change is human-format only.
  - Exit-code non-regression: mismatch exits 0 (pinned `tests/eval.rs:279-296`); `eval` reports, never gates.
  - Fixture gap honesty: `CaseResult.score` captures feedback VERBATIM — AC-7 must NOT truncate, summarize, or reword it (user sees the same message the grader produced; first-line-only is acceptable IF the message is single-line; full verbatim is the default).
- Literal grep result (render-token pins — done at plan time per mandate):
  - `"Accuracy:"` + row format `case N: expected X, got Y [match|mismatch]`: `output.rs:383,389-398`, snapshot `.snap` (:5,:7,:9), `output.rs:539-541` test-comment describes fixture (two matches, one mismatch → 2/3).
  - `"[mismatch]"` exactly-one-count assertion: `crates/cli/tests/eval.rs:83` — the next-steps footer MUST NOT contain the literal `"[mismatch]"` (bracketed token) or that count breaks. Footer uses plain words ("did not match" / "mismatched cases: 3").
  - `evidence`: mock provider in `tests/eval.rs` drives alpha/beta/gamma cases (:27-28) with mixed verdicts — mismatched case's feedback string available for assertion.
- **Executable spec**
  - predicate:
    - F1 (mismatch detail): human render shows, immediately under each `[mismatch]` case row, the grader's verbatim `feedback_message` for that case (labeled, e.g. indented `grader: <message>`); `[match]` rows carry no feedback line.
    - F2 (next-steps footer): when ≥1 mismatch, output ends with a guidance footer that (a) names the 1-based numbers of the mismatched cases, (b) points the author at `blendtutor eval <lesson> --case N` to inspect a single case, and (c) names `llm_evaluation_prompt` (and the reference `solution`) as the knobs that shape grading. All-matched run (accuracy 1/1) emits NO footer and NO guidance lines.
    - P1 (row/headline non-regression): headline stays `Accuracy: m/t (p.p%)`; per-case rows stay `case N: expected X, got Y [match|mismatch]`; token set unchanged so `tests/eval.rs` pins (incl. exactly-one `[mismatch]`) stay green.
    - P2 (JSON non-regression): `--format json` output byte-identical to before (existing JSON assertions in `tests/eval.rs` pass unedited).
    - P3 (exit-code non-regression): mismatch still exits 0.
  - probe: `cargo test -p blendtutor-cli` (output.rs unit tests incl. insta snapshot re-review via `cargo insta` + integration `tests/eval.rs`) — snapshot MUST be regenerated+reviewed, not blind-accepted.
  - negative:
    - footer-present-on-full-match arm: all-matched suite emits footer → FAIL (proves footer is mismatch-driven, not unconditional).
    - missing-feedback arm: mismatched case rendered without its feedback message → FAIL (proves the detail is the mismatch's own, not a template).
    - wrong-case-attribution arm: feedback attached to a `[match]` row, or numbering off-by-one vs `case N` rows → FAIL.
    - JSON-drift arm: any byte change in `--format json` output → FAIL (P2).
  - fixture status: EXISTING — `output.rs` mod tests fixture (two matches + one mismatch shape, :539-541) extended with asserted feedback strings; snapshot `.snap` updated via insta review; `tests/eval.rs` mock already returns per-case verdict messages. No new harness.
  - rubric anchor: §2 (render stays pure over `EvalReport`; no lesson path threading), §3.4 (feedback text sourced from `CaseResult::feedback_message()`, the single captured source — never re-derived), §4 (output.rs owns all rendering; no command change), §5.1 (footer builder is one pure helper, not scattered format! calls).
  - assertion-quality: exact-equality (snapshot + row formats) · observable-effect (feedback present for mismatched case) · absence (no footer on full match, no `[mismatch]` literal in footer) · exit-code (P3) — per `docs/test-strategy/assertion-quality.md`.
  - design intent (§2/§3/§4/§5): render_eval gains a pure `feedback line per mismatch` + pure `next_steps_footer(&[case_numbers])`; everything keyed off `EvalReport`'s own data; docs note in whole-game evals section minimal (one short paragraph + example), editing the region AC-6 finalized.

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open

## AC-8 — durable smevals-free eval report artifact

**Verdicts (in-spec resolutions):**
1. Overwrite → WARN-and-proceed (B wins). Re-running eval on own course is the common case; refuse forces manual rm every iteration = friction; --force = bloat. Committed-artifact protection comes from git, not exit 1. Warning MUST name path + word "overwrit" (stderr).
2. Partial report → REFUSE (A wins). --case N --write-report exits 1 before any write: durable artifact flows to build → site renders as course-level accuracy; 1-case report is a misleading durable artifact. Error names --case + --write-report.
3. Shared constant → eval.rs-local const, doc-comment pointing to build.rs:14 as contract source. Extraction touches frozen build.rs/core boundary; mandatory build round-trip test (P8) makes duplication safe — drift fails the round-trip.
4. Shape → FULL {cases, accuracy} via serde_json::to_string(&report) — byte-identical to --format json stdout path (output.rs:415). Single serialization path, zero drift. Committed examples' minimal shape = hand-written shortcut, not canonical; consumer reads accuracy only, both parse.

## AC spec: `eval --write-report` persists build-consumable full-shape eval-report.json at course root, smevals-free

### Executable Spec
- predicate: Given a tempdir course (3-line blendtutor.toml marker + demo_lesson.yaml + eval_demo_lesson.yaml copied from crates/core/tests/fixtures/eval_command/) and a wiremock stub mounting three scripted verdicts (alpha→correct, beta→incorrect, gamma→incorrect — crates/cli/tests/eval.rs:29-33 harness), when `blendtutor eval <lesson> --write-report` runs from a CWD that is NOT the course root, with default human format, then ALL of:
  1. exit code == 0
  2. stdout still contains the human accuracy line (2/3) — --write-report is format-independent, terminal render unchanged (no silent no-op gated on --format json)
  3. <course_root>/eval-report.json exists AND does NOT exist at CWD (course root = nearest blendtutor.toml ancestor via course_root_for, smevals_gen.rs:361 — catches write-to-CWD sneaky-pass)
  4. file parses as one JSON doc; doc["cases"].as_array().len() == 3 (non-empty, full-case count — catches partial/empty-cases write); doc["accuracy"].as_f64() == Some(2.0/3.0) (number in [0,1], not string; catches placeholder/stale content)
  5. file bytes byte-identical to serde_json::to_string(&report) — same serialization path as --format json stdout (output.rs:414-415); running with --format json --write-report yields file content == stdout JSON (no second serialization path to drift)
  6. confirmation emitted: stdout or stderr contains the string eval-report.json on success (catches silent-write sneaky-pass)
  7. blendtutor eval --help contains --write-report; subcommand surface stays 9 (AC-6 P5 — flag on existing Eval variant, not a new subcommand)
  8. bidirectional contract (mandatory): blendtutor build --target webr <course_root> -o <out> succeeds on the same tempdir course AND <out>/eval-results.html contains the accuracy percentage derived from the written file (write side + read side agree; catches wrong-schema write that eval exits 0 on but build rejects — schema pinned by eval_summary_from_report_json, site/mod.rs:392)
- probe:
  cargo test -p blendtutor-cli --test eval eval_write_report
  cargo test -p blendtutor-cli --test eval eval_write_report_no_course_root
  cargo test -p blendtutor-cli --test eval eval_write_report_overwrite_warns
  cargo test -p blendtutor-cli --test eval eval_write_report_partial_refused
  cargo test -p blendtutor-cli --test eval eval_write_report_failure_propagates
  cargo test -p blendtutor-cli --test eval eval_write_report_build_roundtrip
  (Rscript-dependent, same skip-with-notice harness as existing eval tests, eval.rs:58-60; round-trip test pattern mirrors crates/cli/tests/build.rs:1292-1303 tempdir course)
- negative:
  - N1 no course root: lesson in dir with no blendtutor.toml ancestor (course_root_for → None) → exit 1, stderr names missing course root, NO file written anywhere (catches silent-no-op; check runs BEFORE any scoring side effect)
  - N2 overwrite: pre-create <course_root>/eval-report.json with sentinel bytes → exit 0 (proceeds), stderr contains "overwrit" + names the path, file replaced with new full-shape content (catches silent-overwrite; warn-and-proceed verdict)
  - N3 partial report refused: --case 1 --write-report → exit 1, stderr names --case and --write-report as incompatible, NO file written (single-case accuracy rendered as course-level by build = misleading durable artifact)
  - N4 write failure propagates: <course_root> read-only (chmod 0o555; skip-with-notice when running as root where chmod is ineffective) → exit 1, stderr names write error, no partial/leftover .tmp artifact (catches swallowed-error sneaky-pass)
- verification: code — cargo integration tests in crates/cli/tests/eval.rs (built binary, real course-root walk, real file I/O, real build consumer)
- fixture status: NEW — tempdir setup inside crates/cli/tests/eval.rs (copy two existing fixture files from crates/core/tests/fixtures/eval_command/ + write 3-line blendtutor.toml marker; course_root_for only checks manifest existence, smevals_gen.rs:364). Existing eval_command/ fixture dir has NO blendtutor.toml (glob-verified) — reusing as-is would hit N1. No new fixture tree.
- rubric anchor: §1.3.1 (refusal before effects — no-course/partial checks precede any write; range-checked consumer at read boundary), §2.3 (effectful shell around pure core — run_eval→EvalReport stays pure; write is the single effectful step after emit_eval, mirroring build's plan_site/write_site split), §3.2 (consume as-is — artifact schema frozen, consumer reads accuracy only), §5.1 (thin orchestration — write logic its own small fn, testable without patches)
- assertion-quality: behavioral-integration with value-equality core (built binary → file-system state → parse → exact accuracy/cases assertions → build round-trip; structural, no implementation-mirroring). ⚠️ Pattern flag (both proposers): docs/test-strategy/assertion-quality.md does NOT exist in this repo (glob empty) — classification uses role-prompt-implied vocabulary; Director should reconcile doc path or drop the pointer from spec prompts.

### Design Intent
- Types (§1): no new types — EvalReport (Serialize-only, eval.rs:295-299) is the typed artifact; --write-report is a plain bool clap flag (not enum); course_root_for returns Option<PathBuf> — None = "no course" refusal state; overwrite/partial refusal keeps "silently clobbered artifact" + "1-case report masquerading as course accuracy" unrepresentable as success.
- Pure/effectful (§2): run_eval stays pure-ish core producing EvalReport; the write is exactly one effectful step AFTER scoring + emit_eval, never interleaved with scoring. Single serialization path (serde_json::to_string(&report)) shared with --format json stdout. crates/core untouched.
- Boundaries (§3): course root = nearest blendtutor.toml ancestor — existing domain joint (course_root_for, shared with eval_report.rs), no new walker invented. Flag lives on eval (AC-6 P5 forbids new subcommand). Schema consumed as-is by build.
- Module responsibility (§4): eval.rs owns "measure + optionally persist"; build.rs stays consumer-only; no new module. eval.rs doc comment (:29-31, "measures, it is not a gate") MUST be updated to name the new side effect + eval-report.json filename.
- Function discipline (§5): run() gains one bool param; write step is its own fn write_report_artifact(report, course_root) -> anyhow::Result<PathBuf> returning the written path so run() prints the confirmation. Filename const local to eval.rs with doc-comment pointer to build.rs:14 as contract source (drift caught by round-trip test).

### Technical Context
- Files touched: crates/cli/src/commands/eval.rs:29-58 (flag param, canonicalize lesson_path BEFORE course_root_for — mirrors eval_report.rs:56; refusal checks; write step; updated doc comment), crates/cli/src/main.rs:71-81 + 143-147 (#[arg(long)] write_report: bool on Eval variant + dispatch), crates/cli/tests/eval.rs (new tests: positive + N1-N4 negatives + round-trip; 0 existing tests change — clap bool defaults false), docs/book/src/whole-game.md:44-80 (eval region: --write-report becomes the native durable path, replaces manual --format json > redirect), docs/book/src/creating-lessons.md:319-332 (same region), README.md (optional one-line mention — builder discretion, not spec-pinned)
- Files NOT touched (frozen): crates/cli/src/commands/build.rs (consumer, EVAL_REPORT_FILE:14 + load fn — see shared-const verdict), crates/core/src/eval.rs (EvalReport Serialize shape frozen), crates/core/src/site/mod.rs:389-415 (eval_summary_from_report_json consumer frozen), crates/cli/src/output.rs:26-33 (OutputFormat — NO new variant), crates/cli/src/commands/eval_report.rs (smevals path stays uvx-gated, unchanged), scripts/eval-course.sh + scripts/tests/eval-course.sh (smevals-path writers, untouched)
- Implementation requirements (in-spec, non-negotiable):
  - Canonicalize first: canonicalize lesson_path before course_root_for — relative path from non-course-root CWD walks relative ancestors and misses the manifest (same bug eval_report.rs:56 fixed).
  - Atomic write: write <course_root>/.eval-report.json.tmp then std::fs::rename; on Windows rename fails if target exists — remove target first (small non-atomic window, acceptable for dev tool; same remove-then-rename pattern as eval_report.rs replace_dir :206-214).
  - Confirmation: success message names eval-report.json and the resolved course-root path (stdout or stderr; tests accept either).
  - --format interaction: file ALWAYS full-JSON regardless of --format; --format controls stdout only.
  - Shape note: committed examples/write-less-code-*/eval-report.json use minimal {"accuracy": 1} — hand-written shortcut, NOT canonical; consumer accepts both. Overwrite warning (not refusal) lets users regenerate example-course artifacts deliberately.
- Expected test migration: 1 file extended (crates/cli/tests/eval.rs), 0 existing tests changed. commands::eval::run signature gains bool param; main.rs dispatch mechanical. Literal grep done (A): 63 repo-wide eval-report.json matches all accounted (consumer + smevals path + docs); no existing test asserts "eval does not write files".

### Dependencies
- Depends on: AC-6 (9-subcommand surface pinned, P5), AC-7 (serialized on crates/cli/tests/eval.rs + whole-game.md eval region)
- Blocks: nothing downstream (AC-8, wave-2 tail)
- Conflict set: crates/cli/tests/eval.rs + crates/cli/src/commands/eval.rs + main.rs Eval arm (shared with AC-7), whole-game.md eval region (shared with AC-7) — serialize AC-7 → AC-8
- Risk: medium

### Pattern Detectors
- Bidirectional-contract flag: TRIGGERED — writer (eval --write-report) + reader (build → eval_summary_from_report_json). Predicate #8 (build round-trip) MANDATORY; without it a wrong-schema write passes write-side tests but build fails. Round-trip test lives in eval.rs (writer is subject); if builder moves it to build.rs, no duplicate write-side setup.
- Dual-consumer field flag: TRIGGERED → RESOLVED — artifact flows to build (reads accuracy only) + user (inspects/diffs/commits). Verdict: FULL shape; minimal-shape committed examples are hand-written shortcuts; overwrite warning (not refusal) permits deliberate regeneration.
- Refusal-arm enumeration: all four arms have explicit tests (N1 no-course, N3 partial, N4 write-failure exit-1; N2 overwrite = warn-not-refuse, tested for warning content + replacement).
- Doc-pointer flag: docs/test-strategy/assertion-quality.md missing from repo — both proposers flagged. Needs Director/user reconciliation. Does NOT block this AC.

**Disagreement level: moderate** (divergence on overwrite policy + shared const + refusal-arm enumeration; converged on surface, shape, fixture, verification medium). All needs-clarification resolved in-spec.

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open
