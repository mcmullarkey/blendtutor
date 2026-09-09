---
ac: 6
depends_on: AC-3, AC-4
risk: low
status: complete
---

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

### Progress
- [x] RED suite committed (4a2c87d): `new_scaffolds_a_sibling_eval_suite` in crates/cli/tests/new.rs (behavioral RED — missing-sibling assertion) + 5 scaffold.rs in-module tests (compile RED on eval_template/eval_sibling_path) — 2026-09-09
- [x] feat committed (28a4ba9): EVAL_SIBLING_PREFIX + eval_sibling_path + eval_template in core::scaffold; add_lesson writes sibling via write_without_clobber; cli sibling_suite_path delegates to core (single source); negative control confirmed (bogus expected token fails 3 tests) — 2026-09-09
- [x] prune+docs committed (c595fc9): .gitignore /evals/ entry + comment removed; creating-lessons.md Step 7 + whole-game.md eval region name the sibling convention; agent-notes/eval.md annotated retired; local `rm -rf evals/` done in main checkout (untracked — PR cannot delete) — 2026-09-09
- [x] evidence committed (b6f166a): docs/evidence/228/ — e2e-new-lesson-sibling.log, e2e-eval-help-sibling-convention.log, e2e-missing-sibling-negative.log, prune-absence-greps.log, test-suite.log — 2026-09-09
- [x] Full gate green: new 4, eval 7, eval_report_cli 10, readme 2, cli 2, cutover 1, core 201, model-alignment OK, smevals-runner 12, judge-feedback 78, quarto-dist 9, demo-docs 5

### Decision Log
- Sibling convention single source lives in **core** (`core::scaffold::eval_sibling_path` + `EVAL_SIBLING_PREFIX`), not cli: dependency only points cli → core, so delegation (cli `sibling_suite_path` → core fn) is the only shape with exactly one implementation. Spec's "or same constant if core/cli crate boundary requires" resolved to the stronger full-delegation form.
- `add_lesson` write order: lesson first, then eval sibling, then manifest append. Duplicate-id refusal (the common case) still fires with zero writes; the rare pre-existing-sibling refusal leaves an unregistered lesson (documented residual window in add_lesson doc comment, same recoverable class as the manifest-append window).
- `AddLessonError::AlreadyExists` Display: "a lesson already exists" → "a file already exists" — accurate for both collision arms (lesson or sibling); no external pins on the old wording.
- eval_template emits ONE case (`expected: correct`) with the language's hello-world submission; the starter course's committed two-case suite remains the fuller example (per needs-clarification #2).
- README left untouched: post-slim line 40 already claims "add lessons/greet.yaml + eval_<name>.yaml sibling" — F1 makes the existing claim TRUE (spec: "becomes TRUE rather than edited weaker").

### Surprises & Discoveries
- The spec's probe greps `crates/` wholesale but its own P1 prose scopes `crates/*/src` with fixture headers exempt — the literal probe has exactly one hit, the archival provenance comment in `crates/core/tests/fixtures/evals/eval_fireworks_vitals.yaml:2` (`# evals/eval_fireworks_vitals.R (the eval_data tibble…)`). Followed the P1 prose scope (zero hits, PASS) and documented the exempt hit in docs/evidence/228/prune-absence-greps.log rather than editing the archival fixture header.
- `Language` is Clone-not-Copy, so `add_lesson`'s two template calls need one `language.clone()` (on the first call; the second consumes the original).
- Post-slim README already carried the sibling claim at line 40 (`eval_<name>.yaml`) — the pre-slim line numbers in the spec (~:80) had drifted; grep located it. No README edit required.
- `blendtutor eval --help` already names `eval_<lesson>.yaml` via clap doc comment (main.rs:72) — P7's help-grep arm passed with zero changes.

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open
