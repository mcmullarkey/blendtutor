# Decomposition: user-friendliness

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

## Raw Prompt

"Ok, we need to do a series of things to make blendtutor more user-friendly 1. We need to use Github releases to make it easy to install the Rust binary using a shell script a la how people can install uv 2. We need to *massively* slim down documentation + instructions. Right now this reads as a bunch of slop instead of clear, succinct, and to-the-point. The closest we have to good is "The Whole Game" chapter in the docs book, but everything else is way too much. We can also get rid of demo/ since we already have example sites + the demo book. Overall, everywhere in docs, we need more succinctness + less fluff. 3. The evals process is also still not really intuitive. I like using smevals, but the nice thing about everything else is that we have one .yaml file that can feed either a website, a quarto extension, etc. smevals can create a nice suite, not asking to completely move off it, but we need to make the evals a more intuitive, easy-to-undestand once they exist, part of the flow. I'm open to ideas on how to do this, but again want to lean toward simple + streamlined vs. bloat"

## AC Table

| AC | Description | Dependencies | Conflict Set | Risk |
|----|-------------|--------------|--------------|------|
| 1  | Release workflow: `.github/workflows/release.yml` — tag-triggered matrix build (linux x86_64/aarch64, macos x86_64/aarch64) producing versioned tarballs + SHA256 checksums as GitHub release assets | none | `.github/workflows/release.yml`, `Cargo.toml` (metadata, maybe) | med |
| 2  | `scripts/install.sh` — uv-style `curl -LsSf …/install.sh \| sh` installer: OS/arch detection, latest-release resolution, tarball fetch + checksum verify, install to `~/.local/bin` (overridable), PATH hint. Plus stub-based shell BDD test | AC-1 (asset URL/naming contract) | `scripts/install.sh`, `scripts/tests/test_install_sh.sh`, `README.md` (install section) | med |
| 3  | README slim: rewrite README.md (373 lines → ~120) to the whole-game concision bar — install-first flow, minimal command reference, links to docs book for depth | AC-2 (install section must match installer) | `README.md` | low |
| 4  | Book slim: cut `creating-lessons.md` (458 → ~150 lines) and audit `introduction.md`/`examples.md`; `whole-game.md` stays as the quality bar; SUMMARY.md only if chapters change | none | `docs/book/src/*.md`, `docs/book/src/SUMMARY.md` | low |
| 5  | Delete demo-standalone: remove `demo-standalone/`, its docs.yml assembly steps + `/demo/` nest, the verify-live job (deleted per user decision), ci.yml demo-render test invocation, `scripts/fix-demo-coi-scope.sh`, and pins in `test_demo_standalone_render.sh` / `test_docs_pages_artifact.sh` / pages-live probe suite | none (but many files) | `demo-standalone/`, `.github/workflows/{docs.yml,ci.yml}`, `scripts/{fix-demo-coi-scope.sh,check-docs.sh}`, `scripts/tests/`, `rodney-probes/pages-live*.js` | med-high |
| 6  | Eval UX redesign: make native `blendtutor eval` / `eval-report` the obvious first-class path (auto-discovery of `eval_<slug>.yaml`, clear command surface + docs), prune legacy `evals/*.R` root scripts, keep smevals as optional integration | AC-3, AC-4 (docs conflicts) | `crates/cli/src/commands/`, `crates/core/src/eval.rs`, `README.md`, `docs/book/src/*.md`, `evals/`, `scripts/smevals/` | high |
| 7  | Eval terminal interpretation: human output shows the grader's verbatim feedback per mismatched case + a next-steps footer naming failed cases and pointing at `--case N` / `llm_evaluation_prompt`; JSON shape and exit codes unchanged | AC-6 (docs + tests/eval.rs serialization; eval command surface finalized) | `crates/cli/src/output.rs`, `crates/cli/src/snapshots/*eval_human*.snap`, `crates/cli/tests/eval.rs`, `docs/book/src/whole-game.md` | low |
| 8  | Durable smevals-free eval report artifact: native path to persist an eval result a course (and `build`) can consume — candidate directions: a `--write-report` flag writing `eval-report.json` to course root, or equivalent minimal surface. NO new subcommand, NO new OutputFormat variant | AC-6, AC-7 (serialization + floor of docs/output) | `crates/cli/src/commands/eval.rs`, `crates/cli/tests/eval.rs`, `README.md`, `docs/book/src/{whole-game,creating-lessons}.md` | med |

## AC Spec Context

### AC-1 — release workflow
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

### AC-2 — install.sh
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

### AC-3 — README slim
- Spec mode: spec-resolved
- Key files: `README.md`
- Invariants/constraints:
  - Quality bar = `docs/book/src/whole-game.md` tone: one idea per section, commands first, prose subordinate.
  - Hard pins (must survive): `quarto add mcmullarkey/blendtutor` install command, BYOK mention, min-Quarto requirement, demo link — `scripts/tests/test_quarto_distribution.sh` asserts a 15-clause README predicate; grep its exact clauses before rewriting and keep every pinned string.
  - Ceiling: ≤ ~130 lines (from 373). Install section leads with the `curl | sh` one-liner; `cargo install --path crates/cli` becomes the fallback.
- Prior art: whole-game.md structure.
- Verification: manual (succinctness judgment) + code (`test_quarto_distribution.sh` pins stay green).
- Test seam: existing `test_quarto_distribution.sh` (grep it verbatim first — it is the structural contract).

### AC-4 — book slim
- Spec mode: spec-resolved
- Key files: `docs/book/src/creating-lessons.md` (458 → ≤150 lines), `docs/book/src/introduction.md`, `docs/book/src/examples.md`, `docs/book/src/SUMMARY.md`, `docs/book/src/api-reference.md`
- Invariants/constraints:
  - Do NOT touch `whole-game.md` (the bar), `docs/adr/*` (historical records), `docs/agent-notes/*` (agent-facing, not user docs), `docs/evidence/*` (historical).
  - Move detail OUT, don't delete info that has no other home: architecture/API depth belongs to rustdoc (`/api`) — link, don't inline.
  - `scripts/check-docs.sh` + `test_docs_pages_artifact.sh` build the mdBook — any SUMMARY.md change must keep `mdbook build docs/book` green.
- Prior art: `whole-game.md`.
- Verification: manual + code (mdbook build green in CI).
- Test seam: existing docs build (`scripts/check-docs.sh`).

### AC-5 — demo-standalone removal
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

### AC-6 — eval UX redesign
- Spec mode: **needs-speculation** (user explicitly "open to ideas"; core CLI change, high risk)
- Key files: `crates/cli/src/commands/` (eval/eval-report/doctor surface), `crates/core/src/eval.rs` (`run_eval`, `EvalReport`, `select_case_index`), `crates/core/src/site/mod.rs` (`eval_summary_from_report_json`, `EvalSummary`), `evals/` root legacy R scripts (`eval_evaluate_with_llm_fireworks.R`, `eval_fireworks_sequential.R`, `eval_fireworks_vitals.R` — prune candidates), `scripts/smevals/{run.sh,check_polarity.sh,judge_feedback.py}`, `README.md`, `docs/book/src/creating-lessons.md` + `whole-game.md`
- Invariants/constraints:
  - Course model already scaffolds sibling `eval_<name>.yaml` via `blendtutor new` (README:86) — auto-discovery of that sibling is the natural simplification.
  - `EvalSummary`/`eval_summary_from_report_json` contract: missing report = NotValidated; present-but-malformed = fail loudly. Accuracy ∈ [0,1] validated at boundary. Do not weaken.
  - smevals stays as OPTIONAL integration (`run.sh` contract pinned by `test_smevals_runner.sh` + `check_polarity.sh` BDD — flags in ci.yml); native path must not require it.
  - Options space for speculators: (a) `blendtutor eval` auto-discovers sibling suite (no path arg), (b) `eval-report` folded into `build` or `validate`-adjacent flow, (c) `blendtutor doctor`-style single "is my course + evals healthy" command, (d) prune root `evals/*.R` legacy scripts. Speculators should weigh minimal vs adversarial; resolver picks.
- Prior art: smevals task-suite idea; existing `examples/write-less-code-*` eval_*.yaml pairing; uv's single-command ergonomics ethos.
- Verification: code (CLI behavior + JSON contract; integration via existing cli tests harness in `crates/cli/tests/`).
- Test seam: existing provider stub harness (`base_url_override` pattern in `crates/core/src/eval.rs` + cli tests).
- Literal grep requirement: eval commands reference provider defaults (`deepseek-v4-flash` dual-consumer — `scripts/check-model-alignment.sh` pins Rust+JS). If speculation changes any model/provider default in eval flow, full-test grep across ALL of `crates/*/tests/` + `scripts/tests/` for the literal is MANDATORY in the spec (retros: fireworks-model-transition, patient-model-swap).

### AC-7 — eval terminal interpretation
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

### AC-8 — durable smevals-free eval report artifact
- Spec mode: **needs-speculation** (command-surface design ambiguity interacts with AC-6 P5's pinned 9-subcommand surface + frozen OutputFormat enum + real doctrine tension on side-effects)
- Key files: `crates/cli/src/commands/eval.rs` (:33-58 — currently side-effect-free), `crates/cli/src/commands/build.rs` (`EVAL_REPORT_FILE` :14, `load_eval_summary` :101-108 — CONSUMER, contract must not change), `crates/core/src/eval.rs` (`EvalReport` Serialize :295), `crates/core/src/site/mod.rs` (`eval_summary_from_report_json` :392-415 — untouched boundary), `crates/cli/tests/eval.rs`, `README.md`, `docs/book/src/whole-game.md` + `creating-lessons.md` (Steps 7-9 + build step)
- Invariants/constraints:
  - AC-6 P5 pins exactly 9 subcommands — a NEW subcommand is FORBIDDEN; this AC is a flag on `eval` or nothing.
  - Frozen `OutputFormat` enum (`Human|Json` variants, exhaustive match discipline output.rs:24-33) — a third format variant forces every renderer (`validate`/`list`/`run`/`eval`) to handle or refuse it; near-certain reject (bloat). Speculators must price this against a `--write-report` flag.
  - Today `eval` is side-effect-free ("measures, it is not a gate" — eval.rs:29-31); default-writing `eval-report.json` on every run is a doctrine reversal — candidate, but speculators must weigh surprise-overwrite of a committed artifact vs the current manual `--format json >` redirect (whole-game/build rely on manual placement today).
  - `eval-report.json` schema (`{cases, accuracy}`, accuracy ∈ [0,1], EvalReportError boundary) UNCHANGED — build consumer and site fold are frozen contracts (AC-6 P3).
  - smevals stays optional (AC-6 P2); no uv/uvx involvement in the native artifact path.
  - UX gap this closes (evidence for speculators): `eval-report` today requires uvx + smevals + a git repo root (`repo_root_for`, eval_report.rs:219-228 — a binary-installed user authoring a plain course CANNOT get any durable report through it); `build` folds accuracy only if `eval-report.json` was manually redirected to course root (build.rs:101-108).
- Prior art: `cargo test --report`-free simplicity ethos; uv single-command ergonomics; examples/write-less-code-*/ committed `eval-report.json` convention (whole-game, creating-lessons :381,452,455).
- Options for speculators (weigh minimal vs adversarial): (a) `blendtutor eval --write-report` writing the exact `--format json` artifact to `<course-root>/eval-report.json` (atomic write, refuses or warns on overwrite), (b) write by default, (c) do nothing code-side and make the redirect workflow first-class in docs only, (d) minimal markdown summary file. Resolver picks; reject ANY option growing OutputFormat or the subcommand surface.
- Verification: code (CLI flag behavior; artifact round-trips through `eval_summary_from_report_json`; integration via existing `tests/eval.rs` provider-stub harness).
- Test seam: existing provider-stub harness in `crates/cli/tests/eval.rs`; course-root walk pattern already in `eval_report.rs` (`course_root_for`) — reuse, no new fixture tree.
- Literal grep requirement: NO model/provider defaults touched by scope. If speculation lands a flag name/stem collision check, grep `--format json` + `eval-report.json` across `crates/cli/tests/`, `scripts/tests/`, `docs/book/src/` before spec-compile (build.rs:14 + whole-game + creating-lessons pin the filename today).


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

## Suggested Batch Schedule

- Batch 1 (parallel): AC-1, AC-4, AC-5*  *(AC-5's README link edit deferred to AC-3's batch to avoid README race)
- Batch 2 (after AC-1): AC-2
- Batch 3 (after AC-2, parallel): AC-3 (+ AC-5's deferred README demo-link swap folded in)
- Batch 4 (wave 2, after AC-3 + AC-4): AC-6 (spec → build)
- Batch 5 (wave 2, after AC-6): AC-7 (spec-resolved — straight to build)
- Batch 6 (wave 2, after AC-7): AC-8 (speculate → resolve → build)

## Wave Plan

- Wave 1 (phase-1): AC-1, AC-2, AC-3, AC-4, AC-5 — ships one-line install + slim docs + demo-standalone removal. Revertable per-AC (release workflow, installer, README, book, demo removal are each independently mergeable).
- Wave 2 (phase-2): AC-6, AC-7, AC-8 — eval UX redesign + user-side interpretation surface (terminal mismatch detail + durable smevals-free artifact). Sequenced after docs are slim so eval docs land on the new concise surfaces; AC-6 → AC-7 → AC-8 serialized on `tests/eval.rs` + whole-game.md eval region. Together they make the native eval loop run-AND-interpretable with no external Python toolchain.

## Open Questions

- [resolved] demo/ = demo-standalone/ deletion; demo-book/ kept (user confirmed).
- [resolved] verify-live DELETED outright, not repointed (user decision). pages-live suite fate: delete if sole consumer is verify-live; else retarget to /demo-book/.
- [resolved] Windows installer out of scope (user confirmed); linux+macOS only.
- [resolved] evals/*.R legacy scripts pruned in AC-6 if verified dead (user confirmed; speculators must verify call sites).
