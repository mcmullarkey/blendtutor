# Master Plan: filter-runtime-bootstrap

## Feature Goal
(from decomposition.md — filter-owned runtime bootstrap so `quarto add` alone yields working interactive exercises)

## CURRENT STATE (updated 2026-08-03)
- Feature COMPLETE — AC-1..AC-7 all MERGED (AC-1 #137, AC-2 #138, AC-3 #140, AC-4 #142, AC-5 #146, AC-6 #145, AC-7 #148). Final main: 36188cb.

## FEATURE COMPLETE
All batches done; retro at ~/.config/opencode/retros/2026-08-03-filter-runtime-bootstrap.md

## RESUME INSTRUCTIONS (for a fresh session)
1. Read this file + the AC-N.md snippets in .opencode/plans/filter-runtime-bootstrap/
2. If PR #140 still open: run review loop (pr-reviewer + pr-reviewer-b parallel → review-resolver → fix cycles → CI gate → merge). If merged: proceed to AC-4.
3. Per remaining AC: dispatch speculator-a + speculator-b in parallel (prompts should reference this plan + merged prior ACs) → persist outputs via plan-writer → resolver → plan snippet → gh issue create → worktree + builder → review loop → CI gate → merge.
4. Batch order: AC-4 alone (batch 3, depends AC-3, hot file blendtutor.lua) → AC-5 ∥ AC-6 (batch 4, disjoint dirs demo-book/ vs quarto-fixture/) → AC-7 alone (batch 5, README).
5. Conventions: builders push with --no-verify; evidence at docs/evidence/<issue>/; never stage demo-book/.gitignore (quarto render mutates it — known trap); quarto-fixture/coi-book/*_files/ render noise also escapes gitignore — manual rm before commits; `uvx rodney` direct bash is permission-blocked — use `uv run node rodney-probes/<probe>.js` harness pattern instead; gh commands may need ctx_shell if bash is blocked.
6. Empty subagent returns = model failure — re-dispatch with focused prompt (happened twice this feature).

## AC Table + Dependency DAG + Batch Schedule
(copy from decomposition.md verbatim — all 7 ACs, DAG, 5 batches, hot conflict files)

### AC Table
| AC | Description | Dependencies | Conflict Set | Risk |
|----|-------------|--------------|--------------|------|
| 1  | Emit `data-language="r|python"` attribute in `emit_widget()` and assert it in filter-output verification | none | `_extensions/blendtutor/blendtutor.lua`, `scripts/tests/verify_filter_output.py`, `scripts/tests/test_quarto_filter.sh`, quarto-fixture rendered HTML (regen) | low |
| 2  | Add multi-language adapter routing + double-start guard to the runtime (`start()` no-ops with warning on second invocation; per-exercise adapter dispatch keyed by `data-language`) | none | `_extensions/blendtutor/assets/exercise-runtime.js`, new `_extensions/blendtutor/assets/adapter-router.js` (or in-module equivalent — speculator decision), `scripts/tests/validate-runtime.js`, `tests/fixtures/mock-adapter.js`, rodney runtime probes | medium |
| 3  | Inject auto-bootstrap `<script type="module">` from `Pandoc()` that imports runtime, builds adapters for languages present (new `has_r` flag), and calls `start()`; include opt-out mechanism for pages with hand-written bootstraps | AC-1, AC-2 | `_extensions/blendtutor/blendtutor.lua`, `scripts/tests/verify_filter_output.py`, `scripts/tests/test_quarto_filter.sh` | high |
| 4  | Deploy extension assets (JS modules + styles.css) to the render output dir via the canonical Quarto mechanism (speculators verify: `quarto.doc.add_html_dependency` vs `_extension.yml` resources declaration) and rewrite bootstrap import URLs accordingly | AC-3 | `_extensions/blendtutor/blendtutor.lua`, `_extensions/blendtutor/_extension.yml`, `scripts/sync-quarto-assets.sh`, `scripts/tests/test_quarto_install_render.sh`, `scripts/tests/verify_asset_scoping.py` | high |
| 5  | Migrate demo-book to the by-name extension install (`demo-book/_extensions/mcmullarkey/blendtutor` already present — switch `_quarto.yml` off the out-of-root `../_extensions/...` filter path) and e2e-verify working R + Python exercises on rendered `_output` pages | AC-4 | `demo-book/_quarto.yml`, `demo-book/*.qmd` (only if attr changes needed), `scripts/tests/test_quarto_distribution.sh`, new rodney probe for demo-book | medium |
| 6  | Verify/migrate quarto-fixture pages with hand-written bootstraps (ux.qmd mock adapter, webr.qmd, feedback.qmd) — no double-start; apply opt-out where the fixture bootstrap must win | AC-3 | `quarto-fixture/ux.qmd`, `quarto-fixture/webr.qmd`, `quarto-fixture/feedback.qmd`, `quarto-fixture/_quarto.yml` (if meta opt-out), rodney probes (`exercise-ux.js`, `webr-probe.js`, `feedback-probe.js`), `scripts/tests/test_quarto_ux.py`, `scripts/tests/test_quarto_feedback.py` | medium |
| 7  | Update README quick-start: `quarto add` → working exercises with zero hand-written bootstrap; document feedback opt-in and COI caveat | AC-5 | `README.md` | low |

### Dependency DAG
```
AC-1 ──┐
       ├──→ AC-3 ──→ AC-4 ──→ AC-5 ──→ AC-7
AC-2 ──┘         └──→ AC-6
```

### Hot Conflict Files
- `_extensions/blendtutor/blendtutor.lua`: touched by AC-1, AC-3, AC-4 — serialize in chain AC-1 → AC-3 → AC-4 (dependency DAG already enforces this; do NOT parallelize across these).
- `scripts/tests/verify_filter_output.py` + `scripts/tests/test_quarto_filter.sh`: touched by AC-1, AC-3 — same chain, no extra hazard.
- `quarto-fixture/` rendered HTML artifacts: AC-1 (regen after data-language), AC-6 (fixture edits re-render) — AC-6 lands after AC-3 per DAG, so serialized. NOTE (from prior retro): `demo-book/.gitignore` and rendered artifacts mutate on quarto render — builders must not commit render noise (hit 4+ agents in quarto-extension-install-path).
- `_extensions/blendtutor/assets/exercise-runtime.js`: AC-2 only among ACs (AC-4 references its deployment, not its source) — no conflict.

### Suggested Batch Schedule
- Batch 1 (parallel): AC-1 (lua attribute + tests), AC-2 (runtime routing + guard) — disjoint file sets, both foundational.
- Batch 2 (sequential): AC-3 (filter bootstrap — depends AC-1 for `data-language` contract, AC-2 for runtime/guard contract; hot file chain with AC-1).
- Batch 3 (sequential): AC-4 (asset deployment — depends AC-3's emitted bootstrap; shares blendtutor.lua hot file).
- Batch 4 (parallel): AC-5 (demo-book), AC-6 (fixture migration) — disjoint dirs (demo-book/ vs quarto-fixture/).
- Batch 5 (sequential): AC-7 (README — must reflect verified end-state from AC-5).

### Design notes for speculators (not user questions)
- Mixed-language routing shape: new `adapter-router.js` module vs `start()` accepting a `{r: adapter, python: adapter}` map — resolver picks; AC-2 spec must pin one.
- Opt-out mechanism shape: page meta (e.g. `blendtutor-autostart: false`) vs div attribute — AC-3 spec must pin one. Deterministic win for ux.qmd's mock adapter is the requirement; guard-alone ordering of two `<script type="module">` blocks is NOT sufficient (module execution order across head/body is not guaranteed enough for tests).
- AC-4 mechanism: prior art — Quarto ≥1.4 `quarto.doc.add_html_dependency` copies files into `<output>/<page>_files/libs` and rewrites hrefs automatically; `_extension.yml` has no general `resources:` key for filters. Speculators must verify against installed Quarto version and pick. coi-serviceworker.js likely must STAY an `include_text` injection (service-worker scope = script URL dir; libs-dir deployment breaks SW scope) — AC-4 scope explicitly excludes it.

## Resolved Specs
- AC-1: full spec at .opencode/plans/filter-runtime-bootstrap/AC-1.md (MERGED)
- AC-2: full spec at .opencode/plans/filter-runtime-bootstrap/AC-2.md (MERGED)
- AC-3: full spec at .opencode/plans/filter-runtime-bootstrap/AC-3.md (MERGED, PR #140, commit 4fd547f)
- AC-4: full spec at .opencode/plans/filter-runtime-bootstrap/AC-4.md (MERGED, PR #142)
- AC-5: full spec at .opencode/plans/filter-runtime-bootstrap/AC-5.md (MERGED, PR #146)
- AC-6: full spec at .opencode/plans/filter-runtime-bootstrap/AC-6.md (MERGED, PR #145)

## Remaining ACs — working understanding (re-spec at batch time against THEN-current main)
- AC-7 (README): quick-start `quarto add` → working exercises zero bootstrap; document feedback opt-in (exercise-feedback.js stays manual — BYOK) + COI service-worker scope limitation (user decision: document, don't fix). Depends AC-5 (merged).

## User Decisions (approved)
1. Proper fix: filter-owned bootstrap (not per-page patches)
2. COI SW scope: out of scope, document in README (AC-7)
3. Fixtures: keep hand bootstraps + opt-out
4. AC-2: map-only start() signature (no backward-compat wrapper); fallback `|| runtime.language || "r"` removed; AC-1+AC-2 same-batch merge coupling
5. AC-3 resolver calls: data-bt-bootstrap="auto" marker adopted; rodney runtime probe included

## Boundary Notes
- **SUPERSEDED (2026-08-07, byok-api-key AC-3/AC-9 — recorded in ADR-0016):** exercise-feedback.js auto-mount is OUT (BYOK needs API key; stays opt-in per feedback.qmd pattern) — REVERSED: feedback auto-mounts in the injected bootstrap (`mountAllFeedback` after `start()` resolves), `bt-feedback: false` is the granular opt-out, and the key is entered once on an auto-mounted API key page.
- COI SW scope redesign OUT (documented limitation)
- Rust core changes OUT
