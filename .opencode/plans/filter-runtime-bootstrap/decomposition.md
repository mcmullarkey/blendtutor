# Decomposition: filter-runtime-bootstrap

## Feature Goal
Make the Quarto extension self-contained: `blendtutor.lua` injects the runtime JS bootstrap itself so `quarto add mcmullarkey/blendtutor` alone yields working interactive exercises (CodeMirror editor, Run/Check/Hint buttons) with no hand-written per-page `<script type="module">`. Also fix the missing `data-language` attribute on emitted exercise divs (latent mixed-language bug: runtime falls back to `runtime.language || "r"`) and make extension assets (JS modules, CSS) land in the render output dir via the canonical Quarto mechanism (in-header relative hrefs are not rewritten/copied — verified ground truth #6).

## Scope boundaries (IN / OUT)
- IN: filter bootstrap emission, adapter selection per language (R→webr-adapter, Python→pyodide-adapter, mixed→routing), `data-language` fix, double-start guard, asset deployment to output dir, demo-book migration to by-name install, README quick-start.
- OUT: exercise-feedback.js auto-mount (BYOK feature, requires API key — stays opt-in per feedback.qmd pattern), COI service-worker scope redesign (pre-existing limitation, flagged in Open Questions), Rust core changes.

## AC Table
| AC | Description | Dependencies | Conflict Set | Risk |
|----|-------------|--------------|--------------|------|
| 1  | Emit `data-language="r|python"` attribute in `emit_widget()` and assert it in filter-output verification | none | `_extensions/blendtutor/blendtutor.lua`, `scripts/tests/verify_filter_output.py`, `scripts/tests/test_quarto_filter.sh`, quarto-fixture rendered HTML (regen) | low |
| 2  | Add multi-language adapter routing + double-start guard to the runtime (`start()` no-ops with warning on second invocation; per-exercise adapter dispatch keyed by `data-language`) | none | `_extensions/blendtutor/assets/exercise-runtime.js`, new `_extensions/blendtutor/assets/adapter-router.js` (or in-module equivalent — speculator decision), `scripts/tests/validate-runtime.js`, `tests/fixtures/mock-adapter.js`, rodney runtime probes | medium |
| 3  | Inject auto-bootstrap `<script type="module">` from `Pandoc()` that imports runtime, builds adapters for languages present (new `has_r` flag), and calls `start()`; include opt-out mechanism for pages with hand-written bootstraps | AC-1, AC-2 | `_extensions/blendtutor/blendtutor.lua`, `scripts/tests/verify_filter_output.py`, `scripts/tests/test_quarto_filter.sh` | high |
| 4  | Deploy extension assets (JS modules + styles.css) to the render output dir via the canonical Quarto mechanism (speculators verify: `quarto.doc.add_html_dependency` vs `_extension.yml` resources declaration) and rewrite bootstrap import URLs accordingly | AC-3 | `_extensions/blendtutor/blendtutor.lua`, `_extensions/blendtutor/_extension.yml`, `scripts/sync-quarto-assets.sh`, `scripts/tests/test_quarto_install_render.sh`, `scripts/tests/verify_asset_scoping.py` | high |
| 5  | Migrate demo-book to the by-name extension install (`demo-book/_extensions/mcmullarkey/blendtutor` already present — switch `_quarto.yml` off the out-of-root `../_extensions/...` filter path) and e2e-verify working R + Python exercises on rendered `_output` pages | AC-4 | `demo-book/_quarto.yml`, `demo-book/*.qmd` (only if attr changes needed), `scripts/tests/test_quarto_distribution.sh`, new rodney probe for demo-book | medium |
| 6  | Verify/migrate quarto-fixture pages with hand-written bootstraps (ux.qmd mock adapter, webr.qmd, feedback.qmd) — no double-start; apply opt-out where the fixture bootstrap must win | AC-3 | `quarto-fixture/ux.qmd`, `quarto-fixture/webr.qmd`, `quarto-fixture/feedback.qmd`, `quarto-fixture/_quarto.yml` (if meta opt-out), rodney probes (`exercise-ux.js`, `webr-probe.js`, `feedback-probe.js`), `scripts/tests/test_quarto_ux.py`, `scripts/tests/test_quarto_feedback.py` | medium |
| 7  | Update README quick-start: `quarto add` → working exercises with zero hand-written bootstrap; document feedback opt-in and COI caveat | AC-5 | `README.md` | low |

## Dependency DAG
```
AC-1 ──┐
       ├──→ AC-3 ──→ AC-4 ──→ AC-5 ──→ AC-7
AC-2 ──┘         └──→ AC-6
```

## Hot Conflict Files
- `_extensions/blendtutor/blendtutor.lua`: touched by AC-1, AC-3, AC-4 — serialize in chain AC-1 → AC-3 → AC-4 (dependency DAG already enforces this; do NOT parallelize across these).
- `scripts/tests/verify_filter_output.py` + `scripts/tests/test_quarto_filter.sh`: touched by AC-1, AC-3 — same chain, no extra hazard.
- `quarto-fixture/` rendered HTML artifacts: AC-1 (regen after data-language), AC-6 (fixture edits re-render) — AC-6 lands after AC-3 per DAG, so serialized. NOTE (from prior retro): `demo-book/.gitignore` and rendered artifacts mutate on quarto render — builders must not commit render noise (hit 4+ agents in quarto-extension-install-path).
- `_extensions/blendtutor/assets/exercise-runtime.js`: AC-2 only among ACs (AC-4 references its deployment, not its source) — no conflict.

## Suggested Batch Schedule
- Batch 1 (parallel): AC-1 (lua attribute + tests), AC-2 (runtime routing + guard) — disjoint file sets, both foundational.
- Batch 2 (sequential): AC-3 (filter bootstrap — depends AC-1 for `data-language` contract, AC-2 for runtime/guard contract; hot file chain with AC-1).
- Batch 3 (sequential): AC-4 (asset deployment — depends AC-3's emitted bootstrap; shares blendtutor.lua hot file).
- Batch 4 (parallel): AC-5 (demo-book), AC-6 (fixture migration) — disjoint dirs (demo-book/ vs quarto-fixture/).
- Batch 5 (sequential): AC-7 (README — must reflect verified end-state from AC-5).

## Design notes for speculators (not user questions)
- Mixed-language routing shape: new `adapter-router.js` module vs `start()` accepting a `{r: adapter, python: adapter}` map — resolver picks; AC-2 spec must pin one.
- Opt-out mechanism shape: page meta (e.g. `blendtutor-autostart: false`) vs div attribute — AC-3 spec must pin one. Deterministic win for ux.qmd's mock adapter is the requirement; guard-alone ordering of two `<script type="module">` blocks is NOT sufficient (module execution order across head/body is not guaranteed enough for tests).
- AC-4 mechanism: prior art — Quarto ≥1.4 `quarto.doc.add_html_dependency` copies files into `<output>/<page>_files/libs` and rewrites hrefs automatically; `_extension.yml` has no general `resources:` key for filters. Speculators must verify against installed Quarto version and pick. coi-serviceworker.js likely must STAY an `include_text` injection (service-worker scope = script URL dir; libs-dir deployment breaks SW scope) — AC-4 scope explicitly excludes it.

## Open Questions
- [needs-clarification] COI service-worker scope on deployed sites: coi-serviceworker.js injected from `_extensions/...` path means SW scope covers only that dir — pages sit outside scope, so SharedArrayBuffer/COI may not activate on real installs (pre-existing, flagged in prior feature AC-1). Options: (a) out of scope, document limitation in README (AC-7); (b) copy SW to output root alongside deployment (AC-4 grows). Default assumption if unanswered: (a).
- [needs-clarification] Fixture strategy for ux.qmd/webr.qmd/feedback.qmd: keep hand-written bootstraps + opt-out (AC-6 minimal diff, mock adapter guaranteed to win) vs delete bootstraps and let filter auto-start (loses mock adapter control for CI tests). Default assumption if unanswered: keep + opt-out, per constraint "must keep working OR be migrated".
- Bootstrap should NOT auto-mount exercise-feedback.js (BYOK needs user key; feedback.qmd mounts manually post-start). Flagging as boundary, not question — correct if wrong.
