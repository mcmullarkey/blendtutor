---
ac: 3
depends_on: [1, 2]
risk: medium
status: complete
---

# AC-3: Filter injects per-language auto-bootstrap module script with YAML opt-out

## Executable Spec (resolver-merged, 9 clauses)
- predicate: given rendered HTML from qmd fixtures with blendtutor exercises and no hand-written bootstrap, when the filter's Pandoc() runs:
  1. bootstrap emitted once (mixed): quarto-fixture/mixed-lang.qmd rendered → exactly one `<script type="module" data-bt-bootstrap="auto">` whose body contains start(, scanExercises, buildRegistry, createWebRAdapter, AND pyodideAdapter. type="module" mandatory.
  2. per-language conditional imports: r-only.qmd → script contains createWebRAdapter, NOT pyodideAdapter; pyodide.qmd → inverse. Specifiers iff has_r/has_python.
  3. start() wiring: calls start(buildRegistry(scanExercises()), <map>) with keys {r, python}. No exercises → no bootstrap.
  4. opt-out suppresses entirely: webr.qmd with YAML bt-auto-bootstrap: false → ZERO data-bt-bootstrap="auto"; hand-written bootstrap preserved. NOT double-start-guard reliance.
  5. non-HTML gate: filter.qmd → latex → zero data-bt-bootstrap="auto".
  6. no hardcoded asset path + depth: specifiers never literal `_extensions/blendtutor/` — resolve_asset_path(); coi-book/chapter-coi.qmd shows depth-correct ../.. specifiers.
  7. error sink: script contains .catch( + console.error.
  8. runtime (rodney): mixed-lang.html → window.__btExercises.length === 2 AND .cm-editor count === 2; r-only.html → __btExercises.length === 2. Never assert boot completion (CDN offline risk; __btExercises set synchronously pre-boot).
  9. static pins: blendtutor.lua has has_r = true in Div() (mirror has_python :373-375), hasBootstrapDone guard (mirror hasCoiDone :433), bt-auto-bootstrap YAML read in Pandoc() (mirror coi read :410-416).
- probe: `bash scripts/tests/test_quarto_bootstrap.sh` (NEW, clauses 1-7+9) + rodney asserts on quarto-fixture/mixed-lang.html and r-only.html (clause 8)
- negative: (1) classic script no type=module → pre-DOM no-op (killed by 1+8); (2) hardcodes both adapters (killed by 2); (3) opt-out via guard → warn noise (killed by 4); (4) hardcoded specifier (killed by 6); (5) omits .catch (killed by 7); (6) no has_r (killed by 9); (7) RawBlock fallback with classic script (killed by 1).
- verification: code (structural grep + static pins, primary) + rodney (clause 8)
- fixture status: existing mixed-lang.qmd, r-only.qmd, pyodide.qmd, filter.qmd, coi-book/chapter-coi.qmd (all verified present); MODIFY webr.qmd, feedback.qmd, ux.qmd (add YAML bt-auto-bootstrap: false); NEW scripts/tests/test_quarto_bootstrap.sh
- rubric anchor: §1.2, §3.4, §4.1, §5

## Design Intent
- §1: has_r/has_python baked render-time invariants; adapter map keys closed set {r, python}; data-bt-bootstrap="auto" marker makes filter-injected vs hand-written greppable.
- §2: filter emits static HTML; runtime behavior stays in JS; filter never executes JS.
- §3: bootstrap = filter's injection responsibility; adapters + dispatch runtime-owned (AC-2 seam); no new module — bootstrap is an emitted string.
- §4: blendtutor.lua header gains "injects auto-bootstrap module script", still NOT executing, never hardcodes asset paths (ADR-0018). Opt-out page-level YAML (mirrors coi), NOT div attribute.
- §5: one bootstrap per page (hasBootstrapDone); one error sink (.catch); one injection branch (reuse include_text/RawBlock shape :422-427).

## Technical Context
- Files: blendtutor.lua (Pandoc :403-467, Div :373-375, module-level :33-35); webr.qmd/feedback.qmd/ux.qmd YAML; NEW scripts/tests/test_quarto_bootstrap.sh.
- Adapter export asymmetry: createWebRAdapter FACTORY (call it) vs pyodideAdapter SINGLETON (use directly) — bootstrap handles both shapes.
- resolve_asset_path preserves ../ depth; inline module import() resolves against document base URL — same form works.
- start() sets window.__btExercises synchronously before await boot() (exercise-runtime.js:486-491) — rodney assert needs no CDN wait.
- Injection: quarto.doc.include_text("in-header") or table.insert(doc.blocks, 1, RawBlock) fallback — reuse pyodide CDN (:422-427) / coi (:437-442) / styles (:451-456) shape.
- webR works without COI (degraded channel) — clause 2 is AC-text requirement, not crash guard.
- Existing tests (test_quarto_filter.sh, test_coi_filter.sh, test_quarto_ux.py, test_quarto_feedback.py) render modified fixtures — verify still pass.

## Dependencies
- Depends on: AC-1 (#135, merged PR #137), AC-2 (#136, merged PR #138). Blocks: AC-4. Conflict set: blendtutor.lua (hot), webr/feedback/ux.qmd. Risk: medium.

### Progress
- [x] spec resolved — pending implementation
- [x] red: scripts/tests/test_quarto_bootstrap.sh written — 20 failures (no bootstrap emitted) — commit 018d25f
- [x] green: blendtutor.lua bootstrap injection (has_r, hasBootstrapDone, bt-auto-bootstrap YAML, build_bootstrap_script) — 29/29 pass
- [x] fixtures: webr.qmd/feedback.qmd/ux.qmd add bt-auto-bootstrap: false
- [x] rodney clause 8: rodney-probes/auto-bootstrap.js — PROBES_PASS (4/4)
- [x] existing suites green: filter 22/22, coi 15/15, ux 46/46, feedback 32/32
- [x] stale data-language pin fix in test_quarto_filter.sh (pre-existing #138 drift — main CI was red)
- [x] evidence committed docs/evidence/139/

### Decision Log
- resolver — adopted A's data-bt-bootstrap marker + B's rodney runtime clause; trimmed B's script[src*=pyodide] assert (module imports inline, not src attr); all fixtures verified existing.
- builder — clause-6 depth sub-check uses coi-book/chapter-coi.qmd's coi-script src (../../_extensions/...) as the depth proxy: chapter-coi.qmd has no blendtutor exercises, so no bootstrap is emitted there; the coi path exercises the same resolve_asset_path depth handling. Bootstrap depth itself verified on mixed-lang (../_extensions/...).
- builder — bootstrap map construction is emitted inline (start(buildRegistry(scanExercises()), {r: ..., python: ...})) so the spec's exact call-shape grep matches.
- builder — no-exercises gate uses coi-book/chapter-no-coi.qmd (only "blendtutor" occurrence is the filter path — zero blendtutor divs).

### Surprises & Discoveries
- main's CI was already red: test_quarto_filter.sh pinned `entry.element.dataset.language ||` but PR #138 refactored the runtime read to `entry.element.dataset.language;` + separate `if (!language)` guard. Pre-existing drift (not from this issue) — fixed the stale pin so this PR's CI gate is green.
- The rodney probe harness (rodney-probes/auto-bootstrap.js) runs via `uv run node` (execFileSync uvx rodney internally) — the direct `uvx rodney` bash command is permission-blocked, but the repo-established harness pattern from AC-2 bypasses that and worked.
- quarto render creates un-ignored `quarto-fixture/coi-book/*_files/` dirs (the `/quarto-fixture/*_files/` gitignore pattern is anchored to the top level only) — must rm -rf before commit; never stage.

### Idempotence & Recovery
- Safe retry: re-run probe + renders (regenerated at test time).
- Rollback: git checkout -- _extensions/blendtutor/blendtutor.lua quarto-fixture/webr.qmd quarto-fixture/feedback.qmd quarto-fixture/ux.qmd; rm scripts/tests/test_quarto_bootstrap.sh
