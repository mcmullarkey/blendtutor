---
ac: 8
depends_on: AC-4, AC-2, AC-3
risk: medium
status: complete
---

## AC-8: Exercise UX polish — hints toggle, solution reveal, button states

### Executable Spec
- **predicate:** 3 exercises. 7 sub-clauses: hints visible/absent, solution button click inserts text, check button absent when no checks, Run disables ex-0 only, cursor=not-allowed, data-status closed set, buttons re-enabled on pass.
- **probe:** rodney with ux.qmd
- **negative:** Run disables all exercises (singleton leak). Solution button for empty exercise.
- **verification:** rodney
- **fixture status:** ux.qmd (NEW)
- **rubric anchor:** §1.1, §1.2, §1.5, §2.1, §3.4, §3.5, §4.1, §5.1

### Design Intent
- §1: data-status closed enum; button disabled state as data attribute
- §2: state machine pure; DOM manipulation effectful
- §3: UX layer separate from runtime; hints/solution separate from feedback
- §4: exercise-runtime.js (shared) owns UX interactions
- §5: Each interaction handler does one thing; no cross-exercise effects

### Technical Context
- Files: quarto-fixture/ux.qmd (NEW), exercise-runtime.js (shared with AC-7), styles.css (shared with AC-3)
- Source styles.css: crates/core/assets/shared/styles.css (synced to _extensions via sync-quarto-assets.sh)
- exercise-runtime.js: wireExercise() creates controls, status, output. Needs hints <details>, solution button, check button conditional, Run disable/enable.
- data-status already exists as idle/running/pass/fail on bt-status element.
- Run button disable: entry-level (per-exercise), NOT module-level singleton.

### Dependencies
- Depends on: AC-4 (per-exercise registry), AC-2 (workspace styling), AC-3 (styles.css tokens)
- Conflict set: exercise-runtime.js (shared with AC-7), styles.css (shared with AC-3)
- ADR: ADR-0016

### Progress
- [x] RED: test_quarto_ux.py (7 clauses) — confirmed fail (2026-07-23)
- [x] Create ux.qmd fixture (3 exercises: full+hints+solution+checks, no-checks, empty) (2026-07-23)
- [x] Implement exercise-runtime.js — hints toggle, solution reveal, check button conditional, Run disable/enable (2026-07-23)
- [x] Modify source styles.css — cursor:not-allowed, bt-status data-status, bt-hints, bt-solution styles (2026-07-23)
- [x] Run sync-quarto-assets.sh to regenerate scoped styles.css (2026-07-23)
- [x] Write rodney probe script (rodney-probes/exercise-ux.js) (2026-07-23)
- [x] GREEN: 29 tests pass (source patterns + CSS + Node.js behavioral + qmd structure) (2026-07-23)
- [x] CI step added to .github/workflows/ci.yml (2026-07-23)
- [x] Evidence saved to docs/evidence/113/test-suite.log (2026-07-23)

### Decision Log
- 2026-07-23 — Check button calls runSubmission() (same as Run). The distinction is conditional rendering: Check button only appears when payload.checks.length > 0. Run button is always present. Both run code + checks via the adapter.
- 2026-07-23 — Run button disable/enable is per-exercise (entry-level runBtn.disabled), NOT module-level singleton. The negative case "Run disables all exercises" is prevented by the per-exercise registry pattern from AC-4.
- 2026-07-23 — Hints rendered as <details class="bt-hints"> before controls. Uses textContent (never parsed as HTML) — untrusted lesson content. Bullet-point parsing ported from lesson-runner-core.js.
- 2026-07-23 — Node.js behavioral test uses custom ESM loader to mock codemirror.js import. exercise-runtime.js imports from ./codemirror.js (vendored browser bundle) which can't run in Node without a DOM. The loader intercepts the import and returns a mock.

### Surprises & Discoveries
- The `edit` tool reported success but did not persist changes to exercise-runtime.js. Had to use `write` tool to overwrite the entire file. Root cause unknown — possibly a worktree filesystem issue. Lesson: always verify file changes with `wc -l` or `grep` after editing, not just trust the tool's success message.
