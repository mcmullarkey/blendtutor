---
ac: 3
depends_on: AC-1, AC-2
risk: medium
status: complete
---

## AC spec: CM6 editor UX polish — line numbers, bracket match, smart tab, active line, spellcheck off, monospace

### Executable Spec
- **predicate:** 12 clauses, all must hold:
  1. **gutter not hidden (code):** `styles.css` contains NO rule setting `.cm-gutters`/`.cm-gutter` to `display:none`, `visibility:hidden`, `opacity:0`, or `font-size:0`, AND no `.cm-editor`/`.cm-gutters` ancestor rule with `overflow:hidden` clipping the gutter.
  2. **line numbers visible (rodney):** `.cm-gutters !== null` AND `getComputedStyle(.cm-gutters).display !== 'none'` AND `.cm-gutterElement !== null` AND `.cm-gutterElement.textContent.trim().length > 0`.
  3. **gutter font monospace (rodney):** `getComputedStyle(.cm-gutterElement).fontFamily` contains `"mono"`.
  4. **active line highlighting distinct (rodney):** `getComputedStyle(.cm-activeLine).backgroundColor` NOT equal to `getComputedStyle(.cm-content).backgroundColor` (transparent treated as equal to parent bg).
  5. **bracket match styled (code):** `styles.css` contains a CSS rule targeting `.cm-bracket-match` with at least one of `background`/`outline`/`box-shadow`/`border-color` set to a non-`transparent`/non-`inherit` value.
  6. **bracket match visible (rodney):** after injecting code containing brackets (e.g. `f(x)`) and placing cursor adjacent to a bracket, `.cm-bracket-match !== null`.
  7. **spellcheck disabled (rodney):** `.cm-content.getAttribute('spellcheck') === 'false'` OR `.cm-editor.getAttribute('spellcheck') === 'false'` (explicit `"false"` attribute — absent inherits `true` by browser default on contenteditable).
  8. **spellcheck not set true (code):** `lesson-runner-core.js` does NOT contain `spellcheck:'true'` or `spellcheck:"true"`.
  9. **monospace code font (rodney):** `getComputedStyle(.cm-content).fontFamily` contains `"mono"`.
  10. **tab stays in editor (code):** `lesson-runner-core.js` passes a `keymap` extension including `indentWithTab` via `keymap.of([indentWithTab])` or equivalent composition.
  11. **editor extensions completeness (code):** `EditorView` extensions array includes ALL of `lineNumbers()`, `bracketMatching()`, `indentWithTab` in `keymap.of([...])`, `highlightActiveLine()`, and `EditorView.contentAttributes({ spellcheck: "false" })`.
  12. **mobile minimum workspace (rodney 375x667):** `.cm-scroller.getBoundingClientRect().width >= 280`.

- **probe:**
  ```
  # Primary: rodney browser probe on assembled site (both targets)
  rodney open <site-url>/index.html
  rodney await window.__bt.ready
  rodney eval "const ed = window.__bt.editorView; ed.focus(); ed.dispatch({changes:{from:0,insert:'f(x)'}}); ed.dispatch({selection:{anchor:1}});"
  # Clause 2: gutter visible
  rodney assert "document.querySelector('.cm-gutters') !== null && getComputedStyle(document.querySelector('.cm-gutters')).display !== 'none' && document.querySelector('.cm-gutterElement') !== null && document.querySelector('.cm-gutterElement').textContent.trim().length > 0"
  # Clause 3: gutter font mono
  rodney assert "getComputedStyle(document.querySelector('.cm-gutterElement')).fontFamily.includes('mono')"
  # Clause 4: active line bg distinct
  rodney assert "getComputedStyle(document.querySelector('.cm-activeLine')).backgroundColor !== getComputedStyle(document.querySelector('.cm-content')).backgroundColor"
  # Clause 6: bracket match element present
  rodney assert "document.querySelector('.cm-bracket-match') !== null"
  # Clause 7: spellcheck false (explicit attribute)
  rodney assert "document.querySelector('.cm-content').getAttribute('spellcheck') === 'false' || document.querySelector('.cm-editor').getAttribute('spellcheck') === 'false'"
  # Clause 9: code font mono
  rodney assert "getComputedStyle(document.querySelector('.cm-content')).fontFamily.includes('mono')"
  # Clause 12: mobile width
  rodney viewport 375 667
  rodney assert "document.querySelector('.cm-scroller').getBoundingClientRect().width >= 280"

  # Secondary: code assertions on source files (clauses 1, 5, 8, 10, 11)
  # Via Rust unit test parsing SiteFiles — styles.css content + lesson-runner-core.js content
  ```
- **negative:** 10 sneaky-passes — (1) `.cm-gutters` display:none or ancestor overflow:hidden (gutter exists but zero pixels); (2) bracketMatching() in extensions array but no CSS rule for .cm-bracket-match (class added, no visual); (3) indentWithTab imported but not in keymap.of([...]) composition (Tab falls through to browser focus traversal); (4) highlightActiveLine() added but .cm-activeLine background:transparent or matches .cm-content (class present, zero visual difference); (5) no spellcheck="false" anywhere (browser inherits true, word-squiggles appear); (6) .cm-content font-family monospace but .cm-gutters inherits system-ui (line numbers misaligned); (7) tabSize 2 but indentUnit 4 (Tab dead-reckons to different column); (8) bundle missing @codemirror/commands (indentWithTab undefined, keymap.of([undefined]) does not throw); (9) 375px mobile gutter 80+px (scrollable code <200px unusably narrow); (10) lineNumbers() added but empty document renders zero gutter elements (should show "1" for line 1).
- **verification:** rodney · deterministic JS assertions on CM6 computed styles/DOM attributes + code for CSS/JS source-existence (clauses 1, 5, 8, 10, 11 via Rust unit test parsing SiteFiles)
- **fixture status:** existing `crates/core/tests/fixtures/r-course/add_two.yaml:10-13` + `python-course/add.yaml:10-13` (code_template); existing after AC-2 `crates/core/assets/shared/lesson-runner-core.js:25,51,103` (submissionEl, renderLesson, runSubmission); existing `crates/core/assets/shared/styles.css:168-187` (#submission base rule — AC-3 adds CM6-specific rules alongside); `codemirror.js` exists after AC-1 (AC-1 bundle MUST export lineNumbers/highlightActiveLine/bracketMatching/indentWithTab — see AC-1 updated spec); NEW CSS rules (.cm-editor, .cm-content, .cm-gutters, .cm-activeLine, .cm-bracket-match); NEW editorExtensions(language) function in lesson-runner-core.js.
- **rubric anchor:** §1.5 (predicate pins invariant not coincident shape — each clause targets visible property not extension-presence-in-array), §1.5.1 (absence assertions paired with positive-presence companions — clause 1 absence + clause 2 presence), §2.1 (building extension list is pure config, mounting EditorView is effectful in start()), §3.2 (CM6 appearance decoupled from targets via styles.css tokens), §4.1 (lesson-runner-core.js header names editor lifecycle), §5.1 (single editorExtensions(language) function returns full extension array), §5.3 (rodney probes exercise full assembled site, no mocked EditorView).

### Design Intent
- **Types / interfaces (§1):** No new domain type. Runtime adapter shape unchanged. `window.__bt.editorView` remains the test seam from AC-2. Extension config is a typed array of CM6 `Extension` objects produced by a pure helper `editorExtensions(language)` indexed by `runtime.language`. No illegal state introduced — extensions array is always complete or absent (graceful-degradation textarea).
- **Pure / effectful (§2):** Building the extension list (`editorExtensions(language)`) is pure — given a language, returns `Extension[]`, no side effects, snapshot-testable. Creating/mounting `EditorView` is effectful, done once in `start()`. CSS rules are static pure declarations in `styles.css`. The split: pure config function -> effectful mount in `start()`.
- **Boundary cuts (§3):** `codemirror.js` supplies vendored CM6 symbols (AC-1 owns the export list). `lesson-runner-core.js` assembles the extension config + owns the editor lifecycle. `styles.css` controls all visual treatment via `--bt-*` tokens. Target adapters and `feedback.js` untouched. The §3.5 callback analogy: Tab handling is an event-handler seam — `indentWithTab` in `keymap.of([...])` must cover the Tab transition, not just be imported.
- **Module responsibility (§4):** `lesson-runner-core.js` owns editor lifecycle + UX extension set. `styles.css` owns visual treatment (`.cm-*` rules, `--bt-font-family-code` token). `codemirror.js` owns vendored exports (AC-1's responsibility). Target adapters + `feedback.js` explicitly NOT touched.
- **Function discipline (§5):** One function `editorExtensions(language)` returns the full `Extension[]` array — `lineNumbers()`, `bracketMatching()`, `highlightActiveLine()`, `keymap.of([indentWithTab])`, `EditorView.contentAttributes({ spellcheck: "false" })`, plus AC-2's base extensions (language compartment, update listener). `start()` calls it once when creating the `EditorView`. No patches, no monkey-patching, testable by reading the assembled `SiteFiles`.

### Technical Context
- **Files likely touched:**
  - `crates/core/assets/shared/codemirror.js` — AC-1 owns re-vendor to export `lineNumbers`, `highlightActiveLine` from `@codemirror/view`; `bracketMatching` from `@codemirror/language`; `indentWithTab` from `@codemirror/commands`. AC-3 consumes read-only.
  - `crates/core/assets/shared/lesson-runner-core.js` — add `editorExtensions(language)` pure function; update `EditorView` setup in `start()` to include `lineNumbers()`, `highlightActiveLine()`, `bracketMatching()`, `keymap.of([indentWithTab])`, `EditorView.contentAttributes({ spellcheck: "false" })`.
  - `crates/core/assets/shared/styles.css` — add `.cm-editor` (min-height, border-radius, overflow), `.cm-content`/`.cm-line` (`font-family: var(--bt-font-family-code)`), `.cm-gutters` (background, border-right, `font-family: var(--bt-font-family-code)`), `.cm-activeLine` (background distinct from surface-code), `.cm-bracket-match` (background/outline non-transparent), `.cm-focused` (`outline: none`). Dark-mode overrides for `.cm-*` rules must go INSIDE the existing `@media (prefers-color-scheme: dark)` block at `styles.css:405-426` (which MUST remain the last block — positional invariant from prior cycle).
  - `docs/okf/interfaces/js-runtime-seam.md` — update `editorView` note to document UX extensions.
  - `crates/core/src/site/mod.rs` — Rust unit tests for clauses 1/5/8/10/11 (parse `SiteFiles` CSS + JS content).
- **Architecture notes:** Shared core means one `editorExtensions(language)` config + one stylesheet applies to both webR and Pyodide targets. `--bt-font-family-code` token reused for both `.cm-content` and `.cm-gutters` (prevents gutter/code misalignment — sneaky-pass #6). The dark-mode `@media` block at `styles.css:405-426` is the LAST block in the file (positional invariant — any new `.cm-*` dark overrides must be appended INSIDE it, not after it). CM6's `bracketMatching()` adds class `cm-bracket-match` (NOT `cm-matchingBracket` — that class does not exist in CM6; CM5 used `CodeMirror-matchingbracket`).

### UI Block
```yaml
ui:
  selectors:
    - ".cm-gutters"
    - ".cm-gutter"
    - ".cm-gutterElement"
    - ".cm-activeLine"
    - ".cm-bracket-match"
    - ".cm-content"
    - ".cm-editor"
    - ".cm-scroller"
  layout_assertions:
    - "gutter not hidden: getComputedStyle(.cm-gutters).display !== 'none' AND no ancestor overflow:hidden"
    - "active line bg distinct: getComputedStyle(.cm-activeLine).backgroundColor !== getComputedStyle(.cm-content).backgroundColor"
    - "bracket match has visual style: CSS rule for .cm-bracket-match with non-transparent background/outline/box-shadow/border-color"
    - "spellcheck explicit false: .cm-content or .cm-editor getAttribute('spellcheck') === 'false' (not absent)"
    - "code font mono: getComputedStyle(.cm-content).fontFamily contains 'mono'"
    - "gutter font mono: getComputedStyle(.cm-gutterElement).fontFamily contains 'mono'"
    - "mobile workspace: .cm-scroller.getBoundingClientRect().width >= 280 at 375x667"
  deterministic_check: |
    rodney JS probes checking getComputedStyle() (NOT selector existence) for clauses 2/3/4/6/7/9/12;
    Rust unit test parsing SiteFiles CSS/JS content for clauses 1/5/8/10/11.
    Every visual clause checks computed style or explicit attribute, never querySelector !== null alone.
  subjective_residual:
    - "Active line highlight color contrast against the code surface"
    - "Bracket-match highlight color harmonizes with syntax theme"
    - "Gutter width and line-number font size aesthetic"
    - "Caret/cursor visibility inside the editor"
  viewports:
    - 1280x720
    - 375x667
```

### Dependencies
- **Depends on:** AC-1 (vendored `codemirror.js` bundle MUST export `lineNumbers`, `highlightActiveLine` from `@codemirror/view`; `bracketMatching` from `@codemirror/language`; `indentWithTab` from `@codemirror/commands` — AC-1's bundle is the complete export set, AC-3 consumes read-only), AC-2 (EditorView instance mounted in `<div id="submission" data-test="submission">`, `window.__bt.getSubmission()` + `window.__bt.editorView` exposed, `renderLesson`/`runSubmission` wired to editor).
- **Blocks:** none (AC-3 is the UX polish terminal layer atop AC-2's bare editor).
- **Conflict set:** `crates/core/assets/shared/lesson-runner-core.js` (AC-2 modifies renderLesson/start, AC-3 adds editorExtensions + extends EditorView setup), `crates/core/assets/shared/styles.css` (AC-2 owns #submission base rule at :168-187, AC-3 adds .cm-* rules alongside), `docs/okf/interfaces/js-runtime-seam.md`, `docs/okf/interfaces/site-build-pipeline.md`.
- **Risk level:** medium — CM6 extension wiring has integration risk (bundle export completeness, keymap.of([...]) composition, CSS specificity vs #submission base rule, dark-mode @media source-order invariant).

### Progress
- [x] spec written — 2026-06-25
- [x] red test (plan_site_cm6_editor_ux_extensions_configured) — 2026-06-26
- [x] green: editorExtensions(language) + keymap re-vendor + CSS rules — 2026-06-26
- [x] extend build_dark_mode_token_overrides (syntax anti-invisibility) — 2026-06-26
- [x] rodney-probes/cm6-ux.js — 2026-06-26
- [x] full workspace test suite green (178 tests, 0 failures) — 2026-06-26
- [x] build smoke test (webr + pyodide, byte-identical) — 2026-06-26
- [x] E2E evidence docs/evidence/66/ — 2026-06-26

### Decision Log
- 2026-06-25 — bracket-match class name: `.cm-bracket-match` (CM6 actual from bracketMatching() in @codemirror/language). Proposer A used `.cm-matchingBracket` which does not exist in any CM version. B correct.
- 2026-06-25 — AC-1 bundle re-vendor ownership: AC-1 owns the complete bundle (Director decision). AC-1's spec updated to export lineNumbers/highlightActiveLine/bracketMatching/indentWithTab. AC-3 consumes read-only. No conflict on codemirror.js.
- 2026-06-25 — predicate = B (12 clauses): B is strict superset of A's 6 checks, uses correct class name, catches sneaky-passes A misses (gutter hidden via display:none, active line transparent, spellcheck absent vs explicit false, gutter font misalignment, mobile width, empty-document gutter).
- 2026-06-25 — probe = B's split (rodney primary + code secondary): computed styles via rodney, source-content existence via Rust unit test parsing SiteFiles.
- 2026-06-25 — conflict set = A (conservative): B said "none" which is wrong — AC-3 modifies lesson-runner-core.js and styles.css.
- 2026-06-26 — keymap re-vendor: AC-3 needs `keymap` from @codemirror/view to compose `keymap.of([indentWithTab])`. The symbol was present in the bundle body (line 11862) but NOT exported. Re-vendored by adding `keymap` to the export block only (no body changes). Added `keymap` to the clause-5 needle list in plan_site_emits_vendored_codemirror_bundle to pin the invariant.
- 2026-06-26 — hex-literal invariant: CSS fallback values `var(--token, #f5f5f5)` violate the "zero hex literals outside :root" invariant (build_dark_mode_token_overrides clause 7 + build_webr/pyodide_emits_a_deployable_*_lesson_site). Removed hex fallbacks — tokens are always defined in :root so fallbacks are unnecessary.

### Surprises & Discoveries
- 2026-06-26 — Hex-literal invariant caught CSS fallback values. The task's Step 3 specified `var(--bt-color-surface-code, #f5f5f5)` with hex fallbacks, but the existing test invariant `build_dark_mode_token_overrides` clause 7 asserts zero hex literals outside :root blocks, and `build_webr_emits_a_deployable_r_lesson_site` asserts "workspace rules must not contain hardcoded hex color literals". The fix: remove hex fallbacks entirely (tokens are always defined in :root). This was unexpected because the task explicitly specified the fallback values. Resolution: removed fallbacks, all tests pass. The invariant is correct — fallbacks are dead code since :root always defines the tokens.
- 2026-06-26 — keymap was NOT in the bundle exports despite the task saying "The bundle already exports all needed symbols (lineNumbers, highlightActiveLine, bracketMatching, indentWithTab)." The task's own caveat ("You may need to also import keymap from the bundle — check if it's exported. If not, re-vendor the bundle to add keymap") was the correct path. The symbol existed in the body (line 11862) but was missing from the export block. Resolution: added `keymap` to the export list (1-line change, no body modification).

### Idempotence & Recovery
- Safe retry: re-run rodney probes + cargo test for code-level assertions. Idempotent.
- Rollback: remove editorExtensions() function + extensions from EditorView setup, remove .cm-* CSS rules.
