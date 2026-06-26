# Evidence — Issue #66: CM6 editor UX polish

## Summary
Configure the CodeMirror 6 editor with standard code-editor UX: line numbers,
bracket matching, smart tab handling, active line highlighting, spellcheck off,
and monospace font.

## Verification medium
`rodney + code` — build-time clauses (1/5/8/10/11) pinned by Rust unit tests
parsing SiteFiles content; runtime clauses (2/3/4/6/7/9/12) documented as
rodney probes in `rodney-probes/cm6-ux.js` for @builder-vision-probe.

## Test results — full workspace

Command: `uv run cargo test --workspace`

- Exit code: 0
- Result: 17 test binaries, all `ok`, 0 failures
- Full log: `test-suite.log`

Test counts per binary:
- blendtutor (cli unit): 9 passed
- build (integration): 12 passed
- cli: 2 passed
- cutover: 2 passed
- eval (cli): 2 passed
- init: 2 passed
- list: 3 passed
- new: 3 passed
- readme: 1 passed
- run: 7 passed
- validate: 4 passed
- blendtutor-core (lib unit): 112 passed
- eval (core): 2 passed
- grade: 3 passed
- llm: 9 passed
- runner: 5 passed
- doc-tests: 0 passed

## Lint results
- `cargo fmt --check`: clean (no diff)
- `cargo clippy --workspace --all-targets`: clean (no warnings)

## Build smoke test

Commands:
```
cargo run -- build --target webr crates/core/tests/fixtures/r-course -o /tmp/bt-ux-webr
cargo run -- build --target pyodide crates/core/tests/fixtures/python-course -o /tmp/bt-ux-pyodide
```

Results:
- webr: `built 2 lesson(s) for webr into /tmp/bt-ux-webr`
- pyodide: `built 1 lesson(s) for pyodide into /tmp/bt-ux-pyodide`

### Built-site content verification

`lesson-runner-core.js` (both targets, byte-identical):
- `function editorExtensions(language)` present (line 68)
- `lineNumbers()` wired
- `highlightActiveLine()` wired
- `bracketMatching()` wired
- `keymap.of([indentWithTab])` composition present (line 83)
- `EditorView.contentAttributes.of({ spellcheck: "false" })` present (line 82)
- `import { EditorView` single-line import preserved (test invariant)

`styles.css` (both targets, byte-identical):
- `.cm-gutters` rule (surface-code bg, border-right, mono font)
- `.cm-gutterElement` rule (mono font)
- `.cm-activeLine` rule (rgba(0,0,0,0.04) bg — distinct from content)
- `.cm-bracket-match` rule (rgba(0,0,0,0.12) bg + outline — non-transparent)
- `.cm-focused` rule (outline: none)
- Dark-mode overrides inside `@media` block (rgba(255,255,255,*) tints)
- `@media` block remains LAST in file (positional invariant)
- Comment token `#6a6a6a` (WCAG AA fix from #999)

`codemirror.js` (both targets, byte-identical):
- `keymap` added to export block (re-vendor event — AC-3 consumes it)

### Cross-target byte-identity
- `diff -q lesson-runner-core.js`: IDENTICAL
- `diff -q styles.css`: IDENTICAL
- `diff -q codemirror.js`: IDENTICAL (shared, not forked)

## Rodney probes
Runtime clauses (2/3/4/6/7/9/12) are documented in
`rodney-probes/cm6-ux.js` and will be executed by @builder-vision-probe
against a served built site. The coding builder does NOT run rodney.

## Re-vendor event (transparency)
This PR re-vendors `crates/core/assets/shared/codemirror.js` to add `keymap`
(from `@codemirror/view`) to the export block. AC-3 composes `indentWithTab`
via `keymap.of([indentWithTab])` so Tab stays in the editor (not browser focus
traversal). The `keymap` symbol was already present in the bundle body (line
11862: `var keymap = Facet.define(...)`) but was not exported. This PR adds it
to the export list only — no body changes. Documented per the vendored-bundle
transparency rule.
