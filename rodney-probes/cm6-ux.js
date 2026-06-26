// Rodney probe script for issue #66 (AC-3: CM6 editor UX polish).
//
// Runtime clauses 2/3/4/6/7/9/12 from the executable spec — these CANNOT be
// pinned at build time (they need a real browser + CM6 boot to check computed
// styles) and are run by @builder-vision-probe against a served built site.
// This file documents the deterministic rodney assertions; the builder-vision-
// probe agent executes them (the coding builder does NOT run rodney).
//
// Build-time clauses 1/5/8/10/11 are pinned by the Rust unit test
// plan_site_cm6_editor_ux_extensions_configured in crates/core/src/site/mod.rs
// (parses SiteFiles CSS/JS content).
//
// Build + serve (run by builder-vision-probe):
//   cargo run -p blendtutor-cli --bin blendtutor -- build --target webr \
//     crates/core/tests/fixtures/r-course -o /tmp/bt-66-webr
//   cargo run -p blendtutor-cli --bin blendtutor -- build --target pyodide \
//     crates/core/tests/fixtures/python-course -o /tmp/bt-66-pyodide
//   # Neuter coi-serviceworker.js in both output dirs (no-op for probes).
//   # Serve each on a localhost port (e.g. 58080 webr, 58081 pyodide).
//
// Preconditions for every clause: navigate to the served index.html, await
// `window.__bt.ready` (the boot promise), then await `window.__bt.selectLesson(0)`
// so the editor has a doc loaded from code_template. Then focus the editor and
// inject bracket-containing code so bracketMatching() has a pair to highlight.

// --- Setup (run once before all clauses) -----------------------------------
// Focus the editor and inject code with brackets so the bracket-matching
// extension has a pair to highlight. Place the cursor adjacent to a bracket.
//
// rodney eval (webr + pyodide):
//   const bt = window.__bt;
//   await bt.ready;
//   await bt.selectLesson(0);
//   const ed = bt.editorView;
//   ed.focus();
//   ed.dispatch({ changes: { from: 0, to: ed.state.doc.length, insert: "f(x)" } });
//   ed.dispatch({ selection: { anchor: 1 } }); // cursor next to "f"

// --- Clause 2: line numbers visible ---------------------------------------
// The gutter must be present in the DOM, NOT display:none, and must render at
// least one gutter element with non-empty text (the "1" for line 1). An empty
// document renders zero gutter elements (sneaky-pass #10) — the injected code
// guarantees a non-empty doc.
//
// rodney assert (webr + pyodide):
//   const gutters = document.querySelector('.cm-gutters');
//   assert(gutters !== null, "clause 2: .cm-gutters must exist");
//   assert(getComputedStyle(gutters).display !== 'none',
//          "clause 2: .cm-gutters must not be display:none");
//   const gutterEl = document.querySelector('.cm-gutterElement');
//   assert(gutterEl !== null, "clause 2: .cm-gutterElement must exist");
//   assert(gutterEl.textContent.trim().length > 0,
//          "clause 2: gutter element must have non-empty text (line number)");

// --- Clause 3: gutter font monospace --------------------------------------
// The gutter font must be monospace so line numbers align with code lines
// (sneaky-pass #6: .cm-content monospace but .cm-gutters inherits system-ui).
//
// rodney assert (webr + pyodide):
//   const gutterEl = document.querySelector('.cm-gutterElement');
//   assert(/mono/i.test(getComputedStyle(gutterEl).fontFamily),
//          "clause 3: gutter font must be monospace, got " + getComputedStyle(gutterEl).fontFamily);

// --- Clause 4: active line bg distinct from content bg ---------------------
// The active line (cursor line) must have a background DISTINCT from the
// .cm-content background (sneaky-pass #4: transparent or matching bg = zero
// visual difference). transparent is treated as equal to the parent bg.
//
// rodney assert (webr + pyodide):
//   const activeLine = document.querySelector('.cm-activeLine');
//   assert(activeLine !== null, "clause 4: .cm-activeLine must exist");
//   const content = document.querySelector('.cm-content');
//   const activeBg = getComputedStyle(activeLine).backgroundColor;
//   const contentBg = getComputedStyle(content).backgroundColor;
//   assert(activeBg !== contentBg,
//          "clause 4: active line bg must differ from content bg; active=" + activeBg + " content=" + contentBg);

// --- Clause 6: bracket match element present ------------------------------
// After injecting code containing brackets (f(x)) and placing the cursor
// adjacent to a bracket, the .cm-bracket-match element must be present in the
// DOM. bracketMatching() adds this class; without the extension it is absent.
//
// rodney assert (webr + pyodide):
//   const match = document.querySelector('.cm-bracket-match');
//   assert(match !== null,
//          "clause 6: .cm-bracket-match element must be present after injecting brackets");

// --- Clause 7: spellcheck disabled (explicit false) ------------------------
// The .cm-content (or .cm-editor) must have an EXPLICIT spellcheck="false"
// attribute. An absent attribute inherits true by browser default on
// contenteditable (sneaky-pass #5: no attribute = word squiggles appear).
//
// rodney assert (webr + pyodide):
//   const content = document.querySelector('.cm-content');
//   const editor = document.querySelector('.cm-editor');
//   assert(content.getAttribute('spellcheck') === 'false'
//          || editor.getAttribute('spellcheck') === 'false',
//          "clause 7: spellcheck must be explicitly 'false' (not absent)");

// --- Clause 9: monospace code font -----------------------------------------
// The .cm-content font must be monospace (the code vocabulary).
//
// rodney assert (webr + pyodide):
//   const content = document.querySelector('.cm-content');
//   assert(/mono/i.test(getComputedStyle(content).fontFamily),
//          "clause 9: code font must be monospace, got " + getComputedStyle(content).fontFamily);

// --- Clause 12: mobile minimum workspace (375x667) ------------------------
// At a 375x667 mobile viewport, the .cm-scroller must be at least 280px wide
// so the code area is usable (sneaky-pass #9: 80px+ gutter leaves <200px
// scrollable code, unusably narrow).
//
// rodney viewport 375 667
// rodney assert (webr + pyodide):
//   const scroller = document.querySelector('.cm-scroller');
//   assert(scroller !== null, "clause 12: .cm-scroller must exist");
//   const width = scroller.getBoundingClientRect().width;
//   assert(width >= 280,
//          "clause 12: .cm-scroller width must be >= 280 at 375x667, got " + width);
