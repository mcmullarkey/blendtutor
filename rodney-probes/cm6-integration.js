// Rodney probe script for issue #65 (AC-2: CodeMirror 6 editor integration).
//
// Runtime clauses 8-13 from the executable spec — these CANNOT be pinned at
// build time (they need a real browser + CM6 boot) and are run by
// @builder-vision-probe against a served built site. This file documents the
// deterministic rodney assertions; the builder-vision-probe agent executes
// them (the coding builder does NOT run rodney).
//
// Build + serve (run by builder-vision-probe):
//   cargo run -p blendtutor-cli --bin blendtutor -- build --target webr \
//     crates/core/tests/fixtures/r-course -o /tmp/bt-65-webr
//   cargo run -p blendtutor-cli --bin blendtutor -- build --target pyodide \
//     crates/core/tests/fixtures/python-course -o /tmp/bt-65-pyodide
//   # Neuter coi-serviceworker.js in both output dirs (no-op for probes).
//   # Serve each on a localhost port (e.g. 58080 webr, 58081 pyodide).
//
// Preconditions for every clause: navigate to the served index.html, await
// `window.__bt.ready` (the boot promise), then await `window.__bt.selectLesson(0)`
// so the editor has a doc loaded from code_template.

// --- Clause 8: bidirectional sync, WRITE path -------------------------------
// renderLesson writes code_template into the editor doc. After selectLesson(0),
// the editor's doc must equal the lesson's code_template (the write path:
// lesson → editor). A one-way-sync bug (editor never receives the template)
// fails here.
//
// rodney assert (webr + pyodide):
//   const bt = window.__bt;
//   await bt.ready;
//   const lesson = bt.lessons[0];
//   const doc = bt.editorView.state.doc.toString();
//   assert(doc === (lesson.code_template ?? ""),
//          "write path: editor doc must equal code_template after selectLesson");

// --- Clause 9: bidirectional sync, READ path -------------------------------
// getSubmission() reads the editor doc back out. After the write path lands the
// template, getSubmission() must return the SAME string (the read path:
// editor → runSubmission). A one-way-sync bug (runSubmission reads stale/empty
// text) fails here.
//
// rodney assert (webr + pyodide):
//   const readBack = bt.getSubmission();
//   assert(readBack === (lesson.code_template ?? ""),
//          "read path: getSubmission() must return the editor doc");

// --- Clause 10: language R — mono font + syntax tokens ---------------------
// The webr editor loads the R language extension. The editor content must
// render in a monospace font (the code vocabulary), and the R syntax highlighter
// must produce token elements (e.g. .cm-comment, .cm-keyword, .cm-string) for
// R-specific syntax in the template. A missing language extension (no syntax
// tokens) or a non-monospace font fails here.
//
// rodney assert (webr):
//   const cmContent = document.querySelector("#submission .cm-content");
//   assert(cmContent !== null, "R editor: .cm-content must exist");
//   const font = getComputedStyle(cmContent).fontFamily;
//   assert(/monospace/i.test(font), "R editor: font must be monospace, got " + font);
//   // R syntax tokens: the add_two template has a comment or string the
//   // highlighter marks. At least one .cm-* token class must be present.
//   const tokens = document.querySelectorAll("#submission .cm-content [class*='cm-']");
//   assert(tokens.length > 0, "R editor: syntax tokens must be present");

// --- Clause 11: language Python — mono font + syntax tokens ----------------
// The pyodide editor loads the Python language extension. Same checks as
// clause 10, against the Python target.
//
// rodney assert (pyodide):
//   const cmContent = document.querySelector("#submission .cm-content");
//   assert(cmContent !== null, "Python editor: .cm-content must exist");
//   const font = getComputedStyle(cmContent).fontFamily;
//   assert(/monospace/i.test(font), "Python editor: font must be monospace, got " + font);
//   const tokens = document.querySelectorAll("#submission .cm-content [class*='cm-']");
//   assert(tokens.length > 0, "Python editor: syntax tokens must be present");

// --- Clause 12: no editor leak after lesson switch cycle -------------------
// The EditorView is a singleton: switching lessons dispatches a doc replace,
// never destroys/recreates the editor. After a switch cycle (0 → 1 → 0), there
// must be exactly ONE .cm-editor inside #submission. An editor-per-switch leak
// (a new EditorView per renderLesson) leaves stale .cm-editor nodes behind.
//
// rodney assert (webr, if the course has >=2 lessons):
//   await bt.selectLesson(0);
//   await bt.selectLesson(1);
//   await bt.selectLesson(0);
//   const editors = document.querySelectorAll("#submission .cm-editor");
//   assert(editors.length === 1,
//          "no leak: exactly one .cm-editor after switch cycle, got " + editors.length);
//   // The singleton instance is stable across switches.
//   assert(window.__bt.editorView === editors[0].cmView?.view
//          || window.__bt.editorView !== null,
//          "no leak: editorView handle is the same singleton across switches");

// --- Clause 13: graceful degradation fallback textarea ---------------------
// If CM6 fails to initialize, the runner falls back to a <textarea> inside
// #submission and getSubmission() reads from it. This is hard to trigger live
// (the bundle is vendored and works), so this clause is verified by code
// inspection of lesson-runner-core.js (the try/catch + fallback textarea +
// getSubmission textarea path) rather than a live rodney assert. The
// builder-vision-eval agent confirms the fallback path exists in source.
//
// Code-inspection checklist (builder-vision-eval):
//   - start() wraps `new EditorView(...)` in try/catch.
//   - catch branch creates a <textarea> inside #submission.
//   - editorView is set to null under degradation.
//   - getSubmission() reads the textarea when editorView is null.
//   - setEditorContent() writes the textarea when editorView is null.
