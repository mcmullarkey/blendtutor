// blendtutor in-browser lesson runner — shared core (ADR-0008).
//
// The cross-target scaffolding both the webR and the Pyodide runners reuse: it
// loads the lessons emitted by `blendtutor build` (the SiteLesson JSON contract),
// renders the lesson UI, wires the controls, and reports pass/fail — everything
// except *which* runtime executes the code. A target's own `lesson-runner.js`
// supplies that as a small `runtime` adapter and calls `start(runtime)`; this is
// the §5.1 cut that keeps language-specific wiring minimal atop shared assembly.
//
// A `runtime` adapter is `{ name, language, boot(), run(code, checks) }`:
//   - `name`     : label shown in the boot status line (e.g. "webR", "Pyodide").
//   - `language` : "r" | "python" — selects the CodeMirror 6 language extension
//                  loaded into the editor (closed set, §1.5). NOT a string match
//                  on `name`; a dedicated field so an adapter cannot accidentally
//                  get the wrong highlighting by a label typo.
//   - `boot()`   : async, initializes the runtime; resolve when ready to run.
//   - `run(code, checks)` : async, evaluates the submission followed by the
//                 lesson's checks and resolves to `{ output, ok }` — `ok` true
//                 when nothing raised (a pass), false when anything did (a fail).
//                 The core never inspects the language for grading; the adapter
//                 owns grading. The `language` field is for the editor only.
//
// The `window.__bt` handle (lessons / selectLesson / runSubmission / getSubmission
// / editorView / ready) is the test seam the rodney browser probe drives,
// identical across targets.

import { EditorView, r, python, syntaxHighlighting, defaultHighlightStyle, HighlightStyle, lineNumbers, highlightActiveLine, bracketMatching, indentWithTab, keymap } from "./codemirror.js";
import { tags } from "./codemirror.js";

// Language extension lookup: closed set keyed by the runtime adapter's
// `language` field. An unknown language yields no extension (the editor still
// works, just without syntax highlighting) — the rodney probe catches the
// missing tokens. This is NOT a string match on `runtime.name` (§1.5): a label
// typo cannot silently load the wrong language.
const LANG_EXT = { r: r(), python: python() };

// Custom HighlightStyle with deterministic `.tok-*` class names. CM6's
// defaultHighlightStyle generates opaque unicode classes (e.g. `ͼa`) that the
// AC-2 deterministic check cannot grep for; this style maps lezer tags to
// stable `.tok-*` classes so token spans are testable. Added AFTER the default
// style in the extension list so it takes precedence for the tags it defines
// (later extensions win on tag-match ties). The default style remains as a
// fallback for tags not covered here.
const tokHighlightStyle = HighlightStyle.define([
  { tag: tags.keyword, class: "tok-keyword" },
  { tag: tags.variableName, class: "tok-variableName" },
  { tag: tags.string, class: "tok-string" },
  { tag: tags.number, class: "tok-number" },
  { tag: tags.comment, class: "tok-comment" },
  { tag: tags.function(tags.variableName), class: "tok-function" },
  { tag: tags.operator, class: "tok-operator" },
]);

// Build the full CM6 extension array for a given language — a PURE function
// (§2.1, §5.1): given a language, returns the Extension[] with no side effects.
// start() calls it once when mounting the EditorView. The array composes:
//   - the language parser (r()/python()) for syntax-tree building,
//   - the default + custom HighlightStyles for token coloring (.tok-* classes),
//   - lineNumbers() for the gutter,
//   - highlightActiveLine() for the active-line background,
//   - bracketMatching() for bracket-pair highlighting (.cm-matchingBracket),
//   - keymap.of([indentWithTab]) so Tab indents/dedents inside the editor
//     (NOT browser focus traversal — sneaky-pass #3: importing indentWithTab
//     alone is insufficient; it must be composed via keymap.of),
//   - contentAttributes({ spellcheck: "false" }) so the contenteditable .cm-
//     content does NOT inherit the browser's default spellcheck=true
//     (sneaky-pass #5: absent attribute inherits true, word-squiggles appear).
function editorExtensions(language) {
  return [
    LANG_EXT[language] ?? [],
    // Token styling — distinct from the language parser above. CM6 needs
    // BOTH: the language support (r()/python()) for parsing into a tree, and
    // a highlight style for mapping tree nodes to CSS classes. The default
    // style is kept as a fallback; tokHighlightStyle (added after, so it
    // wins on tag-match ties) overrides it with deterministic `.tok-*`
    // classes the AC-2 deterministic check can grep for.
    syntaxHighlighting(defaultHighlightStyle),
    syntaxHighlighting(tokHighlightStyle),
    lineNumbers(),
    highlightActiveLine(),
    bracketMatching(),
    EditorView.contentAttributes.of({ spellcheck: "false" }),
    keymap.of([indentWithTab]),
  ];
}

const statusEl = document.querySelector("[data-test=lesson-status]");
const bootEl = document.getElementById("boot-status");
const titleEl = document.getElementById("lesson-title");
const promptEl = document.getElementById("lesson-prompt");
const submissionEl = document.getElementById("submission");
const selectEl = document.getElementById("lesson-select");
const outputEl = document.getElementById("output");
const runButton = document.getElementById("run");

// The status element exposes `data-status` (idle | running | pass | fail); the
// browser probe polls `[data-status="pass"]`, and the page CSS keys off it.
function setStatus(state, text) {
  statusEl.dataset.status = state;
  statusEl.textContent = text ?? state;
}

// Fetch the ordered slug index, then each per-lesson JSON by position. Keyed by
// index, not slug, so a lesson slug is never a URL/path component.
async function loadLessons() {
  const slugs = await (await fetch("lessons.json")).json();
  const lessons = [];
  for (let i = 0; i < slugs.length; i++) {
    lessons.push(await (await fetch(`lessons/${i}.json`)).json());
  }
  return lessons;
}

// The CodeMirror 6 editor instance — a module-level singleton created once in
// start() and reused across lesson switches. Null until start() runs, or when
// graceful degradation fell back to a textarea (CM6 construction failure).
let editorView = null;

// Replace the editor's doc in one transaction. Singleton: the EditorView is
// created once in start(); switching lessons dispatches a full-doc replace
// (from 0 to doc.length) rather than destroying and recreating the editor — the
// pattern that would leak an editor per switch (§5.1). Under graceful
// degradation (editorView null), writes to the fallback textarea instead.
function setEditorContent(code) {
  if (editorView) {
    editorView.dispatch({
      changes: { from: 0, to: editorView.state.doc.length, insert: code },
    });
    return;
  }
  const ta = submissionEl.querySelector("textarea");
  if (ta) {
    ta.value = code;
  }
}

function renderLesson(lesson) {
  titleEl.textContent = lesson.title;
  promptEl.textContent = lesson.prompt;
  setEditorContent(lesson.code_template ?? "");
  outputEl.textContent = "";
  setStatus("idle", "idle");
}

// Build the shared lesson state, bound to a runtime adapter. Grading itself lives
// in `runtime.run`; this object owns only selection, the pass/fail reporting
// that is identical for every language, and reading the editor doc.
function makeBt(runtime) {
  return {
    lessons: [],
    current: 0,

    async selectLesson(index) {
      this.current = index;
      renderLesson(this.lessons[index]);
    },

    // Evaluate the submission and the current lesson's checks via the runtime
    // adapter. A clean run (nothing raised) is a pass; anything raising is a fail.
    async runSubmission(code) {
      const lesson = this.lessons[this.current];
      setStatus("running", "running…");
      const { output, ok } = await runtime.run(code, lesson.checks ?? []);
      outputEl.textContent = output;
      setStatus(ok ? "pass" : "fail", ok ? "pass" : "fail");
      return ok ? "pass" : "fail";
    },

    // Read the current editor doc — the single source for submission text (§3.4).
    // Falls back to a textarea (graceful degradation) if CM6 failed to boot, so
    // feedback.js and the run button read the same contract either way.
    getSubmission() {
      if (editorView) {
        return editorView.state.doc.toString();
      }
      const ta = submissionEl.querySelector("textarea");
      return ta ? ta.value : "";
    },
  };
}

/// Boot a built lesson site against `runtime`. Loads the lessons, wires the UI,
/// creates the CodeMirror 6 editor in `#submission`, then initializes the runtime
/// — exposing `window.__bt` for the test probe.
export async function start(runtime) {
  const bt = makeBt(runtime);
  window.__bt = bt;

  // Create the CM6 editor ONCE — a singleton mounted in #submission. On lesson
  // switch, renderLesson dispatches a doc replace (never destroys/recreates), so
  // there is no editor leak across switches (§5.1). Wrapped in try-catch for
  // graceful degradation: if the CM6 bundle fails to initialize (e.g. an
  // unsupported browser API), fall back to a plain textarea inside #submission
  // so the site still runs — the learner can write and submit code, just without
  // syntax highlighting. getSubmission() reads either path transparently.
  try {
    editorView = new EditorView({
      doc: "",
      extensions: editorExtensions(runtime.language),
      parent: submissionEl,
    });
  } catch (err) {
    console.warn("CodeMirror 6 failed to initialize; falling back to textarea.", err);
    editorView = null;
    const fallback = document.createElement("textarea");
    fallback.spellcheck = false;
    submissionEl.replaceChildren(fallback);
  }
  // Expose the editor for rodney test access (null under graceful degradation).
  window.__bt.editorView = editorView;

  async function main() {
    setStatus("idle", "idle");
    // Disabled until the runtime has booted: a click before init would fail in
    // the runtime and surface as a spurious check failure.
    runButton.disabled = true;
    bootEl.textContent = `Booting ${runtime.name}…`;

    bt.lessons = await loadLessons();
    // Build the picker with the DOM Option API, never by parsing titles as HTML:
    // a lesson title comes from a course that may be untrusted (shared), so it
    // must never be parsed as HTML (the same threat model that keeps slugs off
    // the filesystem).
    selectEl.replaceChildren(
      ...bt.lessons.map((lesson, i) => new Option(lesson.title, String(i))),
    );
    selectEl.addEventListener("change", () => bt.selectLesson(Number(selectEl.value)));
    runButton.addEventListener("click", () => bt.runSubmission(bt.getSubmission()));
    if (bt.lessons.length > 0) {
      await bt.selectLesson(0);
    }

    await runtime.boot();
    runButton.disabled = false;
    bootEl.textContent = `${runtime.name} ready — pick a lesson, write your answer, run the checks.`;
  }

  // Expose the boot promise so the test probe can await readiness before driving.
  bt.ready = main().catch((err) => {
    bootEl.textContent = `${runtime.name} failed to boot: ` + err;
    throw err;
  });
  return bt.ready;
}
