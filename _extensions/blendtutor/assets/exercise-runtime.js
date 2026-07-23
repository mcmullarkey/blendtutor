// exercise-runtime.js — multi-exercise runtime core (AC-4).
//
// WHAT:  DOM scan, per-exercise registry, N CodeMirror editors, adapter injection.
// WHERE: _extensions/blendtutor/assets/exercise-runtime.js
// NOT:   NOT execution (delegated to adapter), NOT feedback (AC-7), NOT filter (AC-2).
//
// FORK of lesson-runner-core.js — kills 3 singletons:
//   1. Module-level `editorView` (line 147) → per-exercise editorView in registry entry.
//   2. `document.getElementById('submission')` (line 121) → per-exercise element from div.bt-exercise.
//   3. `window.__bt` (line 308) → `window.__btExercises` registry (Array + get(id)).
//
// The runtime adapter protocol is reused unchanged from lesson-runner-core.js:
//   { name, language, boot(), run(code, checks, packages) } → { output, ok }
//
// Registry entry shape (§1):
//   { id, editorView, element, payload, editorContainer,
//     getSubmission(), setEditorContent(code), setStatus(state, text),
//     runSubmission(), _running }
//
// HTML contract (from AC-2):
//   <div class="bt-exercise" data-language="r|python">
//     <script type="application/json">{ ... SiteLesson JSON ... }</script>
//   </div>

import {
  EditorView,
  r,
  python,
  syntaxHighlighting,
  defaultHighlightStyle,
  HighlightStyle,
  lineNumbers,
  highlightActiveLine,
  bracketMatching,
  indentWithTab,
  keymap,
  tags,
} from "./codemirror.js";

// ---------------------------------------------------------------------------
// CM6 extensions (reused from lesson-runner-core.js — identical configuration)
// ---------------------------------------------------------------------------

// Language extension lookup: closed set keyed by the runtime adapter's
// `language` field or the div's `data-language` attribute. An unknown
// language yields no extension (the editor still works, just without syntax
// highlighting). This is NOT a string match on `runtime.name` (§1.5).
const LANG_EXT = { r: r(), python: python() };

// Custom HighlightStyle with deterministic `.tok-*` class names. CM6's
// defaultHighlightStyle generates opaque unicode classes that the AC-2
// deterministic check cannot grep for; this style maps lezer tags to stable
// `.tok-*` classes so token spans are testable. Added AFTER the default style
// in the extension list so it takes precedence for the tags it defines.
const tokHighlightStyle = HighlightStyle.define([
  { tag: tags.keyword, class: "tok-keyword" },
  { tag: tags.variableName, class: "tok-variableName" },
  { tag: tags.string, class: "tok-string" },
  { tag: tags.number, class: "tok-number" },
  { tag: tags.comment, class: "tok-comment" },
  { tag: tags.function(tags.variableName), class: "tok-function" },
  { tag: tags.operator, class: "tok-operator" },
]);

// CM6 theme extension for cursor visibility (v4). Uses var(--bt-color-cursor)
// so the cursor adapts to light/dark mode.
const cursorTheme = EditorView.theme({
  ".cm-cursor": {
    borderLeftColor: "var(--bt-color-cursor, #ffffff)",
    borderLeftWidth: "2px",
    marginLeft: "-1px",
  },
  ".cm-content": {
    caretColor: "var(--bt-color-cursor, #ffffff)",
  },
});

// Build the full CM6 extension array for a given language — a PURE function
// (§2.1, §5.1): given a language, returns the Extension[] with no side effects.
function editorExtensions(language) {
  return [
    LANG_EXT[language] ?? [],
    syntaxHighlighting(defaultHighlightStyle),
    syntaxHighlighting(tokHighlightStyle),
    lineNumbers(),
    highlightActiveLine(),
    bracketMatching(),
    cursorTheme,
    EditorView.contentAttributes.of({ spellcheck: "false" }),
    keymap.of([indentWithTab]),
  ];
}

// ---------------------------------------------------------------------------
// Pure helpers (§2.1, §5.1) — testable without a browser
// ---------------------------------------------------------------------------

/**
 * Parse the JSON payload from a bt-exercise div's script[type=application/json].
 * Returns null on malformed JSON (isolation: one bad exercise does not break others).
 * @param {HTMLElement} div — The div.bt-exercise element.
 * @returns {Object|null} — Parsed payload, or null if missing/malformed.
 */
export function parsePayload(div) {
  const script = div.querySelector('script[type="application/json"]');
  if (!script) {
    console.warn("[blendtutor] Exercise div has no script[type=application/json], skipping.");
    return null;
  }
  try {
    return JSON.parse(script.textContent);
  } catch (err) {
    console.warn("[blendtutor] Malformed JSON in exercise div, skipping.", err);
    return null;
  }
}

/**
 * Scan the DOM for div.bt-exercise elements and parse their payloads.
 * Skips malformed JSON and duplicate IDs (skip+warn).
 * @returns {Array} — Array of { id, element, payload } entries (pre-registry).
 */
export function scanExercises() {
  const divs = document.querySelectorAll("div.bt-exercise");
  const entries = [];
  const seenIds = new Set();

  for (const div of divs) {
    const payload = parsePayload(div);
    if (!payload) continue; // malformed JSON — already warned in parsePayload

    const id = payload.id || `bt-exercise-${entries.length}`;
    if (seenIds.has(id)) {
      console.warn(`[blendtutor] Duplicate exercise ID "${id}", skipping.`);
      continue;
    }
    seenIds.add(id);

    entries.push({ id, element: div, payload });
  }

  return entries;
}

/**
 * Build a registry from scan entries. The registry is an Array with a get(id)
 * method for O(1) lookup by exercise ID.
 * @param {Array} entries — Array of { id, element, payload } from scanExercises().
 * @returns {Array} — Registry Array with .get(id) method.
 */
export function buildRegistry(entries) {
  const registry = [...entries];
  registry.get = function (id) {
    return this.find((e) => e.id === id) || null;
  };
  return registry;
}

// ---------------------------------------------------------------------------
// Effectful: mount + wire (§2.2)
// ---------------------------------------------------------------------------

/**
 * Mount a CodeMirror 6 editor for a single exercise. Creates a container div
 * inside the exercise element, then mounts the editor. On CM6 failure, falls
 * back to a textarea for THIS exercise only (graceful degradation, §1.5).
 * @param {Object} entry — Registry entry { id, element, payload }.
 * @param {string} language — Editor language ("r" | "python" | other).
 */
function mountEditor(entry, language) {
  const editorContainer = document.createElement("div");
  editorContainer.className = "bt-editor";
  entry.element.appendChild(editorContainer);
  entry.editorContainer = editorContainer;

  let editorView = null;
  try {
    // Test hook: data-cm-fail="true" simulates CM6 failure for testing.
    if (entry.element.dataset.cmFail === "true") {
      throw new Error("Simulated CM6 failure (data-cm-fail)");
    }
    editorView = new EditorView({
      doc: entry.payload.code_template || "",
      extensions: editorExtensions(language),
      parent: editorContainer,
    });
  } catch (err) {
    console.warn(
      `[blendtutor] CodeMirror 6 failed for exercise "${entry.id}", falling back to textarea.`,
      err,
    );
    editorView = null;
    const fallback = document.createElement("textarea");
    fallback.spellcheck = false;
    fallback.value = entry.payload.code_template || "";
    editorContainer.replaceChildren(fallback);
  }

  entry.editorView = editorView;
}

/**
 * Wire Check/Run buttons and per-exercise state for a single exercise.
 * Creates status + output elements inside the exercise div, and binds
 * getSubmission/setEditorContent/setStatus/runSubmission to the entry.
 * @param {Object} entry — Registry entry (mutated: adds methods + state).
 * @param {Object} runtime — Runtime adapter { name, language, boot(), run() }.
 */
function wireExercise(entry, runtime) {
  // Create UI elements inside the exercise div
  const controls = document.createElement("div");
  controls.className = "bt-controls";

  const runBtn = document.createElement("button");
  runBtn.textContent = "Run";
  runBtn.className = "bt-run-btn";

  controls.appendChild(runBtn);
  entry.element.appendChild(controls);

  const statusEl = document.createElement("div");
  statusEl.className = "bt-status";
  statusEl.dataset.status = "idle";
  statusEl.textContent = "idle";
  entry.element.appendChild(statusEl);

  const outputEl = document.createElement("div");
  outputEl.className = "bt-output";
  entry.element.appendChild(outputEl);

  // Per-exercise getSubmission — reads THIS exercise's editor (§3.4).
  // Falls back to textarea under graceful degradation.
  entry.getSubmission = function () {
    if (entry.editorView) {
      return entry.editorView.state.doc.toString();
    }
    const ta = entry.editorContainer.querySelector("textarea");
    return ta ? ta.value : "";
  };

  // Per-exercise setEditorContent — replaces THIS exercise's editor doc.
  entry.setEditorContent = function (code) {
    if (entry.editorView) {
      entry.editorView.dispatch({
        changes: { from: 0, to: entry.editorView.state.doc.length, insert: code },
      });
      return;
    }
    const ta = entry.editorContainer.querySelector("textarea");
    if (ta) ta.value = code;
  };

  // Per-exercise setStatus — updates THIS exercise's data-status only.
  entry.setStatus = function (state, text) {
    statusEl.dataset.status = state;
    statusEl.textContent = text ?? state;
  };

  // Per-exercise runSubmission — evaluates via the injected runtime adapter.
  // Concurrent run safety: rejects if already running (§5.3).
  entry._running = false;
  entry.runSubmission = async function () {
    if (entry._running) {
      console.warn(`[blendtutor] Exercise "${entry.id}" already running, ignoring concurrent request.`);
      return "fail";
    }
    entry._running = true;
    try {
      const code = entry.getSubmission();
      entry.setStatus("running", "running…");
      const { output, ok } = await runtime.run(
        code,
        entry.payload.checks ?? [],
        entry.payload.packages ?? [],
      );
      outputEl.textContent = output;
      entry.setStatus(ok ? "pass" : "fail", ok ? "pass" : "fail");
      return ok ? "pass" : "fail";
    } finally {
      entry._running = false;
    }
  };

  // Wire the Run button
  runBtn.addEventListener("click", () => {
    entry.runSubmission();
  });
}

// ---------------------------------------------------------------------------
// Main entry point — adapter injection seam (§3.2, §5.1)
// ---------------------------------------------------------------------------

/**
 * Boot the multi-exercise runtime. Mounts editors, wires buttons, boots the
 * runtime adapter, and exposes the registry as window.__btExercises.
 *
 * @param {Array} registry — Registry from buildRegistry(scanExercises()).
 * @param {Object} runtime — Runtime adapter { name, language, boot(), run() }.
 * @returns {Promise<Array>} — Resolves to the registry after runtime.boot().
 */
export async function start(registry, runtime) {
  // Mount editors and wire exercises
  for (const entry of registry) {
    const language = entry.element.dataset.language || runtime.language || "r";
    mountEditor(entry, language);
    wireExercise(entry, runtime);
  }

  // Expose registry globally (replaces window.__bt singleton)
  window.__btExercises = registry;

  // Boot the runtime adapter
  await runtime.boot();

  return registry;
}
