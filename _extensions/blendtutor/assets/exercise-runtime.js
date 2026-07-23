// exercise-runtime.js — multi-exercise runtime core (AC-4) + UX polish (AC-8).
//
// WHAT:  DOM scan, per-exercise registry, N CodeMirror editors, adapter injection,
//        hints <details> toggle, solution reveal, check button conditional,
//        Run button disable/enable, data-status state machine.
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
 * Parse bullet-point text into a <ul> with <li> elements. Each line starting
 * with "- " or "* " (after trimming leading whitespace) becomes a list item;
 * the marker is stripped and the remaining text is set via textContent (never
 * parsed as HTML — untrusted lesson content). Returns null when no bullet lines
 * are found, so the caller can fall back to a <p> with textContent.
 *
 * Ported from lesson-runner-core.js (byte-identical logic, §4.2 reuse).
 * @param {string} text — The hints/gotchas text to parse.
 * @returns {HTMLUListElement|null} — A <ul> with <li> items, or null.
 */
function renderBullets(text) {
  const bullets = text
    .split("\n")
    .filter((line) => {
      const trimmed = line.trimStart();
      return trimmed.startsWith("- ") || trimmed.startsWith("* ");
    })
    .map((line) => line.trimStart().slice(2).trim());
  if (bullets.length === 0) {
    return null;
  }
  const ul = document.createElement("ul");
  for (const bullet of bullets) {
    const li = document.createElement("li");
    li.textContent = bullet;
    ul.appendChild(li);
  }
  return ul;
}

/**
 * Render the exercise's hints as an expandable <details> panel inside the
 * exercise div, before the controls. Created only when payload.hints is
 * non-null and non-empty (clause 1: hints visible/absent). Uses textContent
 * (never parsed as HTML) — hints are untrusted lesson content.
 *
 * Bullet-point content ("- " or "* " prefixed lines) renders as <ul><li>;
 * plain text falls back to a <p> with textContent.
 * @param {Object} entry — Registry entry (mutated: hints element appended).
 */
function renderHintsForExercise(entry) {
  const hints = entry.payload.hints;
  if (!hints || !hints.trim()) {
    return;
  }
  const details = document.createElement("details");
  details.className = "bt-hints";
  const summary = document.createElement("summary");
  summary.textContent = "Hints";
  details.appendChild(summary);
  const ul = renderBullets(hints);
  if (ul) {
    details.appendChild(ul);
  } else {
    const body = document.createElement("p");
    body.textContent = hints;
    details.appendChild(body);
  }
  entry.element.appendChild(details);
}

/**
 * Wire Check/Run buttons and per-exercise state for a single exercise.
 * Creates hints <details>, controls (Run + Check + Solution buttons), status,
 * and output elements inside the exercise div, and binds
 * getSubmission/setEditorContent/setStatus/runSubmission to the entry.
 *
 * UX polish (AC-8):
 *   - Hints <details> toggle rendered before controls (clause 1).
 *   - Check button only when payload.checks is non-empty (clause 3).
 *   - Solution button only when payload.solution is non-null (clause 2).
 *   - Run button disabled during runSubmission, re-enabled in finally (clauses 4+7).
 *   - data-status closed set: idle → running → (pass | fail) (clause 6).
 *
 * @param {Object} entry — Registry entry (mutated: adds methods + state).
 * @param {Object} runtime — Runtime adapter { name, language, boot(), run() }.
 */
function wireExercise(entry, runtime) {
  // Render hints <details> before controls (clause 1: hints visible/absent)
  renderHintsForExercise(entry);

  // Create UI elements inside the exercise div
  const controls = document.createElement("div");
  controls.className = "bt-controls";

  const runBtn = document.createElement("button");
  runBtn.textContent = "Run";
  runBtn.className = "bt-run-btn";

  controls.appendChild(runBtn);

  // Check button — only when the exercise has checks (clause 3: absent when
  // no checks). The button runs the same submission flow as Run.
  if (entry.payload.checks && entry.payload.checks.length > 0) {
    const checkBtn = document.createElement("button");
    checkBtn.textContent = "Check";
    checkBtn.className = "bt-check-btn";
    controls.appendChild(checkBtn);
    checkBtn.addEventListener("click", () => {
      entry.runSubmission();
    });
  }

  // Solution button — only when the exercise has a solution (clause 2).
  // Clicking it inserts the solution text into the editor via setEditorContent.
  if (entry.payload.solution) {
    const solutionBtn = document.createElement("button");
    solutionBtn.textContent = "Show solution";
    solutionBtn.className = "bt-solution-btn";
    controls.appendChild(solutionBtn);
    solutionBtn.addEventListener("click", () => {
      entry.setEditorContent(entry.payload.solution);
    });
  }

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
  // The closed set is { idle, running, pass, fail } (clause 6, §1.2).
  entry.setStatus = function (state, text) {
    statusEl.dataset.status = state;
    statusEl.textContent = text ?? state;
  };

  // Per-exercise runSubmission — evaluates via the injected runtime adapter.
  // Concurrent run safety: rejects if already running (§5.3).
  // Disables THIS exercise's Run button only (clause 4: per-exercise, not
  // singleton). Re-enables in finally (clause 7: buttons re-enabled on pass).
  entry._running = false;
  entry.runSubmission = async function () {
    if (entry._running) {
      console.warn(`[blendtutor] Exercise "${entry.id}" already running, ignoring concurrent request.`);
      return "fail";
    }
    entry._running = true;
    runBtn.disabled = true;
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
      runBtn.disabled = false;
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
