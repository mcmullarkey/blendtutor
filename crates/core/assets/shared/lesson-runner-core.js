// blendtutor in-browser lesson runner — shared core (ADR-0008).
//
// The cross-target scaffolding both the webR and the Pyodide runners reuse: it
// loads the lessons emitted by `blendtutor build` (the SiteLesson JSON contract),
// renders the lesson UI, wires the controls, and reports pass/fail — everything
// except *which* runtime executes the code. A target's own `lesson-runner.js`
// supplies that as a small `runtime` adapter and calls `start(runtime)`; this is
// the §5.1 cut that keeps language-specific wiring minimal atop shared assembly.
//
// A `runtime` adapter is `{ name, boot(), run(code, checks) }`:
//   - `name`    : label shown in the boot status line (e.g. "webR", "Pyodide").
//   - `boot()`  : async, initializes the runtime; resolve when ready to run.
//   - `run(code, checks)` : async, evaluates the submission followed by the
//                 lesson's checks and resolves to `{ output, ok }` — `ok` true
//                 when nothing raised (a pass), false when anything did (a fail).
//                 The core never inspects the language; the adapter owns grading.
//
// The `window.__bt` handle (lessons / selectLesson / runSubmission / ready) is the
// test seam the rodney browser probe drives, identical across targets.

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

function renderLesson(lesson) {
  titleEl.textContent = lesson.title;
  promptEl.textContent = lesson.prompt;
  submissionEl.value = lesson.code_template ?? "";
  outputEl.textContent = "";
  setStatus("idle", "idle");
}

// Build the shared lesson state, bound to a runtime adapter. Grading itself lives
// in `runtime.run`; this object owns only selection and the pass/fail reporting
// that is identical for every language.
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
  };
}

/// Boot a built lesson site against `runtime`. Loads the lessons, wires the UI,
/// then initializes the runtime — exposing `window.__bt` for the test probe.
export async function start(runtime) {
  const bt = makeBt(runtime);
  window.__bt = bt;

  async function main() {
    setStatus("idle", "idle");
    // Disabled until the runtime has booted: a click before init would fail in
    // the runtime and surface as a spurious check failure.
    runButton.disabled = true;
    bootEl.textContent = `Booting ${runtime.name}…`;

    bt.lessons = await loadLessons();
    // Build the picker with the DOM Option API, never innerHTML: a lesson title
    // comes from a course that may be untrusted (shared), so it must never be
    // parsed as HTML (the same threat model that keeps slugs off the filesystem).
    selectEl.replaceChildren(
      ...bt.lessons.map((lesson, i) => new Option(lesson.title, String(i))),
    );
    selectEl.addEventListener("change", () => bt.selectLesson(Number(selectEl.value)));
    runButton.addEventListener("click", () => bt.runSubmission(submissionEl.value));
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
