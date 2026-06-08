// blendtutor in-browser lesson runner (webR target, ADR-0008).
//
// Loads the lessons emitted by `blendtutor build` (the SiteLesson JSON contract),
// boots webR, and runs a learner submission plus the lesson's checks entirely in
// the browser. Pass = the submission and every check evaluate without an R error;
// fail = any of them raises. The grading lives client-side; no LLM is called here
// (BYOK feedback arrives in Slice 18).
//
// The `window.__bt` handle (lessons / selectLesson / runSubmission / ready) is the
// test seam the rodney browser probe drives.

import { WebR } from "https://webr.r-wasm.org/latest/webr.mjs";

const statusEl = document.querySelector("[data-test=lesson-status]");
const bootEl = document.getElementById("boot-status");
const titleEl = document.getElementById("lesson-title");
const promptEl = document.getElementById("lesson-prompt");
const submissionEl = document.getElementById("submission");
const selectEl = document.getElementById("lesson-select");
const outputEl = document.getElementById("output");
const runButton = document.getElementById("run");

function setStatus(state, text) {
  statusEl.dataset.state = state;
  statusEl.textContent = text ?? state;
}

const webR = new WebR();

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

const bt = {
  lessons: [],
  current: 0,

  async selectLesson(index) {
    this.current = index;
    renderLesson(this.lessons[index]);
  },

  // Evaluate the submission and the current lesson's checks in a fresh R scope.
  // Any R error (a failed `stopifnot`, a parse error, an undefined symbol) rejects
  // and is reported as a fail; clean evaluation is a pass.
  async runSubmission(code) {
    const lesson = this.lessons[this.current];
    setStatus("running", "running…");
    const program = [code, ...(lesson.checks ?? [])].join("\n");
    const shelter = await new webR.Shelter();
    try {
      const { output } = await shelter.captureR(`local({\n${program}\n})`);
      outputEl.textContent = output.map((line) => line.data).join("\n");
      setStatus("pass", "pass");
      return "pass";
    } catch (err) {
      outputEl.textContent = String(err && err.message ? err.message : err);
      setStatus("fail", "fail");
      return "fail";
    } finally {
      shelter.purge();
    }
  },
};

window.__bt = bt;

async function main() {
  setStatus("idle", "idle");
  // Disabled until webR has booted: a click before init would fail in the R
  // shelter and surface as a spurious check failure.
  runButton.disabled = true;

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

  await webR.init();
  runButton.disabled = false;
  bootEl.textContent = "webR ready — pick a lesson, write your answer, run the checks.";
}

// Expose the boot promise so the test probe can await readiness before driving.
bt.ready = main().catch((err) => {
  bootEl.textContent = "webR failed to boot: " + err;
  throw err;
});
