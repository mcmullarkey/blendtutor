// blendtutor in-browser lesson runner — webR target (ADR-0008).
//
// The thin runtime adapter atop the shared core (`lesson-runner-core.js`): it
// boots webR and evaluates a learner submission plus the lesson's checks in R,
// entirely in the browser. All the lesson-loading, rendering, and pass/fail
// reporting is the shared core's job; this file owns only what is R-specific.
// No LLM is called here (BYOK feedback arrives in Slice 18).

import { start } from "./lesson-runner-core.js";
import { WebR } from "https://webr.r-wasm.org/latest/webr.mjs";

// Default (Automatic) channel: uses the fast SharedArrayBuffer transport when the
// page is cross-origin isolated (the coi-serviceworker achieves this on GitHub
// Pages), and degrades to a working channel otherwise — so a host where isolation
// fails still runs, just slower. Don't pin a channel; that would break the fallback.
const webR = new WebR();

start({
  name: "webR",
  language: "r",
  async boot() {
    await webR.init();
  },
  // Evaluate the submission and checks in a fresh R scope. Any R error (a failed
  // `stopifnot`, a parse error, an undefined symbol) is caught and reported as a
  // fail; clean evaluation is a pass.
  async run(code, checks) {
    const program = [code, ...checks].join("\n");
    const shelter = await new webR.Shelter();
    try {
      const { output } = await shelter.captureR(`local({\n${program}\n})`);
      return { output: output.map((line) => line.data).join("\n"), ok: true };
    } catch (err) {
      return { output: String(err && err.message ? err.message : err), ok: false };
    } finally {
      shelter.purge();
    }
  },
});
