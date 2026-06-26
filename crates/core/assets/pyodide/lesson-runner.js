// blendtutor in-browser lesson runner — Pyodide target (ADR-0008).
//
// The thin runtime adapter atop the shared core (`lesson-runner-core.js`): it
// boots Pyodide and evaluates a learner submission plus the lesson's checks in
// Python, entirely in the browser. All the lesson-loading, rendering, and
// pass/fail reporting is the shared core's job; this file owns only what is
// Python-specific. No LLM is called here (BYOK feedback arrives in Slice 18).

import { start } from "./lesson-runner-core.js";

// `loadPyodide` is the global exposed by the `pyodide.js` classic script that
// index.html loads before this module — so the runtime download starts as soon
// as the page parses, and `boot()` only has to await it.
let pyodide;

start({
  name: "Pyodide",
  language: "python",
  async boot() {
    pyodide = await loadPyodide();
  },
  // Evaluate the submission and checks in a fresh namespace so a prior run never
  // leaks definitions into the next. Any Python error (a failed `assert`, a
  // SyntaxError, a NameError) is caught and reported as a fail; clean evaluation
  // is a pass. CPython seeds `__builtins__` into a fresh globals dict, so an empty
  // namespace still has the standard builtins available.
  async run(code, checks) {
    const program = [code, ...checks].join("\n");
    const namespace = pyodide.toPy({});
    let result;
    try {
      result = await pyodide.runPythonAsync(program, { globals: namespace });
      // A clean run ending in the checks returns None (-> undefined); any other
      // trailing value is shown for context via its str() (PyProxy.toString()).
      const output = result == null ? "All checks passed." : result.toString();
      return { output, ok: true };
    } catch (err) {
      return { output: String(err && err.message ? err.message : err), ok: false };
    } finally {
      // Destroy a PyProxy result (a non-primitive trailing value) so it never
      // leaks; a primitive converts to JS and has no `destroy`. Then drop the scope.
      if (result != null && typeof result.destroy === "function") {
        result.destroy();
      }
      namespace.destroy();
    }
  },
});
