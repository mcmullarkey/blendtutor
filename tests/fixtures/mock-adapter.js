// mock-adapter.js — test-only runtime adapter for exercise-runtime.js (AC-4).
//
// WHAT:  Implements the runtime adapter protocol {name, language, boot(), run()}
//        with call recording for probe verification.
// WHERE: tests/fixtures/mock-adapter.js (test-only, never shipped to production).
// NOT:   Not a real runtime — no code execution, no webR/Pyodide. The mock
//        records every run() call so rodney probes can assert per-exercise
//        Check sends correct checks, per-exercise run isolation, etc.
//
// The mock adapter protocol matches lesson-runner-core.js's adapter contract:
//   - name     : label shown in boot status
//   - language : default editor language (overridden per-exercise by data-language)
//   - boot()   : async, resolves immediately (no real init needed)
//   - run(code, checks, packages) : async, returns {output, ok}
//
// Call recording: every run() call pushes {code, checks, packages} to .calls.
// The rodney probe reads adapter.calls to verify per-exercise isolation.

/**
 * Create a mock runtime adapter for testing.
 * @returns {Object} A mock adapter with call recording.
 */
export function createMockAdapter() {
  const calls = [];

  return {
    name: "mock",
    language: "r",
    calls,

    async boot() {
      // No-op — mock needs no initialization.
    },

    async run(code, checks, packages) {
      calls.push({ code, checks: [...checks], packages: [...packages] });
      // Simple mock logic: pass if code is non-empty, fail otherwise.
      const ok = code.trim().length > 0;
      const output = ok ? `[mock] executed: ${code.trim().slice(0, 50)}` : "[mock] empty submission";
      return { output, ok };
    },
  };
}
