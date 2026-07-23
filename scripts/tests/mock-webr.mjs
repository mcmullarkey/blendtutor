// mock-webr.mjs — Mock webR ES module for behavioral testing of webr-adapter.js.
//
// WHAT:  Configurable mock that simulates the webR API (init, Shelter,
//        captureR, evalR, purge, installPackages) without fetching WASM.
// WHERE: scripts/tests/mock-webr.mjs
// NOT:   NOT a real webR — no R evaluation, no WASM. For adapter contract tests only.
//
// Configuration via globalThis.__webrMockConfig:
//   captureRResult:        { output: Array<{type, data}> } — returned by captureR
//   captureRThrows:        string — if set, captureR throws Error(captureRThrows)
//   installPackagesThrows: string — if set, installPackages throws Error(...)
//   purgeThrows:           string — if set, purge throws Error(purgeThrows)
//   evalRResult:           string — if set, evalR returns a mock RObject whose
//                          toString() returns this value.
//
// Side-effect: each captureR call records the code argument in
// globalThis.__webrMockCaptureRCalls (Array<string>) so tests can verify
// the adapter wraps code in local({ ... }).
//
// evalR is included so tests can verify the old evalR fallback (removed in
// commit 34ffe21) would have returned ok:true instead of ok:false. If someone
// re-adds the inner catch with evalR fallback, behavioral tests fail because
// evalR returns a result (ok:true) instead of propagating to outer catch (ok:false).
//
// Shelter is modeled as an async constructor: `new webR.Shelter()` returns a
// Promise that resolves to the shelter instance (matching the real webR API).
// The `await` in `await new webR.Shelter()` is mandatory — without it, the
// adapter gets an unresolved Promise and captureR/purge are undefined.

// Shelter class — constructor returns a Promise (async constructor pattern).
// This mirrors the real webR Shelter: `new webR.Shelter()` returns a Promise
// that resolves to { captureR, evalR, purge }.
class Shelter {
  constructor() {
    const config = globalThis.__webrMockConfig || {};
    return Promise.resolve({
      async captureR(code, options = {}) {
        // Record the code argument and options so tests can verify env-based
        // isolation (not local() wrapper).
        if (!globalThis.__webrMockCaptureRCalls) {
          globalThis.__webrMockCaptureRCalls = [];
        }
        globalThis.__webrMockCaptureRCalls.push({ code, options });
        if (config.captureRThrows) {
          throw new Error(config.captureRThrows);
        }
        return (
          config.captureRResult || {
            output: [],
            result: { type: "integer", names: null, values: [] },
          }
        );
      },
      async evalR(code) {
        // Record evalR calls so tests can verify fresh env creation.
        if (!globalThis.__webrMockEvalRCalls) {
          globalThis.__webrMockEvalRCalls = [];
        }
        globalThis.__webrMockEvalRCalls.push(code);
        if (config.evalRThrows) {
          throw new Error(config.evalRThrows);
        }
        // Return a mock env object. The adapter passes this to captureR's
        // `env` option. The mock just needs to be truthy and have a toString.
        const resultStr = config.evalRResult || "";
        return {
          toString: async () => resultStr,
          // Marker so tests can identify the fresh env object.
          __isMockEnv: true,
        };
      },
      async purge() {
        if (config.purgeThrows) {
          throw new Error(config.purgeThrows);
        }
      },
    });
  }
}

export class WebR {
  constructor() {
    this._initCalled = false;
    // Expose Shelter class on the instance — matches real webR API where
    // `new webR.Shelter()` is the documented way to create a Shelter.
    this.Shelter = Shelter;
  }

  async init() {
    this._initCalled = true;
  }

  async installPackages(pkgs) {
    const config = globalThis.__webrMockConfig || {};
    if (config.installPackagesThrows) {
      throw new Error(config.installPackagesThrows);
    }
  }
}

export default WebR;
