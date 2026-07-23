// mock-webr.mjs — Mock webR ES module for behavioral testing of webr-adapter.js.
//
// WHAT:  Configurable mock that simulates the webR API (init, newShelter,
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
// evalR is included so tests can verify the old evalR fallback (removed in
// commit 34ffe21) would have returned ok:true instead of ok:false. If someone
// re-adds the inner catch with evalR fallback, behavioral tests fail because
// evalR returns a result (ok:true) instead of propagating to outer catch (ok:false).

export class WebR {
  constructor() {
    this._initCalled = false;
  }

  async init() {
    this._initCalled = true;
  }

  newShelter() {
    const config = globalThis.__webrMockConfig || {};
    return {
      async captureR(code) {
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
        const resultStr = config.evalRResult || "";
        return {
          toString: async () => resultStr,
        };
      },
      async purge() {
        if (config.purgeThrows) {
          throw new Error(config.purgeThrows);
        }
      },
    };
  }

  async installPackages(pkgs) {
    const config = globalThis.__webrMockConfig || {};
    if (config.installPackagesThrows) {
      throw new Error(config.installPackagesThrows);
    }
  }
}

export default WebR;
