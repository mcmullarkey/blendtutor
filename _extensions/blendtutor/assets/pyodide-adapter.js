// pyodide-adapter.js — Pyodide shared-boot adapter (AC-6).
//
// WHAT:  One Pyodide instance per page, lazy boot, per-run fresh globals.
// WHERE: _extensions/blendtutor/assets/pyodide-adapter.js
// NOT:   NOT webR (AC-5), NOT LLM feedback (AC-7), NOT DOM scanning (AC-4),
//        NOT filter (AC-2). This module owns Pyodide lifecycle only (§4.1).
//
// Implements the runtime adapter protocol from AC-4:
//   { name, language, boot(), run(code, checks, packages) } → { output, ok }
//
// Boot state: 3-state machine (null → Promise → Pyodide) (§1.2, §5.1).
// Per-run: pyodide.toPy({}) creates fresh globals namespace (§2.1).
// CDN: classic <script> tag for pyodide.js (NOT ES module — loadPyodide is
//      a global function, not an ES export) (§3.4).
// Cleanup: namespace.destroy() in finally block (§1.3.1).
// Coexistence: webR uses ES module import, Pyodide uses classic script —
//              different boot mechanisms, zero shared state (§3.4).

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/** Pinned CDN URL for pyodide.js (classic script, not ES module). */
const PYODIDE_CDN = "https://cdn.jsdelivr.net/pyodide/v0.27.2/full/pyodide.js";

/** Base URL for Pyodide indexURL (WASM + packages). */
const PYODIDE_BASE = "https://cdn.jsdelivr.net/pyodide/v0.27.2/full/";

// ---------------------------------------------------------------------------
// Boot state — 3-state machine (§1.2, §5.1)
// ---------------------------------------------------------------------------

/**
 * Boot promise state machine:
 *   null    → not yet booted
 *   Promise → booting (pending)
 *   (resolved value accessed via the promise)
 *
 * Idempotent: bootPyodide() returns the same promise on subsequent calls.
 * @type {Promise<Pyodide>|null}
 */
let bootPromise = null;

/**
 * hasDoneSetup guard — prevents double CDN injection.
 * Set to true after the first ensureCDN() call, so subsequent calls are no-ops.
 * @type {boolean}
 */
let hasDoneSetup = false;

// ---------------------------------------------------------------------------
// CDN injection (§3.4 — effectful shell)
// ---------------------------------------------------------------------------

/**
 * Ensure the pyodide.js CDN script tag is present in the document head.
 *
 * Idempotent: checks hasDoneSetup and existing script tags before injecting.
 * Uses a classic <script> tag (NOT type=module) because loadPyodide is a
 * global function, not an ES module export.
 */
function ensureCDN() {
  if (hasDoneSetup) return;

  // Check if script tag already exists (e.g., injected by Lua filter).
  const existing = document.querySelector(`script[src="${PYODIDE_CDN}"]`);
  if (existing) {
    hasDoneSetup = true;
    return;
  }

  // Inject classic script tag (NOT ES module — loadPyodide is a global).
  const script = document.createElement("script");
  script.src = PYODIDE_CDN;
  document.head.appendChild(script);
  hasDoneSetup = true;
}

/**
 * Wait for loadPyodide to be available on the global scope.
 *
 * Polls every 50ms until loadPyodide is defined or timeout is reached.
 * This handles the async nature of classic script loading.
 *
 * @param {number} timeout - Timeout in milliseconds (default 30s).
 * @returns {Promise<void>} - Resolves when loadPyodide is available.
 * @throws {Error} If loadPyodide is undefined after timeout.
 */
function waitForLoadPyodide(timeout = 30000) {
  return new Promise((resolve, reject) => {
    const start = Date.now();
    function check() {
      if (typeof loadPyodide !== "undefined") {
        resolve();
      } else if (Date.now() - start > timeout) {
        reject(
          new Error("loadPyodide is undefined — CDN script failed to load"),
        );
      } else {
        setTimeout(check, 50);
      }
    }
    check();
  });
}

// ---------------------------------------------------------------------------
// Boot — idempotent 3-state machine (§2.2, §5.1)
// ---------------------------------------------------------------------------

/**
 * Boot Pyodide. Idempotent: returns the same promise on subsequent calls.
 *
 * State transitions:
 *   null → Promise (pending) → Pyodide (resolved)
 *
 * The promise is stored in bootPromise so all callers share the same boot.
 * If boot fails, bootPromise is reset to null so a retry is possible.
 *
 * @returns {Promise<Pyodide>} - The booted Pyodide instance.
 */
function bootPyodide() {
  if (bootPromise !== null) {
    return bootPromise;
  }

  ensureCDN();

  bootPromise = waitForLoadPyodide().then(() =>
    loadPyodide({ indexURL: PYODIDE_BASE }),
  );

  // Reset on failure so a retry is possible.
  bootPromise.catch(() => {
    bootPromise = null;
  });

  return bootPromise;
}

// ---------------------------------------------------------------------------
// Pure helpers (§2.1, §5.1)
// ---------------------------------------------------------------------------

/**
 * Create a fresh globals namespace for per-run isolation.
 *
 * Uses pyodide.toPy({}) to create a Python dict that serves as a fresh
 * globals namespace. Each run gets its own namespace, so variables from
 * one run are not visible in the next.
 *
 * @param {Pyodide} pyodide - The Pyodide instance.
 * @returns {PyProxy} - A fresh Python dict namespace.
 */
function createNamespace(pyodide) {
  return pyodide.toPy({});
}

/**
 * Normalize an error into a human-readable message.
 *
 * @param {Error|string} err - The error to normalize.
 * @returns {string} - The error message.
 */
function normalizeError(err) {
  if (err instanceof Error) {
    return err.message;
  }
  return String(err);
}

// ---------------------------------------------------------------------------
// Adapter — implements {name, language, boot(), run()} protocol (§3.2)
// ---------------------------------------------------------------------------

/**
 * Pyodide shared-boot adapter.
 *
 * Implements the runtime adapter protocol from AC-4:
 *   { name, language, boot(), run(code, checks, packages) } → { output, ok }
 *
 * - name: "pyodide" — label for boot status display
 * - language: "python" — default editor language
 * - boot(): idempotent, boots Pyodide once per page
 * - run(): per-run fresh globals, cumulative packages, PyProxy cleanup
 *
 * @type {{ name: string, language: string, boot: Function, run: Function }}
 */
export const pyodideAdapter = {
  name: "pyodide",
  language: "python",

  /**
   * Boot Pyodide. Idempotent — safe to call multiple times.
   *
   * Delegates to bootPyodide() which implements the 3-state machine.
   * Does NOT load WASM until the first call (lazy trigger, clause 3).
   *
   * @returns {Promise<void>}
   */
  async boot() {
    await bootPyodide();
  },

  /**
   * Run Python code with per-run fresh globals.
   *
   * Flow:
   *   1. Boot Pyodide (idempotent — reuses existing instance)
   *   2. Load packages (cumulative — packages persist across runs)
   *   3. Create fresh namespace (per-run isolation)
   *   4. Execute code in namespace
   *   5. Run checks in same namespace
   *   6. Destroy namespace in finally (PyProxy cleanup)
   *
   * Error surface (clause 5):
   *   - Boot failure → { output: "Boot error: ...", ok: false }
   *   - Package failure → { output: "Package error: ...", ok: false }
   *   - Python error → { output: "Python error: ...", ok: false }
   *   - Check failure → { output: "Check error: ...", ok: false }
   *   - loadPyodide undefined → caught by boot failure
   *
   * @param {string} code - The Python code to execute.
   * @param {string[]} checks - Check expressions to run after code.
   * @param {string[]} packages - Pyodide packages to load (cumulative).
   * @returns {Promise<{output: string, ok: boolean}>}
   */
  async run(code, checks, packages) {
    // 1. Boot Pyodide (idempotent)
    let pyodide;
    try {
      pyodide = await bootPyodide();
    } catch (err) {
      return { output: `Boot error: ${normalizeError(err)}`, ok: false };
    }

    // 2. Load packages (cumulative — packages persist across runs, globals are fresh)
    if (packages && packages.length > 0) {
      try {
        await pyodide.loadPackage(packages);
      } catch (err) {
        return { output: `Package error: ${normalizeError(err)}`, ok: false };
      }
    }

    // 3-6. Per-run fresh globals + PyProxy cleanup in finally
    let namespace = null;
    try {
      // Create fresh namespace for per-run isolation (clause 4)
      namespace = createNamespace(pyodide);

      // 4. Execute code in the fresh namespace
      try {
        pyodide.runPython(code, { globals: namespace });
      } catch (err) {
        return { output: `Python error: ${normalizeError(err)}`, ok: false };
      }

      // 5. Run checks in the same namespace
      for (const check of checks) {
        try {
          pyodide.runPython(check, { globals: namespace });
        } catch (err) {
          return { output: `Check error: ${normalizeError(err)}`, ok: false };
        }
      }

      return { output: "ok", ok: true };
    } finally {
      // 6. PyProxy cleanup — always destroy the namespace (clause 6)
      if (namespace) {
        namespace.destroy();
      }
    }
  },

  /**
   * Check whether boot has been initiated (lazy trigger, clause 3).
   *
   * Returns true once bootPyodide() has been called (bootPromise !== null).
   * Before the first boot()/run() call, returns false — confirming no WASM
   * is loaded until explicitly triggered.
   *
   * @returns {boolean} — true if boot has been initiated.
   */
  isBooted() {
    return bootPromise !== null;
  },

  /**
   * Get the booted Pyodide instance (for testing/debugging, clause 6).
   *
   * Returns the resolved Pyodide instance from the boot promise. Allows
   * probes to spy on pyodide.toPy to verify namespace.destroy() is called.
   *
   * @returns {Promise<Pyodide>} — The booted Pyodide instance.
   */
  async getPyodide() {
    return await bootPyodide();
  },
};
