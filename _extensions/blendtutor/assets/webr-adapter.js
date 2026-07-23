// webr-adapter.js — webR shared-boot adapter (AC-5).
//
// WHAT:  One webR instance per page, lazy boot, per-run Shelter isolation,
//        degraded channel support (no COI / SharedArrayBuffer required).
// WHERE: _extensions/blendtutor/assets/webr-adapter.js
// NOT:   NOT exercise UI (exercise-runtime.js), NOT filter (blendtutor.lua),
//        NOT prompt construction, NOT CodeMirror editor.
//
// Adapter protocol (matches exercise-runtime.js contract):
//   { name, language, boot(), run(code, checks, packages) } → { output, ok }
//
// Boot state machine (§1.2 — explicit states, not nullable):
//   uninitialized → booting → ready → failed
//
// Lazy boot (§2.1, §5.1):
//   boot() is a no-op — marks adapter as available but does NOT fetch WASM.
//   Actual webR init happens on first run() call via ensureBooted().
//   This ensures no webr.wasm fetch until the user clicks Run
//   (verified via performance.getEntriesByType('resource')).
//
// Boot promise dedup (§5.1):
//   ensureBooted() stores Promise<WebR>. Subsequent calls return the same
//   promise — no re-boot. The shared instance is reused across all exercises.
//
// Per-run Shelter isolation (§3.4):
//   Each run() creates a new Shelter via `await new webR.Shelter()`, evaluates
//   code, then shelter.purge() in finally. Variables do not leak between
//   exercises. Exercise 2's exists("x") returns FALSE because Exercise 1's x
//   was purged. The Shelter constructor is async — it returns a Promise that
//   resolves to the Shelter instance, so `await` is mandatory.
//
// Concurrent run guard (§5.3):
//   Adapter-level `running` flag. Second run() while first is executing →
//   returns { output: "Another exercise is already running…", ok: false }.
//
// CDN failure (resolved decision):
//   On boot failure, state → failed, error message "webR failed to boot: <error>".
//   Run button stays disabled. User must reload page to retry. No retry button.
//
// Degraded channel (§3.5):
//   webR works without SharedArrayBuffer (crossOriginIsolated === false).
//   Slower (single-threaded) but functional. No special handling needed —
//   webR detects the absence and falls back automatically.

// CDN URL for the webR ES module (latest version).
const WEBR_CDN_URL = "https://webr.r-wasm.org/latest/webr.mjs";

// Boot state enum (§1.2 — explicit, not nullable).
// These are observable behaviors, not internal implementation details.
const BootState = {
  UNINITIALIZED: "uninitialized",
  BOOTING: "booting",
  READY: "ready",
  FAILED: "failed",
};

/**
 * Create a webR runtime adapter with lazy boot and per-run Shelter isolation.
 *
 * @param {Object} [options] — Optional configuration.
 * @param {string} [options.cdnUrl] — CDN URL for webR ES module.
 * @param {function(string): void} [options.onBootProgress] — Callback for boot progress updates.
 * @returns {Object} Adapter { name, language, boot(), run(), getBootState() }.
 */
export function createWebRAdapter(options = {}) {
  const cdnUrl = options.cdnUrl ?? WEBR_CDN_URL;
  const onBootProgress = options.onBootProgress ?? (() => {});

  // Boot state — starts uninitialized, transitions through the state machine.
  let bootState = BootState.UNINITIALIZED;

  // The shared webR instance — set once during boot, reused across all runs.
  let webRInstance = null;

  // Boot promise dedup — stores the in-flight boot promise so subsequent
  // ensureBooted() calls return the same promise (§5.1).
  let bootPromise = null;

  // Concurrent run guard — prevents overlapping run() calls across exercises.
  let running = false;

  // Track installed packages (boot-phase only — installed once, shared).
  const installedPackages = new Set();

  /**
   * Ensure webR is booted. Called lazily on first run(), NOT on boot().
   *
   * State transitions:
   *   uninitialized → booting → ready (success)
   *   uninitialized → booting → failed (error)
   *
   * Dedup: if boot is in progress, returns the existing promise.
   * If already ready, returns the instance immediately.
   * If failed, throws an error.
   *
   * @returns {Promise<Object>} The webR instance.
   * @throws {Error} If boot has previously failed.
   */
  async function ensureBooted() {
    // Already ready — return immediately (shared instance, no re-boot).
    if (bootState === BootState.READY) {
      return webRInstance;
    }

    // Already failed — throw (user must reload page to retry).
    if (bootState === BootState.FAILED) {
      throw new Error("webR failed to boot. Reload the page to retry.");
    }

    // Boot in progress — return existing promise (dedup, §5.1).
    if (bootPromise) {
      return bootPromise;
    }

    // Start boot — transition to BOOTING.
    bootState = BootState.BOOTING;
    onBootProgress("Booting webR…");

    bootPromise = (async () => {
      try {
        // Dynamic import of the webR ES module from CDN.
        const WebRModule = await import(cdnUrl);

        // The WebR class is the default or named export.
        const WebR = WebRModule.WebR ?? WebRModule.default;

        // Create and initialize the webR instance.
        // init() fetches the WASM binary and starts the R worker.
        const webR = new WebR();
        await webR.init();

        webRInstance = webR;
        bootState = BootState.READY;
        onBootProgress("webR ready.");
        return webR;
      } catch (err) {
        // Boot failed — transition to FAILED state.
        // Reset bootPromise so a page reload can retry.
        bootState = BootState.FAILED;
        bootPromise = null;
        const errMsg = err instanceof Error ? err.message : String(err);
        throw new Error(`webR failed to boot: ${errMsg}`);
      }
    })();

    return bootPromise;
  }

  return {
    name: "webr",
    language: "r",

    /**
     * Boot the adapter. Called by exercise-runtime.js start().
     *
     * LAZY BOOT: This is a no-op. The actual webR initialization (WASM fetch,
     * worker startup) is deferred to the first run() call via ensureBooted().
     * This ensures no webr.wasm fetch occurs until the user clicks Run,
     * verifiable via performance.getEntriesByType('resource').
     *
     * @returns {Promise<void>} Resolves immediately (no-op).
     */
    async boot() {
      // Intentionally empty — lazy boot.
      // WASM is fetched on first run() via ensureBooted().
    },

    /**
     * Run R code in a per-run Shelter.
     *
     * First call triggers lazy boot (shows boot progress via callback).
     * Subsequent calls reuse the shared webR instance (no re-boot).
     * Concurrent calls are rejected (§5.3).
     *
     * Per-run Shelter isolation (§3.4): each call creates a new
     * `await new webR.Shelter()`, evaluates the code, captures output, then
     * purges the Shelter in finally. Variables do not leak between exercises.
     *
     * @param {string} code — R code to evaluate.
     * @param {string[]} [checks=[]] — Check expressions (unused by webR adapter,
     *   included for protocol conformance).
     * @param {string[]} [packages=[]] — R packages to install (boot-phase only —
     *   installed once on first run, shared across all exercises).
     * @returns {Promise<{output: string, ok: boolean}>} The R output and success flag.
     */
    async run(code, checks = [], packages = []) {
      // Concurrent run guard (§5.3) — reject if another exercise is running.
      if (running) {
        return {
          output: "Another exercise is already running. Please wait for it to finish.",
          ok: false,
        };
      }

      running = true;
      let shelter = null;

      try {
        // Lazy boot — first run triggers WASM fetch via ensureBooted().
        // Subsequent runs return immediately (shared instance).
        const webR = await ensureBooted();

        // Install packages (boot-phase only — first run).
        // Packages are installed once and shared across all exercises.
        const newPackages = packages.filter((p) => !installedPackages.has(p));
        if (newPackages.length > 0) {
          onBootProgress(`Installing packages: ${newPackages.join(", ")}…`);
          for (const pkg of newPackages) {
            // Security: use webR.installPackages() only — NEVER interpolate
            // package names into R source code. String interpolation of
            // package names into R install calls would allow RCE if a
            // package name contains malicious R code.
            if (typeof webR.installPackages !== "function") {
              throw new Error(
                `webR.installPackages is not available — cannot install package "${pkg}". ` +
                  "Ensure you are using a supported webR version.",
              );
            }
            await webR.installPackages([pkg]);
            installedPackages.add(pkg);
          }
        }

        // Per-run Shelter isolation (§3.4).
        // Each run gets a fresh Shelter — variables from one exercise
        // do not leak into another. Exercise 2's exists("x") returns FALSE
        // because Exercise 1's x was purged after its run completed.
        //
        // `new webR.Shelter()` is the documented API. The Shelter constructor
        // is async — it returns a Promise that resolves to the Shelter
        // instance (with captureR, purge methods). The `await` is mandatory;
        // without it, `shelter` is an unresolved Promise and captureR/purge
        // are undefined, causing "shelter.captureR is not a function" errors.
        // This matches the reference implementation in
        // crates/core/assets/webr/lesson-runner.js (line 32).
        shelter = await new webR.Shelter();

        // Wrap code in local({ ... }) for environment isolation.
        //
        // webR Shelter provides object-lifecycle tracking (purge frees R
        // objects), but does NOT provide environment isolation — variables
        // created with `<-` in captureR are assigned to the global R
        // environment and persist across Shelter boundaries. Without local(),
        // Exercise 1's `x <- 5` leaks into Exercise 2's `exists("x")` → TRUE.
        //
        // local() evaluates code in a fresh R environment, so variables
        // created inside do not persist. This matches the reference
        // implementation in crates/core/assets/webr/lesson-runner.js (line 37).
        const isolatedCode = `local({\n${code}\n})`;

        // Capture output from R code evaluation.
        // captureR wraps the code in capture.output() and returns
        // { result: RObject, output: Array<{type, data}> }.
        let outputText = "";
        const captured = await shelter.captureR(isolatedCode);
        // output is an array of { type: 'stdout'|'stderr'|'message'|'warning', data: string }
        if (captured.output && captured.output.length > 0) {
          outputText = captured.output.map((o) => o.data).join("\n");
        }

        return { output: outputText, ok: true };
      } catch (err) {
        // Return error message in output, ok=false.
        const errMsg = err instanceof Error ? err.message : String(err);
        return {
          output: `Error: ${errMsg}`,
          ok: false,
        };
      } finally {
        // Purge Shelter — variables do not leak (§3.4).
        // Best-effort: if purge fails, we still release the running flag.
        if (shelter) {
          try {
            await shelter.purge();
          } catch {
            // Best-effort cleanup — ignore purge errors.
          }
        }
        running = false;
      }
    },

    /**
     * Get the current boot state (for testing/debugging).
     * @returns {string} Current boot state: 'uninitialized' | 'booting' | 'ready' | 'failed'.
     */
    getBootState() {
      return bootState;
    },
  };
}
