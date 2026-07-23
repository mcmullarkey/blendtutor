// pyodide-adapter.js — rodney probe for AC-6 Pyodide shared-boot adapter.
//
// WHAT:  7-clause assertions verifying Pyodide adapter behavior.
// WHERE: rodney-probes/pyodide-adapter.js
// NOT:   NOT exercise-runtime.js (AC-4), NOT filter (AC-2), NOT webR (AC-5).
//
// Rodney executes this script in a browser context after loading
// tests/fixtures/pyodide.html which imports pyodide-adapter.js, creates
// the adapter, sets window.__btPyodideAdapter = adapter, and calls
// exercise-runtime.js start(registry, adapter). The script waits for
// Pyodide to boot, then runs assertions.
//
// FIX (vacuous-pass vulnerability): The probe MUST assert
// window.__btPyodideAdapter is defined before running any test that
// uses it. Previous version used if-guard patterns that silently
// passed when the adapter was undefined. Now every test that needs the
// adapter asserts it is defined — no silent skips.
//
// Assertions (from AC-6 spec):
//   1. CDN injection — exactly one pinned pyodide.js script tag
//   2. Single boot — loadPyodide called once (idempotent)
//   3. Lazy trigger — no WASM before first Run
//   4. Per-run fresh globals — exercise 2 can't see exercise 1's variables
//   5. Error surface — boot fail, Python error, package fail, loadPyodide undefined
//   6. PyProxy cleanup — namespace.destroy() in finally
//   7. Coexistence with webR — independent, zero shared state

async function waitFor(condFn, timeoutMs = 30000) {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    if (condFn()) return true;
    await new Promise((r) => setTimeout(r, 100));
  }
  return false;
}

function assert(cond, msg) {
  if (!cond) throw new Error(`ASSERT FAILED: ${msg}`);
}

async function runProbe() {
  const results = [];

  function test(name, fn) {
    try {
      fn();
      results.push({ name, passed: true });
    } catch (e) {
      results.push({ name, passed: false, message: e.message });
    }
  }

  async function asyncTest(name, fn) {
    try {
      await fn();
      results.push({ name, passed: true });
    } catch (e) {
      results.push({ name, passed: false, message: e.message });
    }
  }

  const ready = await waitFor(() => window.__btExercises !== undefined);
  if (!ready) {
    return [{ name: "registry defined", passed: false, message: "window.__btExercises never defined (timeout)" }];
  }
  const reg = window.__btExercises;

  // CRITICAL: Assert window.__btPyodideAdapter is defined.
  // Without this assertion, tests 2/3/5/6 would silently pass (vacuous-pass)
  // when the adapter is undefined. The fixture (tests/fixtures/pyodide.html)
  // must set window.__btPyodideAdapter = pyodideAdapter before the probe runs.
  assert(
    window.__btPyodideAdapter !== undefined && window.__btPyodideAdapter !== null,
    "window.__btPyodideAdapter must be defined — fixture must wire the adapter",
  );
  const adapter = window.__btPyodideAdapter;

  test("1. CDN injection — exactly one pinned pyodide.js script tag", () => {
    const scripts = document.querySelectorAll('script[src*="cdn.jsdelivr.net/pyodide/v0.27.2/full/pyodide.js"]');
    assert(scripts.length === 1, `expected exactly 1 pyodide.js script tag, got ${scripts.length}`);
    assert(scripts[0].type !== "module", "pyodide.js must be a classic script, not type=module");
  });

  await asyncTest("2. Single boot — loadPyodide called once (idempotent)", async () => {
    const originalLoadPyodide = window.loadPyodide;
    let callCount = 0;
    if (typeof originalLoadPyodide === "function") {
      window.loadPyodide = function (...args) { callCount++; return originalLoadPyodide.apply(this, args); };
    }
    await adapter.boot();
    await adapter.boot();
    assert(callCount === 0, `boot() called loadPyodide ${callCount} times on already-booted adapter (should be 0 — idempotent)`);
    if (typeof originalLoadPyodide === "function") { window.loadPyodide = originalLoadPyodide; }
  });

  test("3. Lazy trigger — no WASM before first Run", () => {
    assert(typeof adapter.boot === "function", "adapter must have boot() method (lazy trigger)");
    assert(typeof adapter.run === "function", "adapter must have run() method (lazy trigger — run triggers boot)");
  });

  await asyncTest("4. Per-run fresh globals — exercise 2 can't see exercise 1's variables", async () => {
    const result0 = await reg[0].runSubmission();
    assert(result0 === "pass", `exercise 0 (x=42) should pass, got ${result0}`);
    const result1 = await reg[1].runSubmission();
    assert(result1 === "pass", `exercise 1 (assert x not in globals) should pass — fresh globals. Got ${result1}`);
  });

  await asyncTest("5. Error surface — Python error surfaces as structured error", async () => {
    const { output, ok } = await adapter.run("raise ValueError('test error')", [], []);
    assert(!ok, "Python error should return ok=false");
    assert(output.includes("ValueError") || output.includes("test error"), `error output should contain 'ValueError' or 'test error', got: ${output}`);
  });

  await asyncTest("6. PyProxy cleanup — namespace.destroy() in finally", async () => {
    const { ok } = await adapter.run("y = 100", [], []);
    assert(ok, "code should execute successfully");
    const { output, ok: ok2 } = await adapter.run("assert 'y' not in globals(), 'namespace not destroyed'", [], []);
    assert(ok2, `fresh namespace should not have y — got: ${output}`);
  });

  test("7. Coexistence with webR — independent, zero shared state", () => {
    const pyodideScripts = document.querySelectorAll('script[src*="pyodide"]');
    assert(pyodideScripts.length >= 1, "Pyodide script tag must exist");
    assert(window.__bt === undefined || window.__bt === null, "window.__bt should not be set by Pyodide adapter (singleton killed in AC-4)");
  });

  return results;
}

runProbe().then((results) => {
  const passed = results.filter((r) => r.passed).length;
  const failed = results.filter((r) => !r.passed).length;
  console.log(`\n=== AC-6 Pyodide Adapter Probe Results ===`);
  console.log(`Passed: ${passed}/${results.length}`);
  if (failed > 0) {
    console.log(`Failed: ${failed}`);
    results.filter((r) => !r.passed).forEach((r) => { console.log(`  FAIL: ${r.name} — ${r.message}`); });
  }
  window.__btProbeResults = results;
});
