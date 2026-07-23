// webr-probe.js — rodney probe for AC-5 webR shared-boot adapter.
//
// WHAT:  8 assertions verifying lazy boot, shared instance, per-run Shelter
//        isolation, concurrent run guard, and degraded channel (no COI).
// WHERE: scripts/tests/webr-probe.js
// NOT:   NOT a Node.js test — this runs in a browser via rodney.
//        The coding builder does NOT run rodney; builder-vision-probe does.
//
// Rodney executes this script in a browser context after loading
// webr-runtime.html (or the rendered webr.qmd). The script waits for
// window.__btExercises to be defined, then runs assertions.
//
// Build + serve (run by builder-vision-probe):
//   # Option A: standalone HTML fixture (no quarto needed)
//   cd /path/to/repo && python3 -m http.server 8888
//   # Navigate to http://localhost:8888/tests/fixtures/webr-runtime.html
//
//   # Option B: rendered Quarto fixture
//   quarto render quarto-fixture/webr.qmd --to html
//   # Serve and navigate to webr.html
//
// Assertions (from AC-5 spec):
//   1.  crossOriginIsolated === false (degraded channel, no SharedArrayBuffer)
//   2.  Before first Run: no webr.wasm fetch (lazy boot via resource timing API)
//   3.  First Run: boot progress shown in output/status
//   4.  Exercise 1 (x <- 5; print(x)) output contains [1] 5
//   5.  Second Run: NO boot progress (shared instance, no re-boot)
//   6.  Exercise 2 (print(exists("x"))) output contains [1] FALSE (Shelter isolation)
//   7.  Concurrent run guard: second concurrent Run rejected
//   8.  Boot state machine: adapter exposes getBootState() returning ready after boot

async function waitFor(condFn, timeoutMs = 30000) {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    if (condFn()) return true;
    await new Promise(r => setTimeout(r, 100));
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

  // Wait for registry to be defined
  const ready = await waitFor(() => window.__btExercises !== undefined);
  if (!ready) {
    return [{ name: "registry defined", passed: false, message: "window.__btExercises never defined (timeout)" }];
  }
  const reg = window.__btExercises;
  const adapter = window.__btWebRAdapter;

  // 1. crossOriginIsolated === false (degraded channel, no SharedArrayBuffer)
  test("1. crossOriginIsolated === false (degraded channel)", () => {
    assert(window.crossOriginIsolated === false,
      `crossOriginIsolated should be false (no COI), got ${window.crossOriginIsolated}`);
  });

  // 2. Before first Run: no webr.wasm fetch (lazy boot)
  test("2. Before first Run: no webr.wasm fetch (lazy boot)", () => {
    const resources = performance.getEntriesByType("resource");
    const webrResources = resources.filter(r =>
      r.name.includes("webr") && (r.name.includes(".wasm") || r.name.includes("webr.mjs"))
    );
    assert(webrResources.length === 0,
      `Expected 0 webr WASM/module fetches before first Run, got ${webrResources.length}: ${webrResources.map(r => r.name).join(", ")}`);
  });

  // 3. First Run: boot progress shown
  await asyncTest("3. First Run: boot progress shown", async () => {
    // Click Run on Exercise 1
    const runBtn = reg[0].element.querySelector(".bt-run-btn");
    assert(runBtn !== null, "Exercise 1 Run button not found");

    // Record output before run
    const outputEl = reg[0].element.querySelector(".bt-output");
    const statusEl = reg[0].element.querySelector(".bt-status");
    assert(outputEl !== null, "Exercise 1 output element not found");
    assert(statusEl !== null, "Exercise 1 status element not found");

    // Click Run
    runBtn.click();

    // Wait for boot progress to appear (status should show booting/booting…)
    const bootProgressSeen = await waitFor(() => {
      const status = statusEl.dataset.status;
      const statusText = statusEl.textContent || "";
      const outputText = outputEl.textContent || "";
      return status === "running" ||
             statusText.toLowerCase().includes("boot") ||
             outputText.toLowerCase().includes("boot");
    }, 5000);
    assert(bootProgressSeen,
      "Boot progress should be visible during first Run (status or output contains 'boot')");

    // Wait for run to complete
    await waitFor(() => {
      const status = statusEl.dataset.status;
      return status === "pass" || status === "fail";
    }, 60000); // webR boot can take up to 60s
  });

  // 4. Exercise 1 (x <- 5; print(x)) output contains [1] 5
  await asyncTest("4. Exercise 1 output contains [1] 5", async () => {
    const outputEl = reg[0].element.querySelector(".bt-output");
    const outputText = outputEl.textContent || "";
    assert(outputText.includes("[1] 5"),
      `Exercise 1 output should contain '[1] 5', got: ${outputText}`);
  });

  // 5. Second Run: NO boot progress (shared instance, no re-boot)
  await asyncTest("5. Second Run: NO boot progress (shared instance)", async () => {
    // Record resource count before second run
    const resourcesBefore = performance.getEntriesByType("resource").length;

    // Click Run on Exercise 2
    const runBtn = reg[1].element.querySelector(".bt-run-btn");
    assert(runBtn !== null, "Exercise 2 Run button not found");

    const statusEl = reg[1].element.querySelector(".bt-status");
    const outputEl = reg[1].element.querySelector(".bt-output");

    // Record status before run
    const statusBefore = statusEl.dataset.status;

    // Click Run
    runBtn.click();

    // Wait for run to complete
    await waitFor(() => {
      const status = statusEl.dataset.status;
      return status === "pass" || status === "fail";
    }, 30000);

    // Check that no new WASM fetch occurred (shared instance)
    const resourcesAfter = performance.getEntriesByType("resource").length;
    const newResources = performance.getEntriesByType("resource")
      .slice(resourcesBefore)
      .filter(r => r.name.includes(".wasm") || r.name.includes("webr.mjs"));

    assert(newResources.length === 0,
      `Second Run should NOT fetch WASM (shared instance), but found ${newResources.length} new WASM/module fetches: ${newResources.map(r => r.name).join(", ")}`);

    // Check that boot progress was NOT shown (no "boot" in status/output during second run)
    const outputText = outputEl.textContent || "";
    // The output should NOT contain boot progress messages
    // (it should only contain the R output [1] FALSE)
    assert(!outputText.toLowerCase().includes("booting webr"),
      `Second Run should NOT show boot progress, but output contains 'booting webr': ${outputText}`);
  });

  // 6. Exercise 2 (print(exists("x"))) output contains [1] FALSE (Shelter isolation)
  await asyncTest("6. Exercise 2 output contains [1] FALSE (Shelter isolation)", async () => {
    const outputEl = reg[1].element.querySelector(".bt-output");
    const outputText = outputEl.textContent || "";
    assert(outputText.includes("FALSE"),
      `Exercise 2 output should contain 'FALSE' (x does not leak across Shelter), got: ${outputText}`);
  });

  // 7. Concurrent run guard: second concurrent Run rejected
  await asyncTest("7. Concurrent run guard: second concurrent Run rejected", async () => {
    // Start a run on Exercise 1
    const runBtn0 = reg[0].element.querySelector(".bt-run-btn");
    const runPromise = reg[0].runSubmission();

    // Immediately try to run Exercise 2 (concurrent)
    const result = await reg[1].runSubmission();

    // The concurrent run should be rejected
    assert(result === "fail",
      `Concurrent run should be rejected (return 'fail'), got '${result}'`);

    // Wait for the first run to complete
    await runPromise;
  });

  // 8. Boot state machine: adapter exposes getBootState() returning ready after boot
  test("8. Boot state machine: getBootState() returns 'ready' after boot", () => {
    assert(typeof adapter.getBootState === "function",
      "Adapter should expose getBootState() method");
    const state = adapter.getBootState();
    assert(state === "ready",
      `Boot state should be 'ready' after boot, got '${state}'`);
  });

  return results;
}

// Run and report
runProbe().then(results => {
  const passed = results.filter(r => r.passed).length;
  const failed = results.filter(r => !r.passed).length;
  console.log(`\n=== AC-5 webR Adapter Probe Results ===`);
  console.log(`Passed: ${passed}/${results.length}`);
  if (failed > 0) {
    console.log(`Failed: ${failed}`);
    results.filter(r => !r.passed).forEach(r => {
      console.log(`  FAIL: ${r.name} — ${r.message}`);
    });
  }
  // Expose results for rodney
  window.__btProbeResults = results;
});
