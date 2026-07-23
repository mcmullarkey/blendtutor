// runtime-edge-probe.js — rodney probe for AC-4 runtime-edge.html fixture.
//
// WHAT:  3 edge-case assertions verifying isolation and graceful degradation.
// WHERE: scripts/tests/runtime-edge-probe.js
// NOT:   Main runtime assertions — see runtime-probe.js.
//
// Rodney executes this script in a browser context after loading runtime-edge.html.
//
// Assertions (from AC-4 spec):
//   14. malformed JSON isolation (bad exercise skipped, good exercises survive)
//   15. duplicate-id defense (skip+warn)
//   16. graceful degradation (CM6 fail→textarea)

async function waitFor(condFn, timeoutMs = 5000) {
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

  // Wait for registry to be defined
  const ready = await waitFor(() => window.__btExercises !== undefined);
  if (!ready) {
    return [{ name: "registry defined", passed: false, message: "window.__btExercises never defined (timeout)" }];
  }
  const reg = window.__btExercises;

  // 14. malformed JSON isolation
  //    Fixture has 5 divs: valid R, malformed JSON, duplicate ID, CM6 fail, valid Python.
  //    Malformed JSON div should be skipped. Registry should have 3 entries
  //    (valid R, CM6 fail, valid Python — duplicate ID also skipped).
  test("14. malformed JSON isolation", () => {
    // Registry should NOT contain the malformed JSON exercise
    const allIds = reg.map(e => e.id);
    assert(!allIds.includes(undefined), "malformed JSON exercise should not be in registry");
    // Valid exercises should survive
    assert(reg.get("bt-edge-0") !== null, "valid R exercise (bt-edge-0) should be in registry");
    assert(reg.get("bt-edge-2") !== null, "valid Python exercise (bt-edge-2) should be in registry");
  });

  // 15. duplicate-id defense (skip+warn)
  //    Fixture has two exercises with id "bt-edge-0". Second should be skipped.
  test("15. duplicate-id defense", () => {
    const entriesWithEdge0 = reg.filter(e => e.id === "bt-edge-0");
    assert(entriesWithEdge0.length === 1, `duplicate ID should be skipped, found ${entriesWithEdge0.length} entries with bt-edge-0`);
  });

  // 16. graceful degradation (CM6 fail→textarea)
  //    Fixture has a div with data-cm-fail="true". That exercise should
  //    have a textarea fallback instead of a cm-editor.
  test("16. graceful degradation (CM6 fail→textarea)", () => {
    const failEntry = reg.get("bt-edge-1");
    assert(failEntry !== null, "CM6-fail exercise (bt-edge-1) should be in registry");
    assert(failEntry.editorView === null, "CM6-fail exercise should have null editorView");
    // Should have a textarea fallback
    const textarea = failEntry.element.querySelector("textarea");
    assert(textarea !== null, "CM6-fail exercise should have textarea fallback");
    // Other exercises should still have cm-editor
    const validEntry = reg.get("bt-edge-0");
    assert(validEntry.editorView !== null, "valid exercise should have non-null editorView after CM6 failure on another exercise");
  });

  return results;
}

// Run and report
runProbe().then(results => {
  const passed = results.filter(r => r.passed).length;
  const failed = results.filter(r => !r.passed).length;
  console.log(`\n=== AC-4 Runtime Edge Probe Results ===`);
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
