// runtime-probe.js — rodney probe for AC-4 runtime.html fixture.
//
// WHAT:  13 assertions verifying multi-exercise runtime core behavior.
// WHERE: scripts/tests/runtime-probe.js
// NOT:   Edge cases (malformed JSON, duplicate IDs, CM6 failure) — see runtime-edge-probe.js.
//
// Rodney executes this script in a browser context after loading runtime.html.
// The script waits for window.__btExercises to be defined, then runs assertions.
//
// Assertions (from AC-4 spec):
//   1.  registry is Array
//   2.  registry has get(id) method, length 3
//   3.  each entry {id, editorView, element, payload}
//   4.  3 distinct cm-editor instances
//   5.  empty-template survives
//   6.  per-exercise Check sends correct checks
//   7.  per-exercise data-status isolation
//   8.  per-exercise getSubmission distinct
//   9.  per-exercise run isolation
//   10. mock not leaked (window.__bt undefined)
//   11. no global #submission
//   12. concurrent run safety
//   13. adapter injection seam (start(registry, runtime) signature)

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

  // 1. registry is Array
  test("1. registry is Array", () => {
    assert(Array.isArray(reg), "registry is not an Array");
  });

  // 2. registry has get(id) method, length 3
  test("2. registry has get(id) method, length 3", () => {
    assert(typeof reg.get === "function", "registry has no get(id) method");
    assert(reg.length === 3, `registry length should be 3, got ${reg.length}`);
  });

  // 3. each entry {id, editorView, element, payload}
  test("3. each entry has {id, editorView, element, payload}", () => {
    for (const entry of reg) {
      assert(typeof entry.id === "string", `entry id missing: ${entry.id}`);
      assert(entry.editorView !== undefined, `entry editorView undefined for ${entry.id}`);
      assert(entry.element instanceof HTMLElement, `entry element not HTMLElement for ${entry.id}`);
      assert(entry.payload !== undefined && entry.payload !== null, `entry payload missing for ${entry.id}`);
    }
  });

  // 4. 3 distinct cm-editor instances
  test("4. 3 distinct cm-editor instances", () => {
    const editors = document.querySelectorAll(".cm-editor");
    assert(editors.length === 3, `expected 3 .cm-editor elements, got ${editors.length}`);
    // Verify they are distinct DOM nodes
    const set = new Set(editors);
    assert(set.size === 3, "cm-editor elements are not distinct");
  });

  // 5. empty-template survives (exercise 2 has code_template: null)
  test("5. empty-template survives", () => {
    const emptyEntry = reg.get("bt-exercise-2");
    assert(emptyEntry !== null, "empty-template exercise not found in registry");
    assert(emptyEntry.payload.code_template === null, "empty-template code_template should be null");
    // Editor should still be mounted (empty doc)
    assert(emptyEntry.editorView !== null, "empty-template editorView should not be null");
  });

  // 6. per-exercise Check sends correct checks
  await asyncTest("6. per-exercise Check sends correct checks", async () => {
    const adapter = window.__btTestAdapter;
    const callsBefore = adapter.calls.length;
    // Run exercise 0 (R with 2 checks)
    await reg[0].runSubmission();
    const callsAfter0 = adapter.calls.length;
    assert(callsAfter0 === callsBefore + 1, "exercise 0 run did not call adapter");
    const lastCall0 = adapter.calls[adapter.calls.length - 1];
    assert(lastCall0.checks.length === 2, `exercise 0 should send 2 checks, got ${lastCall0.checks.length}`);
    assert(lastCall0.checks[0] === "stopifnot(add(1, 2) == 3)", "exercise 0 check[0] mismatch");

    // Run exercise 1 (Python with 1 check)
    await reg[1].runSubmission();
    const lastCall1 = adapter.calls[adapter.calls.length - 1];
    assert(lastCall1.checks.length === 1, `exercise 1 should send 1 check, got ${lastCall1.checks.length}`);
    assert(lastCall1.packages.includes("numpy"), "exercise 1 should send numpy package");
  });

  // 7. per-exercise data-status isolation
  await asyncTest("7. per-exercise data-status isolation", async () => {
    // Reset exercise 1 to a known baseline (idle). Prior assertion 6 calls
    // reg[1].runSubmission(), which leaves exercise 1's status as "pass" on a
    // successful mock run. Without this reset the isolation check would
    // trivially fail on stale state rather than on a real leak.
    reg[1].setStatus("idle", "idle");
    // Set exercise 0 status to "pass"
    reg[0].setStatus("pass", "pass");
    // Exercise 1 status should remain unchanged (idle, not pass)
    assert(reg[0].element.querySelector(".bt-status").dataset.status === "pass", "exercise 0 status not set");
    assert(reg[1].element.querySelector(".bt-status").dataset.status !== "pass", "exercise 1 status leaked from exercise 0");
  });

  // 8. per-exercise getSubmission distinct
  test("8. per-exercise getSubmission distinct", () => {
    const sub0 = reg[0].getSubmission();
    const sub1 = reg[1].getSubmission();
    const sub2 = reg[2].getSubmission();
    assert(sub0 !== sub1, "exercise 0 and 1 submissions are the same");
    assert(sub1 !== sub2, "exercise 1 and 2 submissions are the same");
    assert(sub0.includes("add"), `exercise 0 submission should contain 'add', got: ${sub0}`);
    assert(sub1.includes("print"), `exercise 1 submission should contain 'print', got: ${sub1}`);
  });

  // 9. per-exercise run isolation
  await asyncTest("9. per-exercise run isolation", async () => {
    const adapter = window.__btTestAdapter;
    const callsBefore = adapter.calls.length;
    // Run exercise 0
    await reg[0].runSubmission();
    // Run exercise 1
    await reg[1].runSubmission();
    const callsAfter = adapter.calls.length;
    assert(callsAfter === callsBefore + 2, "running 2 exercises should produce 2 adapter calls");
    // Verify each call used the correct exercise's code
    const call0 = adapter.calls[callsBefore];
    const call1 = adapter.calls[callsBefore + 1];
    assert(call0.code.includes("add"), "exercise 0 run used wrong code");
    assert(call1.code.includes("print"), "exercise 1 run used wrong code");
  });

  // 10. mock not leaked (window.__bt undefined)
  test("10. mock not leaked (window.__bt undefined)", () => {
    assert(window.__bt === undefined, "window.__bt should be undefined (singleton killed)");
  });

  // 11. no global #submission
  test("11. no global #submission", () => {
    const submission = document.getElementById("submission");
    assert(submission === null, "#submission element should not exist (singleton killed)");
  });

  // 12. concurrent run safety
  await asyncTest("12. concurrent run safety", async () => {
    const adapter = window.__btTestAdapter;
    const callsBefore = adapter.calls.length;
    // Fire two concurrent runs on the same exercise
    const p1 = reg[0].runSubmission();
    const p2 = reg[0].runSubmission();
    await Promise.all([p1, p2]);
    const callsAfter = adapter.calls.length;
    // Second concurrent run should be rejected (only 1 call, not 2)
    assert(callsAfter === callsBefore + 1, `concurrent run should produce 1 call, got ${callsAfter - callsBefore}`);
  });

  // 13. adapter injection seam (start(registry, runtime) signature)
  test("13. adapter injection seam", () => {
    // Verify the mock adapter was injected and is accessible
    assert(window.__btTestAdapter !== undefined, "mock adapter not accessible");
    assert(window.__btTestAdapter.name === "mock", "mock adapter name mismatch");
    // Verify registry entries use the injected adapter (calls were recorded)
    assert(window.__btTestAdapter.calls.length > 0, "adapter was never called — injection seam broken");
  });

  return results;
}

// Run and report
runProbe().then(results => {
  const passed = results.filter(r => r.passed).length;
  const failed = results.filter(r => !r.passed).length;
  console.log(`\n=== AC-4 Runtime Probe Results ===`);
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
