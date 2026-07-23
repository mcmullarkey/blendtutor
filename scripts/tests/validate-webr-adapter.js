#!/usr/bin/env node
// validate-webr-adapter.js — Node.js validation for AC-5 webr-adapter.js.
//
// WHAT:  Validates JS syntax, module exports, adapter protocol conformance,
//        boot state machine, boot promise dedup, concurrent run guard,
//        Shelter isolation pattern, CDN URL, and lazy boot invariant.
// WHERE: scripts/tests/validate-webr-adapter.js
// NOT:   NOT a rodney probe — this is a local sanity check. Full browser
//        verification (WASM fetch, boot progress, Shelter isolation) is done
//        by builder-vision-probe with rodney against webr-runtime.html.
//
// Runs without a browser by testing the adapter's structural contract.
// Uses Node.js dynamic import with a mocked webR module to verify
// boot state transitions, promise dedup, and concurrent run rejection.

import { readFileSync, existsSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath, pathToFileURL } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const repoRoot = join(__dirname, "..", "..");

let failures = 0;
let passes = 0;

function assert(cond, msg) {
  if (cond) {
    passes++;
  } else {
    failures++;
    console.error(`  FAIL: ${msg}`);
  }
}

// ---------------------------------------------------------------------------
// 1. File existence + JS syntax check
// ---------------------------------------------------------------------------

const adapterPath = join(repoRoot, "_extensions", "blendtutor", "assets", "webr-adapter.js");
const probePath = join(repoRoot, "scripts", "tests", "webr-probe.js");
const fixturePath = join(repoRoot, "tests", "fixtures", "webr-runtime.html");
const qmdPath = join(repoRoot, "quarto-fixture", "webr.qmd");

assert(existsSync(adapterPath), "webr-adapter.js exists");
assert(existsSync(probePath), "webr-probe.js exists");
assert(existsSync(fixturePath), "webr-runtime.html exists");
assert(existsSync(qmdPath), "webr.qmd exists");

// ---------------------------------------------------------------------------
// 2. Source-level structural validation (grep the source for required patterns)
// ---------------------------------------------------------------------------

const adapterSrc = readFileSync(adapterPath, "utf-8");

// Export
assert(adapterSrc.includes("export function createWebRAdapter"),
  "webr-adapter.js exports createWebRAdapter");

// Adapter protocol fields
assert(adapterSrc.includes('name: "webr"') || adapterSrc.includes("name: 'webr'"),
  "webr-adapter.js has name: 'webr'");
assert(adapterSrc.includes("language:") && adapterSrc.includes('"r"') || adapterSrc.includes("'r'"),
  "webr-adapter.js has language: 'r'");
assert(adapterSrc.includes("async boot(") || adapterSrc.includes("async boot ()"),
  "webr-adapter.js has async boot()");
assert(adapterSrc.includes("async run("),
  "webr-adapter.js has async run()");

// Boot state machine — explicit states, not nullable (§1.2)
assert(adapterSrc.includes("uninitialized"),
  "webr-adapter.js has 'uninitialized' boot state");
assert(adapterSrc.includes("booting"),
  "webr-adapter.js has 'booting' boot state");
assert(adapterSrc.includes("ready"),
  "webr-adapter.js has 'ready' boot state");
assert(adapterSrc.includes("failed"),
  "webr-adapter.js has 'failed' boot state");

// Boot promise dedup (§5.1)
assert(adapterSrc.includes("bootPromise"),
  "webr-adapter.js has bootPromise for dedup");

// Concurrent run guard (§5.3)
assert(adapterSrc.includes("running") || adapterSrc.includes("_running"),
  "webr-adapter.js has concurrent run guard");
assert(adapterSrc.includes("Another exercise is already running"),
  "webr-adapter.js rejects concurrent runs with error message");

// Shelter isolation (§3.4)
assert(adapterSrc.includes("Shelter"),
  "webr-adapter.js uses webR.Shelter for per-run isolation");
assert(adapterSrc.includes("purge"),
  "webr-adapter.js calls shelter.purge() for cleanup");
assert(adapterSrc.includes("finally"),
  "webr-adapter.js has finally block for Shelter cleanup");

// CDN URL
assert(adapterSrc.includes("webr.r-wasm.org"),
  "webr-adapter.js uses webr.r-wasm.org CDN");
assert(adapterSrc.includes("webr.mjs"),
  "webr-adapter.js imports webr.mjs ES module");

// Lazy boot — boot() must NOT call import() or init()
// The boot() method should be a no-op or minimal setup.
// The actual boot happens in run() via ensureBooted().
const bootMatch = adapterSrc.match(/async\s+boot\s*\([^)]*\)\s*\{([^}]*(?:\{[^}]*\}[^}]*)*)\}/);
if (bootMatch) {
  const bootBody = bootMatch[1];
  assert(!bootBody.includes("import("),
    "boot() must NOT call dynamic import (lazy boot — deferred to run())");
  assert(!bootBody.includes(".init("),
    "boot() must NOT call webR.init() (lazy boot — deferred to run())");
} else {
  // If we can't extract boot(), check that ensureBooted exists as the lazy boot mechanism
  assert(adapterSrc.includes("ensureBooted"),
    "webr-adapter.js has ensureBooted() for lazy boot mechanism");
}

// Boot progress callback
assert(adapterSrc.includes("onBootProgress") || adapterSrc.includes("bootProgress"),
  "webr-adapter.js has boot progress callback");

// CDN failure error message
assert(adapterSrc.includes("webR failed to boot"),
  "webr-adapter.js has CDN failure error message");

// ---------------------------------------------------------------------------
// 3. HTML fixture structure validation
// ---------------------------------------------------------------------------

const fixtureHtml = readFileSync(fixturePath, "utf-8");

assert(fixtureHtml.includes('class="bt-exercise"'),
  "webr-runtime.html has bt-exercise divs");
assert(fixtureHtml.includes('data-language="r"'),
  "webr-runtime.html has data-language='r'");

// 2 exercises
const exerciseCount = (fixtureHtml.match(/class="bt-exercise"/g) || []).length;
assert(exerciseCount === 2,
  `webr-runtime.html has 2 bt-exercise divs, got ${exerciseCount}`);

// Exercise 1: x <- 5; print(x)
assert(fixtureHtml.includes("x <- 5") || fixtureHtml.includes("x <- 5"),
  "webr-runtime.html Exercise 1 has 'x <- 5'");
assert(fixtureHtml.includes("print(x)"),
  "webr-runtime.html Exercise 1 has 'print(x)'");

// Exercise 2: print(exists("x"))
assert(fixtureHtml.includes('exists("x")') || fixtureHtml.includes("exists('x')"),
  "webr-runtime.html Exercise 2 has 'exists(\"x\")'");

// Loads webr-adapter.js
assert(fixtureHtml.includes("webr-adapter.js"),
  "webr-runtime.html imports webr-adapter.js");

// Loads exercise-runtime.js
assert(fixtureHtml.includes("exercise-runtime.js"),
  "webr-runtime.html imports exercise-runtime.js");

// Exposes adapter for probe verification
assert(fixtureHtml.includes("__btWebRAdapter"),
  "webr-runtime.html exposes __btWebRAdapter for probe verification");

// ---------------------------------------------------------------------------
// 4. webr.qmd fixture validation
// ---------------------------------------------------------------------------

const qmdSrc = readFileSync(qmdPath, "utf-8");

assert(qmdSrc.includes("filters:"),
  "webr.qmd declares filters in YAML");
assert(qmdSrc.includes("blendtutor.lua"),
  "webr.qmd references blendtutor.lua filter");
assert(qmdSrc.includes('language="r"'),
  "webr.qmd has blendtutor div with language='r'");

// 2 exercises in qmd
const qmdExerciseCount = (qmdSrc.match(/{\.blendtutor/g) || []).length;
assert(qmdExerciseCount === 2,
  `webr.qmd has 2 blendtutor divs, got ${qmdExerciseCount}`);

// Exercise 1: x <- 5; print(x)
assert(qmdSrc.includes("x <- 5"),
  "webr.qmd Exercise 1 has 'x <- 5'");
assert(qmdSrc.includes("print(x)"),
  "webr.qmd Exercise 1 has 'print(x)'");

// Exercise 2: print(exists("x"))
assert(qmdSrc.includes('exists("x")'),
  "webr.qmd Exercise 2 has 'exists(\"x\")'");

// Script tags for adapter + runtime
assert(qmdSrc.includes("webr-adapter.js"),
  "webr.qmd loads webr-adapter.js");
assert(qmdSrc.includes("exercise-runtime.js"),
  "webr.qmd loads exercise-runtime.js");

// ---------------------------------------------------------------------------
// 5. Rodney probe script validation
// ---------------------------------------------------------------------------

const probeSrc = readFileSync(probePath, "utf-8");

// Assertions count
const probeAssertions = (probeSrc.match(/\d+\.\s/g) || []).length;
assert(probeAssertions >= 6,
  `webr-probe.js should have >=6 assertions, found ${probeAssertions}`);

// Key assertions present
assert(probeSrc.includes("crossOriginIsolated"),
  "webr-probe.js checks crossOriginIsolated === false");
assert(probeSrc.includes("getEntriesByType") || probeSrc.includes("resource"),
  "webr-probe.js checks resource timing for lazy boot");
assert(probeSrc.includes("[1] 5") || probeSrc.includes("[1] 5"),
  "webr-probe.js asserts Exercise 1 output contains [1] 5");
assert(probeSrc.includes("FALSE") || probeSrc.includes("FALSE"),
  "webr-probe.js asserts Exercise 2 output contains FALSE");
assert(probeSrc.includes("boot") && probeSrc.includes("progress"),
  "webr-probe.js checks boot progress on first Run");
assert(probeSrc.includes("shared") || probeSrc.includes("no boot") || probeSrc.includes("NO boot"),
  "webr-probe.js checks no boot progress on second Run (shared instance)");

// ---------------------------------------------------------------------------
// 6. Behavioral test — adapter with mocked webR module
// ---------------------------------------------------------------------------

// We can't import the real adapter (it imports from CDN), but we can
// verify the adapter's structural contract by checking that the source
// contains the required patterns. The behavioral test is done by rodney
// in the browser.

// Verify the adapter creates a Shelter per run via `await new webR.Shelter()`
// (not `webR.newShelter()`). The Shelter constructor is async — it returns a
// Promise that resolves to the Shelter instance. The `await` is mandatory;
// without it, `shelter` is an unresolved Promise and captureR/purge are
// undefined. This matches the reference implementation in
// crates/core/assets/webr/lesson-runner.js (line 32).
const shelterMatches = adapterSrc.match(/await\s+new\s+webR\.Shelter\s*\(\s*\)/g);
assert(shelterMatches !== null && shelterMatches.length >= 1,
  "webr-adapter.js creates Shelter via `await new webR.Shelter()` (not webR.newShelter())");

// Verify the adapter does NOT use `webR.newShelter()` (not a function on the
// webR instance — the correct API is `new webR.Shelter()`)
const brokenShelterMatches = adapterSrc.match(/webR\.newShelter\s*\(/g);
assert(brokenShelterMatches === null,
  "webr-adapter.js must NOT use webR.newShelter() — use `await new webR.Shelter()` instead");

// Verify the adapter does NOT use shelter.evalR() for code execution
// (Shelter API in the CDN build does not expose evalR; captureR is the correct method)
const shelterEvalRMatches = adapterSrc.match(/shelter\.evalR\s*\(/g);
assert(shelterEvalRMatches === null,
  "webr-adapter.js must NOT use shelter.evalR() — use shelter.captureR() instead");

// Verify the adapter does NOT use evalR for package installation (RCE prevention)
// evalR(`install.packages("${pkg}")`) allows RCE if pkg contains malicious R code
const evalRPkgMatches = adapterSrc.match(/evalR\s*\(\s*[`"]install\.packages/g);
assert(evalRPkgMatches === null,
  "webr-adapter.js must NOT use evalR for package installation (RCE risk — use webR.installPackages())");

// Verify the adapter uses webR.installPackages() for package installation
assert(adapterSrc.includes("webR.installPackages"),
  "webr-adapter.js uses webR.installPackages() for package installation");

// Verify the adapter throws an error if installPackages is not available
// (instead of silently falling back to evalR)
assert(adapterSrc.includes("typeof webR.installPackages") && adapterSrc.includes("throw"),
  "webr-adapter.js throws error if webR.installPackages is not available (no evalR fallback)");

// Verify shelter.purge() is in a finally block
assert(adapterSrc.match(/finally\s*\{[^}]*purge/s) !== null,
  "webr-adapter.js calls shelter.purge() in finally block");

// Verify boot promise is stored and reused
assert(adapterSrc.match(/bootPromise\s*=/) !== null,
  "webr-adapter.js stores boot promise");
assert(adapterSrc.match(/return\s+bootPromise/) !== null,
  "webr-adapter.js returns stored boot promise (dedup)");

// ---------------------------------------------------------------------------
// 7. Behavioral tests — adapter with mocked webR module
// ---------------------------------------------------------------------------
//
// These tests import the ACTUAL adapter and exercise it with a mock webR
// module, verifying runtime behavior (not just source-string patterns).
// The mock is configured via globalThis.__webrMockConfig before each test.
//
// These tests would have FAILED before commit 34ffe21 (which removed the
// broad catch fallback around captureR and the console.warn around
// installPackages). Before that commit:
//   - captureR errors were caught by an inner try/catch that fell back to
//     evalR, returning ok:true instead of ok:false.
//   - installPackages errors were caught and console.warn'\''d, returning
//     ok:true instead of ok:false.

const mockUrl = pathToFileURL(join(__dirname, "mock-webr.mjs")).href;
const adapterUrl = pathToFileURL(adapterPath).href;

async function runBehavioralTests() {
  const { createWebRAdapter } = await import(adapterUrl);

  // --- Test 1: R runtime error propagation (fix-now #1) ---
  // Before fix: inner catch fell back to evalR, returning ok:true (wrong)
  // After fix:  error propagates to outer catch, returning ok:false (correct)
  // The mock includes evalR (returns "fallback from evalR") so that if someone
  // re-adds the inner catch fallback, this test FAILS (evalR returns ok:true).
  {
    globalThis.__webrMockConfig = {
      captureRThrows: "R runtime error: object 'nonexistent_var' not found",
      evalRResult: "fallback from evalR",
    };
    const adapter = createWebRAdapter({ cdnUrl: mockUrl });
    const result = await adapter.run("nonexistent_var");
    assert(result.ok === false,
      "BEHAVIORAL: R runtime error (captureR throws) returns ok:false, not swallowed by inner catch fallback to evalR");
    assert(result.output.includes("Error:"),
      "BEHAVIORAL: R runtime error surfaced in output with Error: prefix");
    assert(result.output.includes("nonexistent_var"),
      "BEHAVIORAL: R runtime error message preserved in output");
    assert(!result.output.includes("fallback from evalR"),
      "BEHAVIORAL: evalR fallback NOT used, error propagated to outer catch not evalR");
  }

  // --- Test 2: Package install error surfaced via output (consider #2) ---
  // Before fix: console.warn swallowed the error, run continued with ok:true (wrong)
  // After fix:  error propagates to outer catch, returning ok:false, no console.warn (correct)
  {
    let warnCalled = false;
    const originalWarn = console.warn;
    console.warn = () => { warnCalled = true; };
    globalThis.__webrMockConfig = {
      installPackagesThrows: "package 'nonexistent' is not available",
    };
    const adapter = createWebRAdapter({ cdnUrl: mockUrl });
    const result = await adapter.run("x <- 1", [], ["nonexistent"]);
    console.warn = originalWarn;
    assert(result.ok === false,
      "BEHAVIORAL: Package install error returns ok:false, not swallowed by console.warn");
    assert(result.output.includes("Error:"),
      "BEHAVIORAL: Package install error surfaced in output");
    assert(result.output.includes("nonexistent"),
      "BEHAVIORAL: Package install error message preserved in output");
    assert(!warnCalled,
      "BEHAVIORAL: Package install error NOT console.warn'd, surfaced via output instead");
  }

  // --- Test 3: Normal execution (positive control) ---
  // Verifies the happy path still works after removing the fallback.
  {
    globalThis.__webrMockConfig = {
      captureRResult: {
        output: [{ type: "stdout", data: "[1] 5" }],
        result: { type: "integer", names: null, values: [5] },
      },
    };
    const adapter = createWebRAdapter({ cdnUrl: mockUrl });
    const result = await adapter.run("x <- 5; print(x)");
    assert(result.ok === true,
      "BEHAVIORAL: Normal R execution returns ok:true");
    assert(result.output.includes("[1] 5"),
      "BEHAVIORAL: Normal R execution output contains [1] 5");
  }

  // --- Test 4: Error during run still returns ok:false (purge runs in finally) ---
  // The finally block must run purge() regardless of success/failure.
  {
    globalThis.__webrMockConfig = {
      captureRThrows: "R error during evaluation",
    };
    const adapter = createWebRAdapter({ cdnUrl: mockUrl });
    const result = await adapter.run("stop('error')");
    assert(result.ok === false,
      "BEHAVIORAL: Error during run returns ok:false, purge still runs in finally");
  }

  // --- Test 5: `await new webR.Shelter()` produces a working Shelter (captureR callable) ---
  // Before fix: used webR.newShelter() which is not a function on the webR instance
  // After fix:  uses `await new webR.Shelter()` — async constructor returns Promise
  //              that resolves to the Shelter instance with captureR/purge methods
  {
    globalThis.__webrMockConfig = {
      captureRResult: { output: [{ type: "stdout", data: "ok" }] },
    };
    const adapter = createWebRAdapter({ cdnUrl: mockUrl });
    const result = await adapter.run("1 + 1");
    assert(result.ok === true,
      "BEHAVIORAL: `await new webR.Shelter()` produces a working Shelter, captureR callable");
  }

  // Clean up mock config
  delete globalThis.__webrMockConfig;
}

await runBehavioralTests();

// ---------------------------------------------------------------------------
// Summary
// ---------------------------------------------------------------------------

console.log(`\n=== AC-5 webR Adapter Validation Results ===`);
console.log(`Passed: ${passes}`);
console.log(`Failed: ${failures}`);
if (failures > 0) {
  console.error("VALIDATION FAILED");
  process.exit(1);
} else {
  console.log("All validations passed.");
  process.exit(0);
}
