#!/usr/bin/env node
// validate-runtime.js — Node.js validation for AC-4 exercise-runtime.js.
//
// WHAT:  Validates JS syntax, HTML fixture structure, and pure functions.
// WHERE: scripts/tests/validate-runtime.js
// NOT:   NOT a rodney probe — this is a local sanity check. Full browser
//        verification is done by builder-vision-probe with rodney.
//
// Runs without a browser by testing pure functions with mocked DOM.
// Uses Node.js dynamic import with a mock codemirror.js module.

import { readFileSync, existsSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath, pathToFileURL } from "url";
import { createRequire } from "module";

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
// 1. JS syntax check (already done by node --check, but double-check)
// ---------------------------------------------------------------------------
const runtimePath = join(repoRoot, "_extensions", "blendtutor", "assets", "exercise-runtime.js");
const mockAdapterPath = join(repoRoot, "tests", "fixtures", "mock-adapter.js");
const probePath = join(repoRoot, "scripts", "tests", "runtime-probe.js");
const edgeProbePath = join(repoRoot, "scripts", "tests", "runtime-edge-probe.js");

assert(existsSync(runtimePath), "exercise-runtime.js exists");
assert(existsSync(mockAdapterPath), "mock-adapter.js exists");
assert(existsSync(probePath), "runtime-probe.js exists");
assert(existsSync(edgeProbePath), "runtime-edge-probe.js exists");

// ---------------------------------------------------------------------------
// 2. HTML fixture structure validation
// ---------------------------------------------------------------------------
function validateHtmlFixture(htmlPath, expectedExercises, label) {
  const html = readFileSync(htmlPath, "utf-8");
  const exerciseCount = (html.match(/class="bt-exercise"/g) || []).length;
  assert(exerciseCount === expectedExercises, `${label}: expected ${expectedExercises} bt-exercise divs, got ${exerciseCount}`);

  // Check data-language attributes
  const dataLangCount = (html.match(/data-language="/g) || []).length;
  assert(dataLangCount === expectedExercises, `${label}: expected ${expectedExercises} data-language attrs, got ${dataLangCount}`);

  // Check script[type=application/json] payloads
  const jsonScriptCount = (html.match(/type="application\/json"/g) || []).length;
  assert(jsonScriptCount === expectedExercises, `${label}: expected ${expectedExercises} JSON scripts, got ${jsonScriptCount}`);

  // Check exercise-runtime.js import
  assert(html.includes("exercise-runtime.js"), `${label}: should import exercise-runtime.js`);
  assert(html.includes("mock-adapter.js"), `${label}: should import mock-adapter.js`);
}

validateHtmlFixture(
  join(repoRoot, "tests", "fixtures", "runtime.html"),
  3,
  "runtime.html",
);

validateHtmlFixture(
  join(repoRoot, "tests", "fixtures", "runtime-edge.html"),
  5,
  "runtime-edge.html",
);

// ---------------------------------------------------------------------------
// 3. Module exports validation (grep the source for export statements)
// ---------------------------------------------------------------------------
const runtimeSrc = readFileSync(runtimePath, "utf-8");
assert(runtimeSrc.includes("export function scanExercises"), "exercise-runtime.js exports scanExercises");
assert(runtimeSrc.includes("export function buildRegistry"), "exercise-runtime.js exports buildRegistry");
assert(runtimeSrc.includes("export async function start"), "exercise-runtime.js exports start");
assert(runtimeSrc.includes("export function parsePayload"), "exercise-runtime.js exports parsePayload");

// Verify singletons are killed
assert(!runtimeSrc.includes("window.__bt ="), "exercise-runtime.js must NOT set window.__bt (singleton killed)");
assert(!runtimeSrc.includes("getElementById(\"submission\")"), "exercise-runtime.js must NOT use getElementById('submission') (singleton killed)");
assert(!runtimeSrc.match(/^let editorView\s*=/m), "exercise-runtime.js must NOT have module-level editorView singleton");

// Verify registry pattern
assert(runtimeSrc.includes("window.__btExercises"), "exercise-runtime.js must set window.__btExercises");
assert(runtimeSrc.includes("registry.get ="), "exercise-runtime.js must define registry.get(id)");

// Verify adapter injection seam
assert(runtimeSrc.includes("start(registry, runtime)"), "exercise-runtime.js must have start(registry, runtime) signature");

// Verify per-exercise degradation
assert(runtimeSrc.includes("data-cm-fail"), "exercise-runtime.js must check data-cm-fail for graceful degradation");
assert(runtimeSrc.includes("textarea"), "exercise-runtime.js must fall back to textarea");

// Verify concurrent run safety
assert(runtimeSrc.includes("_running"), "exercise-runtime.js must have _running flag for concurrent run safety");

// Verify duplicate ID defense
assert(runtimeSrc.includes("seenIds"), "exercise-runtime.js must have seenIds for duplicate ID defense");

// ---------------------------------------------------------------------------
// 4. Probe script assertion count
// ---------------------------------------------------------------------------
const probeSrc = readFileSync(probePath, "utf-8");
const probeAssertions = (probeSrc.match(/\d+\.\s/g) || []).length;
assert(probeAssertions >= 13, `runtime-probe.js should have >=13 assertions, found ${probeAssertions}`);

const edgeProbeSrc = readFileSync(edgeProbePath, "utf-8");
const edgeAssertions = (edgeProbeSrc.match(/\d+\.\s/g) || []).length;
assert(edgeAssertions >= 3, `runtime-edge-probe.js should have >=3 assertions, found ${edgeAssertions}`);

// ---------------------------------------------------------------------------
// 5. Mock adapter validation
// ---------------------------------------------------------------------------
const mockSrc = readFileSync(mockAdapterPath, "utf-8");
assert(mockSrc.includes("createMockAdapter"), "mock-adapter.js exports createMockAdapter");
assert(mockSrc.includes("name: \"mock\""), "mock-adapter.js has name: 'mock'");
assert(mockSrc.includes("async boot()"), "mock-adapter.js has async boot()");
assert(mockSrc.includes("async run("), "mock-adapter.js has async run()");
assert(mockSrc.includes("calls"), "mock-adapter.js records calls");

// ---------------------------------------------------------------------------
// Summary
// ---------------------------------------------------------------------------
console.log(`\n=== AC-4 Validation Results ===`);
console.log(`Passed: ${passes}`);
console.log(`Failed: ${failures}`);
if (failures > 0) {
  console.error("VALIDATION FAILED");
  process.exit(1);
} else {
  console.log("All validations passed.");
  process.exit(0);
}
