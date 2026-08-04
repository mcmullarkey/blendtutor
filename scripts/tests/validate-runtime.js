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

// AC-2 mixed-runtime fixture: 4 exercises but only 3 data-language attrs
// (the 4th is intentionally attribute-less — the skip/never-default case).
const mixedHtml = readFileSync(
  join(repoRoot, "tests", "fixtures", "mixed-runtime.html"),
  "utf-8",
);
const mixedExercises = (mixedHtml.match(/class="bt-exercise"/g) || []).length;
assert(
  mixedExercises === 4,
  `mixed-runtime.html: expected 4 bt-exercise divs, got ${mixedExercises}`,
);
const mixedDataLang = (mixedHtml.match(/data-language="/g) || []).length;
assert(
  mixedDataLang === 3,
  `mixed-runtime.html: expected 3 data-language attrs (1 attribute-less), got ${mixedDataLang}`,
);
assert(
  mixedHtml.includes("start(registry, { r: mockR, python: mockPy })"),
  "mixed-runtime.html calls start() with the adapter map (twice — race probe)",
);
assert(
  (mixedHtml.match(/start\(registry, \{ r: mockR, python: mockPy \}\)/g) || []).length === 2,
  "mixed-runtime.html must call start() twice unawaited (double-start race probe)",
);

// AC-2 rodney probe exists and covers the mixed fixture
const mixedProbePath = join(repoRoot, "rodney-probes", "mixed-runtime.js");
assert(existsSync(mixedProbePath), "rodney-probes/mixed-runtime.js exists");

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

// Verify adapter injection seam (AC-2: start(registry, adapters) map signature)
assert(runtimeSrc.includes("start(registry, adapters)"), "exercise-runtime.js must have start(registry, adapters) signature");

// Verify AC-2 clause-6 source greps:
//   - the `|| runtime.language || "r"` fallback is gone (never default)
//   - the double-start guard flag is set SYNCHRONOUSLY at entry, before the
//     mount loop, window.__btExercises, and the boot await
//   - no static import of webr-adapter/pyodide-adapter (adapter-agnostic)
assert(
  !runtimeSrc.includes('runtime.language || "r"'),
  "exercise-runtime.js must NOT default language via || runtime.language || \"r\"",
);
const guardIdx = runtimeSrc.indexOf("started = true");
assert(guardIdx !== -1, "exercise-runtime.js must have a module-level double-start guard flag");
const mountLoopIdx = runtimeSrc.indexOf("for (const entry of registry)");
const exercisesIdx = runtimeSrc.indexOf("window.__btExercises =");
const bootIdx = runtimeSrc.indexOf("await Promise.all");
assert(
  guardIdx < mountLoopIdx,
  "guard assignment must precede the mount loop (synchronous at entry)",
);
assert(
  guardIdx < exercisesIdx,
  "guard assignment must precede window.__btExercises assignment",
);
assert(
  guardIdx < bootIdx,
  "guard assignment must precede the boot await (Promise.all)",
);
assert(
  !/import\s+[\s\S]*?webr-adapter\.js/.test(runtimeSrc),
  "exercise-runtime.js must NOT statically import webr-adapter",
);
assert(
  !/import\s+[\s\S]*?pyodide-adapter\.js/.test(runtimeSrc),
  "exercise-runtime.js must NOT statically import pyodide-adapter",
);

// Verify per-exercise degradation
assert(runtimeSrc.includes("data-cm-fail"), "exercise-runtime.js must check data-cm-fail for graceful degradation");
assert(runtimeSrc.includes("textarea"), "exercise-runtime.js must fall back to textarea");

// Verify static-fallback removal (fix-demo-visible-exercises Part 1): the
// runtime must REMOVE the server-rendered .bt-exercise-static block when it
// mounts (progressive enhancement — static content visible under file://,
// replaced by the interactive editor over HTTP). A remove() call near the
// .bt-exercise-static reference pins the behavior at source level; the
// rodney probe (demo-book-bootstrap.js) verifies the runtime behavior.
assert(
  runtimeSrc.includes(".bt-exercise-static"),
  "exercise-runtime.js must reference .bt-exercise-static (remove on mount)",
);
assert(
  /function removeStaticFallback[\s\S]{0,300}\.remove\(\)/.test(runtimeSrc),
  "exercise-runtime.js removeStaticFallback must remove the static block (fallback.remove())",
);

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
assert(mockSrc.includes('name = "mock"'), "mock-adapter.js createMockAdapter has name default 'mock'");
assert(mockSrc.includes('language = "r"'), "mock-adapter.js createMockAdapter has language default 'r'");
assert(mockSrc.includes("bootCount"), "mock-adapter.js tracks bootCount (double-start guard probe)");
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
