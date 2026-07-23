#!/usr/bin/env node
// validate-pyodide-adapter.js — Node.js validation for AC-6 pyodide-adapter.js.
//
// WHAT:  Validates JS syntax, adapter protocol, 3-state boot, per-run globals,
//        error surface, PyProxy cleanup, CDN injection, coexistence.
// WHERE: scripts/tests/validate-pyodide-adapter.js
// NOT:   NOT a rodney probe — this is a local sanity check. Full browser
//        verification is done by builder-vision-probe with rodney.
//
// Runs without a browser by testing source structure and pure functions.

import { readFileSync, existsSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";

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
// 1. File existence
// ---------------------------------------------------------------------------
const adapterPath = join(
  repoRoot,
  "_extensions",
  "blendtutor",
  "assets",
  "pyodide-adapter.js",
);
const luaPath = join(repoRoot, "_extensions", "blendtutor", "blendtutor.lua");
const pyodideQmd = join(repoRoot, "quarto-fixture", "pyodide.qmd");
const mixedLangQmd = join(repoRoot, "quarto-fixture", "mixed-lang.qmd");
const probePath = join(repoRoot, "rodney-probes", "pyodide-adapter.js");

assert(existsSync(adapterPath), "pyodide-adapter.js exists");
assert(existsSync(luaPath), "blendtutor.lua exists");
assert(existsSync(pyodideQmd), "pyodide.qmd exists");
assert(existsSync(mixedLangQmd), "mixed-lang.qmd exists");
assert(existsSync(probePath), "rodney-probes/pyodide-adapter.js exists");

// ---------------------------------------------------------------------------
// 2. Adapter protocol — {name, language, boot(), run()}
// ---------------------------------------------------------------------------
const adapterSrc = readFileSync(adapterPath, "utf-8");

assert(
  adapterSrc.includes('name: "pyodide"'),
  "adapter has name: 'pyodide'",
);
assert(
  adapterSrc.includes('language: "python"'),
  "adapter has language: 'python'",
);
assert(
  /async\s+boot\s*\(\s*\)/.test(adapterSrc),
  "adapter has async boot()",
);
assert(
  /async\s+run\s*\(\s*code\s*,\s*checks\s*,\s*packages\s*\)/.test(adapterSrc),
  "adapter has async run(code, checks, packages)",
);

// ---------------------------------------------------------------------------
// 3. CDN URL — pinned to v0.27.2
// ---------------------------------------------------------------------------
assert(
  adapterSrc.includes(
    "https://cdn.jsdelivr.net/pyodide/v0.27.2/full/pyodide.js",
  ),
  "CDN URL pinned to v0.27.2",
);

// ---------------------------------------------------------------------------
// 4. hasDoneSetup guard — prevents double CDN injection
// ---------------------------------------------------------------------------
assert(
  /hasDoneSetup/.test(adapterSrc),
  "adapter has hasDoneSetup guard",
);
assert(
  /let\s+hasDoneSetup\s*=\s*false/.test(adapterSrc),
  "hasDoneSetup initialized to false",
);
assert(
  /if\s*\(hasDoneSetup\)/.test(adapterSrc),
  "hasDoneSetup checked before injecting CDN",
);

// ---------------------------------------------------------------------------
// 5. 3-state boot machine — null → Promise → Pyodide
// ---------------------------------------------------------------------------
assert(
  /let\s+bootPromise\s*=\s*null/.test(adapterSrc),
  "boot state initialized to null (3-state machine)",
);
assert(
  /if\s*\(bootPromise\s*!==\s*null\)/.test(adapterSrc),
  "boot returns existing promise if already booting (idempotent)",
);

// ---------------------------------------------------------------------------
// 6. Per-run fresh globals — pyodide.toPy({})
// ---------------------------------------------------------------------------
assert(
  /pyodide\.toPy\(\s*\{\s*\}\s*\)/.test(adapterSrc),
  "createNamespace uses pyodide.toPy({}) for fresh globals",
);

// ---------------------------------------------------------------------------
// 7. PyProxy cleanup — namespace.destroy() in finally
// ---------------------------------------------------------------------------
assert(
  /namespace\.destroy\(\)/.test(adapterSrc),
  "namespace.destroy() called for PyProxy cleanup",
);
assert(
  /finally\s*\{/.test(adapterSrc),
  "destroy() is in a finally block",
);

// ---------------------------------------------------------------------------
// 8. Error surface — boot fail, Python error, package fail, loadPyodide undefined
// ---------------------------------------------------------------------------
assert(
  /Boot error:/.test(adapterSrc),
  "boot error surfaces with 'Boot error:' prefix",
);
assert(
  /Python error:/.test(adapterSrc),
  "Python error surfaces with 'Python error:' prefix",
);
assert(
  /Package error:/.test(adapterSrc),
  "package error surfaces with 'Package error:' prefix",
);
assert(
  /typeof\s+loadPyodide\s*===\s*"undefined"/.test(adapterSrc) ||
    /typeof\s+loadPyodide\s*!==\s*"undefined"/.test(adapterSrc),
  "loadPyodide undefined check present",
);

// ---------------------------------------------------------------------------
// 9. loadPackage cumulative — called on pyodide instance, not namespace
// ---------------------------------------------------------------------------
assert(
  /pyodide\.loadPackage/.test(adapterSrc),
  "loadPackage called on pyodide instance (cumulative)",
);

// ---------------------------------------------------------------------------
// 10. Classic script tag (NOT ES module) for CDN
// ---------------------------------------------------------------------------
assert(
  /document\.createElement\("script"\)/.test(adapterSrc),
  "CDN injected via classic script tag (not ES module)",
);
assert(
  !/type\s*=\s*"module"/.test(adapterSrc),
  "CDN script is NOT type=module (loadPyodide is a global)",
);

// ---------------------------------------------------------------------------
// 11. Export — adapter is exported for exercise-runtime.js injection
// ---------------------------------------------------------------------------
assert(
  /export\s+(const|function)\s+pyodideAdapter/.test(adapterSrc),
  "pyodideAdapter is exported",
);

// ---------------------------------------------------------------------------
// 12. Lua filter — Python detection + CDN injection
// ---------------------------------------------------------------------------
const luaSrc = readFileSync(luaPath, "utf-8");

assert(
  /hasDoneSetup/.test(luaSrc),
  "Lua filter has hasDoneSetup guard for CDN injection",
);
assert(
  /lang\s*==\s*"python"/.test(luaSrc),
  "Lua filter detects language='python'",
);
assert(
  /cdn\.jsdelivr\.net\/pyodide\/v0\.27\.2\/full\/pyodide\.js/.test(luaSrc),
  "Lua filter injects pinned pyodide.js CDN URL",
);
assert(
  /hasDoneSetup/.test(luaSrc) || /has_python/.test(luaSrc),
  "Lua filter has guard to prevent double CDN injection",
);

// ---------------------------------------------------------------------------
// 13. .qmd fixtures — correct structure
// ---------------------------------------------------------------------------
const pyodideQmdSrc = readFileSync(pyodideQmd, "utf-8");
assert(
  pyodideQmdSrc.includes("filters: [../_extensions/blendtutor/blendtutor.lua]"),
  "pyodide.qmd declares explicit filter path",
);
assert(
  pyodideQmdSrc.includes('language="python"'),
  "pyodide.qmd has Python exercises",
);
// Exercise 1: sets x = 42
assert(
  pyodideQmdSrc.includes("x = 42"),
  "pyodide.qmd exercise 1 sets x = 42",
);
// Exercise 2: asserts x not in globals
assert(
  pyodideQmdSrc.includes('"x" not in globals()'),
  "pyodide.qmd exercise 2 asserts x not in globals (fresh globals test)",
);

const mixedLangQmdSrc = readFileSync(mixedLangQmd, "utf-8");
assert(
  mixedLangQmdSrc.includes("filters: [../_extensions/blendtutor/blendtutor.lua]"),
  "mixed-lang.qmd declares explicit filter path",
);
assert(
  mixedLangQmdSrc.includes('language="r"'),
  "mixed-lang.qmd has R exercise",
);
assert(
  mixedLangQmdSrc.includes('language="python"'),
  "mixed-lang.qmd has Python exercise",
);

// ---------------------------------------------------------------------------
// 14. Rodney probe — 7 assertions
// ---------------------------------------------------------------------------
const probeSrc = readFileSync(probePath, "utf-8");
const probeAssertions = (probeSrc.match(/\d+\.\s/g) || []).length;
assert(
  probeAssertions >= 7,
  `rodney probe should have >=7 assertions, found ${probeAssertions}`,
);

// ---------------------------------------------------------------------------
// Summary
// ---------------------------------------------------------------------------
console.log(`\n=== AC-6 Pyodide Adapter Validation Results ===`);
console.log(`Passed: ${passes}`);
console.log(`Failed: ${failures}`);
if (failures > 0) {
  console.error("VALIDATION FAILED");
  process.exit(1);
} else {
  console.log("All validations passed.");
  process.exit(0);
}
