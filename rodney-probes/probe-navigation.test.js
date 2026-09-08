"use strict";
/**
 * Regression guard for the PR #233 CI failure — `rodney reload` CDP panic.
 *
 * WHAT HAPPENED: key-page-probe.js P7 called `rodney(["reload", "--hard"])`.
 * rodney 0.4.0's cmdReload → go-rod v0.116.2 Page.MustWaitLoad panicked with
 * CDP -32000 "Object reference chain is too long" against the pyodide-laden
 * demo-book key page (reproduced twice in CI). The panic killed the harness
 * after all P6 probes had already passed → PROBES_FAIL → red CI.
 *
 * THE INARIANT: every harness in rodney-probes/ navigates via the blank-page
 * bootstrap pattern (rodney open <blank> → js location.href → poll), which
 * never runs MustWaitLoad against a heavy page. No harness may invoke the
 * panicking `rodney reload` command; a fresh page load (same re-mount
 * semantics) is achieved by re-navigation instead.
 *
 * This is a structural pin of harness command usage — the only testable seam
 * without driving a real browser (the panic lives inside the rodney 0.4.0
 * binary + Chrome CDP, not in repo code). Same precedent as
 * rodney-chrome.test.js pinning the wrapper's argv contract.
 *
 * Run: uv run node --test rodney-probes/probe-navigation.test.js
 */

const { test } = require("node:test");
const assert = require("node:assert/strict");
const fs = require("fs");
const path = require("path");

const PROBES_DIR = __dirname;

// Invocation form only — prose mentions of "reload" in comments must not trip.
const RELOAD_INVOCATION = /rodney\(\s*\[\s*["']reload["']/;

// Every rodney harness in this dir (test files excluded — they only pin
// structure, they never drive rodney).
const harnesses = fs
  .readdirSync(PROBES_DIR)
  .filter((f) => f.endsWith(".js") && !f.endsWith(".test.js"))
  .sort();

test("scan found the probe harnesses (glob not silently empty)", () => {
  const names = harnesses.join(", ");
  assert.ok(
    harnesses.includes("key-page-probe.js") && harnesses.includes("feedback-probe.js"),
    `CI-gated harnesses missing from scan: ${names}`,
  );
});

test("no probe harness invokes the panicking `rodney reload` command", () => {
  const offenders = [];
  for (const file of harnesses) {
    const src = fs.readFileSync(path.join(PROBES_DIR, file), "utf8");
    if (RELOAD_INVOCATION.test(src)) {
      offenders.push(file);
    }
  }
  assert.deepEqual(
    offenders,
    [],
    "harnesses invoking `rodney reload` (cmdReload → MustWaitLoad panics with CDP " +
      "-32000 'Object reference chain is too long' on heavy pages; navigate via " +
      "the blank-page bootstrap pattern instead): " +
      offenders.join(", "),
  );
});
