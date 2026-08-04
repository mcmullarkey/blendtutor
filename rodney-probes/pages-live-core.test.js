"use strict";
/**
 * Pure-part unit tests for pages-live-core.js (issue #153 — AC-3 live/local
 * rodney probe harness). These tests exercise ONLY the pure core: report
 * schema (verdict closed enum, per-assertion status), assertion-record
 * construction, path normalization (local /demo-standalone/ vs live /demo/),
 * and timeout-budget constants. No rodney, no browser, no DOM — node:test.
 */

const { test } = require("node:test");
const assert = require("node:assert/strict");

const core = require("./pages-live-core.js");

// ---------------------------------------------------------------------------
// Timeout budgets (user-approved in decomposition)
// ---------------------------------------------------------------------------
test("timeout-budget constants match approved budgets", () => {
  assert.equal(core.COI_TIMEOUT_S, 30, "COI stabilization budget is 30s");
  assert.equal(core.WEBR_TIMEOUT_S, 120, "webR cold boot+exec budget is 120s");
  assert.equal(core.PYODIDE_TIMEOUT_S, 60, "pyodide boot+exec budget is 60s");
});

// ---------------------------------------------------------------------------
// Verdict closed enum {pass, coi_failure, exec_failure, cm6_fallback_noted}
// ---------------------------------------------------------------------------
test("verdict closed enum contains exactly the 4 spec values", () => {
  assert.deepEqual(
    [...new Set(Object.values(core.VERDICTS))].sort(),
    ["cm6_fallback_noted", "coi_failure", "exec_failure", "pass"],
  );
});

test("computeVerdict: clean run → pass", () => {
  assert.equal(
    core.computeVerdict({ coiFailed: false, cm6Fallback: false, failedCount: 0 }),
    "pass",
  );
});

test("computeVerdict: any failed assertion → exec_failure", () => {
  assert.equal(
    core.computeVerdict({ coiFailed: false, cm6Fallback: false, failedCount: 2 }),
    "exec_failure",
  );
});

test("computeVerdict: COI timeout → coi_failure (distinct from exec_failure)", () => {
  assert.equal(
    core.computeVerdict({ coiFailed: true, cm6Fallback: false, failedCount: 0 }),
    "coi_failure",
  );
});

test("computeVerdict: coi_failure wins even with failed assertions", () => {
  assert.equal(
    core.computeVerdict({ coiFailed: true, cm6Fallback: false, failedCount: 3 }),
    "coi_failure",
  );
});

test("computeVerdict: textarea-only CM6 fallback → cm6_fallback_noted (distinct, non-fatal)", () => {
  assert.equal(
    core.computeVerdict({ coiFailed: false, cm6Fallback: true, failedCount: 0 }),
    "cm6_fallback_noted",
  );
});

test("computeVerdict: exec_failure beats cm6_fallback — a real failure is never masked by the non-fatal note", () => {
  assert.equal(
    core.computeVerdict({ coiFailed: false, cm6Fallback: true, failedCount: 1 }),
    "exec_failure",
  );
});

test("computeVerdict: coi_failure wins over everything, including cm6_fallback + failures", () => {
  assert.equal(
    core.computeVerdict({ coiFailed: true, cm6Fallback: true, failedCount: 1 }),
    "coi_failure",
  );
});

// ---------------------------------------------------------------------------
// Assertion-record construction: {name, status, details}, status closed over
// {pass, fail, skip}; skip only with recorded reason
// ---------------------------------------------------------------------------
test("per-assertion status closed set is exactly {pass, fail, skip}", () => {
  assert.deepEqual(
    [...new Set(Object.values(core.ASSERTION_STATUSES))].sort(),
    ["fail", "pass", "skip"],
  );
});

test("makeRecord: pass record carries name/status/details", () => {
  assert.deepEqual(core.makeRecord("P1 mounts", "pass", "2 editors"), {
    name: "P1 mounts",
    status: "pass",
    details: "2 editors",
  });
});

test("makeRecord: fail record allowed without details (defaults to empty)", () => {
  assert.deepEqual(core.makeRecord("P3 R exec", "fail"), {
    name: "P3 R exec",
    status: "fail",
    details: "",
  });
});

test("makeRecord: skip record REQUIRES a reason in details", () => {
  assert.throws(
    () => core.makeRecord("P3 R exec", "skip"),
    /skip records require a reason/,
  );
  assert.deepEqual(
    core.makeRecord("P3 R exec", "skip", "skipped: coi_failure (P2 gate)"),
    { name: "P3 R exec", status: "skip", details: "skipped: coi_failure (P2 gate)" },
  );
});

test("makeRecord: rejects status outside the closed set", () => {
  assert.throws(
    () => core.makeRecord("P1", "nope", "details"),
    /invalid assertion status "nope"/,
  );
});

test("makeRecord: rejects empty name", () => {
  assert.throws(() => core.makeRecord("", "pass", "d"), /non-empty string/);
});

// ---------------------------------------------------------------------------
// Path normalization: local /demo-standalone/ vs live /demo/ (DEPLOYED_URL)
// ---------------------------------------------------------------------------
test("pageUrlFor: local mode serves /demo-standalone/index.html on base URL", () => {
  assert.equal(
    core.pageUrlFor({
      mode: "local",
      deployedUrl: "ignored",
      baseUrl: "http://localhost:8088",
    }),
    "http://localhost:8088/demo-standalone/index.html",
  );
});

test("pageUrlFor: live mode uses DEPLOYED_URL verbatim with trailing slash stripped", () => {
  assert.equal(
    core.pageUrlFor({
      mode: "live",
      deployedUrl: "https://user.github.io/blendtutor/demo/",
      baseUrl: "ignored",
    }),
    "https://user.github.io/blendtutor/demo",
  );
});

test("pageUrlFor: live mode keeps URL without trailing slash unchanged", () => {
  assert.equal(
    core.pageUrlFor({
      mode: "live",
      deployedUrl: "https://user.github.io/blendtutor/demo",
      baseUrl: "ignored",
    }),
    "https://user.github.io/blendtutor/demo",
  );
});

test("pageUrlFor: live mode without DEPLOYED_URL throws", () => {
  assert.throws(
    () =>
      core.pageUrlFor({
        mode: "live",
        deployedUrl: "",
        baseUrl: "ignored",
      }),
    /live mode requires DEPLOYED_URL/,
  );
});

test("normalizeDeployedUrl strips any number of trailing slashes", () => {
  assert.equal(core.normalizeDeployedUrl("https://x.io/a///"), "https://x.io/a");
  assert.equal(core.normalizeDeployedUrl(""), "");
});

test("normalizeMode defaults unknown/absent arg to local", () => {
  assert.equal(core.normalizeMode(undefined), "local");
  assert.equal(core.normalizeMode("live"), "live");
  assert.equal(core.normalizeMode("local"), "local");
});

// ---------------------------------------------------------------------------
// Report assembly (§2: pure artifact from captured DOM snapshots)
// ---------------------------------------------------------------------------
test("buildReport preserves records, snapshots, timings, and mode metadata", () => {
  const report = core.buildReport({
    issue: 153,
    branch: "153-live-probe",
    mode: "local",
    deployedUrl: "local",
    timestamp: "2026-08-04T00:00:00.000Z",
    verdict: "pass",
    probes: [
      { name: "P2 COI active", status: "pass", details: "settled in 1500ms" },
    ],
    crossOriginIsolated: "true",
    swController: "true",
    rStatus: "pass",
    rOutput: "3",
    pythonStatusA: "pass",
    pythonStatusB: "fail",
    pythonOutput: "ok | P4b: Check error",
    timings: { webR: 42000, pyPass: 8000, pyFail: 9000 },
  });

  assert.equal(report.issue, 153);
  assert.equal(report.branch, "153-live-probe");
  assert.equal(report.mode, "local");
  assert.equal(report.deployedUrl, "local");
  assert.equal(report.verdict, "pass");
  assert.equal(report.probes.length, 1);
  assert.equal(report.crossOriginIsolated, "true");
  assert.equal(report.swController, "true");
  assert.equal(report.rOutput, "3");
  assert.equal(report.pythonStatusB, "fail");
  assert.equal(report.timings.webR, 42000);
});

test("buildReport: live mode records deployedUrl, not the literal 'local'", () => {
  const report = core.buildReport({
    issue: 153,
    branch: "153-live-probe",
    mode: "live",
    deployedUrl: "https://user.github.io/blendtutor/demo",
    timestamp: "2026-08-04T00:00:00.000Z",
    verdict: "pass",
    probes: [],
    crossOriginIsolated: "true",
    swController: "true",
    rStatus: "pass",
    rOutput: "3",
    pythonStatusA: "pass",
    pythonStatusB: "pass",
    pythonOutput: "ok | P4b: ok",
    timings: {},
  });

  assert.equal(report.deployedUrl, "https://user.github.io/blendtutor/demo");
  assert.notEqual(report.deployedUrl, "local");
});
