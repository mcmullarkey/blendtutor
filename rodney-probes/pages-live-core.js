"use strict";
/**
 * Pure core for the pages-live rodney probe harness (issue #153 — AC-3
 * live/local verification of REAL R + Python execution + COI on the
 * demo-standalone page).
 *
 * WHAT:  The pure, effect-free half of pages-live.js (design intent §2):
 *          - timeout-budget constants (COI 30s / webR 120s / pyodide 60s,
 *            user-approved in decomposition)
 *          - report schema: verdict closed enum {pass, coi_failure,
 *            exec_failure, cm6_fallback_noted}; per-assertion status closed
 *            set {pass, fail, skip} — skip only with a recorded reason
 *          - assertion-record construction ({name, status, details})
 *          - path normalization: local /demo-standalone/index.html vs live
 *            DEPLOYED_URL (/demo/) verbatim with trailing slash stripped
 *          - pure report assembly from captured DOM snapshots
 *
 * WHAT NOT: NO effectful work — no rodney, no child_process, no fs, no DOM,
 * no static server. That is pages-live.js's job (effectful shell). Keeping
 * this module free of side effects makes every function here unit-testable
 * with `uv run node --test` and no browser.
 *
 * Unit tests: rodney-probes/pages-live-core.test.js
 *   uv run node --test rodney-probes/pages-live-core.test.js
 */

// Timeout budgets (user-approved in decomposition; see spec Technical
// Context "Timeouts: COI 30s / webR 120s / pyodide 60s").
const COI_TIMEOUT_S = 30; // P2 SW self-reload cycle stabilization
const WEBR_TIMEOUT_S = 120; // P3 webR cold boot (30-90s) + exec
const PYODIDE_TIMEOUT_S = 60; // P4 pyodide boot + exec
const VACUOUS_GUARD_TIMEOUT_S = 60; // P1 __btExercises population (PR #123 pattern)
const REGISTRY_SETTLE_S = 15; // post-COI final-load mount settle

// Report schema — closed enums (design intent §1). Do NOT extend ad hoc:
// reviewers and the vision-probe agent rely on this exact closed set.
const VERDICTS = Object.freeze({
  PASS: "pass",
  COI_FAILURE: "coi_failure",
  EXEC_FAILURE: "exec_failure",
  CM6_FALLBACK_NOTED: "cm6_fallback_noted",
});

const ASSERTION_STATUSES = Object.freeze({
  PASS: "pass",
  FAIL: "fail",
  SKIP: "skip",
});

const VALID_STATUSES = new Set(Object.values(ASSERTION_STATUSES));

// Path normalization: local mode serves the rendered demo-standalone page;
// live mode navigates to DEPLOYED_URL verbatim (e.g.
// https://<user>.github.io/<repo>/demo/). Trailing slashes are stripped so
// both modes produce a stable, comparable URL.
const LOCAL_PAGE_PATH = "/demo-standalone/index.html";

/** Strip trailing slashes from a URL; "" stays "". */
function normalizeDeployedUrl(url) {
  return (url || "").replace(/\/+$/, "");
}

/**
 * Resolve the page URL for a probe mode.
 * @param {Object} opts — { mode: "local"|"live", deployedUrl, baseUrl }
 * @returns {string} — local: `${baseUrl}/demo-standalone/index.html`;
 *   live: normalized DEPLOYED_URL.
 */
function pageUrlFor({ mode, deployedUrl, baseUrl }) {
  if (mode === "live") {
    const normalized = normalizeDeployedUrl(deployedUrl);
    if (!normalized) {
      throw new Error("live mode requires DEPLOYED_URL");
    }
    return normalized;
  }
  return `${baseUrl}${LOCAL_PAGE_PATH}`;
}

/** Normalize the CLI arg: only "live" is live; anything else is local. */
function normalizeMode(raw) {
  return raw === "live" ? "live" : "local";
}

/**
 * Construct an assertion record. Status is closed over {pass, fail, skip};
 * a skip record REQUIRES a recorded reason in details (spec: "skip only
 * with recorded reason" — an unexplained skip would silently drop a probe).
 * Throws TypeError on invalid input (programming error, fail fast).
 */
function makeRecord(name, status, details) {
  if (typeof name !== "string" || name.length === 0) {
    throw new TypeError("record name must be a non-empty string");
  }
  if (!VALID_STATUSES.has(status)) {
    throw new TypeError(
      `invalid assertion status "${status}" — must be one of ${[...VALID_STATUSES].join(", ")}`,
    );
  }
  if (status === ASSERTION_STATUSES.SKIP && !details) {
    throw new TypeError("skip records require a reason in details");
  }
  return { name, status, details: details || "" };
}

/**
 * Compute the closed-enum verdict from probe outcomes. Priority: coi_failure
 * wins over everything (P2 gate failed — execution never attempted);
 * cm6_fallback_noted is a distinct, non-fatal outcome recorded when CM6
 * degraded to textarea-only; otherwise any failed assertion ⇒ exec_failure.
 */
function computeVerdict({ coiFailed, cm6Fallback, failedCount }) {
  if (coiFailed) return VERDICTS.COI_FAILURE;
  if (cm6Fallback) return VERDICTS.CM6_FALLBACK_NOTED;
  return failedCount > 0 ? VERDICTS.EXEC_FAILURE : VERDICTS.PASS;
}

/**
 * Pure report assembly (§2) from captured DOM snapshots + assertion records.
 * The effectful harness calls this once, at the end, with everything it
 * captured; the returned object is written verbatim as probe-report.json.
 */
function buildReport({
  issue,
  branch,
  mode,
  deployedUrl,
  timestamp,
  verdict,
  probes,
  crossOriginIsolated,
  swController,
  rStatus,
  rOutput,
  pythonStatusA,
  pythonStatusB,
  pythonOutput,
  timings,
}) {
  return {
    issue,
    branch,
    mode,
    deployedUrl,
    timestamp,
    verdict,
    probes,
    crossOriginIsolated,
    swController,
    rStatus,
    rOutput,
    pythonStatusA,
    pythonStatusB,
    pythonOutput,
    timings,
  };
}

module.exports = {
  COI_TIMEOUT_S,
  WEBR_TIMEOUT_S,
  PYODIDE_TIMEOUT_S,
  VACUOUS_GUARD_TIMEOUT_S,
  REGISTRY_SETTLE_S,
  VERDICTS,
  ASSERTION_STATUSES,
  LOCAL_PAGE_PATH,
  normalizeDeployedUrl,
  pageUrlFor,
  normalizeMode,
  makeRecord,
  computeVerdict,
  buildReport,
};
