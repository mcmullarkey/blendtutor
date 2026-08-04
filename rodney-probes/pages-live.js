#!/usr/bin/env node
/**
 * Rodney probe harness for issue #153 — AC-3 live/local verification of REAL
 * R + Python execution + COI on the demo-standalone page.
 *
 * WHAT:  Verifies the deployed-or-local demo-standalone page end-to-end in a
 *        headless browser driven by uvx rodney:
 *          P1  CM6 editors mount (real .cm-content children, registry .get()
 *              works, length 2 — cm6_fallback recorded distinct if
 *              textarea-only).
 *          P2  COI active: poll ≤30s THROUGH the SW self-reload cycle
 *              (coi-serviceworker.js registers → self-reloads once gated on
 *              sessionStorage.coiReloadedBySelf) until crossOriginIsolated
 *              === true AND navigator.serviceWorker.controller !== null.
 *              Timeout → coi_failure verdict, P3/P4 skipped.
 *          P3  REAL R execution: setEditorContent('add <- function(a, b) { a
 *              + b }\nprint(add(1, 2))') → click .bt-run-btn → poll ≤120s
 *              (webR cold boot 30-90s) until data-status="pass" AND
 *              .bt-output textContent contains "3". Status-only is
 *              insufficient — webR ignores the checks param and a pure
 *              definition yields output "", so a fake adapter
 *              {output:"", ok:true} fails P3.
 *          P4  REAL Python execution, BIDIRECTIONAL: (P4a) correct square
 *              solution → data-status="pass" + output "ok" ≤60s; (P4b)
 *              incorrect body (return 0) → data-status="fail" + output
 *              "Check error" — proves the pyodide adapter executes user code
 *              AND runs checks (always-pass adapter caught).
 *          P5  Exit 0 + probe-report.json + rodney.log written with verdict,
 *              per-assertion {name, status, details}, actual R + Python
 *              outputs, crossOriginIsolated, controller, boot timings, and
 *              DEPLOYED_URL / "local".
 *
 * WHAT NOT: NOT layout/visual assertions (no screenshots), NOT runtime unit
 * behavior (earlier probes own that), NOT modifying exercise-runtime.js,
 * adapters, filter, or demo-standalone/ source (AC-1 surface untouched).
 *
 * Modes:
 *   local (default) — renders demo-standalone/ if index.html missing
 *   (quarto render + scripts/fix-demo-coi-scope.sh), serves the worktree
 *   root on :8088 (localhost is a secure context so the SW registers), then
 *   navigates to /demo-standalone/index.html.
 *   live — DEPLOYED_URL must be set; curl HEAD pre-checks
 *   coi-serviceworker.js at the deployed root (fail fast with a clear
 *   message), then navigates to DEPLOYED_URL verbatim (path normalized by
 *   stripping a trailing slash).
 *
 * Usage:
 *   uv run node rodney-probes/pages-live.js local
 *   DEPLOYED_URL=https://<user>.github.io/<repo>/demo/ uv run node rodney-probes/pages-live.js live
 *
 * Environment:
 *   DEPLOYED_URL - live-mode target URL (local mode ignores it)
 *   STATIC_PORT  - local static server port (default: 8088)
 *   EVIDENCE_DIR - evidence output dir relative to repo root (default:
 *                  docs/evidence/153)
 *   ROD_CHROME_BIN - Chrome binary/wrapper for rodney. If unset, this harness
 *                  sets it to scripts/rodney-chrome.sh (committed wrapper that
 *                  strips rodney's hardcoded --single-process /
 *                  --disable-site-isolation-trials / --disable-features=...
 *                  flags, which would otherwise permanently break P2
 *                  crossOriginIsolated). The wrapper resolves a real Chrome
 *                  via $REAL_CHROME env → macOS → Linux → rodney-managed
 *                  Chromium.orig; see scripts/rodney-chrome.sh.
 *   REAL_CHROME   - (optional) explicit Chrome/Chromium binary path for the
 *                  wrapper, e.g. REAL_CHROME=/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome
 */

const { execFileSync, spawn } = require("child_process");
const fs = require("fs");
const path = require("path");
const os = require("os");

// Pure core (§2): verdict enum, assertion-record construction, path
// normalization, timeout budgets, report assembly — unit-tested in
// pages-live-core.test.js (no rodney/browser required).
const core = require("./pages-live-core.js");

const WORKTREE = path.resolve(__dirname, "..");

// rodney offers no Chrome-flag override; the only escape hatches are
// ROD_CHROME_BIN (binary swap) or the connect API. rodney 0.4.0 hardcodes
// --single-process and go-rod adds --disable-site-isolation-trials +
// --disable-features=site-per-process — all of which permanently break
// crossOriginIsolated (P2). Route rodney's Chrome through the committed
// scripts/rodney-chrome.sh wrapper UNLESS the caller already overrode it,
// so this harness is self-sufficient on any machine (see script header for
// the full research rationale).
const RODNEY_CHROME_WRAPPER = path.join(WORKTREE, "scripts", "rodney-chrome.sh");
if (!process.env.ROD_CHROME_BIN) {
  if (!fs.existsSync(RODNEY_CHROME_WRAPPER)) {
    console.error(
      `FATAL: committed rodney Chrome wrapper missing at ${RODNEY_CHROME_WRAPPER} — cannot assert P2 crossOriginIsolated (rodney's default Chrome flags break it). Install the wrapper or set ROD_CHROME_BIN explicitly.`,
    );
    process.exit(2);
  }
  process.env.ROD_CHROME_BIN = RODNEY_CHROME_WRAPPER;
}

const BRANCH = execFileSync(
  "git",
  ["-C", WORKTREE, "rev-parse", "--abbrev-ref", "HEAD"],
  { encoding: "utf8" },
).trim();
const EVIDENCE_DIR = path.resolve(
  WORKTREE,
  process.env.EVIDENCE_DIR || "docs/evidence/153",
);
const STATIC_PORT = parseInt(process.env.STATIC_PORT || "8088", 10);

// ---------------------------------------------------------------------------
// Mode dispatch (§3 boundary cut: probe owns serving/driving/reporting)
// ---------------------------------------------------------------------------
const MODE = core.normalizeMode(process.argv[2]);
const DEPLOYED_URL = core.normalizeDeployedUrl(process.env.DEPLOYED_URL || "");
if (MODE === "live" && !DEPLOYED_URL) {
  console.error(
    "live mode requires DEPLOYED_URL (e.g. DEPLOYED_URL=https://<user>.github.io/<repo>/demo/ uv run node rodney-probes/pages-live.js live)",
  );
  process.exit(2);
}

const BASE_URL = `http://localhost:${STATIC_PORT}`;
const BLANK_URL = `${BASE_URL}/quarto-fixture/_probe-blank.html`;
// Path normalization (§3, pure core): local /demo-standalone/ vs live
// DEPLOYED_URL verbatim (e.g. https://<user>.github.io/<repo>/demo/).
const PAGE_URL = core.pageUrlFor({
  mode: MODE,
  deployedUrl: DEPLOYED_URL,
  baseUrl: BASE_URL,
});

const STATIC_SERVER_PY = `#!/usr/bin/env python3
import http.server, socketserver, sys
PORT = int(sys.argv[1]) if len(sys.argv) > 1 else 8088
class Handler(http.server.SimpleHTTPRequestHandler):
    def log_message(self, fmt, *args): pass
socketserver.ThreadingTCPServer.allow_reuse_address = True
with socketserver.ThreadingTCPServer(("", PORT), Handler) as httpd:
    print(f"static server on port {PORT}", flush=True)
    httpd.serve_forever()
`;

// ---------------------------------------------------------------------------
// Effectful shell state (no shared mutable probe state — §5)
// ---------------------------------------------------------------------------
const servers = [];
let rodneyStarted = false;
const probeLog = [];
let coiFailed = false;
let cm6Fallback = false;
let crossOriginIsolated = "not-checked";
let swController = "not-checked";
let rOutput = "";
let pythonOutput = "";
let rStatus = "";
let pythonStatusA = "";
let pythonStatusB = "";
const timings = {};

function sleep(seconds) {
  if (seconds > 0) {
    execFileSync("sleep", [String(seconds)]);
  }
}

function writeTempScript(name, code) {
  const file = path.join(os.tmpdir(), `pages-live-${name}.py`);
  fs.writeFileSync(file, code);
  return file;
}

function waitForPort(port, timeoutSeconds = 10) {
  const deadline = Date.now() + timeoutSeconds * 1000;
  while (Date.now() < deadline) {
    try {
      execFileSync(
        "curl",
        ["-s", "-o", "/dev/null", `http://localhost:${port}/`],
        { timeout: 500 },
      );
      return true;
    } catch (_) {
      sleep(0.2);
    }
  }
  return false;
}

function startServer() {
  const script = writeTempScript("serve", STATIC_SERVER_PY);
  const proc = spawn("python3", [script, String(STATIC_PORT)], {
    cwd: WORKTREE,
    detached: true,
    stdio: "ignore",
  });
  proc.unref();
  servers.push(proc);
  if (!waitForPort(STATIC_PORT)) {
    throw new Error(`Static server did not start on port ${STATIC_PORT}`);
  }
}

function stopServers() {
  for (const proc of servers) {
    try {
      process.kill(-proc.pid, "SIGTERM");
    } catch (_) {}
  }
}

function rodney(args, timeoutMs = 60000) {
  // Pin rodney to 0.4.0 (--from) so go-rod's default Chrome flags can't
  // drift under us — the wrapper's flag-stripping targets exactly this
  // version's hardcoded flags. Established uvx invocation pattern
  // (demo-book-bootstrap.js:124), with the version pin added.
  const out = execFileSync("uvx", ["--from", "rodney==0.4.0", "rodney", ...args], {
    cwd: WORKTREE,
    encoding: "utf8",
    timeout: timeoutMs,
  });
  return out.trim();
}

function record(name, status, details) {
  // Status validated against the closed set {pass, fail, skip} (pure core);
  // a skip without a reason throws — an unexplained skip would silently
  // drop a probe from the report.
  const rec = core.makeRecord(name, status, details);
  probeLog.push(rec);
  console.log(`[${rec.status.toUpperCase()}] ${rec.name}: ${rec.details}`);
}

function rodneyJs(code) {
  return rodney(["js", code]);
}

/** Evaluate a boolean expression via rodney js and record {pass|fail}. */
function assertExpr(name, expr) {
  let raw;
  try {
    raw = rodneyJs(`(() => { return ${expr}; })()`);
  } catch (err) {
    record(name, "fail", err.stderr || err.message || "rodney js failed");
    return false;
  }
  const ok = raw === "true";
  record(name, ok ? "pass" : "fail", ok ? "expression true" : `returned: ${raw}`);
  return ok;
}

/**
 * Poll a boolean expression via rodney js until it returns true or the
 * timeout elapses. Transient rodney failures (page navigation, SW reload)
 * are swallowed and retried. Returns elapsed ms on success, -1 on timeout.
 */
function waitForExpr(expr, timeoutSeconds = 30) {
  const deadline = Date.now() + timeoutSeconds * 1000;
  const start = Date.now();
  while (Date.now() < deadline) {
    try {
      const raw = rodneyJs(`(() => { return ${expr}; })()`);
      if (raw === "true") return Date.now() - start;
    } catch (_) {}
    sleep(0.2);
  }
  return -1;
}

function generateBlankPage() {
  fs.writeFileSync(
    path.join(WORKTREE, "quarto-fixture", "_probe-blank.html"),
    "<!DOCTYPE html><html><body></body></html>",
  );
}

function navigateToPage() {
  // rodney open panics on heavy first loads; navigate from a blank page
  // (established pattern in demo-book-bootstrap.js / feedback-probe.js).
  rodney(["open", BLANK_URL]);
  rodney(["js", `window.location.href = '${PAGE_URL}'`]);
  sleep(3);
}

function renderDemoStandaloneIfNeeded() {
  const html = path.join(WORKTREE, "demo-standalone", "index.html");
  const shim = path.join(WORKTREE, "demo-standalone", "coi-serviceworker.js");
  if (!fs.existsSync(html)) {
    console.log("Rendering demo-standalone/ (quarto render) ...");
    execFileSync(
      "quarto",
      ["render", "demo-standalone", "--to", "html"],
      { cwd: WORKTREE, encoding: "utf8", timeout: 300000 },
    );
  }
  if (!fs.existsSync(html)) {
    throw new Error("demo-standalone/index.html missing after quarto render");
  }
  if (!fs.existsSync(shim)) {
    console.log("Running scripts/fix-demo-coi-scope.sh demo-standalone ...");
    execFileSync(
      "bash",
      ["scripts/fix-demo-coi-scope.sh", "demo-standalone"],
      { cwd: WORKTREE, encoding: "utf8", timeout: 60000 },
    );
  }
}

function precheckLiveDeployment() {
  const shimUrl = `${PAGE_URL}/coi-serviceworker.js`;
  let status;
  try {
    status = execFileSync(
      "curl",
      ["-sI", "-o", "/dev/null", "-w", "%{http_code}", shimUrl],
      { encoding: "utf8", timeout: 30000 },
    ).trim();
  } catch (err) {
    console.error(
      `FAIL: live pre-check — coi-serviceworker.js not reachable at ${shimUrl} (${err.message})`,
    );
    record(
      "live pre-check: coi-serviceworker.js reachable",
      "fail",
      `curl HEAD failed for ${shimUrl}`,
    );
    return false;
  }
  const ok = status.startsWith("2") || status.startsWith("3");
  record(
    "live pre-check: coi-serviceworker.js reachable",
    ok ? "pass" : "fail",
    `curl HEAD ${shimUrl} → HTTP ${status}`,
  );
  return ok;
}

// ---------------------------------------------------------------------------
// P1 — CM6 mounts + registry (§5: one function per P-clause)
// ---------------------------------------------------------------------------
function probeP1Mounts() {
  // Vacuous-guard kill (PR #123 pattern): __btExercises must be defined
  // BEFORE any guarded block; a silently-missing registry would vacuous-pass
  // every subsequent assertion.
  const elapsed = waitForExpr(
    "window.__btExercises !== undefined",
    core.VACUOUS_GUARD_TIMEOUT_S,
  );
  if (elapsed === -1) {
    record(
      "P1 vacuous guard: __btExercises defined",
      "fail",
      `registry undefined after ${core.VACUOUS_GUARD_TIMEOUT_S}s — bootstrap module did not run`,
    );
    return;
  }

  record(
    "P1 vacuous guard: __btExercises defined",
    "pass",
    `registry populated (${elapsed}ms)`,
  );

  assertExpr(
    "P1 registry: __btExercises.length === 2",
    "window.__btExercises.length === 2",
  );
  assertExpr(
    "P1 registry: .get('bt-exercise-0') non-null",
    "window.__btExercises.get('bt-exercise-0') !== null",
  );
  assertExpr(
    "P1 registry: .get('bt-exercise-1') non-null",
    "window.__btExercises.get('bt-exercise-1') !== null",
  );

  const cmCount = rodneyJs(
    "document.querySelectorAll('.cm-editor').length",
  );
  const taCount = rodneyJs(
    "document.querySelectorAll('.bt-exercise textarea').length",
  );
  if (cmCount === "0" && taCount === "2") {
    // Distinct finding, NOT a hard fail (spec AC-3 line 12 + design intent):
    // execution remains valid via the textarea fallback (exercise-runtime.js
    // mountEditor graceful degradation). Recorded as pass-with-fallback-note
    // so it does NOT count toward failedCount — the cm6Fallback flag alone
    // drives the non-fatal cm6_fallback_noted verdict. Predicate is EXACTLY
    // zero CM6 editors (spec literal): a partial mount (e.g. 1 .cm-editor +
    // 2 textarea) must NOT be misclassified non-fatal — it falls through to
    // the hard-fail assert below, as does a real mount failure (no CM6 AND
    // no textarea).
    cm6Fallback = true;
    record(
      "P1 mounts: textarea fallback (CM6 unavailable)",
      "pass",
      `cm6_fallback — ${cmCount} .cm-editor, ${taCount} textarea (CM6 degraded, textarea fallback active)`,
    );
  } else {
    assertExpr(
      "P1 mounts: 2 real CM6 editors",
      "document.querySelectorAll('.cm-editor').length === 2",
    );
    // Real CM6 editors (not fake divs) expose a .cm-content child.
    assertExpr(
      "P1 mounts: every .cm-editor has a .cm-content child",
      "Array.from(document.querySelectorAll('.cm-editor')).every(e => e.querySelector('.cm-content') !== null)",
    );
  }
}

// ---------------------------------------------------------------------------
// P2 — COI gate: poll ≤30s through the SW self-reload cycle
// ---------------------------------------------------------------------------
function probeP2Coi() {
  const elapsed = waitForExpr(
    "crossOriginIsolated === true && navigator.serviceWorker && navigator.serviceWorker.controller !== null",
    core.COI_TIMEOUT_S,
  );
  timings.coi = elapsed;
  if (elapsed === -1) {
    coiFailed = true;
    record(
      "P2 COI active (crossOriginIsolated + controller)",
      "fail",
      `timeout ${core.COI_TIMEOUT_S}s — SW self-reload cycle did not settle; coi_failure verdict, P3/P4 skipped`,
    );
    return;
  }
  crossOriginIsolated = rodneyJs("crossOriginIsolated");
  swController = rodneyJs(
    "navigator.serviceWorker && navigator.serviceWorker.controller !== null",
  );
  record(
    "P2 COI active (crossOriginIsolated + controller)",
    "pass",
    `settled in ${elapsed}ms; crossOriginIsolated=${crossOriginIsolated}, controller=${swController}`,
  );
}

// ---------------------------------------------------------------------------
// P3 — REAL R execution via webR (status-only insufficient: assert "3")
// ---------------------------------------------------------------------------
function probeP3RExec() {
  const R_CODE = "add <- function(a, b) { a + b }\nprint(add(1, 2))";
  const sel = '.bt-exercise[data-language="r"]';

  let setOk;
  try {
    setOk =
      rodneyJs(
        `(() => { const e = window.__btExercises.find(x => x.element.dataset.language === 'r'); if (!e) return false; e.setEditorContent(${JSON.stringify(R_CODE)}); return true; })()`,
      ) === "true";
  } catch (err) {
    setOk = false;
  }
  if (!setOk) {
    record(
      "P3 R exec: setEditorContent + run",
      "fail",
      "R exercise entry not found or setEditorContent failed",
    );
    return;
  }

  rodney(["click", `${sel} .bt-run-btn`]);
  const elapsed = waitForExpr(
    `document.querySelector('${sel} .bt-status').dataset.status === 'pass'`,
    core.WEBR_TIMEOUT_S,
  );
  timings.webR = elapsed === -1 ? null : elapsed;

  try {
    rStatus = rodneyJs(
      `document.querySelector('${sel} .bt-status').dataset.status`,
    );
    rOutput = JSON.parse(
      rodneyJs(
        `JSON.stringify(document.querySelector('${sel} .bt-output').textContent)`,
      ),
    );
  } catch (_) {
    rStatus = "unreadable";
    rOutput = "";
  }

  const statusOk = elapsed !== -1;
  record(
    "P3 R exec: data-status reaches 'pass'",
    statusOk ? "pass" : "fail",
    statusOk
      ? `webR boot+exec settled in ${elapsed}ms (status ${rStatus})`
      : `timeout ${core.WEBR_TIMEOUT_S}s — status stuck at ${rStatus}`,
  );

  // Sneaky-pass kill: webR ignores checks and a pure definition yields
  // output "" — status alone proves nothing. The computed value "3" must
  // appear in .bt-output.
  const outputOk = statusOk && rOutput.includes("3");
  record(
    "P3 R exec: .bt-output contains computed '3'",
    outputOk ? "pass" : "fail",
    `output=${JSON.stringify(rOutput)}`,
  );
}

// ---------------------------------------------------------------------------
// P4 — REAL Python execution, bidirectional (proves checks execute)
// ---------------------------------------------------------------------------
function probeP4PyExec() {
  const sel = '.bt-exercise[data-language="python"]';

  function runOnce(code) {
    rodneyJs(
      `(() => { const e = window.__btExercises.find(x => x.element.dataset.language === 'python'); if (!e) return false; e.setEditorContent(${JSON.stringify(code)}); return true; })()`,
    );
    rodney(["click", `${sel} .bt-run-btn`]);
    return waitForExpr(
      `document.querySelector('${sel} .bt-status').dataset.status === 'pass'`,
      core.PYODIDE_TIMEOUT_S,
    );
  }

  // P4a — correct square: checks pass → output "ok" literal (pyodide
  // adapter contract).
  const t0 = Date.now();
  const passElapsed = runOnce("def square(n):\n    return n * n");
  timings.pyPass = passElapsed === -1 ? Date.now() - t0 : passElapsed;
  try {
    pythonStatusA = rodneyJs(
      `document.querySelector('${sel} .bt-status').dataset.status`,
    );
    pythonOutput = JSON.parse(
      rodneyJs(
        `JSON.stringify(document.querySelector('${sel} .bt-output').textContent)`,
      ),
    );
  } catch (_) {
    pythonStatusA = "unreadable";
    pythonOutput = "";
  }

  const aOk = passElapsed !== -1 && pythonOutput === "ok";
  record(
    "P4a Python: correct square → pass + output 'ok'",
    aOk ? "pass" : "fail",
    `status=${pythonStatusA}, output=${JSON.stringify(pythonOutput)}, ${passElapsed !== -1 ? passElapsed + "ms" : `timeout ${core.PYODIDE_TIMEOUT_S}s`}`,
  );

  // P4b — incorrect body (return 0): checks must FAIL → data-status="fail"
  // + "Check error" output. An always-pass adapter cannot reach "fail".
  rodneyJs(
    `(() => { const e = window.__btExercises.find(x => x.element.dataset.language === 'python'); if (!e) return false; e.setEditorContent(${JSON.stringify("def square(n):\n    return 0")}); return true; })()`,
  );
  rodney(["click", `${sel} .bt-run-btn`]);
  const t1 = Date.now();
  const failElapsed = waitForExpr(
    `document.querySelector('${sel} .bt-status').dataset.status === 'fail'`,
    core.PYODIDE_TIMEOUT_S,
  );
  timings.pyFail = failElapsed === -1 ? Date.now() - t1 : failElapsed;
  try {
    pythonStatusB = rodneyJs(
      `document.querySelector('${sel} .bt-status').dataset.status`,
    );
    const outB = JSON.parse(
      rodneyJs(
        `JSON.stringify(document.querySelector('${sel} .bt-output').textContent)`,
      ),
    );
    pythonOutput = `${pythonOutput} | P4b: ${outB}`;
    const bOk = failElapsed !== -1 && outB.includes("Check error");
    record(
      "P4b Python: incorrect return 0 → fail + 'Check error'",
      bOk ? "pass" : "fail",
      `status=${pythonStatusB}, output=${JSON.stringify(outB)}, ${failElapsed !== -1 ? failElapsed + "ms" : `timeout ${core.PYODIDE_TIMEOUT_S}s`}`,
    );
  } catch (_) {
    record(
      "P4b Python: incorrect return 0 → fail + 'Check error'",
      "fail",
      "output unreadable after run",
    );
  }
}

// ---------------------------------------------------------------------------
// Report construction (§2: pure artifact from captured DOM snapshots)
// ---------------------------------------------------------------------------
function writeReport() {
  fs.mkdirSync(EVIDENCE_DIR, { recursive: true });
  const failed = probeLog.filter((p) => p.status === "fail");

  // Verdict closed enum {pass, coi_failure, exec_failure, cm6_fallback_noted}
  // computed by the pure core (coi_failure wins; cm6_fallback distinct).
  const verdict = core.computeVerdict({
    coiFailed,
    cm6Fallback,
    failedCount: failed.length,
  });

  const report = core.buildReport({
    issue: 153,
    branch: BRANCH,
    mode: MODE,
    deployedUrl: MODE === "live" ? DEPLOYED_URL : "local",
    timestamp: new Date().toISOString(),
    verdict,
    probes: probeLog,
    crossOriginIsolated,
    swController,
    rStatus,
    rOutput,
    pythonStatusA,
    pythonStatusB,
    pythonOutput,
    timings,
  });

  fs.writeFileSync(
    path.join(EVIDENCE_DIR, "probe-report.json"),
    JSON.stringify(report, null, 2),
  );

  const lines = [
    `# Rodney probes for issue #153 (${MODE} mode)`,
    `verdict: ${verdict}`,
    `deployedUrl: ${report.deployedUrl}`,
    `timestamp: ${report.timestamp}`,
    "",
    ...probeLog.map((p) => `- ${p.status}: ${p.name}\n  ${p.details}`),
  ];
  fs.writeFileSync(path.join(EVIDENCE_DIR, "rodney.log"), lines.join("\n"));

  console.log(`\n=== ${verdict} ===`);
  console.log(`report: ${path.join(EVIDENCE_DIR, "probe-report.json")}`);
  console.log(`log:    ${path.join(EVIDENCE_DIR, "rodney.log")}`);
  return verdict;
}

function main() {
  let verdict = "exec_failure";
  try {
    if (MODE === "live") {
      if (!precheckLiveDeployment()) {
        throw new Error("live pre-check failed — aborting before navigation");
      }
    } else {
      renderDemoStandaloneIfNeeded();
    }
    // Serve the worktree root in BOTH modes: navigateToPage() bootstraps
    // from the local blank page (established pattern — rodney open panics on
    // heavy first loads) before hopping to PAGE_URL, so the blank page must
    // resolve even when the asserted target is the deployed URL. In live
    // mode the localhost server just sits idle after the hop.
    startServer();
    generateBlankPage();

    rodney(["start"]);
    rodneyStarted = true;

    navigateToPage();

    // COI gate FIRST (P2): the SW self-reload cycle must settle before any
    // execution attempt. P1's vacuous guard absorbs the initial load(s).
    probeP2Coi();

    // P1 is UNCONDITIONAL (spec AC-3 line 12): mounts/registry diagnostics
    // must be captured even on coi_failure — a COI report with zero P1
    // records cannot tell whether the page mounted. Wait for the page to
    // settle, then assert P1; ONLY P3/P4 skip on the P2 timeout.
    waitForExpr(
      "window.__btExercises !== undefined && document.querySelectorAll('.cm-editor').length === 2",
      core.REGISTRY_SETTLE_S,
    );
    probeP1Mounts();

    if (coiFailed) {
      record("P3 R exec", "skip", "skipped: coi_failure (P2 gate)");
      record("P4 Python exec", "skip", "skipped: coi_failure (P2 gate)");
    } else {
      // COI true implies the SW reload cycle finished and the page is
      // settled: execute P3/P4.
      probeP3RExec();
      probeP4PyExec();
    }

    verdict = writeReport();
  } catch (err) {
    console.error("Probe harness failed:", err.message);
    record("harness", "fail", err.message);
    verdict = writeReport();
  } finally {
    if (rodneyStarted) {
      try {
        rodney(["stop"]);
      } catch (_) {}
    }
    stopServers();
  }

  process.exit(verdict === "pass" || verdict === "cm6_fallback_noted" ? 0 : 1);
}

main();
