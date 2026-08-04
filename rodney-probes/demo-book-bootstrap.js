#!/usr/bin/env node
/**
 * Rodney probe harness for issue #143 — AC-5 demo-book by-name install e2e
 * R/Python boot verification (clause 8).
 *
 * Verifies clause 8 of the AC-5 predicate against the RENDERED demo-book
 * _output pages in a headless browser driven by uvx rodney:
 *   r-exercises.html      → window.__btExercises.length === 2 AND
 *                           .cm-editor count === 2 AND every entry
 *                           data-language === "r"
 *   python-exercises.html → window.__btExercises.length === 2 AND
 *                           .cm-editor count === 2 AND every entry
 *                           data-language === "python"
 *
 * The assertions NEVER wait for boot completion: webR/pyodide boot pulls CDN
 * assets that may be offline in the probe environment. start() sets
 * window.__btExercises synchronously BEFORE the awaited adapter boot
 * (exercise-runtime.js:485-491) and mounts editors before that — so
 * __btExercises.length and .cm-editor count are safe, deterministic probes
 * of the auto-bootstrap's dispatch wiring without any CDN dependency.
 *
 * The probe loads demo-book/_output/<page>.html (never the source qmd) and
 * waits ONLY on __btExercises population.
 *
 * Usage:
 *   uv run node rodney-probes/demo-book-bootstrap.js
 *
 * Environment:
 *   STATIC_PORT - port for static demo-book server (default: 8087)
 */

const { execFileSync, spawn, execSync } = require("child_process");
const fs = require("fs");
const path = require("path");
const os = require("os");

const WORKTREE = path.resolve(__dirname, "..");
// Evidence dir is parameterized (fix-demo-visible-exercises): default to
// docs/evidence/<branch>, override with EVIDENCE_DIR. Hardcoded issue dirs
// go stale across fixes — the report metadata derives from the branch + dir
// basename instead.
const BRANCH = execSync("git rev-parse --abbrev-ref HEAD", { cwd: WORKTREE })
  .toString()
  .trim();
const EVIDENCE_DIR = path.join(
  WORKTREE,
  "docs",
  "evidence",
  process.env.EVIDENCE_DIR || BRANCH,
);
const STATIC_PORT = parseInt(process.env.STATIC_PORT || "8087", 10);

const BASE_URL = `http://localhost:${STATIC_PORT}`;
const BLANK_URL = `${BASE_URL}/quarto-fixture/_probe-blank.html`;
const INDEX_URL = `${BASE_URL}/demo-book/_output/index.html`;
const R_URL = `${BASE_URL}/demo-book/_output/r-exercises.html`;
const PY_URL = `${BASE_URL}/demo-book/_output/python-exercises.html`;

const STATIC_SERVER_PY = `#!/usr/bin/env python3
import http.server, socketserver, sys
PORT = int(sys.argv[1]) if len(sys.argv) > 1 else 8087
class Handler(http.server.SimpleHTTPRequestHandler):
    def log_message(self, fmt, *args): pass
socketserver.ThreadingTCPServer.allow_reuse_address = True
with socketserver.ThreadingTCPServer(("", PORT), Handler) as httpd:
    print(f"static server on port {PORT}", flush=True)
    httpd.serve_forever()
`;

const servers = [];
let rodneyStarted = false;
const probeLog = [];

function sleep(seconds) {
  if (seconds > 0) {
    execFileSync("sleep", [String(seconds)]);
  }
}

function writeTempScript(name, code) {
  const file = path.join(os.tmpdir(), `demo-book-${name}.py`);
  fs.writeFileSync(file, code);
  return file;
}

function waitForPort(port, timeoutSeconds = 10) {
  const deadline = Date.now() + timeoutSeconds * 1000;
  while (Date.now() < deadline) {
    try {
      execFileSync("curl", ["-s", "-o", "/dev/null", `http://localhost:${port}/`], {
        timeout: 500,
      });
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

function rodney(args) {
  const out = execFileSync("uvx", ["rodney", ...args], {
    cwd: WORKTREE,
    encoding: "utf8",
    timeout: 60000,
  });
  return out.trim();
}

function record(name, passed, details) {
  const status = passed ? "PASS" : "FAIL";
  probeLog.push({ name, status, details });
  console.log(`[${status}] ${name}: ${details}`);
}

function rodneyJs(code) {
  return rodney(["js", code]);
}

function rodneyAssert(name, expr) {
  // Evaluate a boolean expression via `rodney js` and record the result.
  const wrapped = `(() => { ${expr}; })()`;
  let raw;
  try {
    raw = rodneyJs(wrapped);
  } catch (err) {
    record(name, false, err.stderr || err.message || "rodney js failed");
    return;
  }
  const passed = raw === "true";
  record(name, passed, passed ? "assertion passed" : `assertion returned: ${raw}`);
}

function generateBlankPage() {
  fs.writeFileSync(
    path.join(WORKTREE, "quarto-fixture", "_probe-blank.html"),
    "<!DOCTYPE html><html><body></body></html>",
  );
}

function navigateToPage(url) {
  rodney(["open", BLANK_URL]);
  rodneyJs(`window.location.href = '${url}'`);
  sleep(3);
}

function waitForExpr(expr, timeoutSeconds = 30) {
  const deadline = Date.now() + timeoutSeconds * 1000;
  while (Date.now() < deadline) {
    try {
      const raw = rodneyJs(expr);
      if (raw === "true") return true;
    } catch (_) {}
    sleep(0.2);
  }
  return false;
}

function runProbes() {
  // ------------------------------------------------------------------
  // Boot
  // ------------------------------------------------------------------
  rodney(["start"]);
  rodneyStarted = true;

  // ------------------------------------------------------------------
  // Clause 8: demo-book/_output/r-exercises.html — 2 R exercises mounted
  // ------------------------------------------------------------------
  navigateToPage(R_URL);
  if (!waitForExpr("window.__btExercises !== undefined")) {
    throw new Error("r-exercises.html: __btExercises not populated (auto-bootstrap did not run)");
  }

  rodneyAssert(
    "clause-8 r page: __btExercises.length === 2 (2 R exercises mounted)",
    "return window.__btExercises.length === 2",
  );
  rodneyAssert(
    "clause-8 r page: .cm-editor count === 2 (both exercises have editors)",
    "return document.querySelectorAll('.cm-editor').length === 2",
  );
  rodneyAssert(
    "clause-8 r page: every entry data-language === 'r'",
    "return window.__btExercises.every(e => e.element && e.element.dataset.language === 'r')",
  );
  rodneyAssert(
    "clause-8 r page: mounted entries reference real elements",
    "return window.__btExercises.every(e => e.element && e.element.classList.contains('bt-exercise'))",
  );
  rodneyAssert(
    "fix r page: static fallback removed after boot (.bt-exercise-static count 0)",
    "return document.querySelectorAll('.bt-exercise-static').length === 0",
  );
  rodneyAssert(
    "fix r page: payload script still present after boot (static block did not clobber it)",
    "return document.querySelectorAll('div.bt-exercise script[type=\"application/json\"]').length === 2",
  );

  // ------------------------------------------------------------------
  // Clause 8: demo-book/_output/python-exercises.html — 2 python mounted
  // ------------------------------------------------------------------
  navigateToPage(PY_URL);
  if (!waitForExpr("window.__btExercises !== undefined")) {
    throw new Error("python-exercises.html: __btExercises not populated (auto-bootstrap did not run)");
  }

  rodneyAssert(
    "clause-8 python page: __btExercises.length === 2 (2 python exercises mounted)",
    "return window.__btExercises.length === 2",
  );
  rodneyAssert(
    "clause-8 python page: .cm-editor count === 2 (both exercises have editors)",
    "return document.querySelectorAll('.cm-editor').length === 2",
  );
  rodneyAssert(
    "clause-8 python page: every entry data-language === 'python'",
    "return window.__btExercises.every(e => e.element && e.element.dataset.language === 'python')",
  );
  rodneyAssert(
    "clause-8 python page: mounted entries reference real elements",
    "return window.__btExercises.every(e => e.element && e.element.classList.contains('bt-exercise'))",
  );
  rodneyAssert(
    "fix python page: static fallback removed after boot (.bt-exercise-static count 0)",
    "return document.querySelectorAll('.bt-exercise-static').length === 0",
  );

  // ------------------------------------------------------------------
  // Fix (Part 2): index.html (book entry page) mounts the 2 new exercises
  // (1 R + 1 Python) and removes their static fallbacks on boot.
  // ------------------------------------------------------------------
  navigateToPage(INDEX_URL);
  if (!waitForExpr("window.__btExercises !== undefined")) {
    throw new Error("index.html: __btExercises not populated (auto-bootstrap did not run)");
  }

  rodneyAssert(
    "fix index page: __btExercises.length === 2 (1 R + 1 Python mounted)",
    "return window.__btExercises.length === 2",
  );
  rodneyAssert(
    "fix index page: .cm-editor count === 2 (both exercises have editors)",
    "return document.querySelectorAll('.cm-editor').length === 2",
  );
  rodneyAssert(
    "fix index page: static fallback removed after boot (.bt-exercise-static count 0)",
    "return document.querySelectorAll('.bt-exercise-static').length === 0",
  );

  // ------------------------------------------------------------------
  // Fix (Part 1) defect scenario: file:// — browsers CORS-block ES-module
  // imports under file://, so the bootstrap never runs. The server-rendered
  // static fallback must REMAIN visible (title/prompt/code template/hints).
  // NOTE: rodney open file:// directly — window.location.href assignment to
  // a file:// URL is silently blocked (verified empirically; the page stays
  // on the prior URL).
  // ------------------------------------------------------------------
  rodney(["open", `file://${WORKTREE}/demo-book/_output/index.html`]);
  sleep(2);
  rodneyAssert(
    "fix file:// index: static fallback remains visible (.bt-exercise-static count 2)",
    "return document.querySelectorAll('.bt-exercise-static').length === 2",
  );
  rodneyAssert(
    "fix file:// index: static title visible (bt-static-title count 2)",
    "return document.querySelectorAll('.bt-static-title').length === 2",
  );
  rodneyAssert(
    "fix file:// index: runtime did not boot (__btExercises undefined under file://)",
    "return window.__btExercises === undefined",
  );
}

function writeReport() {
  fs.mkdirSync(EVIDENCE_DIR, { recursive: true });
  const failed = probeLog.filter((p) => p.status === "FAIL");
  const verdict = failed.length === 0 ? "PROBES_PASS" : "PROBES_FAIL";

  const report = {
    issue: path.basename(EVIDENCE_DIR),
    branch: BRANCH,
    worktree: WORKTREE,
    timestamp: new Date().toISOString(),
    probes: probeLog,
    verdict,
  };

  fs.writeFileSync(
    path.join(EVIDENCE_DIR, "probe-report.json"),
    JSON.stringify(report, null, 2),
  );

  const lines = [
    `# Rodney probes for branch ${BRANCH}`,
    `verdict: ${verdict}`,
    `timestamp: ${report.timestamp}`,
    "",
    ...probeLog.map((p) => `- ${p.status}: ${p.name}\n  ${p.details}`),
  ];
  fs.writeFileSync(path.join(EVIDENCE_DIR, "rodney.log"), lines.join("\n"));

  console.log(`\n=== ${verdict} ===`);
  console.log(`report: ${path.join(EVIDENCE_DIR, "probe-report.json")}`);
  console.log(`log:    ${path.join(EVIDENCE_DIR, "rodney.log")}`);
}

function main() {
  try {
    generateBlankPage();
    startServer();
    runProbes();
  } catch (err) {
    console.error("Probe harness failed:", err.message);
    record("harness", false, err.message);
  } finally {
    if (rodneyStarted) {
      try {
        rodney(["stop"]);
      } catch (_) {}
    }
    stopServers();
    writeReport();
  }
}

main();
