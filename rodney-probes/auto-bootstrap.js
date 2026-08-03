#!/usr/bin/env node
/**
 * Rodney probe harness for issue #139 — AC-3 filter auto-bootstrap runtime.
 *
 * Verifies clause 8 of the AC-3 predicate against the RENDERED fixture pages
 * in a headless browser driven by uvx rodney:
 *   mixed-lang.html → window.__btExercises.length === 2 AND
 *                     .cm-editor count === 2 (1 R + 1 python, both mounted)
 *   r-only.html     → window.__btExercises.length === 2 (2 R exercises)
 *
 * The assertions NEVER wait for boot completion: webR/pyodide boot pulls CDN
 * assets that may be offline in the probe environment. start() sets
 * window.__btExercises synchronously BEFORE the awaited adapter boot
 * (exercise-runtime.js:485-491) and mounts editors before that — so
 * __btExercises.length and .cm-editor count are safe, deterministic probes
 * of the auto-bootstrap's dispatch wiring without any CDN dependency.
 *
 * Usage:
 *   uv run node rodney-probes/auto-bootstrap.js
 *
 * Environment:
 *   STATIC_PORT - port for static fixture server (default: 8085)
 */

const { execFileSync, spawn } = require("child_process");
const fs = require("fs");
const path = require("path");
const os = require("os");

const WORKTREE = path.resolve(__dirname, "..");
const EVIDENCE_DIR = path.join(WORKTREE, "docs", "evidence", "139");
const STATIC_PORT = parseInt(process.env.STATIC_PORT || "8085", 10);

const BASE_URL = `http://localhost:${STATIC_PORT}`;
const BLANK_URL = `${BASE_URL}/quarto-fixture/_probe-blank.html`;
const MIXED_URL = `${BASE_URL}/quarto-fixture/mixed-lang.html`;
const R_ONLY_URL = `${BASE_URL}/quarto-fixture/r-only.html`;

const STATIC_SERVER_PY = `#!/usr/bin/env python3
import http.server, socketserver, sys
PORT = int(sys.argv[1]) if len(sys.argv) > 1 else 8085
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
  const file = path.join(os.tmpdir(), `auto-bootstrap-${name}.py`);
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

function renderFixture(relative) {
  // Render fixture HTML if missing (same shape as test_quarto_bootstrap.sh).
  if (fs.existsSync(path.join(WORKTREE, relative))) return;
  execFileSync("quarto", ["render", relative, "--to", "html"], {
    cwd: WORKTREE,
    encoding: "utf8",
    timeout: 120000,
  });
}

function generateBlankPage() {
  fs.writeFileSync(
    path.join(WORKTREE, "quarto-fixture", "_probe-blank.html"),
    "<!DOCTYPE html><html><body></body></html>",
  );
}

function navigateToFixture(url) {
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
  // Clause 8: mixed-lang.html — both languages mounted
  // ------------------------------------------------------------------
  navigateToFixture(MIXED_URL);
  if (!waitForExpr("window.__btExercises !== undefined")) {
    throw new Error("mixed-lang.html: __btExercises not populated (auto-bootstrap did not run)");
  }

  rodneyAssert(
    "clause-8 mixed: __btExercises.length === 2 (1 R + 1 python mounted)",
    "return window.__btExercises.length === 2",
  );
  rodneyAssert(
    "clause-8 mixed: .cm-editor count === 2 (both exercises have editors)",
    "return document.querySelectorAll('.cm-editor').length === 2",
  );
  rodneyAssert(
    "clause-8 mixed: mounted entries reference real elements",
    "return window.__btExercises.every(e => e.element && e.element.classList.contains('bt-exercise'))",
  );

  // ------------------------------------------------------------------
  // Clause 8: r-only.html — R-only page mounts both R exercises
  // ------------------------------------------------------------------
  navigateToFixture(R_ONLY_URL);
  if (!waitForExpr("window.__btExercises !== undefined")) {
    throw new Error("r-only.html: __btExercises not populated (auto-bootstrap did not run)");
  }

  rodneyAssert(
    "clause-8 r-only: __btExercises.length === 2 (2 R exercises mounted)",
    "return window.__btExercises.length === 2",
  );
}

function writeReport() {
  fs.mkdirSync(EVIDENCE_DIR, { recursive: true });
  const failed = probeLog.filter((p) => p.status === "FAIL");
  const verdict = failed.length === 0 ? "PROBES_PASS" : "PROBES_FAIL";

  const report = {
    issue: 139,
    branch: "139-filter-auto-bootstrap",
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
    `# Rodney probes for issue #139`,
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
    // Ensure the fixture pages exist (clause 8 targets rendered HTML).
    renderFixture("quarto-fixture/mixed-lang.qmd");
    renderFixture("quarto-fixture/r-only.qmd");
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
