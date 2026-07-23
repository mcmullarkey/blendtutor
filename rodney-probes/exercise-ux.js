#!/usr/bin/env node
/**
 * Rodney probe harness for issue #113 — AC-8 Exercise UX polish.
 *
 * Runs the 7 deterministic clauses (plus negative cases) from
 * rodney-probes/exercise-ux.js against the rendered ux.qmd fixture in a
 * headless browser driven by uvx rodney.
 *
 * Usage:
 *   node rodney-probes/exercise-ux.js
 *
 * Environment:
 *   QUARTO_BIN   - path to quarto binary (default: /private/tmp/quarto/bin/quarto)
 *   STATIC_PORT  - port for static fixture server (default: 8083)
 */

const { execFileSync, spawn } = require("child_process");
const fs = require("fs");
const path = require("path");
const os = require("os");

const WORKTREE = path.resolve(__dirname, "..");
const EVIDENCE_DIR = path.join(WORKTREE, "docs", "evidence", "113");
const STATIC_PORT = parseInt(process.env.STATIC_PORT || "8083", 10);
const QUARTO_BIN =
  process.env.QUARTO_BIN ||
  (fs.existsSync("/private/tmp/quarto/bin/quarto")
    ? "/private/tmp/quarto/bin/quarto"
    : "quarto");

const BASE_URL = `http://localhost:${STATIC_PORT}`;
const FIXTURE_URL = `${BASE_URL}/quarto-fixture/ux.html`;
const BLANK_URL = `${BASE_URL}/quarto-fixture/_probe-blank.html`;

const STATIC_SERVER_PY = `#!/usr/bin/env python3
import http.server, socketserver, sys
PORT = int(sys.argv[1]) if len(sys.argv) > 1 else 8080
class Handler(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header("Cross-Origin-Opener-Policy", "same-origin")
        self.send_header("Cross-Origin-Embedder-Policy", "require-corp")
        super().end_headers()
    def log_message(self, fmt, *args): pass
socketserver.ThreadingTCPServer.allow_reuse_address = True
with socketserver.ThreadingTCPServer(("", PORT), Handler) as httpd:
    print(f"COI server on port {PORT}", flush=True)
    httpd.serve_forever()
`;

const servers = [];
let rodneyStarted = false;
const probeLog = [];
const screenshots = [];

function sleep(seconds) {
  if (seconds > 0) {
    execFileSync("sleep", [String(seconds)]);
  }
}

function writeTempScript(name, code) {
  const file = path.join(os.tmpdir(), `exercise-ux-${name}.py`);
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
  const script = writeTempScript("coiserve", STATIC_SERVER_PY);
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
  // Wrap in an IIFE so we can use statements (const, etc.) inside the probe.
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

function ensureRenderedFixture() {
  const fixtureHtml = path.join(WORKTREE, "quarto-fixture", "ux.html");
  if (!fs.existsSync(fixtureHtml)) {
    console.log("Rendering ux.qmd ...");
    execFileSync(QUARTO_BIN, ["render", "quarto-fixture/ux.qmd"], {
      cwd: WORKTREE,
      stdio: "inherit",
    });
  }
}

function generateBlankPage() {
  fs.writeFileSync(
    path.join(WORKTREE, "quarto-fixture", "_probe-blank.html"),
    "<!DOCTYPE html><html><body></body></html>",
  );
}

function navigateToFixture() {
  rodney(["open", BLANK_URL]);
  rodneyJs(`window.location.href = '${FIXTURE_URL}'`);
  sleep(3);
}

function waitForBoot(timeoutSeconds = 15) {
  const deadline = Date.now() + timeoutSeconds * 1000;
  while (Date.now() < deadline) {
    try {
      const raw = rodneyJs("window.__btExercises && window.__btExercises.length >= 3");
      if (raw === "true") return true;
    } catch (_) {}
    sleep(0.2);
  }
  return false;
}

function screenshot(name, description, acReference) {
  const relPath = path.join("docs", "evidence", "113", `${name}.png`);
  const absPath = path.join(WORKTREE, relPath);
  fs.mkdirSync(path.dirname(absPath), { recursive: true });
  rodney(["screenshot", absPath]);
  screenshots.push({
    path: relPath,
    ui_state: description,
    ac_reference: acReference,
  });
  console.log(`[screenshot] ${relPath}`);
}

function runProbes() {
  // ------------------------------------------------------------------
  // Boot
  // ------------------------------------------------------------------
  rodney(["start"]);
  rodneyStarted = true;
  navigateToFixture();
  if (!waitForBoot()) {
    throw new Error("Runtime did not boot or registry was not populated");
  }

  // ------------------------------------------------------------------
  // Clause 1: hints visible/absent
  // ------------------------------------------------------------------
  screenshot(
    "01-initial-state",
    "Initial render: three exercises with hints on ex0, no hints on ex2, Run/Check/Solution buttons",
    "AC-8 clause 1 (hints visible/absent), clause 3 (check button conditional), negative (solution button for empty exercise)",
  );
  rodneyAssert(
    "clause-1: hints visible/absent",
    "const exercises = document.querySelectorAll('.bt-exercise'); " +
      "const ex0 = exercises[0]; const ex2 = exercises[2]; " +
      "return ex0.querySelector('details.bt-hints') !== null && " +
      "ex2.querySelector('details.bt-hints') === null",
  );

  // ------------------------------------------------------------------
  // Clause 3: check button absent when no checks
  // ------------------------------------------------------------------
  rodneyAssert(
    "clause-3: check button absent when no checks",
    "const exercises = document.querySelectorAll('.bt-exercise'); " +
      "const ex0 = exercises[0]; const ex1 = exercises[1]; const ex2 = exercises[2]; " +
      "return ex0.querySelector('.bt-check-btn') !== null && " +
      "ex1.querySelector('.bt-check-btn') === null && " +
      "ex2.querySelector('.bt-check-btn') === null",
  );

  // ------------------------------------------------------------------
  // Negative: solution button for empty exercise
  // ------------------------------------------------------------------
  rodneyAssert(
    "negative: solution button for empty exercise",
    "return document.querySelectorAll('.bt-exercise')[2].querySelector('.bt-solution-btn') === null",
  );

  // ------------------------------------------------------------------
  // Clause 2: solution button click inserts text
  // ------------------------------------------------------------------
  rodneyAssert(
    "clause-2: solution button inserts solution text",
    "const exercises = document.querySelectorAll('.bt-exercise'); " +
      "const ex0 = exercises[0]; " +
      "const entry = window.__btExercises[0]; " +
      "const before = entry.getSubmission(); " +
      "ex0.querySelector('.bt-solution-btn').click(); " +
      "const after = entry.getSubmission(); " +
      "return after === entry.payload.solution && after !== before",
  );
  screenshot(
    "02-solution-inserted",
    "Exercise 0 after clicking Show solution: editor contains 'a + b'",
    "AC-8 clause 2 (solution button click inserts text)",
  );

  // ------------------------------------------------------------------
  // Clause 4: Run disables ex-0 only (per-exercise, not singleton)
  // Patch the mock adapter to delay so we can inspect the disabled state.
  // ------------------------------------------------------------------
  rodneyJs(
    "(() => { " +
      "window.__btTestAdapter.run = async (code, checks, packages) => { " +
      "await new Promise(r => setTimeout(r, 2000)); " +
      "return { output: 'slow mock pass', ok: true }; " +
      "}; " +
      "})()",
  );
  rodneyJs(
    "(() => { " +
      "document.querySelectorAll('.bt-exercise')[0].querySelector('.bt-run-btn').click(); " +
      "})()",
  );
  sleep(0.5);
  screenshot(
    "03-run-disabled-isolation",
    "During exercise 0 run: ex0 Run button disabled, ex1 and ex2 Run buttons still enabled",
    "AC-8 clause 4 (Run disables ex-0 only), clause 5 (cursor on disabled buttons), clause 7 (buttons re-enabled on pass)",
  );
  rodneyAssert(
    "clause-4: Run disables ex-0 only",
    "const exercises = document.querySelectorAll('.bt-exercise'); " +
      "const runBtn0 = exercises[0].querySelector('.bt-run-btn'); " +
      "const runBtn1 = exercises[1].querySelector('.bt-run-btn'); " +
      "const runBtn2 = exercises[2].querySelector('.bt-run-btn'); " +
      "return runBtn0.disabled === true && runBtn1.disabled === false && runBtn2.disabled === false",
  );

  // ------------------------------------------------------------------
  // Clause 5: cursor=not-allowed on disabled buttons
  // ------------------------------------------------------------------
  rodneyAssert(
    "clause-5: cursor not-allowed on disabled buttons",
    "const runBtn0 = document.querySelectorAll('.bt-exercise')[0].querySelector('.bt-run-btn'); " +
      "runBtn0.disabled = true; " +
      "const cursor = getComputedStyle(runBtn0).cursor; " +
      "runBtn0.disabled = false; " +
      "return cursor === 'not-allowed'",
  );

  // Wait for the slow run to finish before the next assertions.
  sleep(2);

  // ------------------------------------------------------------------
  // Clause 6: data-status closed set
  // ------------------------------------------------------------------
  rodneyAssert(
    "clause-6: data-status closed set",
    "const ex0 = document.querySelectorAll('.bt-exercise')[0]; " +
      "const statusEl = ex0.querySelector('.bt-status'); " +
      "return ['idle','running','pass','fail'].includes(statusEl.dataset.status)",
  );

  // ------------------------------------------------------------------
  // Clause 7: buttons re-enabled on pass
  // ------------------------------------------------------------------
  rodneyAssert(
    "clause-7: buttons re-enabled on pass",
    "const exercises = document.querySelectorAll('.bt-exercise'); " +
      "const runBtn0 = exercises[0].querySelector('.bt-run-btn'); " +
      "const runBtn1 = exercises[1].querySelector('.bt-run-btn'); " +
      "const runBtn2 = exercises[2].querySelector('.bt-run-btn'); " +
      "return runBtn0.disabled === false && runBtn1.disabled === false && runBtn2.disabled === false",
  );
  screenshot(
    "04-status-pass",
    "After exercise 0 run completes: status shows pass, all Run buttons re-enabled",
    "AC-8 clause 6 (data-status closed set), clause 7 (buttons re-enabled on pass)",
  );
}

function writeReport() {
  fs.mkdirSync(EVIDENCE_DIR, { recursive: true });
  const failed = probeLog.filter((p) => p.status === "FAIL");
  const verdict = failed.length === 0 ? "PROBES_PASS" : "PROBES_FAIL";

  const report = {
    issue: 113,
    branch: "113-ux-polish",
    worktree: WORKTREE,
    timestamp: new Date().toISOString(),
    probes: probeLog,
    screenshots,
    verdict,
  };

  fs.writeFileSync(
    path.join(EVIDENCE_DIR, "probe-report.json"),
    JSON.stringify(report, null, 2),
  );

  const lines = [
    `# Rodney probes for issue #113`,
    `verdict: ${verdict}`,
    `timestamp: ${report.timestamp}`,
    "",
    ...probeLog.map((p) => `- ${p.status}: ${p.name}\n  ${p.details}`),
    "",
    "## Screenshots",
    ...screenshots.map(
      (s) => `- ${s.path} — ${s.ui_state} (${s.ac_reference})`,
    ),
  ];
  fs.writeFileSync(path.join(EVIDENCE_DIR, "rodney.log"), lines.join("\n"));

  console.log(`\n=== ${verdict} ===`);
  console.log(`report: ${path.join(EVIDENCE_DIR, "probe-report.json")}`);
  console.log(`log:    ${path.join(EVIDENCE_DIR, "rodney.log")}`);
}

function main() {
  try {
    ensureRenderedFixture();
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
