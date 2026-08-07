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
  const out = execFileSync("uvx", ["--from", "rodney==0.4.0", "rodney", ...args], {
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

function ensureAssetSymlink() {
  // Create a symlink quarto-fixture/_extensions -> ../_extensions so that
  // asset URLs emitted by blendtutor.lua resolve when the fixture HTML is
  // served from quarto-fixture/. Since issue #129 the filter derives asset
  // paths from its own location (PANDOC_SCRIPT_FILE, ADR-0018), so ux.qmd's
  // filter reference "../_extensions/blendtutor/blendtutor.lua" produces
  // hrefs like "../_extensions/blendtutor/assets/styles.css" that resolve
  // up one level to the repo-root _extensions/ — the symlink is a belt-and-
  // suspenders fallback for older rendered fixtures that still carry the
  // pre-#129 hardcoded href, which would otherwise resolve to
  // quarto-fixture/_extensions/... and 404 (getComputedStyle() checks for
  // clause-5 cursor:not-allowed would fail because the stylesheet never
  // applied).
  const linkPath = path.join(WORKTREE, "quarto-fixture", "_extensions");
  const target = "../_extensions";
  try {
    if (fs.existsSync(linkPath)) {
      const stats = fs.lstatSync(linkPath);
      if (stats.isSymbolicLink()) {
        return; // symlink already exists
      }
      // Not a symlink — could be a real directory; don't touch it.
      console.warn(`[setup] ${linkPath} exists but is not a symlink — skipping`);
      return;
    }
    fs.symlinkSync(target, linkPath);
    console.log(`[setup] created symlink ${linkPath} -> ${target}`);
  } catch (err) {
    console.warn(`[setup] could not create symlink: ${err.message}`);
  }
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
    "Initial render: three exercises with hints on ex0, no hints on ex2, Check/Solution/Get-feedback toolbar (issue #182: no Run button)",
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
  // Issue #182: Run button removed — Get feedback replaces its slot.
  // Negative pin: no .bt-run-btn anywhere; ex0 (has checks) has a
  // .bt-check-btn; the controls row exposes Get feedback's slot.
  // ------------------------------------------------------------------
  rodneyAssert(
    "issue-182: no .bt-run-btn anywhere",
    "return document.querySelectorAll('.bt-run-btn').length === 0",
  );
  rodneyAssert(
    "issue-182: ex0 controls row exists with check + solution",
    "const ex0 = document.querySelectorAll('.bt-exercise')[0]; " +
      "return ex0.querySelector('.bt-controls') !== null && " +
      "ex0.querySelector('.bt-check-btn') !== null && " +
      "ex0.querySelector('.bt-solution-btn') !== null",
  );

  // ------------------------------------------------------------------
  // Clause 4: per-exercise button disable (issue #182 reworked)
  // Patch the mock adapter to delay so we can inspect the disabled state.
  // The ux fixture mounts NO feedback (its bootstrap calls start() only), so
  // the only button on ex0 that exists on a checks-having exercise is Check;
  // ex1/ex2 have no Check button at all — disable can never leak to them.
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
      "document.querySelectorAll('.bt-exercise')[0].querySelector('.bt-check-btn').click(); " +
      "})()",
  );
  sleep(0.5);
  screenshot(
    "03-run-disabled-isolation",
    "During exercise 0 run: ex0 Check button disabled, ex1/ex2 have no Check button (per-exercise scoping, issue #182)",
    "AC-8 clause 4 (per-exercise disable), clause 5 (cursor on disabled buttons), clause 7 (buttons re-enabled on pass)",
  );
  rodneyAssert(
    "clause-4: Check disables ex-0 only (per-exercise, no singleton)",
    "const exercises = document.querySelectorAll('.bt-exercise'); " +
      "const check0 = exercises[0].querySelector('.bt-check-btn'); " +
      "return check0.disabled === true && " +
      "exercises[1].querySelector('.bt-check-btn') === null && " +
      "exercises[2].querySelector('.bt-check-btn') === null",
  );

  // ------------------------------------------------------------------
  // Clause 5: cursor=not-allowed on disabled buttons
  // ------------------------------------------------------------------
  rodneyAssert(
    "clause-5: cursor not-allowed on disabled buttons",
    "const check0 = document.querySelectorAll('.bt-exercise')[0].querySelector('.bt-check-btn'); " +
      "check0.disabled = true; " +
      "const cursor = getComputedStyle(check0).cursor; " +
      "check0.disabled = false; " +
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
    "const check0 = document.querySelectorAll('.bt-exercise')[0].querySelector('.bt-check-btn'); " +
      "return check0.disabled === false",
  );
  screenshot(
    "04-status-pass",
    "After exercise 0 run completes: status shows pass, Check button re-enabled",
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
    ensureAssetSymlink();
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
