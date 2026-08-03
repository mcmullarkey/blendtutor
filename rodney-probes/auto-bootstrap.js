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
 * Also verifies AC-6 (issue #144) clauses 5-6 against the REAL rendered
 * opt-out fixture pages (ux.html mock adapter, webr.html, feedback.html):
 *   ux.html/webr.html/feedback.html → __btExercises.length === 3/2/2 and
 *   ZERO "[blendtutor] start() called twice" console warns (the AC-2
 *   double-start guard at exercise-runtime.js:436). The warn spy is injected
 *   serve-time into <head> of these three pages ONLY (passive observer; the
 *   hand-written bootstrap module script stays byte-identical) — rodney 0.4.0
 *   has no addInitScript, so this is the only way to spy before page boot.
 *   generateProbeHtml-style bootstrap substitution is FORBIDDEN here.
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
 *   EVIDENCE_DIR - evidence output dir relative to repo root (default:
 *     docs/evidence/139 — AC-3; AC-6 run passes docs/evidence/144 so AC-3
 *     evidence is never clobbered)
 */

const { execFileSync, spawn } = require("child_process");
const fs = require("fs");
const path = require("path");
const os = require("os");

const WORKTREE = path.resolve(__dirname, "..");
// EVIDENCE_DIR is parameterized (AC-6): default preserves the AC-3 evidence
// path docs/evidence/139; the AC-6 run overrides EVIDENCE_DIR=docs/evidence/144.
const EVIDENCE_DIR = path.resolve(
  WORKTREE,
  process.env.EVIDENCE_DIR || "docs/evidence/139",
);
const EVIDENCE_ISSUE = path.basename(EVIDENCE_DIR);
const EVIDENCE_BRANCH = execFileSync(
  "git",
  ["-C", WORKTREE, "rev-parse", "--abbrev-ref", "HEAD"],
  { encoding: "utf8" },
).trim();
const STATIC_PORT = parseInt(process.env.STATIC_PORT || "8085", 10);

const BASE_URL = `http://localhost:${STATIC_PORT}`;
const BLANK_URL = `${BASE_URL}/quarto-fixture/_probe-blank.html`;
const MIXED_URL = `${BASE_URL}/quarto-fixture/mixed-lang.html`;
const R_ONLY_URL = `${BASE_URL}/quarto-fixture/r-only.html`;
// AC-6 (issue #144): real rendered opt-out fixture pages — ux (mock
// adapter), webr, feedback. All three carry bt-auto-bootstrap: false.
const UX_URL = `${BASE_URL}/quarto-fixture/ux.html`;
const WEBR_URL = `${BASE_URL}/quarto-fixture/webr.html`;
const FEEDBACK_URL = `${BASE_URL}/quarto-fixture/feedback.html`;

const STATIC_SERVER_PY = `#!/usr/bin/env python3
import http.server, socketserver, sys
PORT = int(sys.argv[1]) if len(sys.argv) > 1 else 8085
# AC-6 (issue #144): the opt-out fixture pages are served as REAL rendered
# HTML with one passive observer injected into <head> — a console.warn spy
# that records into window.__btWarnSpy and forwards to the original warn.
# This is an OBSERVER, not a generateProbeHtml-style substitution: the
# hand-written bootstrap module script and the runtime stay byte-identical,
# and the spy runs before the deferred module script (classic inline script
# in head vs deferred module at end of body). rodney 0.4.0 has no
# addInitScript/console-capture, so serve-time injection is the only way to
# install the spy before the page's bootstrap executes.
SPY_PATHS = {"/quarto-fixture/ux.html", "/quarto-fixture/webr.html", "/quarto-fixture/feedback.html"}
SPY_SCRIPT = """<script>
window.__btWarnSpy = [];
var __btOrigWarn = console.warn.bind(console);
console.warn = function () {
  window.__btWarnSpy.push(Array.prototype.map.call(arguments, String).join(" "));
  __btOrigWarn.apply(console, arguments);
};
</script>"""
class Handler(http.server.SimpleHTTPRequestHandler):
    def log_message(self, fmt, *args): pass
    def do_GET(self):
        if self.path in SPY_PATHS:
            import pathlib
            html_path = pathlib.Path(self.path.lstrip("/"))
            if html_path.exists():
                body = html_path.read_text()
                if "<head>" in body:
                    body = body.replace("<head>", "<head>" + SPY_SCRIPT, 1)
                data = body.encode()
                self.send_response(200)
                self.send_header("Content-Type", "text/html")
                self.send_header("Content-Length", str(len(data)))
                self.end_headers()
                self.wfile.write(data)
                return
        return super().do_GET()
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

  // ------------------------------------------------------------------
  // AC-6 clauses 5-6 (issue #144): opt-out fixture pages — real-page
  // population + zero double-start warns. These are the REAL rendered
  // ux/webr/feedback.html (serve-time warn-spy injection only — the
  // bootstrap module script is untouched). Counts pinned by fixture:
  // ux === 3 (mock adapter), webr === 2, feedback === 2. CDN-safe:
  // __btExercises is set synchronously before the awaited adapter boot
  // (exercise-runtime.js:488 before :491) — never await boot completion.
  // ------------------------------------------------------------------
  const fixturePages = [
    { url: UX_URL, name: "ux", count: 3 },
    { url: WEBR_URL, name: "webr", count: 2 },
    { url: FEEDBACK_URL, name: "feedback", count: 2 },
  ];
  for (const page of fixturePages) {
    navigateToFixture(page.url);
    if (
      !waitForExpr(
        "window.__btExercises !== undefined && window.__btWarnSpy !== undefined",
      )
    ) {
      throw new Error(
        `${page.name}.html: __btExercises not populated (hand-written bootstrap did not run)`,
      );
    }

    rodneyAssert(
      `clause-5 ${page.name}: __btExercises.length === ${page.count} (${page.count} hand-written exercises mounted)`,
      `return window.__btExercises.length === ${page.count}`,
    );
    rodneyAssert(
      `clause-6 ${page.name}: zero double-start warns (no filter-injected duplicate start)`,
      `return window.__btWarnSpy.filter(w => w.indexOf('start() called twice') !== -1).length === 0`,
    );
  }
}

function writeReport() {
  fs.mkdirSync(EVIDENCE_DIR, { recursive: true });
  const failed = probeLog.filter((p) => p.status === "FAIL");
  const verdict = failed.length === 0 ? "PROBES_PASS" : "PROBES_FAIL";

  const report = {
    issue: parseInt(EVIDENCE_ISSUE, 10) || EVIDENCE_ISSUE,
    branch: EVIDENCE_BRANCH,
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
    `# Rodney probes for issue #${EVIDENCE_ISSUE}`,
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
    // Ensure the fixture pages exist (clause 8 + AC-6 target rendered HTML).
    renderFixture("quarto-fixture/mixed-lang.qmd");
    renderFixture("quarto-fixture/r-only.qmd");
    renderFixture("quarto-fixture/ux.qmd");
    renderFixture("quarto-fixture/webr.qmd");
    renderFixture("quarto-fixture/feedback.qmd");
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
