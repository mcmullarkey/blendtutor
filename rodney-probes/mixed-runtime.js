#!/usr/bin/env node
/**
 * Rodney probe harness for issue #136 — AC-2 adapter-map dispatch.
 *
 * Verifies the 6-clause predicate from AC-2 against tests/fixtures/mixed-runtime.html
 * (plus a harness-generated map-validation page for clause 4) in a headless
 * browser driven by uvx rodney. The fixture calls start() twice unawaited
 * (race probe), spies console.warn/error BEFORE start, and exposes both mocks
 * on window so the probe can assert calls/bootCount/warns.
 *
 * Assertions (from AC-2 spec):
 *   1. routing — R Run → mockR.calls grows, mockPy unchanged; python Run →
 *      mockPy grows, mockR unchanged; two R exercises route to SAME mockR.
 *   2. double-start guard — module-level flag set SYNCHRONOUSLY at entry;
 *      two fire-and-forget start() calls → each adapter bootCount === 1,
 *      exactly one console.warn matching /called twice|already started/,
 *      no re-mount (.cm-editor count stays 3), resolves without throw.
 *   3. fallback removal — attribute-less exercise SKIPPED with warn, absent
 *      from window.__btExercises.
 *   4. map validation — unknown language → skip + warn, others mount, start()
 *      resolves; map key ≠ adapter.language → console.error + skip; non-function
 *      boot/run → console.error + skip.
 *   5. boot — every adapter gets exactly one boot() via Promise.all.
 *   6. (source greps — validated in scripts/tests/validate-runtime.js)
 *
 * Usage:
 *   uv run node rodney-probes/mixed-runtime.js
 *
 * Environment:
 *   STATIC_PORT - port for static fixture server (default: 8084)
 */

const { execFileSync, spawn } = require("child_process");
const fs = require("fs");
const path = require("path");
const os = require("os");

const WORKTREE = path.resolve(__dirname, "..");
const EVIDENCE_DIR = path.join(WORKTREE, "docs", "evidence", "136");
const STATIC_PORT = parseInt(process.env.STATIC_PORT || "8084", 10);

const BASE_URL = `http://localhost:${STATIC_PORT}`;
const BLANK_URL = `${BASE_URL}/quarto-fixture/_probe-blank.html`;
const MIXED_URL = `${BASE_URL}/tests/fixtures/mixed-runtime.html`;
const VALIDATION_URL = `${BASE_URL}/quarto-fixture/_mixed-validation.html`;

const STATIC_SERVER_PY = `#!/usr/bin/env python3
import http.server, socketserver, sys
PORT = int(sys.argv[1]) if len(sys.argv) > 1 else 8084
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
  const file = path.join(os.tmpdir(), `mixed-runtime-${name}.py`);
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

function generateValidationPage() {
  // Clause-4 page: exercises in languages r, python, julia (unknown),
  // bad (map key ≠ adapter.language), broken (non-function boot/run).
  // Written to a gitignored path (quarto-fixture/*.html) so it is never
  // committed — the committed fixture stays exactly as spec'd (4 exercises).
  const html = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>AC-2 Map Validation Fixture (generated)</title>
</head>
<body>
  <div class="bt-exercise" data-language="r"><script type="application/json">{"id":"val-0","title":"R","code_template":"x <- 1","checks":[],"packages":[],"solution":null,"hints":null,"gotchas":null}</script></div>
  <div class="bt-exercise" data-language="python"><script type="application/json">{"id":"val-1","title":"Python","code_template":"print(1)","checks":[],"packages":[],"solution":null,"hints":null,"gotchas":null}</script></div>
  <div class="bt-exercise" data-language="julia"><script type="application/json">{"id":"val-2","title":"Unknown lang","code_template":"x = 1","checks":[],"packages":[],"solution":null,"hints":null,"gotchas":null}</script></div>
  <div class="bt-exercise" data-language="bad"><script type="application/json">{"id":"val-3","title":"Mismatched map key","code_template":"x = 1","checks":[],"packages":[],"solution":null,"hints":null,"gotchas":null}</script></div>
  <div class="bt-exercise" data-language="broken"><script type="application/json">{"id":"val-4","title":"Non-function adapter","code_template":"x = 1","checks":[],"packages":[],"solution":null,"hints":null,"gotchas":null}</script></div>
  <script type="module">
    import { scanExercises, buildRegistry, start } from "../_extensions/blendtutor/assets/exercise-runtime.js";
    import { createMockAdapter } from "../tests/fixtures/mock-adapter.js";

    window.__btValWarns = [];
    window.__btValErrors = [];
    const origWarn = console.warn.bind(console);
    const origError = console.error.bind(console);
    console.warn = (...args) => {
      window.__btValWarns.push(args.map(String).join(" "));
      origWarn(...args);
    };
    console.error = (...args) => {
      window.__btValErrors.push(args.map(String).join(" "));
      origError(...args);
    };

    const mockR = createMockAdapter({ name: "mockR", language: "r" });
    const mockPy = createMockAdapter({ name: "mockPy", language: "python" });
    // Map key "bad" ≠ adapter.language ("r") — must be rejected with console.error.
    const mismatch = createMockAdapter({ name: "mismatch", language: "r" });
    // Non-function boot/run — must be rejected with console.error.
    const broken = { language: "broken", boot: 42, run: 42 };
    window.__btValMocks = { r: mockR, python: mockPy, mismatch, broken };

    const registry = buildRegistry(scanExercises());
    window.__btValResolved = false;
    start(registry, { r: mockR, python: mockPy, bad: mismatch, broken }).then(() => {
      window.__btValResolved = true;
    });
  </script>
</body>
</html>
`;
  fs.writeFileSync(
    path.join(WORKTREE, "quarto-fixture", "_mixed-validation.html"),
    html,
  );
}

function navigateToFixture(url) {
  rodney(["open", BLANK_URL]);
  rodneyJs(`window.location.href = '${url}'`);
  sleep(3);
}

function waitForExpr(expr, timeoutSeconds = 20) {
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

function clickRun(exerciseIndex) {
  rodneyJs(
    `document.querySelectorAll('.bt-exercise')[${exerciseIndex}].querySelector('.bt-run-btn').click()`,
  );
}

function runProbes() {
  // ------------------------------------------------------------------
  // Boot
  // ------------------------------------------------------------------
  rodney(["start"]);
  rodneyStarted = true;

  // ------------------------------------------------------------------
  // Primary page: clauses 1, 2, 3, 5 (mixed-runtime.html)
  // ------------------------------------------------------------------
  navigateToFixture(MIXED_URL);
  if (!waitForExpr("window.__btExercises !== undefined && window.__btStartResolved === true")) {
    throw new Error("Runtime did not boot or registry was not populated");
  }

  // Clause 1: routing
  clickRun(0); // R exercise 0
  if (!waitForExpr("window.__btMocks.r.calls.length === 1")) {
    throw new Error("mockR.calls did not grow after Run on R exercise 0");
  }
  rodneyAssert(
    "clause-1: R Run routes to mockR only",
    "return window.__btMocks.r.calls.length === 1 && window.__btMocks.python.calls.length === 0",
  );

  clickRun(1); // python exercise 1
  if (!waitForExpr("window.__btMocks.python.calls.length === 1")) {
    throw new Error("mockPy.calls did not grow after Run on python exercise 1");
  }
  rodneyAssert(
    "clause-1: python Run routes to mockPy only",
    "return window.__btMocks.python.calls.length === 1 && window.__btMocks.r.calls.length === 1",
  );

  clickRun(2); // R exercise 2
  if (!waitForExpr("window.__btMocks.r.calls.length === 2")) {
    throw new Error("mockR.calls did not grow after Run on R exercise 2");
  }
  rodneyAssert(
    "clause-1: two R exercises route to SAME mockR instance",
    "return window.__btMocks.r.calls.length === 2 && window.__btMocks.python.calls.length === 1",
  );

  // Clause 2: double-start guard
  rodneyAssert(
    "clause-2: exactly one double-start console.warn",
    "return window.__btWarns.filter(w => /called twice|already started/.test(w)).length === 1",
  );
  rodneyAssert(
    "clause-2: each adapter booted exactly once (bootCount === 1)",
    "return window.__btMocks.r.bootCount === 1 && window.__btMocks.python.bootCount === 1",
  );
  rodneyAssert(
    "clause-2: no re-mount (.cm-editor count stays 3)",
    "return document.querySelectorAll('.cm-editor').length === 3",
  );
  rodneyAssert(
    "clause-2: start() resolves without throw",
    "return window.__btStartResolved === true",
  );

  // Clause 3: fallback removal
  rodneyAssert(
    "clause-3: attribute-less exercise skipped with warn",
    "return window.__btWarns.some(w => /data-language/.test(w))",
  );
  rodneyAssert(
    "clause-3: attribute-less exercise absent from window.__btExercises",
    "return window.__btExercises.length === 3 && !window.__btExercises.some(e => e.id === 'bt-exercise-3')",
  );

  // Clause 5: boot via Promise.all (every adapter booted exactly once)
  rodneyAssert(
    "clause-5: every adapter booted exactly once (Promise.all)",
    "return window.__btMocks.r.bootCount === 1 && window.__btMocks.python.bootCount === 1",
  );

  // ------------------------------------------------------------------
  // Validation page: clause 4 (map validation)
  // ------------------------------------------------------------------
  navigateToFixture(VALIDATION_URL);
  if (!waitForExpr("window.__btValResolved === true")) {
    throw new Error("Validation page start() did not resolve");
  }

  rodneyAssert(
    "clause-4: unknown language skipped with warn, others mount, start() resolves",
    "return document.querySelectorAll('.cm-editor').length === 2 && " +
      "window.__btValWarns.some(w => /julia/.test(w)) && window.__btValResolved === true",
  );
  rodneyAssert(
    "clause-4: map key != adapter.language -> console.error + skip that adapter",
    "return window.__btValErrors.some(e => /mismatch/.test(e)) && window.__btValMocks.mismatch.bootCount === 0",
  );
  rodneyAssert(
    "clause-4: non-function boot/run -> console.error + skip",
    "return window.__btValErrors.some(e => /boot/.test(e))",
  );
  rodneyAssert(
    "clause-4: valid adapters still boot exactly once",
    "return window.__btValMocks.r.bootCount === 1 && window.__btValMocks.python.bootCount === 1",
  );
}

function writeReport() {
  fs.mkdirSync(EVIDENCE_DIR, { recursive: true });
  const failed = probeLog.filter((p) => p.status === "FAIL");
  const verdict = failed.length === 0 ? "PROBES_PASS" : "PROBES_FAIL";

  const report = {
    issue: 136,
    branch: "136-adapter-map-dispatch",
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
    `# Rodney probes for issue #136`,
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
    generateValidationPage();
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
