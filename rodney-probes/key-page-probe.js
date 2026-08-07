#!/usr/bin/env node
/**
 * Rodney probe harness for issue #169 — AC-8 key-page clauses (P6-P9).
 *
 * WHAT:  Drives the REAL rendered demo-book api-key.html in a headless
 *        browser (uvx rodney 0.4.0) against a local LLM stub, verifying the
 *        key-management UI end-to-end through real HTTP:
 *          P6  save flow UI + effect — password input (type + autocomplete),
 *              visible status line (getComputedStyle, not presence-only),
 *              localStorage slot populated, input cleared, GET /models
 *              validation actually fired against the stub;
 *          P7  clear flow counter reset — after Clear, fireworks_api_key AND
 *              bt_feedback_count both null (AC-1 P2 contract);
 *          P8  invalid-key 401 — stub /_config/auth toggle forces 401 on
 *              /models; key page shows the invalid-key error (AC-2
 *              classifyValidation 401 → invalid-key). Storage assertion
 *              follows the MERGED AC-2 advisory contract: the key is stored
 *              optimistically even when validation rejects (verified by
 *              scripts/tests/test_quarto_key_page.py:369 — "advisory: key
 *              stored even when validation rejects"). AC-8 spec P8's "key NOT
 *              stored" wording is a spec error about AC-2; the probe asserts
 *              the real contract.
 *          P9  network error optimistic save — stub unreachable → key still
 *              stored AND network-error status shown (AC-2 optimistic save).
 *
 * WHAT NOT: NOT feedback submission (feedback-probe.js owns P5/P10/P11), NOT
 *        rendering fixtures (serves the gitignored demo-book/_output/ — AC-7
 *        render output), NOT modifying production assets.
 *
 * Usage:
 *   quarto render demo-book --to html
 *   EVIDENCE_DIR=docs/evidence/169 uv run node rodney-probes/key-page-probe.js
 *
 * Environment:
 *   EVIDENCE_DIR  - evidence output dir relative to repo root (default:
 *                   docs/evidence/169 — NO hardcoded issue path, P12)
 *   STATIC_PORT   - COI static server port serving demo-book/_output/
 *                   (default: 8080)
 *   STUB_PORT     - stub LLM server port (default: 8081)
 *   ROD_CHROME_BIN - Chrome binary/wrapper for rodney. If unset, this harness
 *                   sets it to scripts/rodney-chrome.sh (committed wrapper that
 *                   strips rodney 0.4.0's hardcoded --single-process /
 *                   --disable-site-isolation-trials / --disable-features=...
 *                   flags — P13).
 */

const { execFileSync, spawn } = require("child_process");
const fs = require("fs");
const path = require("path");
const os = require("os");

const WORKTREE = path.resolve(__dirname, "..");
const EVIDENCE_DIR = path.resolve(
  WORKTREE,
  process.env.EVIDENCE_DIR || "docs/evidence/169",
);
const STATIC_PORT = parseInt(process.env.STATIC_PORT || "8080", 10);
const STUB_PORT = parseInt(process.env.STUB_PORT || "8081", 10);
const SERVE_ROOT = path.join(WORKTREE, "demo-book", "_output");

// P13 — route rodney's Chrome through the committed wrapper unless the caller
// already overrode it (same pattern as pages-live.js:88-97).
const RODNEY_CHROME_WRAPPER = path.join(WORKTREE, "scripts", "rodney-chrome.sh");
if (!process.env.ROD_CHROME_BIN) {
  if (!fs.existsSync(RODNEY_CHROME_WRAPPER)) {
    console.error(
      `FATAL: committed rodney Chrome wrapper missing at ${RODNEY_CHROME_WRAPPER} — set ROD_CHROME_BIN explicitly.`,
    );
    process.exit(2);
  }
  process.env.ROD_CHROME_BIN = RODNEY_CHROME_WRAPPER;
}

const BASE_URL = `http://localhost:${STATIC_PORT}`;
const BLANK_URL = `${BASE_URL}/_probe-blank.html`;
const KEY_PAGE_URL = `${BASE_URL}/api-key.html?provider=http://localhost:${STUB_PORT}`;
// P9 — dead localhost port: providerBaseUrl honors it (isLocal, no creds), the
// validation fetch throws → classifyValidation(threw) → network status.
const KEY_PAGE_DEAD_PROVIDER_URL = `${BASE_URL}/api-key.html?provider=http://localhost:59999`;

const STATIC_SERVER_PY = `#!/usr/bin/env python3
import http.server, socketserver, sys
PORT = int(sys.argv[1]) if len(sys.argv) > 1 else 8080
class Handler(http.server.SimpleHTTPRequestHandler):
    def log_message(self, fmt, *args): pass
socketserver.ThreadingTCPServer.allow_reuse_address = True
with socketserver.ThreadingTCPServer(("", PORT), Handler) as httpd:
    print(f"static server on port {PORT}", flush=True)
    httpd.serve_forever()
`;

const STUB_SERVER_PY = `#!/usr/bin/env python3
import http.server, json, socketserver, sys
PORT = int(sys.argv[1]) if len(sys.argv) > 1 else 8081
AUTH_FAIL = False
class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, fmt, *args): pass
    def _send_json(self, body, status=200):
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        self.wfile.write(json.dumps(body).encode())
    def do_OPTIONS(self):
        self.send_response(204)
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
        self.send_header("Access-Control-Allow-Headers", "content-type, authorization, x-api-key, anthropic-version, anthropic-dangerous-direct-browser-access")
        self.end_headers()
    def do_GET(self):
        global AUTH_FAIL
        if self.path in ("/models", "/v1/models"):
            if AUTH_FAIL:
                self._send_json({"error": "invalid key"}, 401)
                return
            self._send_json({"data": [{"id": "stub-model"}, {"id": "accounts/fireworks/models/deepseek-v4-flash"}]})
            return
        self._send_json({"error": "not found"}, 404)
    def do_POST(self):
        global AUTH_FAIL
        if self.path == "/_config/auth":
            length = int(self.headers.get("Content-Length", 0))
            cfg = json.loads(self.rfile.read(length)) if length else {}
            AUTH_FAIL = bool(cfg.get("enabled", False))
            self._send_json({"auth": AUTH_FAIL})
            return
        if self.path == "/chat/completions":
            self._send_json({"choices": [{"message": {"tool_calls": [{"function": {"name": "respond_with_feedback", "arguments": json.dumps({"is_correct": True, "feedback_message": "Stub says correct. <img src=x onerror=window.__xss=1>"})}}]}}]})
            return
        self._send_json({"error": "not found"}, 404)
socketserver.ThreadingTCPServer.allow_reuse_address = True
with socketserver.ThreadingTCPServer(("", PORT), Handler) as httpd:
    print(f"Stub LLM server on port {PORT}", flush=True)
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
  const file = path.join(os.tmpdir(), `key-page-probe-${name}.py`);
  fs.writeFileSync(file, code);
  return file;
}

function waitForPort(port, timeoutSeconds = 15) {
  const deadline = Date.now() + timeoutSeconds * 1000;
  while (Date.now() < deadline) {
    try {
      execFileSync("curl", ["-s", "-o", "/dev/null", `http://localhost:${port}/`], { timeout: 500 });
      return true;
    } catch (_) {
      sleep(0.2);
    }
  }
  return false;
}

function startServers() {
  const staticScript = writeTempScript("serve", STATIC_SERVER_PY);
  const staticProc = spawn("python3", [staticScript, String(STATIC_PORT)], {
    cwd: SERVE_ROOT,
    detached: true,
    stdio: "ignore",
  });
  staticProc.unref();
  servers.push(staticProc);
  if (!waitForPort(STATIC_PORT)) {
    throw new Error(`Static server did not start on port ${STATIC_PORT} (serve root ${SERVE_ROOT})`);
  }

  const stubScript = writeTempScript("stubllm", STUB_SERVER_PY);
  const stubProc = spawn("python3", [stubScript, String(STUB_PORT)], {
    detached: true,
    stdio: "ignore",
  });
  stubProc.unref();
  servers.push(stubProc);
  if (!waitForPort(STUB_PORT)) {
    throw new Error(`Stub LLM server did not start on port ${STUB_PORT}`);
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
  const out = execFileSync("uvx", ["--from", "rodney==0.4.0", "rodney", ...args], {
    cwd: WORKTREE,
    encoding: "utf8",
    timeout: timeoutMs,
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

function assertExpr(name, expr) {
  let raw;
  try {
    raw = rodneyJs(`(() => { return ${expr}; })()`);
  } catch (err) {
    record(name, false, "rodney js failed: " + (err.stderr || err.message));
    return false;
  }
  const ok = raw === "true";
  record(name, ok, ok ? "expression true" : `returned: ${raw}`);
  return ok;
}

/** Poll a boolean expression until true or timeout. Returns elapsed ms or -1. */
function waitForExpr(expr, timeoutSeconds = 15) {
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

function renderDemoBookIfNeeded() {
  const html = path.join(SERVE_ROOT, "api-key.html");
  if (!fs.existsSync(html)) {
    console.log("Rendering demo-book/ (quarto render demo-book --to html) ...");
    execFileSync("quarto", ["render", "demo-book", "--to", "html"], {
      cwd: WORKTREE,
      encoding: "utf8",
      timeout: 300000,
    });
  }
  if (!fs.existsSync(html)) {
    throw new Error("demo-book/_output/api-key.html missing after quarto render");
  }
}

function generateBlankPage() {
  fs.writeFileSync(
    path.join(SERVE_ROOT, "_probe-blank.html"),
    "<!DOCTYPE html><html><body></body></html>",
  );
}

/** Bootstrap from a blank page (rodney open panics on heavy first loads). */
function navigateTo(url) {
  rodney(["open", BLANK_URL]);
  rodney(["js", `window.location.href = '${url}'`]);
  sleep(3);
}

function installSpies() {
  rodneyJs(`(() => {
    window.__fetchLog = window.__fetchLog || [];
    const orig = window.fetch;
    window.fetch = (url, init) => {
      window.__fetchLog.push({ url: String(url), method: init ? (init.method || "GET") : "GET" });
      return orig(url, init);
    };
    return "spy-ok";
  })()`);
}

function fetchLog() {
  const raw = rodneyJs("JSON.stringify(window.__fetchLog || [])");
  return JSON.parse(raw || "[]");
}

function setAuthToggle(enabled) {
  execFileSync("curl", [
    "-s", "-X", "POST", "-H", "Content-Type: application/json",
    "-d", JSON.stringify({ enabled }),
    `http://localhost:${STUB_PORT}/_config/auth`,
  ]);
}

function screenshot(name, description) {
  const file = path.join(EVIDENCE_DIR, `${name}.png`);
  fs.mkdirSync(EVIDENCE_DIR, { recursive: true });
  rodney(["screenshot", file]);
  screenshots.push({ path: file, ui_state: description });
  console.log(`[screenshot] ${file}`);
}

function clearLocalStorageViaPage() {
  // Legitimate test-state reset (NOT the P5 no-eval-pre-seed surface — the
  // cross-page save itself is driven through the real UI below). Wipes the
  // key + counter so each clause starts from the documented empty state.
  rodneyJs("(() => { localStorage.clear(); return 'cleared'; })()");
}

// ---------------------------------------------------------------------------
// P6 — save flow UI + effect
// ---------------------------------------------------------------------------
function probeP6SaveFlow() {
  navigateTo(KEY_PAGE_URL);
  // Wait for the key form to mount (mountKeyPage runs after start() resolves).
  const elapsed = waitForExpr(
    "document.querySelector('[data-byok=\"key-input\"]') !== null",
    15,
  );
  if (elapsed === -1) {
    record("P6 vacuous guard: key input mounted", false, "no [data-byok=key-input] after 15s");
    return;
  }
  record("P6 vacuous guard: key input mounted", true, `key form mounted (${elapsed}ms)`);

  assertExpr(
    "P6 input: type=password",
    "document.querySelector('[data-byok=\"key-input\"]').getAttribute('type') === 'password'",
  );
  assertExpr(
    "P6 input: autocomplete=off",
    "document.querySelector('[data-byok=\"key-input\"]').getAttribute('autocomplete') === 'off'",
  );

  installSpies();
  rodney(["input", '[data-byok="key-input"]', "fw_test_123"]);
  rodney(["click", '[data-byok="save"]']);

  // Status text settles after the async validation fetch round-trips the stub.
  const statusElapsed = waitForExpr(
    "document.querySelector('[data-byok=\"key-status\"]') !== null && document.querySelector('[data-byok=\"key-status\"]').textContent.trim().length > 0",
    15,
  );
  if (statusElapsed === -1) {
    record("P6 status: line text settles", false, "key-status empty after 15s");
    return;
  }
  record("P6 status: line text settles", true, `status text set (${statusElapsed}ms)`);

  // Visibility via getComputedStyle + bounding rect — presence alone is a
  // sneaky-pass (a display:none status would pass a selector-only assert).
  const vis = rodneyJs(`(() => {
    const el = document.querySelector('[data-byok="key-status"]');
    if (!el) return "absent";
    const cs = getComputedStyle(el);
    const rect = el.getBoundingClientRect();
    return JSON.stringify({ display: cs.display, height: rect.height });
  })()`);
  let visOk = false;
  try {
    const v = JSON.parse(vis);
    visOk = v.display !== "none" && v.height > 0;
    record(
      "P6 status: visible (getComputedStyle + rect)",
      visOk,
      `display=${v.display}, height=${v.height}`,
    );
  } catch (_) {
    record("P6 status: visible (getComputedStyle + rect)", false, `unparseable: ${vis}`);
  }

  assertExpr(
    "P6 effect: localStorage slot populated",
    "localStorage.getItem('fireworks_api_key') === 'fw_test_123'",
  );
  assertExpr(
    "P6 effect: input cleared after save",
    "document.querySelector('[data-byok=\"key-input\"]').value === ''",
  );

  const log = fetchLog();
  const modelsHit = log.some(
    (e) => e.url.includes(`localhost:${STUB_PORT}`) && e.url.includes("/models") && e.method === "GET",
  );
  record(
    "P6 effect: GET /models validation fired",
    modelsHit,
    `fetch log: ${JSON.stringify(log)}`,
  );

  screenshot("key-01-saved", "key page after save: status visible, input cleared");
}

// ---------------------------------------------------------------------------
// P7 — clear flow counter reset
// ---------------------------------------------------------------------------
function probeP7ClearFlow() {
  // Clear button only renders when mountKeyPage re-runs with a stored key
  // (renderKeySet). Reload the page so the key-set state (Clear affordance)
  // appears. Seed the feedback counter so the reset assertion is non-vacuous.
  rodney(["js", "localStorage.setItem('bt_feedback_count', '3')"]);
  rodney(["reload", "--hard"]);
  sleep(3);

  const clearMounted = waitForExpr(
    "document.querySelector('[data-byok=\"clear\"]') !== null",
    15,
  );
  if (clearMounted === -1) {
    record("P7 vacuous guard: Clear button mounted", false, "no [data-byok=clear] after reload (key-set state absent)");
    return;
  }
  record("P7 vacuous guard: Clear button mounted", true, "key-set state renders Clear");

  rodney(["click", '[data-byok="clear"]']);
  sleep(1);

  assertExpr(
    "P7 clear: fireworks_api_key null",
    "localStorage.getItem('fireworks_api_key') === null",
  );
  assertExpr(
    "P7 clear: bt_feedback_count null",
    "localStorage.getItem('bt_feedback_count') === null",
  );

  screenshot("key-02-cleared", "key page after Clear: key + counter both null");
}

// ---------------------------------------------------------------------------
// P8 — invalid-key 401 (stub /_config/auth toggle)
// ---------------------------------------------------------------------------
function probeP8InvalidKey() {
  setAuthToggle(true);
  try {
    clearLocalStorageViaPage();
    navigateTo(KEY_PAGE_URL);
    const mounted = waitForExpr(
      "document.querySelector('[data-byok=\"key-input\"]') !== null",
      15,
    );
    if (mounted === -1) {
      record("P8 vacuous guard: key input mounted", false, "no [data-byok=key-input] after 15s");
      return;
    }
    installSpies();
    rodney(["input", '[data-byok="key-input"]', "BAD-KEY-401"]);
    rodney(["click", '[data-byok="save"]']);

    const errElapsed = waitForExpr(
      "document.querySelector('[data-byok=\"key-status\"]') !== null && document.querySelector('[data-byok=\"key-status\"]').textContent.includes('rejected')",
      15,
    );
    if (errElapsed === -1) {
      const txt = rodneyJs("document.querySelector('[data-byok=\"key-status\"]') ? document.querySelector('[data-byok=\"key-status\"]').textContent : 'no-status'");
      record("P8 invalid-key: error status shown", false, `status text: ${txt}`);
      return;
    }
    record("P8 invalid-key: error status shown", true, `invalid-key message rendered (${errElapsed}ms)`);

    const log = fetchLog();
    const models401Hit = log.some(
      (e) => e.url.includes(`localhost:${STUB_PORT}`) && e.url.includes("/models") && e.method === "GET",
    );
    record(
      "P8 invalid-key: GET /models 401 actually fired",
      models401Hit,
      `fetch log: ${JSON.stringify(log)}`,
    );

    // AC-2 advisory contract (VERIFIED by test_quarto_key_page.py:369 — "key
    // stored even when validation rejects"). AC-8 spec P8's "key NOT stored"
    // wording misreads AC-2's optimistic-save; the probe asserts the merged
    // contract. See plan AC-8.md Surprises & Discoveries.
    assertExpr(
      "P8 advisory storage: key stored (AC-2 optimistic save)",
      "localStorage.getItem('fireworks_api_key') === 'BAD-KEY-401'",
    );

    screenshot("key-03-invalid-key", "key page after 401: invalid-key error shown");
  } finally {
    setAuthToggle(false);
  }
}

// ---------------------------------------------------------------------------
// P9 — network error optimistic save (stub unreachable)
// ---------------------------------------------------------------------------
function probeP9NetworkError() {
  // P8 stored a key; the key-set state would render Clear, not the form.
  // Clear via the real UI (the Clear button is present), then navigate to the
  // dead-provider URL so the form renders and validation fetch throws.
  const clearPresent = rodneyJs("document.querySelector('[data-byok=\"clear\"]') !== null");
  if (clearPresent === "true") {
    rodney(["click", '[data-byok="clear"]']);
    sleep(1);
  } else {
    clearLocalStorageViaPage();
  }

  navigateTo(KEY_PAGE_DEAD_PROVIDER_URL);
  const mounted = waitForExpr(
    "document.querySelector('[data-byok=\"key-input\"]') !== null",
    15,
  );
  if (mounted === -1) {
    record("P9 vacuous guard: key input mounted", false, "no [data-byok=key-input] after 15s");
    return;
  }
  installSpies();
  rodney(["input", '[data-byok="key-input"]', "fw_net_789"]);
  rodney(["click", '[data-byok="save"]']);

  const netElapsed = waitForExpr(
    "document.querySelector('[data-byok=\"key-status\"]') !== null && document.querySelector('[data-byok=\"key-status\"]').textContent.includes('could not be reached')",
    15,
  );
  if (netElapsed === -1) {
    const txt = rodneyJs("document.querySelector('[data-byok=\"key-status\"]') ? document.querySelector('[data-byok=\"key-status\"]').textContent : 'no-status'");
    record("P9 network error: status shown", false, `status text: ${txt}`);
    return;
  }
  record("P9 network error: status shown", true, `network-error message rendered (${netElapsed}ms)`);

  assertExpr(
    "P9 optimistic save: key stored despite unreachable stub",
    "localStorage.getItem('fireworks_api_key') === 'fw_net_789'",
  );

  screenshot("key-04-network-error", "key page after network error: optimistic save + network status");
}

// ---------------------------------------------------------------------------
// Report + exit-code gate (P1)
// ---------------------------------------------------------------------------
function writeReport() {
  fs.mkdirSync(EVIDENCE_DIR, { recursive: true });
  const failed = probeLog.filter((p) => p.status === "FAIL");
  const verdict = failed.length === 0 ? "PROBES_PASS" : "PROBES_FAIL";

  const report = {
    issue: 169,
    branch: "169-rodney-probes-byok",
    worktree: WORKTREE,
    timestamp: new Date().toISOString(),
    probes: probeLog,
    screenshots,
    verdict,
  };

  fs.writeFileSync(
    path.join(EVIDENCE_DIR, "key-page-probe-report.json"),
    JSON.stringify(report, null, 2),
  );

  const lines = [
    `# Rodney probes — key page (issue #169, AC-8 P6-P9)`,
    `verdict: ${verdict}`,
    `timestamp: ${report.timestamp}`,
    "",
    ...probeLog.map((p) => `- ${p.status}: ${p.name}\n  ${p.details}`),
  ];
  fs.writeFileSync(path.join(EVIDENCE_DIR, "key-page-probe.log"), lines.join("\n"));

  console.log(`\n=== ${verdict} ===`);
  console.log(`report: ${path.join(EVIDENCE_DIR, "key-page-probe-report.json")}`);
  console.log(`log:    ${path.join(EVIDENCE_DIR, "key-page-probe.log")}`);
  return verdict;
}

function main() {
  let verdict = "PROBES_FAIL";
  try {
    renderDemoBookIfNeeded();
    generateBlankPage();
    startServers();

    rodney(["start"]);
    rodneyStarted = true;
    // rodney's Chrome profile persists between runs; clear the HTTP cache so a
    // stale demo-book render from an earlier session cannot leak into the
    // probe (observed locally after a re-render).
    rodney(["open", BLANK_URL]);
    try {
      rodney(["clear-cache"]);
    } catch (_) {}
    // The profile ALSO persists localStorage across runs; P6's precondition is
    // "no stored key". Clear on the blank page (same origin as the key page).
    clearLocalStorageViaPage();

    probeP6SaveFlow();
    probeP7ClearFlow();
    probeP8InvalidKey();
    probeP9NetworkError();

    verdict = writeReport();
  } catch (err) {
    console.error("Probe harness failed:", err.message);
    record("harness", false, err.message);
    verdict = writeReport();
  } finally {
    if (rodneyStarted) {
      try {
        rodney(["stop"]);
      } catch (_) {}
    }
    stopServers();
  }

  // P1 — exit-code gate: a PROBES_FAIL verdict MUST exit non-zero (the old
  // feedback-probe.js defect exited 0; pages-live.js:701 is the reference).
  process.exit(verdict === "PROBES_PASS" ? 0 : 1);
}

main();
