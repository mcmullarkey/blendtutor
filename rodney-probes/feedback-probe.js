#!/usr/bin/env node
/**
 * Rodney probe harness for issue #169 — AC-8 feedback clauses (P5, P10, P11).
 *
 * WHAT:  Drives the REAL rendered demo-book r-exercises.html in a headless
 *        browser (uvx rodney 0.4.0) against a local LLM stub, verifying the
 *        BYOK feedback flow end-to-end through real HTTP:
 *          P5  cross-page persistence — key saved via the key-page UI on
 *              api-key.html (page 1) → real location.href navigation to
 *              r-exercises.html (page 2, same origin, SAME rodney session) →
 *              localStorage key present AND the feedback UI proceeds past the
 *              no-key state. NOT eval pre-seed, NOT a new tab.
 *          P10 verdict end-to-end — with the key present, clicking
 *              [data-byok="submit"] drives a request through the localhost
 *              stub /chat/completions; the verdict renders into
 *              [data-byok="verdict"] via textContent only — the XSS payload
 *              in the stub response renders as literal text (window.__xss
 *              stays undefined). No fetch-spy substitution for this clause:
 *              the verdict must come from the REAL stub round-trip.
 *          P11 inline key form end-to-end — with empty localStorage, the
 *              exercise page mounts the key-page form INLINE inside the
 *              feedback container ([data-byok="key-page-form"], NO
 *              navigation link); saving through the real form stores the key
 *              and the save-continuation (issue #186) fires the feedback
 *              fetch — verdict renders through the stub.
 *
 * P3: NO synthetic-DOM fixture generation — every navigation targets a file
 * under the served demo-book/_output/ root (AC-7 render output; the old
 * feedback-probe.js mock-adapter approach is removed).
 *
 * WHAT NOT: NOT the key-page clauses (P6-P9 — key-page-probe.js owns them),
 *        NOT runtime execution (pages-live.js owns real R/Python), NOT
 *        modifying production assets (probes only READ the rendered output).
 *
 * Usage:
 *   quarto render demo-book --to html
 *   EVIDENCE_DIR=docs/evidence/169 uv run node rodney-probes/feedback-probe.js
 *
 * Environment:
 *   EVIDENCE_DIR  - evidence output dir relative to repo root (default:
 *                   docs/evidence/169 — NO hardcoded issue path, P12)
 *   STATIC_PORT   - static server port serving demo-book/_output/
 *                   (default: 8080)
 *   STUB_PORT     - stub LLM server port (default: 8081)
 *   ROD_CHROME_BIN - Chrome binary/wrapper for rodney. If unset, this harness
 *                   sets it to scripts/rodney-chrome.sh (committed wrapper that
 *                   strips rodney 0.4.0's hardcoded --single-process /
 *                   --disable-site-isolation-trials / --disable-features=...
 *                   flags — P13, the cross-page COI leg).
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
const EXERCISE_PAGE_URL = `${BASE_URL}/r-exercises.html?provider=http://localhost:${STUB_PORT}`;

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
        if self.path in ("/models", "/v1/models"):
            self._send_json({"data": [{"id": "stub-model"}, {"id": "accounts/fireworks/models/deepseek-v4-flash"}]})
            return
        self._send_json({"error": "not found"}, 404)
    def do_POST(self):
        if self.path == "/chat/completions":
            # P10 — respond_with_feedback tool_call shape; feedback_message
            # carries an XSS payload that MUST render as literal text (the
            # page renders via textContent, so window.__xss stays undefined).
            self._send_json({"choices": [{"message": {"tool_calls": [{"function": {"name": "respond_with_feedback", "arguments": json.dumps({"is_correct": True, "feedback_message": "Stub says correct. <img src=x onerror=window.__xss=1>"})}}]}}]})
            return
        if self.path == "/v1/messages":
            self._send_json({"content": [{"type": "tool_use", "name": "respond_with_feedback", "input": {"is_correct": True, "feedback_message": "Stub says correct."}}]})
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
  const file = path.join(os.tmpdir(), `feedback-probe-${name}.py`);
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
  const html = path.join(SERVE_ROOT, "r-exercises.html");
  if (!fs.existsSync(html)) {
    console.log("Rendering demo-book/ (quarto render demo-book --to html) ...");
    execFileSync("quarto", ["render", "demo-book", "--to", "html"], {
      cwd: WORKTREE,
      encoding: "utf8",
      timeout: 300000,
    });
  }
  if (!fs.existsSync(html)) {
    throw new Error("demo-book/_output/r-exercises.html missing after quarto render");
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

function screenshot(name, description) {
  const file = path.join(EVIDENCE_DIR, `${name}.png`);
  fs.mkdirSync(EVIDENCE_DIR, { recursive: true });
  rodney(["screenshot", file]);
  screenshots.push({ path: file, ui_state: description });
  console.log(`[screenshot] ${file}`);
}

/** Wait for the exercise page's feedback UI to mount (registry + buttons). */
function waitForFeedbackUi() {
  return waitForExpr(
    "window.__btExercises !== undefined && document.querySelectorAll('.bt-feedback-btn').length >= 1",
    20,
  );
}

// ---------------------------------------------------------------------------
// P11 — inline key form end-to-end (empty localStorage → inline form, save →
// key stored → feedback fetch fires through the stub → verdict)
// ---------------------------------------------------------------------------
function probeP11NoKeyLink() {
  // Empty localStorage precondition: clear through the page (fresh state;
  // this is a precondition reset, NOT the P5 cross-page save — that one goes
  // through the real key-page UI below).
  navigateTo(EXERCISE_PAGE_URL);
  const mounted = waitForFeedbackUi();
  if (mounted === -1) {
    record("P11 vacuous guard: feedback UI mounted", false, "no feedback buttons after 20s");
    return;
  }
  record("P11 vacuous guard: feedback UI mounted", true, `feedback UI mounted (${mounted}ms)`);

  rodneyJs("(() => { localStorage.clear(); return 'cleared'; })()");
  installSpies();

  rodney(["click", ".bt-exercise:first-of-type .bt-feedback-btn"]);
  sleep(1);

  const formVisible = waitForExpr(
    "document.querySelector('[data-byok=\"key-page-form\"]') !== null",
    10,
  );
  if (formVisible === -1) {
    record("P11 no-key: inline key form rendered", false, "[data-byok=key-page-form] not found after click");
    return;
  }
  record("P11 no-key: inline key form rendered", true, "inline key form appears on click with empty storage");

  assertExpr(
    "P11 no-key: NO navigation link (no data-byok=no-key)",
    "document.querySelector('[data-byok=\"no-key\"]') === null",
  );
  assertExpr(
    "P11 no-key: NO anchor rendered in the form or feedback area",
    "document.querySelector('[data-byok=\"key-page-form\"] a') === null && document.querySelector('[data-byok=\"feedback\"] a') === null",
  );

  const log0 = fetchLog();
  const completions0 = log0.filter((e) => e.url.includes("/chat/completions"));
  record(
    "P11 no-key: ZERO /chat/completions fetches before save",
    completions0.length === 0,
    `fetches observed: ${JSON.stringify(log0)}`,
  );

  // Save through the REAL inline form UI: type the key, click Save. The
  // save-continuation (issue #186) then re-runs the submit flow — the key is
  // now stored, so the feedback fetch fires and the verdict renders.
  rodney(["input", '[data-byok="key-input"]', "fw_inline_probe_789"]);
  rodney(["click", '[data-byok="save"]']);

  const keyStored = waitForExpr(
    "localStorage.getItem('fireworks_api_key') === 'fw_inline_probe_789'",
    15,
  );
  if (keyStored === -1) {
    record("P11 save: key stored via inline form", false, "fireworks_api_key not stored after save click");
    return;
  }
  record("P11 save: key stored via inline form", true, `key stored (${keyStored}ms)`);

  const fetchFired = waitForExpr(
    "JSON.parse(JSON.stringify(window.__fetchLog || [])).filter((e) => String(e.url).includes('/chat/completions')).length >= 1",
    20,
  );
  record(
    "P11 save-continuation: feedback fetch fires through stub",
    fetchFired !== -1,
    fetchFired !== -1 ? `new /chat/completions observed (${fetchFired}ms)` : "no /chat/completions after save",
  );

  const verdictElapsed = waitForExpr(
    "document.querySelector('[data-byok=\"verdict\"]') !== null",
    20,
  );
  record(
    "P11 save-continuation: verdict rendered",
    verdictElapsed !== -1,
    verdictElapsed !== -1 ? `verdict rendered (${verdictElapsed}ms)` : "no [data-byok=verdict] after save",
  );

  screenshot("fb-01-no-key-inline-form", "exercise page with empty storage: inline key form + save-continuation verdict");
}

// ---------------------------------------------------------------------------
// P5 — cross-page persistence (save via key-page UI → real navigation)
// ---------------------------------------------------------------------------
function probeP5CrossPage() {
  // Page 1 — save the key through the REAL key-page UI (password input →
  // Save button). NOT eval localStorage.setItem (sneaky-pass).
  navigateTo(KEY_PAGE_URL);
  const formMounted = waitForExpr(
    "document.querySelector('[data-byok=\"key-input\"]') !== null",
    15,
  );
  if (formMounted === -1) {
    record("P5 vacuous guard: key form mounted", false, "no [data-byok=key-input] after 15s");
    return;
  }
  rodney(["input", '[data-byok="key-input"]', "fw_cross_page_456"]);
  rodney(["click", '[data-byok="save"]']);
  const saved = waitForExpr(
    "localStorage.getItem('fireworks_api_key') === 'fw_cross_page_456'",
    15,
  );
  if (saved === -1) {
    record("P5 save: key stored via key-page UI", false, "fireworks_api_key not stored after save click");
    return;
  }
  record("P5 save: key stored via key-page UI", true, `key saved through UI (${saved}ms)`);

  // Page 2 — REAL navigation in the SAME rodney browser session: same origin
  // (localhost static server), DIRECT location.href hop (no blank-page
  // intermediate — a blank page would clear localStorage and break the
  // cross-page persistence this clause tests). NOT a new tab, NOT eval
  // pre-seed.
  rodney(["js", `window.location.href = '${EXERCISE_PAGE_URL}'`]);
  sleep(3);
  const exerciseMounted = waitForFeedbackUi();
  if (exerciseMounted === -1) {
    record("P5 vacuous guard: exercise page mounted", false, "no feedback buttons after 20s");
    return;
  }
  record("P5 vacuous guard: exercise page mounted", true, `r-exercises.html mounted (${exerciseMounted}ms)`);

  assertExpr(
    "P5 cross-page: localStorage key persists after navigation",
    "localStorage.getItem('fireworks_api_key') === 'fw_cross_page_456'",
  );

  // Proceed past the no-key state: with the key present, clicking the
  // feedback button must NOT render the no-key state (no inline key form —
  // it goes straight to pending → verdict through the stub). The rate-limit
  // config comes from the REAL rendered page: blendtutor.lua
  // build_key_page_config_script now emits
  // window.__btConfig.maxFeedbackPerSession = window.__btConfig.maxFeedbackPerSession ?? 20
  // (issue #179, crates parity) alongside keyPageUrl — so NO manual injection
  // here. A regression to keyPageUrl-only emission would compute
  // (maxFeedbackPerSession || 0) = 0 → rateLimitReached() = 0>=0===true and
  // silently disable feedback; the P10 config guard below catches that.
  installSpies();
  rodney(["click", ".bt-exercise:first-of-type .bt-feedback-btn"]);
  sleep(1);
  const pastNoKey = rodneyJs("document.querySelector('[data-byok=\"no-key\"]') === null");
  const pendingOrVerdict = waitForExpr(
    "document.querySelector('[data-byok=\"pending\"]') !== null || document.querySelector('[data-byok=\"verdict\"]') !== null",
    15,
  );
  record(
    "P5 past-no-key: click proceeds (no no-key link, pending/verdict reached)",
    pastNoKey === "true" && pendingOrVerdict !== -1,
    `no-key absent=${pastNoKey}, pending/verdict reached=${pendingOrVerdict !== -1}`,
  );

  screenshot("fb-02-cross-page-verdict", "after cross-page hop: key present, verdict flow reached");
}

// ---------------------------------------------------------------------------
// P10 — verdict end-to-end through the REAL stub (XSS renders as text)
// ---------------------------------------------------------------------------
function probeP10Verdict() {
  // Key already present from P5 (same rodney session, same origin). Rate-limit
  // config comes from the REAL rendered page (issue #179: blendtutor.lua emits
  // maxFeedbackPerSession ?? 20 — default 20, crates parity; NO probe-side
  // injection). Assert the rendered config is a working non-zero numeric value
  // so a silent-disable regression (keyPageUrl-only emission → undefined →
  // 0>=0===true) fails loudly here rather than vacuous-passing. Deliberately
  // loose on the exact number (>= 1, not === 20): the probe pins the CONTRACT
  // (a working rate limit), not the tuning constant.
  const cfgOk = rodneyJs(
    "(window.__btConfig && typeof window.__btConfig.maxFeedbackPerSession === 'number' && window.__btConfig.maxFeedbackPerSession >= 1)",
  );
  record("P10 config guard: real render carries non-zero maxFeedbackPerSession", cfgOk === "true", `config=${cfgOk}`);

  // P10 explicitly: NO fetch-spy substitution — the verdict must come from a
  // NEW real stub /chat/completions round-trip. P5 already rendered a verdict,
  // so a selector-only wait would vacuous-pass; instead require the stub
  // completions count to INCREASE (real request observed), then assert the
  // refreshed verdict DOM.
  const baseline = fetchLog().filter((e) => e.url.includes("/chat/completions")).length;
  rodney(["click", ".bt-exercise:first-of-type .bt-feedback-btn"]);
  const roundTripElapsed = waitForExpr(
    `JSON.parse(JSON.stringify(window.__fetchLog || [])).filter((e) => String(e.url).includes('/chat/completions')).length > ${baseline}`,
    20,
  );
  if (roundTripElapsed === -1) {
    const errTxt = rodneyJs("document.querySelector('[data-byok=\"error\"]') ? document.querySelector('[data-byok=\"error\"]').textContent : 'no-error'");
    record("P10 verdict: real stub round-trip fired", false, `no new /chat/completions after 20s; error=${errTxt}`);
    return;
  }
  record("P10 verdict: real stub round-trip fired", true, `new /chat/completions observed (${roundTripElapsed}ms)`);

  const verdictElapsed = waitForExpr(
    "document.querySelector('[data-byok=\"verdict\"]') !== null",
    20,
  );
  if (verdictElapsed === -1) {
    record("P10 verdict: rendered from real stub", false, "no [data-byok=verdict] after round-trip");
    return;
  }
  record("P10 verdict: rendered from real stub", true, `verdict rendered (${verdictElapsed}ms)`);

  // textContent-only rendering: the verdict must show the stub message AND
  // the XSS payload as literal text, with NO element injected.
  const verdictText = rodneyJs("JSON.stringify(document.querySelector('[data-byok=\"verdict\"]').textContent)");
  const xssRaw = rodneyJs("window.__xss === undefined");
  const noImgInVerdict = rodneyJs("document.querySelectorAll('[data-byok=\"verdict\"] img').length === 0");

  let textOk = false;
  let payloadLiteral = false;
  try {
    const text = JSON.parse(verdictText);
    textOk = text.includes("Stub says correct.");
    payloadLiteral = text.includes("<img src=x onerror=window.__xss=1>");
  } catch (_) {}
  record(
    "P10 verdict: stub message rendered",
    textOk,
    `verdict text: ${verdictText}`,
  );
  record(
    "P10 XSS: payload renders as literal text (in textContent, no element)",
    payloadLiteral && xssRaw === "true" && noImgInVerdict === "true",
    `payload-literal=${payloadLiteral}, window.__xss undefined=${xssRaw}, img-in-verdict=${noImgInVerdict}`,
  );

  const log = fetchLog();
  const completions = log.filter((e) => e.url.includes("/chat/completions"));
  record(
    "P10 real round-trip: /chat/completions hit stub",
    completions.length >= 1,
    `completions observed: ${JSON.stringify(log)}`,
  );

  screenshot("fb-03-verdict-xss-literal", "verdict rendered with XSS payload as literal text");
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
    path.join(EVIDENCE_DIR, "feedback-probe-report.json"),
    JSON.stringify(report, null, 2),
  );

  const lines = [
    `# Rodney probes — feedback flow (issue #169, AC-8 P5/P10/P11)`,
    `verdict: ${verdict}`,
    `timestamp: ${report.timestamp}`,
    "",
    ...probeLog.map((p) => `- ${p.status}: ${p.name}\n  ${p.details}`),
  ];
  fs.writeFileSync(path.join(EVIDENCE_DIR, "feedback-probe.log"), lines.join("\n"));

  console.log(`\n=== ${verdict} ===`);
  console.log(`report: ${path.join(EVIDENCE_DIR, "feedback-probe-report.json")}`);
  console.log(`log:    ${path.join(EVIDENCE_DIR, "feedback-probe.log")}`);
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
    // Clear the HTTP cache (stale demo-book render from an earlier session
    // must not leak into the probe).
    rodney(["open", BLANK_URL]);
    try {
      rodney(["clear-cache"]);
    } catch (_) {}
    // Persisted profile localStorage: P11's precondition is "no stored key".
    // Clear on the blank page (same origin as the exercise page).
    rodneyJs("(() => { localStorage.clear(); return 'cleared'; })()");

    probeP11NoKeyLink();
    probeP5CrossPage();
    probeP10Verdict();

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

  // P1 — exit-code gate: PROBES_FAIL MUST exit non-zero (old feedback-probe.js
  // exited 0 on failure; pages-live.js:701 is the reference pattern).
  process.exit(verdict === "PROBES_PASS" ? 0 : 1);
}

main();
