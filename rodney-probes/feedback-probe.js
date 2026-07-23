#!/usr/bin/env node
/**
 * Rodney probe harness for issue #112 — per-exercise BYOK LLM feedback.
 *
 * Tests the 9 AC-7 clauses in a headless browser using the feedback.qmd fixture.
 * A mock runtime adapter is injected so the probes exercise the UI without
 * downloading webR.
 *
 * Usage:
 *   node rodney-probes/feedback-probe.js
 *
 * Environment:
 *   QUARTO_BIN   - path to quarto binary (default: /private/tmp/quarto/bin/quarto or "quarto")
 *   STATIC_PORT  - port for static fixture server (default: 8080)
 *   STUB_PORT    - port for stub LLM server (default: 8081)
 */

const { execFileSync, spawn } = require("child_process");
const fs = require("fs");
const path = require("path");
const http = require("http");
const os = require("os");

const WORKTREE = path.resolve(__dirname, "..");
const EVIDENCE_DIR = path.join(WORKTREE, "docs", "evidence", "112");
const STATIC_PORT = parseInt(process.env.STATIC_PORT || "8080", 10);
const STUB_PORT = parseInt(process.env.STUB_PORT || "8081", 10);
const QUARTO_BIN =
  process.env.QUARTO_BIN ||
  (fs.existsSync("/private/tmp/quarto/bin/quarto")
    ? "/private/tmp/quarto/bin/quarto"
    : "quarto");

const BASE_URL = `http://localhost:${STATIC_PORT}`;
const BLANK_URL = `${BASE_URL}/quarto-fixture/_probe-blank.html`;
const FIXTURE_URL = `${BASE_URL}/quarto-fixture/feedback-probe.html?provider=http://localhost:${STUB_PORT}`;

const COI_SERVER_PY = `#!/usr/bin/env python3
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

const STUB_SERVER_PY = `#!/usr/bin/env python3
import http.server, json, socketserver, sys, time
PORT = int(sys.argv[1]) if len(sys.argv) > 1 else 8081
DELAY = float(sys.argv[2]) if len(sys.argv) > 2 else 0.0
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
            self._send_json({"data": [{"id": "stub-model"}, {"id": "accounts/fireworks/models/deepseek-v4-flash"}, {"id": "claude-opus-4-8"}]})
            return
        self._send_json({"error": "not found"}, 404)
    def do_POST(self):
        global DELAY
        if self.path == "/_config/delay":
            length = int(self.headers.get("Content-Length", 0))
            cfg = json.loads(self.rfile.read(length)) if length else {}
            DELAY = float(cfg.get("delay", 0))
            self._send_json({"delay": DELAY})
            return
        if DELAY > 0:
            time.sleep(DELAY)
        if self.path == "/chat/completions":
            self._send_json({"choices": [{"message": {"tool_calls": [{"function": {"name": "respond_with_feedback", "arguments": json.dumps({"is_correct": True, "feedback_message": "Stub says correct."})}}]}}]})
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

function waitForPort(port, timeoutSeconds = 10) {
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
  const staticScript = writeTempScript("coiserve", COI_SERVER_PY);
  const staticProc = spawn("python3", [staticScript, String(STATIC_PORT)], {
    cwd: WORKTREE,
    detached: true,
    stdio: "ignore",
  });
  staticProc.unref();
  servers.push(staticProc);
  if (!waitForPort(STATIC_PORT)) {
    throw new Error(`Static server did not start on port ${STATIC_PORT}`);
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

function ensureRenderedFixture() {
  const fixtureHtml = path.join(WORKTREE, "quarto-fixture", "feedback.html");
  if (!fs.existsSync(fixtureHtml)) {
    console.log("Rendering feedback.qmd ...");
    execFileSync(QUARTO_BIN, ["render", "quarto-fixture/feedback.qmd"], {
      cwd: WORKTREE,
      stdio: "inherit",
    });
  }
}

function generateProbeHtml() {
  const src = fs.readFileSync(
    path.join(WORKTREE, "quarto-fixture", "feedback.html"),
    "utf8",
  );
  const mockScript = `<script type="module">
  import { scanExercises, buildRegistry, start } from "../_extensions/blendtutor/assets/exercise-runtime.js";
  import { mountAllFeedback } from "../_extensions/blendtutor/assets/exercise-feedback.js";

  const mockAdapter = {
    name: "mock",
    language: "r",
    async boot() { console.log("[mock-adapter] boot"); },
    async run(code, checks, packages) { return { output: "[1] 5", ok: true }; },
  };
  const registry = buildRegistry(scanExercises());
  window.__btWebRAdapter = mockAdapter;
  start(registry, mockAdapter).then(() => mountAllFeedback(registry));
</script>`;
  const out = src.replace(/<script type="module">[\s\S]*?<\/script>/, mockScript);
  fs.writeFileSync(
    path.join(WORKTREE, "quarto-fixture", "feedback-probe.html"),
    out,
  );
  fs.writeFileSync(
    path.join(WORKTREE, "quarto-fixture", "_probe-blank.html"),
    "<!DOCTYPE html><html><body></body></html>",
  );
}

function installSpies() {
  rodney([
    "js",
    `(() => {
      window.__btConfig = { maxFeedbackPerSession: 10 };
      const orig = window.fetch;
      window.fetch = async (url, init) => {
        window.__fetchLog = window.__fetchLog || [];
        window.__fetchBodies = window.__fetchBodies || [];
        window.__fetchLog.push(url);
        if (init && init.body) window.__fetchBodies.push(String(init.body));
        return orig(url, init);
      };
    })()`,
  ]);
}

function navigateToFixture() {
  // feedback-probe.html is heavy enough that rodney open panics on first load;
  // navigate from a blank page instead.
  rodney(["open", BLANK_URL]);
  rodney(["js", `window.location.href = '${FIXTURE_URL}'`]);
  sleep(3);
}

function fetchLog() {
  const raw = rodney(["js", "JSON.stringify(window.__fetchLog || [])"]);
  return JSON.parse(raw || "[]");
}

function fetchBodies() {
  const raw = rodney(["js", "JSON.stringify(window.__fetchBodies || [])"]);
  return JSON.parse(raw || "[]");
}

function clearStateAndReload() {
  rodney(["js", "(() => { sessionStorage.clear(); location.reload(); })()"]);
  sleep(3);
}

function freshFixture() {
  rodney(["js", "sessionStorage.clear()"]);
  navigateToFixture();
}

function exists(sel) {
  try {
    rodney(["exists", sel]);
    return true;
  } catch (_) {
    return false;
  }
}

function sourceCheck() {
  const js = fs.readFileSync(
    path.join(WORKTREE, "_extensions", "blendtutor", "assets", "exercise-feedback.js"),
    "utf8",
  );
  const qmd = fs.readFileSync(
    path.join(WORKTREE, "quarto-fixture", "feedback.qmd"),
    "utf8",
  );
  const ok = !js.includes("llm_evaluation_prompt") && !qmd.includes("llm_evaluation_prompt");
  record(
    "clause-6: llm_evaluation_prompt absent",
    ok,
    ok
      ? "llm_evaluation_prompt not found in exercise-feedback.js or feedback.qmd"
      : "llm_evaluation_prompt leaked into browser-facing source",
  );
}

function screenshot(name, description) {
  const file = path.join(EVIDENCE_DIR, `${name}.png`);
  rodney(["screenshot", file]);
  screenshots.push({ path: file, ui_state: description });
  console.log(`[screenshot] ${file}`);
}

function runProbes() {
  // -------------------------------------------------------------------
  // Boot
  // -------------------------------------------------------------------
  rodney(["start"]);
  rodneyStarted = true;
  navigateToFixture();

  // -------------------------------------------------------------------
  // Clause 9: feedback UI visible per exercise
  // -------------------------------------------------------------------
  const btnCount = rodney([
    "js",
    "document.querySelectorAll('.bt-feedback-btn').length",
  ]);
  const uiOk = btnCount === "2";
  record(
    "clause-9: feedback UI visible per exercise",
    uiOk,
    `found ${btnCount} feedback button(s) (expected 2)`,
  );
  screenshot("01-initial-state", "two exercises with per-exercise Get feedback buttons");

  installSpies();

  // -------------------------------------------------------------------
  // Clause 1: key entered once + Clause 2: per-exercise scoping
  // -------------------------------------------------------------------
  rodney(["click", ".bt-exercise:first-of-type .bt-feedback-btn"]);
  sleep(1);
  const keyPromptVisible = exists('[data-byok="key-prompt"]');
  record(
    "clause-1: key prompt shown for first exercise",
    keyPromptVisible,
    keyPromptVisible ? "key prompt rendered" : "key prompt not found",
  );
  screenshot("02-ex1-key-prompt", "first exercise asks for provider + API key");

  rodney([
    "input",
    '[data-byok="key-prompt"] input[name="provider-key"]',
    "fake-key-123",
  ]);
  rodney(["click", '[data-byok="key-prompt"] button[type="submit"]']);
  sleep(1);
  const pickerVisible = exists('[data-byok="model-picker"]');
  record(
    "clause-1: key stored after prompt",
    pickerVisible,
    pickerVisible ? "model picker rendered after saving key" : "model picker not rendered",
  );
  screenshot("03-ex1-model-picker", "model picker rendered after key saved");

  // -------------------------------------------------------------------
  // Clause 7: fetch payload contains STUDENT_CODE fences
  // -------------------------------------------------------------------
  rodney(["click", ".bt-exercise:first-of-type .bt-feedback-btn"]);
  sleep(2);
  const bodies = fetchBodies();
  const lastBody = bodies.length ? bodies[bodies.length - 1] : "";
  const hasBegin = lastBody.includes("<<<STUDENT_CODE_BEGIN>>>");
  const hasEnd = lastBody.includes("<<<STUDENT_CODE_END>>>");
  const hasCode = lastBody.includes("x <- 5") && lastBody.includes("print(x)");
  const clause7 = hasBegin && hasEnd && hasCode;
  record(
    "clause-7: fetch payload has STUDENT_CODE fences",
    clause7,
    `begin=${hasBegin}, end=${hasEnd}, code=${hasCode}`,
  );
  screenshot("04-ex1-verdict", "verdict rendered after feedback fetch");

  // -------------------------------------------------------------------
  // Clause 3: key reused for second exercise
  // -------------------------------------------------------------------
  rodney([
    "js",
    "document.querySelectorAll('.bt-exercise')[1].querySelector('.bt-feedback-btn').click()",
  ]);
  sleep(1);
  const ex2NoPrompt = !exists('[data-byok="key-prompt"]');
  const ex2Picker = exists('[data-byok="model-picker"]');
  const keyInStore = rodney([
    "js",
    "sessionStorage.getItem('fireworks_api_key')",
  ]);
  record(
    "clause-3: key reused across exercises",
    ex2NoPrompt && ex2Picker && keyInStore === "fake-key-123",
    `ex2 key prompt skipped=${ex2NoPrompt}, model picker=${ex2Picker}, stored key=${keyInStore}`,
  );
  screenshot("05-ex2-model-picker", "second exercise reuses key without prompting");

  // -------------------------------------------------------------------
  // Clause 4: provider switch
  // -------------------------------------------------------------------
  clearStateAndReload();
  installSpies();
  rodney(["click", ".bt-exercise:first-of-type .bt-feedback-btn"]);
  sleep(0.5);
  rodney(["select", '[data-byok="provider"]', "anthropic"]);
  rodney([
    "input",
    '[data-byok="key-prompt"] input[name="provider-key"]',
    "fake-key-123",
  ]);
  rodney(["click", '[data-byok="key-prompt"] button[type="submit"]']);
  sleep(1);
  rodney(["click", ".bt-exercise:first-of-type .bt-feedback-btn"]);
  sleep(2);
  const anthropicLog = fetchLog();
  const anthropicOk =
    anthropicLog.some((u) => u.includes("/v1/models")) &&
    anthropicLog.some((u) => u.includes("/v1/messages"));
  record(
    "clause-4: provider switch changes fetch URL",
    anthropicOk,
    `anthropic endpoints called: ${JSON.stringify(anthropicLog)}`,
  );

  // -------------------------------------------------------------------
  // Clause 5: provider override
  // -------------------------------------------------------------------
  const baseOverrideUrl = `${BASE_URL}/quarto-fixture/_probe-blank.html`;

  rodney(["open", `${baseOverrideUrl}?provider=https://attacker.example`]);
  sleep(1);
  const nonLocalReject = rodney([
    "js",
    "(async () => { const mod = await import('/_extensions/blendtutor/assets/exercise-feedback.js'); return mod.providerBaseUrl('fireworks'); })()",
  ]);
  const nonLocalOk =
    !nonLocalReject.includes("attacker") &&
    nonLocalReject.includes("fireworks.ai");
  record(
    "clause-5: non-local provider override rejected",
    nonLocalOk,
    `result=${nonLocalReject}`,
  );

  rodney(["open", `${baseOverrideUrl}?provider=http://user:pass@localhost:8081`]);
  sleep(1);
  const credReject = rodney([
    "js",
    "(async () => { const mod = await import('/_extensions/blendtutor/assets/exercise-feedback.js'); return mod.providerBaseUrl('fireworks'); })()",
  ]);
  const credOk = !credReject.includes("user:pass");
  record(
    "clause-5: credentialed provider override rejected",
    credOk,
    `result=${credReject}`,
  );

  rodney(["open", `${baseOverrideUrl}?provider=http://localhost:8081`]);
  sleep(1);
  const localHonored = rodney([
    "js",
    "(async () => { const mod = await import('/_extensions/blendtutor/assets/exercise-feedback.js'); return mod.providerBaseUrl('fireworks'); })()",
  ]);
  const localOk = localHonored === "http://localhost:8081";
  record(
    "clause-5: localhost provider override honored",
    localOk,
    `result=${localHonored}`,
  );

  // -------------------------------------------------------------------
  // Clause 8: concurrent guard
  // -------------------------------------------------------------------
  // Slow down the stub so the second click lands while the first is pending.
  execFileSync("curl", [
    "-s",
    "-X",
    "POST",
    "-H",
    "Content-Type: application/json",
    "-d",
    '{"delay":3}',
    `http://localhost:${STUB_PORT}/_config/delay`,
  ]);
  freshFixture();
  installSpies();
  rodney(["click", ".bt-exercise:first-of-type .bt-feedback-btn"]);
  sleep(0.5);
  rodney([
    "input",
    '[data-byok="key-prompt"] input[name="provider-key"]',
    "fake-key-123",
  ]);
  rodney(["click", '[data-byok="key-prompt"] button[type="submit"]']);
  sleep(1);
  // Two quick feedback submits on the same exercise.
  rodney(["click", ".bt-exercise:first-of-type .bt-feedback-btn"]);
  rodney(["click", ".bt-exercise:first-of-type .bt-feedback-btn"]);
  sleep(4);
  const concurrentLog = fetchLog();
  const completions = concurrentLog.filter((u) => u.includes("/chat/completions"));
  const concurrentOk = completions.length === 1;
  record(
    "clause-8: concurrent feedback guard",
    concurrentOk,
    `${completions.length} /chat/completions call(s) after two quick clicks`,
  );
  // restore fast stub
  execFileSync("curl", [
    "-s",
    "-X",
    "POST",
    "-H",
    "Content-Type: application/json",
    "-d",
    '{"delay":0}',
    `http://localhost:${STUB_PORT}/_config/delay`,
  ]);

  // -------------------------------------------------------------------
  // Clause 2: per-exercise scoping / no singleton bleed
  // -------------------------------------------------------------------
  // After both exercises rendered verdicts, exercise 1's container should still
  // belong to exercise 1 and exercise 2's container to exercise 2.
  const containers = rodney([
    "js",
    "document.querySelectorAll('[data-byok=\"feedback\"]').length",
  ]);
  const scopedOk = containers === "2";
  record(
    "clause-2: per-exercise feedback containers",
    scopedOk,
    `${containers} feedback container(s) (expected 2)`,
  );

  // -------------------------------------------------------------------
  // Clause 6: source check
  // -------------------------------------------------------------------
  sourceCheck();
}

function writeReport() {
  fs.mkdirSync(EVIDENCE_DIR, { recursive: true });
  const failed = probeLog.filter((p) => p.status === "FAIL");
  const verdict = failed.length === 0 ? "PROBES_PASS" : "PROBES_FAIL";

  const report = {
    issue: 112,
    branch: "112-exercise-feedback",
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
    `# Rodney probes for issue #112`,
    `verdict: ${verdict}`,
    `timestamp: ${report.timestamp}`,
    "",
    ...probeLog.map(
      (p) => `- ${p.status}: ${p.name}\n  ${p.details}`,
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
    generateProbeHtml();
    startServers();
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
