"use strict";
/**
 * Smoke tests for scripts/rodney-chrome.sh (issue #153 — AC-3 cross-origin
 * isolation fix). The wrapper is a bash argv filter: it strips the Chrome
 * flags rodney 0.4.0 / go-rod hardcode (--single-process,
 * --disable-site-isolation-trials, the whole --disable-features=... argument)
 * and execs a real Chrome binary with the cleaned argv.
 *
 * These tests are behavioral, no rodney/browser involved: a fake REAL_CHROME
 * (a tiny executable that echoes its argv) stands in for Chrome, so we can
 * assert exactly which flags reach the browser. Run with:
 *   uv run node --test rodney-probes/rodney-chrome.test.js
 */

const { test } = require("node:test");
const assert = require("node:assert/strict");
const { spawnSync } = require("child_process");
const fs = require("fs");
const os = require("os");
const path = require("path");

const WRAPPER = path.resolve(__dirname, "..", "scripts", "rodney-chrome.sh");

/**
 * Create a fake Chrome binary that echoes its argv (one arg per line).
 * The wrapper resolves it via the REAL_CHROME env override.
 */
function makeFakeChrome() {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "rodney-chrome-test-"));
  const fake = path.join(dir, "fake-chrome");
  fs.writeFileSync(fake, "#!/bin/sh\nprintf '%s\\n' \"$@\"\n");
  fs.chmodSync(fake, 0o755);
  return { dir, fake };
}

function runWrapper(args, env) {
  return spawnSync(WRAPPER, args, {
    encoding: "utf8",
    env: { ...process.env, ...env },
  });
}

test("wrapper exists and is executable (rodney execs ROD_CHROME_BIN directly)", () => {
  assert.ok(fs.existsSync(WRAPPER), `wrapper missing at ${WRAPPER}`);
  const st = fs.statSync(WRAPPER);
  assert.ok(st.mode & 0o111, "wrapper must be executable (chmod +x)");
});

test("wrapper strips --single-process, --disable-site-isolation-trials, and the whole --disable-features=... argument, keeping everything else", () => {
  const { dir, fake } = makeFakeChrome();
  try {
    const res = runWrapper(
      [
        "--no-sandbox",
        "--single-process",
        "--disable-site-isolation-trials",
        "--disable-features=site-per-process",
        "--disable-gpu",
        "--user-data-dir=/tmp/profile",
        "about:blank",
      ],
      { REAL_CHROME: fake },
    );
    assert.equal(res.status, 0, `wrapper failed: ${res.stderr}`);
    const args = res.stdout.trim().split("\n");
    assert.deepEqual(args, [
      "--no-sandbox",
      "--disable-gpu",
      "--user-data-dir=/tmp/profile",
      "about:blank",
    ]);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("wrapper strips --disable-features=... regardless of its value", () => {
  const { dir, fake } = makeFakeChrome();
  try {
    const res = runWrapper(
      ["--disable-features=TranslateUI", "--disable-features=", "about:blank"],
      { REAL_CHROME: fake },
    );
    assert.equal(res.status, 0, `wrapper failed: ${res.stderr}`);
    assert.deepEqual(res.stdout.trim().split("\n"), ["about:blank"]);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("wrapper keeps ordinary flags in order and does not invent new ones", () => {
  const { dir, fake } = makeFakeChrome();
  try {
    const res = runWrapper(
      ["--no-sandbox", "--disable-gpu", "--remote-debugging-port=9222", "about:blank"],
      { REAL_CHROME: fake },
    );
    assert.equal(res.status, 0, `wrapper failed: ${res.stderr}`);
    assert.deepEqual(res.stdout.trim().split("\n"), [
      "--no-sandbox",
      "--disable-gpu",
      "--remote-debugging-port=9222",
      "about:blank",
    ]);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("wrapper errors with a clear message when no real Chrome can be resolved", () => {
  // Empty HOME + no REAL_CHROME + no system Chrome path on the test machine
  // (macOS/linux paths don't exist here) forces the error branch
  // deterministically: none of the resolution steps can match.
  const home = fs.mkdtempSync(path.join(os.tmpdir(), "rodney-chrome-home-"));
  try {
    const res = runWrapper(["about:blank"], { HOME: home, REAL_CHROME: "" });
    assert.notEqual(res.status, 0, "wrapper should fail when no Chrome resolves");
    assert.match(res.stderr, /no real Chrome found/);
    assert.match(res.stderr, /REAL_CHROME/);
  } finally {
    fs.rmSync(home, { recursive: true, force: true });
  }
});
