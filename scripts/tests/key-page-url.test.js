"use strict";
/**
 * Pure unit test for keyPageUrl() in exercise-feedback.js (issue #165 —
 * byok-api-key AC-4, arm 7).
 *
 * keyPageUrl() is a total, exported function: reads window.__btConfig.keyPageUrl
 * at CALL time (never cached at module load — arm 2's lazy-read invariant),
 * rejects javascript:/data: schemes (arm 6), and falls back to "api-key.html"
 * when the config is absent/empty (arm 3).
 *
 * Usage:
 *   uv run node --test scripts/tests/key-page-url.test.js
 *
 * The module is importable in Node because exercise-feedback.js has no
 * module-level effectful code (§2.1); only window/localStorage globals are
 * mocked here.
 */

const { test } = require("node:test");
const assert = require("node:assert/strict");
const { pathToFileURL } = require("node:url");
const path = require("node:path");

const MOD_URL = pathToFileURL(
  path.join(
    __dirname,
    "..",
    "..",
    "_extensions",
    "blendtutor",
    "assets",
    "exercise-feedback.js",
  ),
).href;

// Set up the browser globals BEFORE importing the module. keyPageUrl() reads
// window.__btConfig at call time, so each test controls the config via
// mockWindow and the single cached module instance still sees the fresh value.
// Passing undefined simulates an absent window (Node without a DOM); passing a
// window object without __btConfig simulates "__btConfig undefined" while the
// window exists.
function mockWindow(windowObj) {
  const localStorageMap = new Map();
  const localStorage = {
    getItem: (k) => localStorageMap.get(k) ?? null,
    setItem: (k, v) => localStorageMap.set(k, String(v)),
    removeItem: (k) => localStorageMap.delete(k),
  };
  const location = { search: "" };
  globalThis.window =
    windowObj === undefined
      ? undefined
      : { localStorage, location, ...windowObj };
  globalThis.localStorage = localStorage;
}

test("keyPageUrl() returns api-key.html when window is absent (Node, no DOM)", async () => {
  mockWindow(undefined);
  const mod = await import(MOD_URL);
  assert.equal(mod.keyPageUrl(), "api-key.html");
});

test("keyPageUrl() returns api-key.html when __btConfig is undefined", async () => {
  mockWindow({});
  const mod = await import(MOD_URL);
  assert.equal(mod.keyPageUrl(), "api-key.html");
});

test("keyPageUrl() returns api-key.html when the config key is absent", async () => {
  mockWindow({ __btConfig: {} });
  const mod = await import(MOD_URL);
  assert.equal(mod.keyPageUrl(), "api-key.html");
});

test("keyPageUrl() returns api-key.html when the config key is empty", async () => {
  mockWindow({ __btConfig: { keyPageUrl: "" } });
  const mod = await import(MOD_URL);
  assert.equal(mod.keyPageUrl(), "api-key.html");
});

test("keyPageUrl() returns the configured value verbatim", async () => {
  mockWindow({ __btConfig: { keyPageUrl: "/custom/keys.html" } });
  const mod = await import(MOD_URL);
  assert.equal(mod.keyPageUrl(), "/custom/keys.html");
});

test("keyPageUrl() rejects the javascript: scheme (case-insensitive, trimmed)", async () => {
  mockWindow({ __btConfig: { keyPageUrl: "javascript:alert(1)" } });
  const mod = await import(MOD_URL);
  assert.equal(mod.keyPageUrl(), "api-key.html");

  mockWindow({ __btConfig: { keyPageUrl: "JaVaScRiPt:alert(1)" } });
  assert.equal(mod.keyPageUrl(), "api-key.html");

  mockWindow({ __btConfig: { keyPageUrl: "  javascript:alert(1)  " } });
  assert.equal(mod.keyPageUrl(), "api-key.html");
});

test("keyPageUrl() rejects the data: scheme", async () => {
  mockWindow({ __btConfig: { keyPageUrl: "data:text/html,<script>alert(1)</script>" } });
  const mod = await import(MOD_URL);
  assert.equal(mod.keyPageUrl(), "api-key.html");
});

test("keyPageUrl() allows ordinary relative and https URLs through", async () => {
  mockWindow({ __btConfig: { keyPageUrl: "keys.html" } });
  const mod = await import(MOD_URL);
  assert.equal(mod.keyPageUrl(), "keys.html");

  mockWindow({ __btConfig: { keyPageUrl: "https://example.com/keys.html" } });
  assert.equal(mod.keyPageUrl(), "https://example.com/keys.html");
});
