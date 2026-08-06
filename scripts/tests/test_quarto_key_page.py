#!/usr/bin/env python3
"""Executable spec for issue #163 — key-page.js key-management UI module.

Verifies the 16-clause predicate from AC-2 of byok-api-key:
  P1 (import contract)   — key-page.js imports {readKey, storeKey, clearKey,
                           providerBaseUrl, PROVIDERS} from "./exercise-feedback.js";
                           ZERO literals of fireworks_api_key / anthropic_api_key /
                           byok_provider / bt_feedback_count / api.fireworks.ai;
                           ZERO sessionStorage (AC-1's storage invariant).
  P2 (host-gated)        — validation URL built via providerBaseUrl("fireworks");
                           ?provider=http://localhost:8080 routes to the stub;
                           no hardcoded Fireworks host.
  P3 (discriminated)     — classifyValidation(status, threw): 2xx -> {ok:true};
                           401|403 -> invalid-key; thrown -> network; empty key
                           -> empty BEFORE any fetch (no listModels reuse).
  P4 (statusMessage)     — pure, exported, friendly copy per reason.
  P5 (key hygiene)       — zero console. calls, zero innerHTML; recorded DOM
                           textContent never contains the saved key; at least
                           one non-empty textContent after save.
  P6 (password attrs)    — input type="password" + autocomplete="off".
  P7 (save round-trip)   — Save calls imported storeKey(key, providerId) with the
                           NEW AC-1 signature (key, providerId); readKey round-trips.
  P8 (clear round-trip)  — Clear calls clearKey(providerId): key slot AND
                           bt_feedback_count both removed.
  P9 (empty save no-op)  — empty save does NOT clear the stored key and issues
                           no fetch.
  P10 (mount states)     — empty storage -> password input + Save; key set ->
                           "key is set" + Clear, input never pre-filled; after
                           Clear, UI returns to the empty input form.
  P11 (idempotent mount) — mount twice -> one save -> exactly one fetch.
  P12 (null mount)       — mountKeyPage(null/undefined) is a no-op, no crash.
  P13 (preventDefault)   — submit handler calls preventDefault.
  P14 (reset after store)— input value reset to "" AFTER storeKey.
  P15 (pure helpers)     — buildValidationUrl / classifyValidation / statusMessage
                           bodies contain no fetch(, localStorage, document..
  P16 (module discipline)— docstring header (WHAT/WHERE/NOT) + <=5 public exports.

Negative: key echoed into DOM textContent or console; hardcoded api.fireworks.ai;
innerHTML of status (XSS); Save writing sessionStorage or old slot names; Clear
leaving bt_feedback_count; validation silently skipped or 401 collapsed into
network error (listModels reuse); empty save wiping the stored key; key
pre-filled into the input on key-set mount; duplicate literal slot names instead
of the imported contract; fetch issued twice on double-mount.

Usage: python3 scripts/tests/test_quarto_key_page.py
       uv run pytest scripts/tests/test_quarto_key_page.py -x -q
"""

from __future__ import annotations

import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
JS_PATH = REPO_ROOT / "_extensions" / "blendtutor" / "assets" / "key-page.js"
FEEDBACK_JS_PATH = REPO_ROOT / "_extensions" / "blendtutor" / "assets" / "exercise-feedback.js"

PASS = 0
FAIL = 0


def check(cond: bool, msg: str) -> None:
    """Record a PASS/FAIL and raise on failure so pytest sees it."""
    global PASS, FAIL
    if cond:
        PASS += 1
        print(f"  PASS: {msg}")
    else:
        FAIL += 1
        print(f"  FAIL: {msg}")
        raise AssertionError(msg)


def _import_line(src: str) -> str:
    """The single import statement that pulls the exercise-feedback.js contract."""
    for line in src.split("\n"):
        if line.startswith("import ") and "exercise-feedback.js" in line:
            return line
    return ""


def _function_body(src: str, name: str) -> str | None:
    """Body of `export function <name>(...) { ... }` (closing brace at col 0)."""
    m = re.search(rf"export function {name}\([^)]*\)\s*\{{([\s\S]*?)\n\}}", src)
    return m.group(1) if m else None


def _shutil_which(cmd: str) -> str | None:
    from shutil import which

    return which(cmd)


# ---------------------------------------------------------------------------
# Source-pattern checks
# ---------------------------------------------------------------------------


def test_files_exist() -> None:
    """The two NEW fixtures: key-page.js + its test; the import source exists."""
    check(JS_PATH.exists(), "key-page.js exists")
    check(FEEDBACK_JS_PATH.exists(), "exercise-feedback.js exists (import contract source)")


def test_p1_import_contract() -> None:
    src = JS_PATH.read_text()
    line = _import_line(src)
    check(bool(line), "P1: key-page.js imports from './exercise-feedback.js'")
    for name in ["readKey", "storeKey", "clearKey", "providerBaseUrl", "PROVIDERS"]:
        check(name in line, f"P1: import contract includes {name}")
    for literal in [
        "fireworks_api_key",
        "anthropic_api_key",
        "byok_provider",
        "bt_feedback_count",
        "api.fireworks.ai",
        "sessionStorage",
    ]:
        check(literal not in src, f"P1: no literal '{literal}' anywhere in source")


def test_p3_discriminated_source() -> None:
    src = JS_PATH.read_text()
    check("classifyValidation" in src, "P3: classifyValidation present (discriminated result)")
    check("listModels" not in src, "P3: validation does NOT reuse listModels (401 must not collapse into []/network)")


def test_p5_source_hygiene() -> None:
    src = JS_PATH.read_text()
    check("console." not in src, "P5: zero console. calls (key never logged)")
    check("innerHTML" not in src, "P5: zero innerHTML usage (textContent-only rendering)")
    check("textContent" in src, "P5: rendering is textContent-based")


def test_p6_password_attrs() -> None:
    src = JS_PATH.read_text()
    check('type = "password"' in src, "P6: password input has type=password")
    check('autocomplete = "off"' in src, "P6: password input has autocomplete=off")


def test_p15_pure_helpers() -> None:
    src = JS_PATH.read_text()
    for name in ["buildValidationUrl", "classifyValidation", "statusMessage"]:
        body = _function_body(src, name)
        check(body is not None, f"P15: {name} is an exported pure function")
        if body is not None:
            for token in ["fetch(", "localStorage", "document."]:
                check(token not in body, f"P15: {name} body contains no {token}")


def test_p16_docstring_exports() -> None:
    src = JS_PATH.read_text()
    check("WHAT:" in src and "WHERE:" in src and "NOT:" in src, "P16: docstring header names WHAT/WHERE/NOT")
    exports = re.findall(r"^export function (\w+)", src, re.MULTILINE)
    check(len(exports) <= 5, f"P16: at most 5 public exports (found {len(exports)})")
    for name in ["buildValidationUrl", "classifyValidation", "statusMessage", "mountKeyPage"]:
        check(name in exports, f"P16: public export {name}")


# ---------------------------------------------------------------------------
# Behavioral checks via Node.js (recording DOM / localStorage / fetch mocks)
# ---------------------------------------------------------------------------

NODE_TEST_SCRIPT = r"""
import { pathToFileURL } from "url";

const keyPagePath = process.argv[2];
const feedbackPath = process.argv[3];

// --- recording mocks -------------------------------------------------------
const localStorageMap = new Map();
const textContents = [];
const fetchCalls = [];
let fetchImpl = async () => ({ status: 200, ok: true });
let storageError = null; // when set, localStorage.setItem throws (private mode / quota)

const localStorageMock = {
  getItem: (k) => (localStorageMap.has(k) ? localStorageMap.get(k) : null),
  setItem: (k, v) => {
    if (storageError) throw new Error(storageError);
    localStorageMap.set(k, String(v));
  },
  removeItem: (k) => localStorageMap.delete(k),
};
const location = { search: "" };

function findByDataset(node, key, val) {
  if (node && node.dataset && node.dataset[key] === val) return node;
  for (const child of node?.children ?? []) {
    const found = findByDataset(child, key, val);
    if (found) return found;
  }
  return null;
}

function makeEl(tag) {
  return {
    tagName: tag,
    dataset: {},
    children: [],
    _listeners: {},
    _text: "",
    value: "",
    get textContent() {
      return this._text;
    },
    set textContent(v) {
      this._text = String(v);
      if (String(v).length > 0) textContents.push(String(v));
    },
    append(...kids) { this.children.push(...kids); },
    appendChild(kid) { this.children.push(kid); return kid; },
    replaceChildren(...kids) { this.children = [...kids]; },
    addEventListener(type, fn) { (this._listeners[type] ??= []).push(fn); },
  };
}

globalThis.window = { localStorage: localStorageMock, location, __btConfig: {} };
globalThis.localStorage = localStorageMock;
globalThis.document = { createElement: (tag) => makeEl(tag) };
globalThis.fetch = async (url, options) => {
  fetchCalls.push({ url, options });
  return fetchImpl(url, options);
};

const feedback = await import(feedbackPath);
const mod = await import(keyPagePath);

let failures = 0;
function assert(cond, msg) {
  if (!cond) { console.error("  FAIL: " + msg); failures++; }
  else { console.log("  PASS: " + msg); }
}
function freshTarget() { return makeEl("div"); }

// --- P3: classifyValidation discriminated outcomes --------------------------
assert(mod.classifyValidation(200, false).ok === true, "P3: 2xx -> {ok:true}");
assert(mod.classifyValidation(201, false).ok === true, "P3: any 2xx is ok");
const inv401 = mod.classifyValidation(401, false);
assert(inv401.ok === false && inv401.reason === "invalid-key", "P3: 401 -> invalid-key");
assert(mod.classifyValidation(403, false).reason === "invalid-key", "P3: 403 -> invalid-key");
assert(mod.classifyValidation(500, false).reason === "network", "P3: other non-2xx -> network");
assert(mod.classifyValidation(0, true).reason === "network", "P3: thrown fetch -> network");

// --- P4: statusMessage friendly copy per reason -----------------------------
for (const reason of ["saved", "invalid-key", "network", "empty", "cleared"]) {
  assert(mod.statusMessage(reason).trim().length > 0, "P4: statusMessage(" + reason + ") has friendly copy");
}

// --- P2: buildValidationUrl is host-gated via providerBaseUrl ---------------
location.search = "?provider=http://localhost:8080";
assert(mod.buildValidationUrl("fireworks") === "http://localhost:8080/models", "P2: validation URL honors localhost ?provider= override");
location.search = "";
assert(mod.buildValidationUrl("fireworks").endsWith("/models"), "P2: default validation URL ends with /models");

// --- P10: mount state — no key -> password input + Save ---------------------
localStorageMap.clear();
textContents.length = 0;
fetchCalls.length = 0;
const t1 = freshTarget();
mod.mountKeyPage(t1);
const form1 = findByDataset(t1, "byok", "key-page-form");
const input1 = findByDataset(t1, "byok", "key-input");
const save1 = findByDataset(t1, "byok", "save");
const status1 = findByDataset(t1, "byok", "key-status");
assert(form1 !== null, "P10: no-key mount renders a form");
assert(input1 !== null, "P10: no-key mount renders a password input");
assert(input1.type === "password", "P6: input type is password");
assert(input1.autocomplete === "off", "P6: input autocomplete is off");
assert(save1 !== null, "P10: no-key mount renders the Save button");
assert(findByDataset(t1, "byok", "clear") === null, "P10: no-key mount has no Clear button");

// --- P13/P7/P14/P2/P11/P5: save round-trip ----------------------------------
location.search = "?provider=http://localhost:8080";
input1.value = "SECRET-TOKEN-XYZ";
fetchCalls.length = 0;
textContents.length = 0;
let prevented = false;
await form1._listeners.submit[0]({ preventDefault: () => { prevented = true; } });
assert(prevented, "P13: submit handler calls preventDefault");
assert(localStorageMap.get("fireworks_api_key") === "SECRET-TOKEN-XYZ", "P7: Save stores key via imported storeKey (AC-1 signature, key first)");
assert(feedback.readKey("fireworks") === "SECRET-TOKEN-XYZ", "P7: readKey round-trips the saved key");
assert(input1.value === "", "P14: input value reset to empty AFTER storeKey");
assert(fetchCalls.length === 1, "P11: one save issues exactly one fetch");
assert(fetchCalls[0].url === "http://localhost:8080/models", "P2: validation fetch targets the host-gated models URL");
assert(fetchCalls[0].options.headers["Authorization"] === "Bearer SECRET-TOKEN-XYZ", "key rides Authorization header, never the body");
assert(status1.textContent === mod.statusMessage("saved"), "P7: save reports saved status on 2xx");
for (const t of textContents) {
  assert(!t.includes("SECRET-TOKEN-XYZ"), "P5: key never echoed into any DOM textContent");
}
assert(textContents.some((t) => t.length > 0), "P5: at least one non-empty textContent after save");

// --- P9: empty save is a no-op (existing key survives, no fetch) ------------
input1.value = "   ";
fetchCalls.length = 0;
await form1._listeners.submit[0]({ preventDefault: () => {} });
assert(localStorageMap.get("fireworks_api_key") === "SECRET-TOKEN-XYZ", "P9: empty save does NOT clear the existing key");
assert(fetchCalls.length === 0, "P9: empty save issues no fetch");
assert(status1.textContent === mod.statusMessage("empty"), "P9: empty save reports the empty status");

// --- P8/P10: key-set mount state + Clear round-trip -------------------------
localStorageMap.clear();
fetchCalls.length = 0;
feedback.storeKey("fw_test_123", "fireworks");
feedback.incrementFeedbackCount(); // bt_feedback_count -> "1"
const t2 = freshTarget();
mod.mountKeyPage(t2);
const status2 = findByDataset(t2, "byok", "key-status");
const clearBtn = findByDataset(t2, "byok", "clear");
assert(status2 !== null && status2.textContent.includes("key is set"), "P10: key-set mount shows 'key is set' status");
assert(clearBtn !== null, "P10: key-set mount shows the Clear button");
assert(findByDataset(t2, "byok", "key-input") === null, "P10: key-set mount renders NO input — the key is never pre-filled");
clearBtn._listeners.click[0]();
assert(localStorageMap.get("fireworks_api_key") === undefined, "P8: Clear removes the provider key slot");
assert(localStorageMap.get("bt_feedback_count") === undefined, "P8: Clear removes bt_feedback_count (no rate-lock)");
assert(feedback.readKey("fireworks") === null, "P8: readKey returns null after Clear");
const formAfter = findByDataset(t2, "byok", "key-page-form");
const inputAfter = findByDataset(t2, "byok", "key-input");
assert(formAfter !== null && inputAfter !== null, "P10: after Clear, UI returns to the empty input form");
assert(inputAfter.value === "", "P10: post-Clear input starts empty");
assert(findByDataset(t2, "byok", "clear") === null, "after Clear, the Clear button is gone");
assert(findByDataset(t2, "byok", "key-status").textContent === mod.statusMessage("cleared"), "P4: Clear reports the cleared status");

// --- P11: idempotent mount -> one save -> exactly one fetch -----------------
localStorageMap.clear();
fetchCalls.length = 0;
location.search = "";
const t3 = freshTarget();
mod.mountKeyPage(t3);
mod.mountKeyPage(t3); // second mount must be a no-op
const form3 = findByDataset(t3, "byok", "key-page-form");
const input3 = findByDataset(t3, "byok", "key-input");
assert(findByDataset(t3, "byok", "key-page-form") === form3, "P11: double mount does not re-render (same form instance)");
assert(form3._listeners.submit.length === 1, "P11: double mount does not duplicate the submit listener");
input3.value = "fw_test_123";
fetchCalls.length = 0;
await form3._listeners.submit[0]({ preventDefault: () => {} });
assert(fetchCalls.length === 1, "P11: double mount -> one save -> exactly one fetch");
assert(localStorageMap.get("fireworks_api_key") === "fw_test_123", "P11: save still works after double mount");

// --- P12: mountKeyPage(null/undefined) is a no-op, no crash -----------------
let noCrash = true;
try {
  mod.mountKeyPage(null);
  mod.mountKeyPage(undefined);
} catch (_e) {
  noCrash = false;
}
assert(noCrash, "P12: mountKeyPage(null/undefined) no-op without crash");

// --- P3/P7: 401 reports invalid-key (NOT network); thrown fetch -> network --
localStorageMap.clear();
fetchCalls.length = 0;
const t4 = freshTarget();
mod.mountKeyPage(t4);
const form4 = findByDataset(t4, "byok", "key-page-form");
const input4 = findByDataset(t4, "byok", "key-input");
const status4 = findByDataset(t4, "byok", "key-status");

input4.value = "BAD-KEY-401";
fetchImpl = async () => ({ status: 401, ok: false });
await form4._listeners.submit[0]({ preventDefault: () => {} });
assert(status4.textContent === mod.statusMessage("invalid-key"), "P3: 401 save reports invalid-key (not collapsed into network)");
assert(fetchCalls.length === 1, "401 save issued one fetch");
assert(localStorageMap.get("fireworks_api_key") === "BAD-KEY-401", "advisory: key stored even when validation rejects");

input4.value = "KEY-NET-ERR";
fetchImpl = async () => { throw new Error("network down"); };
fetchCalls.length = 0;
await form4._listeners.submit[0]({ preventDefault: () => {} });
assert(status4.textContent === mod.statusMessage("network"), "P3: thrown fetch reports network");
assert(fetchCalls.length === 1, "network save issued one fetch");

// --- storage-unavailable: storeKey throws -> friendly status, ZERO fetches --
// Refusal arm #4: localStorage unavailable (private mode / quota) must NOT
// crash the save handler, must NOT fetch (no point validating an unstored
// key), and must surface a friendly storage status instead.
localStorageMap.clear();
storageError = "QuotaExceededError: storage unavailable";
fetchCalls.length = 0;
const t5 = freshTarget();
mod.mountKeyPage(t5);
const form5 = findByDataset(t5, "byok", "key-page-form");
const input5 = findByDataset(t5, "byok", "key-input");
const status5 = findByDataset(t5, "byok", "key-status");
input5.value = "KEY-STORE-THROW";
await form5._listeners.submit[0]({ preventDefault: () => {} });
assert(fetchCalls.length === 0, "storage-unavailable: ZERO fetches issued when storeKey throws");
assert(status5.textContent === mod.statusMessage("storage-unavailable"), "storage-unavailable: friendly storage status rendered");
assert(localStorageMap.get("fireworks_api_key") === undefined, "storage-unavailable: key NOT stored");
assert(input5.value === "KEY-STORE-THROW", "storage-unavailable: input keeps its value for retry (no silent wipe)");
storageError = null;

process.exit(failures > 0 ? 1 : 0);
"""


def test_node_behavioral() -> None:
    """Run behavioral key-page tests via Node.js with recording mocks."""
    if not JS_PATH.exists():
        check(False, "Node.js behavioral tests skipped — key-page.js missing")
        return
    if not _shutil_which("node"):
        check(False, "Node.js behavioral tests skipped — node not installed")
        return
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".mjs", delete=False, dir=str(REPO_ROOT)
    ) as f:
        f.write(NODE_TEST_SCRIPT)
        tmp_path = f.name
    try:
        result = subprocess.run(
            ["node", tmp_path, str(JS_PATH), str(FEEDBACK_JS_PATH)],
            capture_output=True,
            text=True,
            cwd=str(REPO_ROOT),
            timeout=30,
            check=False,
        )
        print(result.stdout, end="")
        if result.stderr:
            print(result.stderr, end="", file=sys.stderr)
        check(result.returncode == 0, "Node.js behavioral tests passed (all key-page assertions)")
    except subprocess.TimeoutExpired:
        check(False, "Node.js behavioral tests timed out")
    finally:
        os.unlink(tmp_path)


# ---------------------------------------------------------------------------
# Main (python3 direct invocation; pytest collects the test_* functions)
# ---------------------------------------------------------------------------


def main() -> int:
    tests = [
        test_files_exist,
        test_p1_import_contract,
        test_p3_discriminated_source,
        test_p5_source_hygiene,
        test_p6_password_attrs,
        test_p15_pure_helpers,
        test_p16_docstring_exports,
        test_node_behavioral,
    ]
    print("=== AC-2 key-page.js key-management UI — test_quarto_key_page.py ===\n")
    for t in tests:
        print(f"-- {t.__name__} --")
        try:
            t()
        except AssertionError:
            pass  # already counted by check()
        print()
    print(f"=== Results: {PASS} passed, {FAIL} failed ===")
    return 1 if FAIL else 0


if __name__ == "__main__":
    sys.exit(main())
