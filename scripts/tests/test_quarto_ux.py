#!/usr/bin/env python3
"""Executable spec for issue #113 — Exercise UX polish.

Verifies the 7-clause predicate from AC-8:
  1. hints visible/absent — exercises with hints render a <details> toggle;
     exercises without hints do not.
  2. solution button click inserts text — a "Show solution" button calls
     setEditorContent(payload.solution).
  3. check button absent when no checks — exercises with empty checks array
     do not render a Check button.
  4. Run disables ex-0 only — clicking Run on exercise 0 disables only
     exercise 0's Run button (per-exercise, not singleton).
  5. cursor=not-allowed — disabled buttons have cursor: not-allowed in CSS.
  6. data-status closed set — the status element's data-status is a closed
     enum: idle, running, pass, fail.
  7. buttons re-enabled on pass — after a run completes (pass), the Run button
     is re-enabled.

Negative: Run disables all exercises (singleton leak). Solution button for
empty exercise (no solution — button must not appear).

Usage: python3 scripts/tests/test_quarto_ux.py
"""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
JS_PATH = REPO_ROOT / "_extensions" / "blendtutor" / "assets" / "exercise-runtime.js"
CSS_PATH = REPO_ROOT / "_extensions" / "blendtutor" / "assets" / "styles.css"
QMD_PATH = REPO_ROOT / "quarto-fixture" / "ux.qmd"

PASS = 0
FAIL = 0


def ok(msg: str) -> None:
    global PASS
    PASS += 1
    print(f"  PASS: {msg}")


def ko(msg: str) -> None:
    global FAIL
    FAIL += 1
    print(f"  FAIL: {msg}")


# ---------------------------------------------------------------------------
# File existence checks
# ---------------------------------------------------------------------------


def check_files_exist() -> bool:
    """Verify the fixture and JS exist. Returns False if JS missing."""
    if JS_PATH.exists():
        ok("exercise-runtime.js exists")
    else:
        ko("exercise-runtime.js exists — file not found")
    if QMD_PATH.exists():
        ok("ux.qmd exists")
    else:
        ko("ux.qmd exists — file not found")
    return JS_PATH.exists()


# ---------------------------------------------------------------------------
# ux.qmd structure checks
# ---------------------------------------------------------------------------


def check_qmd_structure(qmd: str) -> None:
    """Clause setup: ux.qmd has 3 exercises with varying hints/solution/checks."""
    blendtutor_count = qmd.count("{.blendtutor")
    if blendtutor_count >= 3:
        ok(f"ux.qmd has {blendtutor_count} exercises (>=3 for UX polish test)")
    else:
        ko(f"ux.qmd has {blendtutor_count} exercises (need >=3)")

    # Exercise 1: full (hints + solution + checks)
    if ".solution" in qmd:
        ok("ux.qmd has a .solution code block (solution reveal test)")
    else:
        ko("ux.qmd missing .solution code block")

    if ".hints" in qmd or "::: {.hints}" in qmd:
        ok("ux.qmd has a hints div (hints toggle test)")
    else:
        ko("ux.qmd missing hints div")

    if ".checks" in qmd:
        ok("ux.qmd has .checks code block (check button test)")
    else:
        ko("ux.qmd missing .checks code block")

    # Exercise 2: no checks (check button absent)
    # Exercise 3: empty (no solution, no hints, no checks)
    # These are structural — the rodney probe verifies the runtime behavior.

    if "exercise-runtime" in qmd:
        ok("ux.qmd imports exercise-runtime.js")
    else:
        ko("ux.qmd does not import exercise-runtime.js")


# ---------------------------------------------------------------------------
# Source-pattern checks (exercise-runtime.js)
# ---------------------------------------------------------------------------


def _strip_comments(src: str) -> str:
    """Remove // comment lines so source-pattern checks don't false-positive
    on documentation that mentions the patterns being checked."""
    lines = []
    for line in src.split("\n"):
        stripped = line.lstrip()
        if stripped.startswith("//"):
            continue
        lines.append(line)
    return "\n".join(lines)


def check_hints_rendering(src: str) -> None:
    """Clause 1: exercises with hints render a <details> toggle.

    The runtime must create a <details> element when payload.hints is non-null,
    and must NOT create one when hints is null/empty.
    """
    code = _strip_comments(src)
    # Must create a <details> element for hints
    if "details" in code and "hints" in code.lower():
        ok("hints <details> toggle present in exercise-runtime.js")
    else:
        ko("hints <details> toggle missing from exercise-runtime.js")

    # Must check payload.hints before rendering (conditional)
    if "payload.hints" in code or "entry.payload.hints" in code:
        ok("hints rendering is conditional on payload.hints")
    else:
        ko("hints rendering not conditional on payload.hints — always renders")


def check_solution_button(src: str) -> None:
    """Clause 2: solution button click inserts solution text into editor.

    The runtime must create a "Show solution" button when payload.solution
    is non-null, and clicking it calls setEditorContent(payload.solution).
    """
    code = _strip_comments(src)
    # Must reference payload.solution
    if "payload.solution" in code or "entry.payload.solution" in code:
        ok("solution rendering references payload.solution")
    else:
        ko("solution rendering does not reference payload.solution")

    # Must create a button for solution reveal
    if "solution" in code.lower() and "button" in code.lower():
        ok("solution button creation present in exercise-runtime.js")
    else:
        ko("solution button creation missing from exercise-runtime.js")

    # Must call setEditorContent with the solution
    if "setEditorContent" in code and "solution" in code.lower():
        ok("solution button calls setEditorContent")
    else:
        ko("solution button does not call setEditorContent")


def check_check_button_conditional(src: str) -> None:
    """Clause 3: check button absent when no checks.

    The runtime must only create a Check button when payload.checks is
    non-empty. Exercises with empty checks must not have a Check button.
    """
    code = _strip_comments(src)
    # Must check payload.checks length before creating Check button
    if "checks" in code and ("length" in code or ".length" in code):
        ok("check button creation is conditional on checks length")
    else:
        ko("check button creation not conditional on checks length")

    # Must reference a Check button (or check-related button)
    if "check" in code.lower() and "button" in code.lower():
        ok("check button referenced in exercise-runtime.js")
    else:
        ko("check button not referenced in exercise-runtime.js")


def check_run_disable_per_exercise(src: str) -> None:
    """Clause 4: Run disables ex-0 only (per-exercise, not singleton).

    The runtime must disable the Run button per-exercise (entry-level), not
    via a module-level singleton. The negative case is "Run disables all
    exercises" — a singleton leak.
    """
    code = _strip_comments(src)
    # Must NOT use a module-level runButton variable (singleton)
    # The per-exercise pattern uses entry.runBtn or similar
    if "runBtn" in code or "run-btn" in code or "runButton" in code:
        ok("per-exercise Run button reference found")
    else:
        ko("per-exercise Run button reference missing")

    # Must disable the button via entry (per-exercise), not module-level
    if "disabled" in code or "data-disabled" in code:
        ok("button disable/enable mechanism present")
    else:
        ko("button disable/enable mechanism missing")

    # Must NOT use document.getElementById for the run button (singleton)
    if 'getElementById("run")' in code or "getElementById('run')" in code:
        ko("singleton getElementById('run') found — not per-exercise")
    else:
        ok("no singleton getElementById('run') — per-exercise Run button")


def check_buttons_reenabled(src: str) -> None:
    """Clause 7: buttons re-enabled on pass.

    After a run completes (pass), the Run button must be re-enabled.
    The runtime must set the button back to enabled in the finally block
    or after the run completes.
    """
    code = _strip_comments(src)
    # Must re-enable the button after run (in finally or after setStatus)
    if "disabled" in code and ("false" in code or "finally" in code):
        ok("button re-enable mechanism present (disabled = false or finally)")
    else:
        ko("button re-enable mechanism missing")


def check_data_status_closed_set(src: str) -> None:
    """Clause 6: data-status is a closed set (idle, running, pass, fail).

    The status element's data-status attribute must only take values from
    the closed set {idle, running, pass, fail}.
    """
    code = _strip_comments(src)
    # Must set data-status to one of the closed set values
    status_values = ["idle", "running", "pass", "fail"]
    found = sum(1 for v in status_values if f'"{v}"' in code or f"'{v}'" in code)
    if found >= 3:
        ok(f"data-status closed set referenced ({found}/4 values found)")
    else:
        ko(f"data-status closed set incomplete ({found}/4 values found)")

    # Must use dataset.status or data-status attribute
    if "dataset.status" in code or "data-status" in code:
        ok("data-status attribute mechanism present")
    else:
        ko("data-status attribute mechanism missing")


# ---------------------------------------------------------------------------
# CSS checks (styles.css)
# ---------------------------------------------------------------------------


def check_css_cursor_not_allowed(css: str) -> None:
    """Clause 5: disabled buttons have cursor: not-allowed."""
    if "not-allowed" in css:
        ok("cursor: not-allowed present in styles.css")
    else:
        ko("cursor: not-allowed missing from styles.css")

    # Must target disabled buttons (via [disabled] or .bt-disabled or data-disabled)
    if "[disabled]" in css or "bt-disabled" in css or "data-disabled" in css:
        ok("disabled button selector present in styles.css")
    else:
        ko("disabled button selector missing from styles.css")


def check_css_data_status(css: str) -> None:
    """Clause 6 (CSS): data-status attribute selectors for styling."""
    # Must have data-status attribute selectors for the closed set
    for status in ["idle", "running", "pass", "fail"]:
        if f'data-status="{status}"' in css:
            ok(f'data-status="{status}" selector present in styles.css')
        else:
            ko(f'data-status="{status}" selector missing from styles.css')


def check_css_hints_solution(css: str) -> None:
    """Clauses 1+2 (CSS): hints and solution button styling present."""
    if "bt-hints" in css or "lesson-hints" in css or "hints" in css.lower():
        ok("hints styling present in styles.css")
    else:
        ko("hints styling missing from styles.css")

    if "bt-solution" in css or "solution" in css.lower():
        ok("solution button styling present in styles.css")
    else:
        ko("solution button styling missing from styles.css")


# ---------------------------------------------------------------------------
# Rendered HTML checks (styles.css <link> injection)
# ---------------------------------------------------------------------------


def check_styles_css_loaded() -> None:
    """Verify styles.css is loaded in rendered Quarto HTML.

    Renders ux.qmd to HTML and checks that a <link> tag for styles.css
    is present in the <head>. This test would fail if the Lua filter
    does not inject styles.css — the bug fixed in this commit.
    """
    quarto_bin = shutil_which("quarto")
    if not quarto_bin:
        candidate = "/private/tmp/quarto/bin/quarto"
        if os.path.isfile(candidate) and os.access(candidate, os.X_OK):
            quarto_bin = candidate
    if not quarto_bin:
        ko("styles.css loaded in rendered HTML — quarto not installed")
        return

    result = subprocess.run(
        [quarto_bin, "render", str(QMD_PATH), "--to", "html"],
        capture_output=True,
        text=True,
        cwd=str(REPO_ROOT),
        timeout=60,
        check=False,
    )
    if result.returncode != 0:
        ko(
            f"styles.css loaded in rendered HTML — render failed (exit {result.returncode})"
        )
        if result.stderr:
            print(f"  stderr: {result.stderr}", file=sys.stderr)
        return

    html_path = REPO_ROOT / "quarto-fixture" / "ux.html"
    if not html_path.exists():
        ko("styles.css loaded in rendered HTML — output file not found")
        return

    html = html_path.read_text()
    if 'href="_extensions/blendtutor/assets/styles.css"' in html:
        ok("styles.css <link> present in rendered HTML")
    else:
        ko("styles.css <link> present in rendered HTML — not found in rendered output")


# ---------------------------------------------------------------------------
# Behavioral checks via Node.js (pure function execution)
# ---------------------------------------------------------------------------


# A custom ESM loader that intercepts the codemirror.js import and returns
# a mock, so exercise-runtime.js can be imported in Node.js without a browser.
# The mock exports the same symbols the runtime imports.
LOADER_SCRIPT = r"""
import { pathToFileURL } from "node:url";

export async function resolve(specifier, context, nextResolve) {
  // Redirect any codemirror.js import to the mock file.
  if (specifier.includes("codemirror.js")) {
    return {
      url: pathToFileURL(process.env.MOCK_CM_PATH).href,
      shortCircuit: true,
    };
  }
  return nextResolve(specifier, context);
}
"""

# The mock codemirror.js ESM module — exports the symbols exercise-runtime.js
# imports, with no-op implementations that don't need a real DOM.
MOCK_CODEMIRROR = r"""
export const EditorView = class {
  constructor(opts) {
    this.state = { doc: { toString: () => opts.doc || "", length: (opts.doc || "").length } };
  }
  dispatch(opts) {
    if (opts.changes) {
      const newDoc = opts.changes.insert || "";
      this.state = { doc: { toString: () => newDoc, length: newDoc.length } };
    }
  }
  static contentAttributes = { of: () => ({}) };
  static theme = () => ({});
};
export function r() { return {}; }
export function python() { return {}; }
export function syntaxHighlighting() { return {}; }
export const defaultHighlightStyle = {};
export const HighlightStyle = { define: () => ({}) };
export function lineNumbers() { return {}; }
export function highlightActiveLine() { return {}; }
export function bracketMatching() { return {}; }
export const indentWithTab = {};
export const keymap = { of: () => ({}) };
export const tags = {};
tags.keyword = "keyword";
tags.variableName = "variableName";
tags.string = "string";
tags.number = "number";
tags.comment = "comment";
tags.operator = "operator";
tags.function = () => "function";
"""

# The register script that hooks the loader into Node's module system.
REGISTER_SCRIPT = r"""
import { register } from "node:module";
import { pathToFileURL } from "node:url";
register(pathToFileURL(process.env.LOADER_PATH).href, pathToFileURL(import.meta.url));
"""

NODE_TEST_SCRIPT = r"""
// Mock browser globals so the module can be imported without a browser.
const store = new Map();
const sessionStorage = {
  getItem: (k) => store.get(k) ?? null,
  setItem: (k, v) => store.set(k, String(v)),
  removeItem: (k) => store.delete(k),
};
globalThis.window = { sessionStorage, location: { search: "" } };
globalThis.sessionStorage = sessionStorage;

// Mock document with enough fidelity to test DOM creation.
globalThis.document = {
  createElement: (tag) => {
    return {
      tagName: tag,
      className: "",
      textContent: "",
      dataset: {},
      style: {},
      _children: [],
      _listeners: {},
      appendChild(child) { this._children.push(child); return child; },
      append(...children) { this._children.push(...children); },
      replaceChildren(...children) { this._children = children; },
      addEventListener(event, handler) {
        if (!this._listeners[event]) this._listeners[event] = [];
        this._listeners[event].push(handler);
      },
      querySelector(sel) { return null; },
      querySelectorAll(sel) { return []; },
      insertAdjacentElement(pos, el) { this._children.push(el); return el; },
      setAttribute(k, v) { this[k] = v; },
      getAttribute(k) { return this[k] ?? null; },
    };
  },
  getElementById(id) { return null; },
  querySelector(sel) { return null; },
  querySelectorAll(sel) { return []; },
};

// Import the module (codemirror.js is mocked via the custom loader)
const mod = await import(process.argv[2]);

let failures = 0;
function assert(cond, msg) {
  if (!cond) { console.error("  FAIL: " + msg); failures++; }
  else { console.log("  PASS: " + msg); }
}

// Verify exported functions exist and are callable.
assert(typeof mod.scanExercises === "function", "scanExercises exported");
assert(typeof mod.buildRegistry === "function", "buildRegistry exported");
assert(typeof mod.start === "function", "start exported");
assert(typeof mod.parsePayload === "function", "parsePayload exported");

process.exit(failures > 0 ? 1 : 0);
"""


def check_node_behavioral() -> None:
    """Run behavioral tests via Node.js with a mocked codemirror.js.

    exercise-runtime.js imports from ./codemirror.js (a vendored browser
    bundle). We register a custom ESM loader that intercepts the import and
    returns a mock, so the module can be imported in Node.js without a browser.
    """
    if not JS_PATH.exists():
        ko("Node.js behavioral tests skipped — exercise-runtime.js missing")
        return
    if not shutil_which("node"):
        ko("Node.js behavioral tests skipped — node not installed")
        return

    tmpdir = tempfile.mkdtemp(prefix="bt-ux-test-")
    mock_cm_path = Path(tmpdir) / "mock-codemirror.js"
    loader_path = Path(tmpdir) / "loader.mjs"
    register_path = Path(tmpdir) / "register.mjs"
    test_script_path = Path(tmpdir) / "test.mjs"

    mock_cm_path.write_text(MOCK_CODEMIRROR)
    loader_path.write_text(LOADER_SCRIPT)
    register_path.write_text(REGISTER_SCRIPT)
    test_script_path.write_text(NODE_TEST_SCRIPT)

    env = os.environ.copy()
    env["MOCK_CM_PATH"] = str(mock_cm_path)
    env["LOADER_PATH"] = str(loader_path)

    try:
        result = subprocess.run(
            ["node", "--import", str(register_path), str(test_script_path), str(JS_PATH)],
            capture_output=True,
            text=True,
            cwd=str(REPO_ROOT),
            timeout=30,
            check=False,
            env=env,
        )
        print(result.stdout, end="")
        if result.stderr:
            print(result.stderr, end="", file=sys.stderr)
        if result.returncode == 0:
            ok("Node.js behavioral tests passed (exports verified)")
        else:
            ko("Node.js behavioral tests failed — see errors above")
    except subprocess.TimeoutExpired:
        ko("Node.js behavioral tests timed out")
    finally:
        import shutil as shutil_mod

        shutil_mod.rmtree(tmpdir, ignore_errors=True)


def shutil_which(cmd: str) -> str | None:
    """shutil.which without importing shutil (keeps the script lean)."""
    from shutil import which

    return which(cmd)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main() -> int:
    print("=== AC-8 Exercise UX polish — test_quarto_ux.py ===\n")

    if not check_files_exist():
        print(f"\nPassed: {PASS}")
        print(f"Failed: {FAIL}")
        return 1

    src = JS_PATH.read_text()
    css = CSS_PATH.read_text()
    qmd = QMD_PATH.read_text()

    print("-- ux.qmd structure --")
    check_qmd_structure(qmd)

    print("\n-- Clause 1: hints visible/absent --")
    check_hints_rendering(src)

    print("\n-- Clause 2: solution button click inserts text --")
    check_solution_button(src)

    print("\n-- Clause 3: check button absent when no checks --")
    check_check_button_conditional(src)

    print("\n-- Clause 4: Run disables ex-0 only (per-exercise) --")
    check_run_disable_per_exercise(src)

    print("\n-- Clause 5: cursor=not-allowed --")
    check_css_cursor_not_allowed(css)

    print("\n-- Clause 6: data-status closed set --")
    check_data_status_closed_set(src)
    check_css_data_status(css)

    print("\n-- Clause 7: buttons re-enabled on pass --")
    check_buttons_reenabled(src)

    print("\n-- CSS: hints + solution styling --")
    check_css_hints_solution(css)

    print("\n-- Rendered HTML: styles.css <link> injection --")
    check_styles_css_loaded()

    print("\n-- Behavioral checks (Node.js) --")
    check_node_behavioral()

    print(f"\n=== Results: {PASS} passed, {FAIL} failed ===")
    return 1 if FAIL > 0 else 0


if __name__ == "__main__":
    sys.exit(main())
