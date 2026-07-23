#!/usr/bin/env bash
# Executable spec for issue #111 — Pyodide shared-boot adapter.
#
# Verifies the 7-clause predicate from AC-6:
#   1. CDN injection — exactly one pinned pyodide.js script tag
#   2. Single boot — loadPyodide called once (idempotent)
#   3. Lazy trigger — no WASM before first Run
#   4. Per-run fresh globals — exercise 2 can't see exercise 1's variables
#   5. Error surface — boot fail, Python error, package fail, loadPyodide undefined
#   6. PyProxy cleanup — namespace.destroy() in finally
#   7. Coexistence with webR — independent, zero shared state
#
# Negative: boot() returns Promise.resolve() without loading. run()
# short-circuits to {ok:true} without evaluating code. Second exercise
# passes → shared globals leaked.
#
# Usage: bash scripts/tests/test_pyodide_adapter.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PASS=0
FAIL=0

ok() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
ko() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

# ---------------------------------------------------------------------------
# Part 1: Node.js validation (structure + pure function checks)
# ---------------------------------------------------------------------------

echo "== Part 1: Node.js validation =="

if ! command -v node &>/dev/null; then
  echo "SKIP: node not installed — Node.js validation skipped"
  ko "Node.js validation — node not installed"
else
  node scripts/tests/validate-pyodide-adapter.js 2>&1 && NODE_RC=0 || NODE_RC=$?
  if [ "$NODE_RC" -eq 0 ]; then
    ok "Node.js validation passed"
  else
    ko "Node.js validation — see errors above"
  fi
fi

# ---------------------------------------------------------------------------
# Part 2: Quarto render — pyodide.qmd produces bt-exercise widgets + CDN script
# ---------------------------------------------------------------------------

echo "== Part 2: Quarto render pyodide.qmd =="

LUA_FILTER="_extensions/blendtutor/blendtutor.lua"
FIXTURE_DIR="quarto-fixture"

# Detect rendering tool — quarto (CI) or pandoc (local dev fallback).
RENDER_TOOL=""
if command -v quarto &>/dev/null; then
  RENDER_TOOL="quarto"
elif command -v pandoc &>/dev/null; then
  RENDER_TOOL="pandoc"
fi

if [ -z "$RENDER_TOOL" ]; then
  echo "SKIP: neither quarto nor pandoc installed"
  echo "  (CI installs quarto via quarto-dev/quarto-actions/setup@v2)"
  ko "Quarto render — no render tool installed"
else
  echo "Using render tool: $RENDER_TOOL"

  render_to_html() {
    local input="$1"
    local output="$2"
    if [ "$RENDER_TOOL" = "quarto" ]; then
      quarto render "$input" --to html -o "$output" 2>&1
    else
      pandoc "$input" --from markdown --to html \
        --lua-filter "$LUA_FILTER" -o "$output" 2>&1
    fi
  }

  # Render pyodide.qmd
  PYODIDE_HTML="$FIXTURE_DIR/pyodide.html"
  rm -f "$PYODIDE_HTML"

  RENDER_OUTPUT=$(render_to_html "$FIXTURE_DIR/pyodide.qmd" "$PYODIDE_HTML") && RENDER_RC=0 || RENDER_RC=$?

  if [ "$RENDER_RC" -ne 0 ]; then
    ko "pyodide.qmd render exits 0 — exit code $RENDER_RC"
    echo "  render output: $RENDER_OUTPUT" >&2
  else
    ok "pyodide.qmd render exits 0"
  fi

  if [ ! -f "$PYODIDE_HTML" ]; then
    ko "pyodide.html exists — not found: $PYODIDE_HTML"
    ko ">=2 bt-exercise widgets — HTML missing"
    ko "CDN script tag injected — HTML missing"
  else
    HTML_CONTENT=$(cat "$PYODIDE_HTML")

    # Check for >=2 bt-exercise widgets
    EXERCISE_COUNT=$(printf '%s' "$HTML_CONTENT" | grep -c 'class="bt-exercise"' || true)
    if [ "$EXERCISE_COUNT" -ge 2 ]; then
      ok ">=2 bt-exercise widgets in pyodide.qmd ($EXERCISE_COUNT found)"
    else
      ko ">=2 bt-exercise widgets — found $EXERCISE_COUNT"
    fi

    # Check for CDN script tag injection (use printf to avoid pipefail SIGPIPE)
    if printf '%s' "$HTML_CONTENT" | grep -qF 'cdn.jsdelivr.net/pyodide/v0.27.2/full/pyodide.js'; then
      ok "CDN script tag injected for Python exercises"
    else
      ko "CDN script tag injected — pyodide.js CDN URL not found in HTML"
    fi

    # Check exactly one CDN script tag
    CDN_COUNT=$(printf '%s' "$HTML_CONTENT" | grep -c 'cdn.jsdelivr.net/pyodide/v0.27.2/full/pyodide.js' || true)
    if [ "$CDN_COUNT" -eq 1 ]; then
      ok "exactly one pyodide.js CDN script tag"
    else
      ko "exactly one pyodide.js CDN script tag — found $CDN_COUNT"
    fi
  fi

  # Render mixed-lang.qmd
  MIXED_HTML="$FIXTURE_DIR/mixed-lang.html"
  rm -f "$MIXED_HTML"

  MIXED_OUTPUT=$(render_to_html "$FIXTURE_DIR/mixed-lang.qmd" "$MIXED_HTML") && MIXED_RC=0 || MIXED_RC=$?

  if [ "$MIXED_RC" -ne 0 ]; then
    ko "mixed-lang.qmd render exits 0 — exit code $MIXED_RC"
    echo "  render output: $MIXED_OUTPUT" >&2
  else
    ok "mixed-lang.qmd render exits 0"
  fi

  if [ ! -f "$MIXED_HTML" ]; then
    ko "mixed-lang.html exists — not found: $MIXED_HTML"
    ko ">=2 bt-exercise widgets (R+Python) — HTML missing"
  else
    MIXED_CONTENT=$(cat "$MIXED_HTML")

    # Check for >=2 bt-exercise widgets
    MIXED_EXERCISE_COUNT=$(printf '%s' "$MIXED_CONTENT" | grep -c 'class="bt-exercise"' || true)
    if [ "$MIXED_EXERCISE_COUNT" -ge 2 ]; then
      ok ">=2 bt-exercise widgets in mixed-lang.qmd ($MIXED_EXERCISE_COUNT found)"
    else
      ko ">=2 bt-exercise widgets in mixed-lang.qmd — found $MIXED_EXERCISE_COUNT"
    fi

    # Check for CDN script tag (Python exercise present)
    if printf '%s' "$MIXED_CONTENT" | grep -qF 'cdn.jsdelivr.net/pyodide/v0.27.2/full/pyodide.js'; then
      ok "CDN script tag injected for mixed-lang (Python present)"
    else
      ko "CDN script tag injected for mixed-lang — pyodide.js CDN URL not found"
    fi
  fi
fi

# ---------------------------------------------------------------------------
# Part 3: Negative case — no CDN injection for R-only exercises
# ---------------------------------------------------------------------------

echo "== Part 3: Negative case — no CDN for R-only =="

if [ -z "$RENDER_TOOL" ]; then
  echo "SKIP: no render tool — negative case skipped"
  ko "Negative case — no render tool"
else
  # Render r-only.qmd — R-only fixture with NO Python exercises.
  # CDN must NOT be injected when no Python exercise is present.
  R_ONLY_HTML="$FIXTURE_DIR/r-only.html"
  rm -f "$R_ONLY_HTML"

  R_ONLY_OUTPUT=$(render_to_html "$FIXTURE_DIR/r-only.qmd" "$R_ONLY_HTML") && R_ONLY_RC=0 || R_ONLY_RC=$?

  if [ "$R_ONLY_RC" -ne 0 ]; then
    ko "r-only.qmd render exits 0 — exit code $R_ONLY_RC"
    echo "  render output: $R_ONLY_OUTPUT" >&2
  else
    ok "r-only.qmd render exits 0"
  fi

  if [ ! -f "$R_ONLY_HTML" ]; then
    ko "r-only.html exists — not found: $R_ONLY_HTML"
    ko ">=2 bt-exercise widgets (R-only) — HTML missing"
    ko "CDN NOT injected for R-only — HTML missing"
  else
    R_ONLY_CONTENT=$(cat "$R_ONLY_HTML")

    # Check for >=2 bt-exercise widgets
    R_ONLY_EXERCISE_COUNT=$(printf '%s' "$R_ONLY_CONTENT" | grep -c 'class="bt-exercise"' || true)
    if [ "$R_ONLY_EXERCISE_COUNT" -ge 2 ]; then
      ok ">=2 bt-exercise widgets in r-only.qmd ($R_ONLY_EXERCISE_COUNT found)"
    else
      ko ">=2 bt-exercise widgets in r-only.qmd — found $R_ONLY_EXERCISE_COUNT"
    fi

    # NEGATIVE CASE: CDN must NOT be injected when no Python exercise is present.
    # This is the key negative test — if the filter injects CDN for R-only
    # exercises, the has_python flag is broken.
    if printf '%s' "$R_ONLY_CONTENT" | grep -qF 'cdn.jsdelivr.net/pyodide/v0.27.2/full/pyodide.js'; then
      ko "CDN NOT injected for R-only — pyodide.js CDN URL found in R-only HTML (should be absent)"
    else
      ok "CDN NOT injected for R-only — pyodide.js CDN URL absent from HTML"
    fi
  fi
fi

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

echo ""
echo "========================================="
echo "  Results: $PASS passed, $FAIL failed"
echo "========================================="

if [ "$FAIL" -gt 0 ]; then
  exit 1
fi

echo "All tests passed."
