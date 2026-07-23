#!/usr/bin/env bash
# Executable spec for issue #106 — Quarto extension skeleton.
#
# Verifies the 7-point compound predicate from AC-1:
#   1. _extension.yml contains contributes.filters: [blendtutor.lua]
#   2. blendtutor.lua is a valid Pandoc filter (function Pandoc(doc) return doc end)
#   3. quarto render quarto-fixture/minimal.qmd exits 0
#   4. HTML output contains standard markdown content ("Hello from Quarto")
#   5. HTML output contains blendtutor fenced-div content ("add(a, b)")
#   6. HTML output contains blendtutor fenced-div content ("stopifnot(add(1, 2) == 3)")
#   7. CI uses quarto-dev/quarto-actions/setup@v2 with NO continue-on-error
#
# Negative case: _extension.yml present but omits contributes.filters →
# structurally dead extension, CI green but filter never loads.
# Caught by assertion 1 which grep-checks the exact key.
#
# Usage: bash scripts/tests/test_quarto_render.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PASS=0
FAIL=0

ok() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
ko() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

# ---------------------------------------------------------------------------
# Assertion 1 — _extension.yml contains contributes.filters: [blendtutor.lua]
# ---------------------------------------------------------------------------

echo "== Assertion 1: contributes.filters present in _extension.yml =="

EXT_YML="_extensions/blendtutor/_extension.yml"

if [ ! -f "$EXT_YML" ]; then
  ko "_extension.yml exists — file not found: $EXT_YML"
else
  if grep -qF 'contributes.filters:' "$EXT_YML"; then
    ok "contributes.filters key present"
  else
    ko "contributes.filters key present — key missing in $EXT_YML"
  fi

  if grep -qF 'blendtutor.lua' "$EXT_YML"; then
    ok "blendtutor.lua listed in contributes.filters"
  else
    ko "blendtutor.lua listed in contributes.filters — filter file not referenced"
  fi
fi

# ---------------------------------------------------------------------------
# Assertion 2 — blendtutor.lua is a valid Pandoc filter
# ---------------------------------------------------------------------------

echo "== Assertion 2: blendtutor.lua is valid Pandoc filter =="

LUA_FILTER="_extensions/blendtutor/blendtutor.lua"

if [ ! -f "$LUA_FILTER" ]; then
  ko "blendtutor.lua exists — file not found: $LUA_FILTER"
else
  # A valid no-op Pandoc filter must define function Pandoc(doc) return doc end
  if grep -qF 'function Pandoc(doc)' "$LUA_FILTER" || grep -qE 'function\s+Pandoc\s*\(' "$LUA_FILTER"; then
    ok "blendtutor.lua defines Pandoc function"
  else
    ko "blendtutor.lua defines Pandoc function — function Pandoc(doc) not found"
  fi

  if grep -qE 'return\s+doc' "$LUA_FILTER"; then
    ok "blendtutor.lua returns doc unmodified"
  else
    ko "blendtutor.lua returns doc unmodified — 'return doc' not found"
  fi
fi

# ---------------------------------------------------------------------------
# Assertions 3-6 — quarto render exits 0 and content survives
# ---------------------------------------------------------------------------

echo "== Assertions 3-6: quarto render + content survival =="

FIXTURE="quarto-fixture/minimal.qmd"

if [ ! -f "$FIXTURE" ]; then
  ko "fixture .qmd exists — file not found: $FIXTURE"
  ko "quarto render exits 0 — fixture missing"
  ko "standard content survived — fixture missing"
  ko "blendtutor content (add(a, b)) survived — fixture missing"
  ko "blendtutor content (stopifnot) survived — fixture missing"
else
  if ! command -v quarto &>/dev/null; then
    echo "  SKIP: quarto not installed locally — render assertions skipped"
    echo "  (CI installs quarto via quarto-dev/quarto-actions/setup@v2)"
    ko "quarto render exits 0 — quarto not installed"
    ko "standard content survived — quarto not installed"
    ko "blendtutor content (add(a, b)) survived — quarto not installed"
    ko "blendtutor content (stopifnot) survived — quarto not installed"
  else
    # Clean any previous render output.
    rm -f quarto-fixture/minimal.html

    RENDER_OUTPUT=$(quarto render "$FIXTURE" --to html 2>&1) && RENDER_RC=0 || RENDER_RC=$?

    if [ "$RENDER_RC" -eq 0 ]; then
      ok "quarto render exits 0"
    else
      ko "quarto render exits 0 — exit code $RENDER_RC"
      echo "  render output: $RENDER_OUTPUT" >&2
    fi

    HTML_FILE="quarto-fixture/minimal.html"

    if [ ! -f "$HTML_FILE" ]; then
      ko "standard content survived — HTML output not found: $HTML_FILE"
      ko "blendtutor content (add(a, b)) survived — HTML output not found"
      ko "blendtutor content (stopifnot) survived — HTML output not found"
    else
      HTML_CONTENT=$(cat "$HTML_FILE")

      if echo "$HTML_CONTENT" | grep -qF 'Hello from Quarto'; then
        ok "standard content survived (Hello from Quarto)"
      else
        ko "standard content survived (Hello from Quarto) — not found in HTML"
      fi

      if echo "$HTML_CONTENT" | grep -qF 'add(a, b)'; then
        ok "blendtutor content survived (add(a, b))"
      else
        ko "blendtutor content survived (add(a, b)) — not found in HTML"
      fi

      if echo "$HTML_CONTENT" | grep -qF 'stopifnot(add(1, 2) == 3)'; then
        ok "blendtutor content survived (stopifnot(add(1, 2) == 3))"
      else
        ko "blendtutor content survived (stopifnot(add(1, 2) == 3)) — not found in HTML"
      fi
    fi
  fi
fi

# ---------------------------------------------------------------------------
# Assertion 7 — CI uses quarto-dev/quarto-actions/setup@v2, no continue-on-error
# ---------------------------------------------------------------------------

echo "== Assertion 7: CI setup@v2 + no continue-on-error =="

CI_FILE=".github/workflows/ci.yml"

if [ ! -f "$CI_FILE" ]; then
  ko "CI file exists — not found: $CI_FILE"
else
  if grep -qF 'quarto-dev/quarto-actions/setup@v2' "$CI_FILE"; then
    ok "CI uses quarto-dev/quarto-actions/setup@v2"
  else
    ko "CI uses quarto-dev/quarto-actions/setup@v2 — action not found in CI"
  fi

  # Check that the quarto-render job has no continue-on-error.
  # We look for continue-on-error in the quarto-render job section.
  # A simple heuristic: if continue-on-error appears anywhere after the
  # quarto-render job definition, flag it.
  if grep -q 'continue-on-error' "$CI_FILE"; then
    # Check if it's in the quarto-render job (not the concurrency block at top).
    QUARTO_JOB_LINE=$(grep -n 'quarto-render' "$CI_FILE" | head -1 | cut -d: -f1)
    if [ -n "$QUARTO_JOB_LINE" ]; then
      CONTINUE_LINE=$(grep -n 'continue-on-error' "$CI_FILE" | head -1 | cut -d: -f1)
      if [ -n "$CONTINUE_LINE" ] && [ "$CONTINUE_LINE" -gt "$QUARTO_JOB_LINE" ]; then
        ko "no continue-on-error in quarto-render job — found at line $CONTINUE_LINE"
      else
        ok "no continue-on-error in quarto-render job"
      fi
    else
      ok "no continue-on-error in quarto-render job (job not found yet)"
    fi
  else
    ok "no continue-on-error in quarto-render job"
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
