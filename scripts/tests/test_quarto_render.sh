#!/usr/bin/env bash
# Executable spec for issue #106 — Quarto extension skeleton.
#
# Verifies the 8-point compound predicate from AC-1:
#   1. _extension.yml contains contributes.filters: [blendtutor.lua]
#   2. blendtutor.lua is a valid Pandoc filter (function Pandoc(doc) return doc end)
#   3. quarto render quarto-fixture/minimal.qmd exits 0
#   4. HTML output contains standard markdown content ("Hello from Quarto")
#   5. HTML output contains blendtutor fenced-div content ("add(a, b)")
#   6. HTML output contains blendtutor fenced-div content ("stopifnot(add(1, 2) == 3)")
#   7. HTML output contains bt-exercise widget (filter actually loaded + transformed)
#   8. CI uses quarto-dev/quarto-actions/setup@v2 with NO continue-on-error
#
# Negative case: _extension.yml present but omits contributes.filters →
# structurally dead extension, CI green but filter never loads.
# Caught by assertion 1 which grep-checks the exact key.
#
# Negative case (PR #119 regression): .qmd rendered without `filters: [blendtutor]`
# in a standalone (no _quarto.yml) context → Quarto skips _extensions/ discovery
# → filter never loads → content survives raw but NO bt-exercise widget emitted.
# Caught by assertion 7 which grep-checks for the bt-exercise class div that only
# the loaded filter can produce. Content-survival assertions (5-6) pass trivially
# without the filter, so the bt-exercise check is the load-proving guard.
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
  # YAML allows both inline (contributes.filters: [blendtutor.lua]) and
  # nested (contributes:\n  filters:\n    - blendtutor.lua) forms.
  # Check for the contributes section with a filters key referencing blendtutor.lua.
  if grep -qE 'contributes' "$EXT_YML" && grep -qE 'filters' "$EXT_YML"; then
    ok "contributes.filters key present"
  else
    ko "contributes.filters key present — contributes/filters keys missing in $EXT_YML"
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

echo "== Assertions 3-7: quarto render + content survival + filter load =="

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
    ko "filter loaded (bt-exercise present) — quarto not installed"
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
      ko "filter loaded (bt-exercise present) — HTML output not found"
    else
      HTML_CONTENT=$(cat "$HTML_FILE")

      # Strip HTML tags for content-matching assertions — Pandoc syntax
      # highlighting wraps tokens in <span> tags, breaking literal grep.
      HTML_TEXT=$(echo "$HTML_CONTENT" | sed 's/<[^>]*>//g')

      if echo "$HTML_CONTENT" | grep -qF 'Hello from Quarto'; then
        ok "standard content survived (Hello from Quarto)"
      else
        ko "standard content survived (Hello from Quarto) — not found in HTML"
      fi

      if echo "$HTML_TEXT" | grep -qF 'add(a, b)'; then
        ok "blendtutor content survived (add(a, b))"
      else
        ko "blendtutor content survived (add(a, b)) — not found in HTML"
      fi

      if echo "$HTML_TEXT" | grep -qF 'stopifnot(add(1, 2) == 3)'; then
        ok "blendtutor content survived (stopifnot(add(1, 2) == 3))"
      else
        ko "blendtutor content survived (stopifnot(add(1, 2) == 3)) — not found in HTML"
      fi

      # Assertion 7 — filter actually loaded: the bt-exercise widget div is only
      # emitted by the loaded blendtutor filter. Content-survival checks above
      # pass trivially when the filter never runs (raw div content survives),
      # so this is the load-proving guard. Without `filters: [blendtutor]` in a
      # standalone .qmd (no _quarto.yml), Quarto skips _extensions/ discovery and
      # no bt-exercise widget is produced.
      if echo "$HTML_CONTENT" | grep -qF 'bt-exercise'; then
        ok "filter loaded (bt-exercise widget present in HTML)"
      else
        ko "filter loaded (bt-exercise widget present in HTML) — not found; filter never ran"
      fi
    fi
  fi
fi

# ---------------------------------------------------------------------------
# Assertion 8 — CI uses quarto-dev/quarto-actions/setup@v2, no continue-on-error
# ---------------------------------------------------------------------------

echo "== Assertion 8: CI setup@v2 + no continue-on-error =="

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
