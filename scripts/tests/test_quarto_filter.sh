#!/usr/bin/env bash
# Executable spec for issue #107 — Lua filter parses .blendtutor divs.
#
# Verifies the compound predicate from AC-2:
#   1. HTML render produces >=3 div.bt-exercise with script[type=application/json]
#   2. JSON has all 9 SiteLesson keys, llm_evaluation_prompt ABSENT
#   3. Full exercise: prompt <code>, code_template <-, checks len 2,
#      solution "a + b", hints <-, gotchas null, packages []
#   4. Minimal Python: packages parsed, all 9 keys present
#   5. Empty exercise: all 9 keys present (null/[] for absent)
#   6. IDs distinct, titles non-empty
#   7. Non-HTML format: no bt-exercise + warning
#   8. Invalid language: warning + skip (no bt-exercise)
#   9. Missing language: warning + skip (no bt-exercise)
#
#   Negative: filter emits llm_evaluation_prompt, hardcodes JSON,
#             omits packages/gotchas, non-HTML emits widget,
#             silent default for invalid language.
#   AC-1 (issue #135) additions:
#   10. EVERY bt-exercise div carries data-language="r|python";
#       count === widget count (all-or-none), pairing [r, python, r, r]
#   11. Static pin: exercise-runtime.js reads entry.element.dataset.language
#   12. Static pin: verify_filter_output.py has no exact <div class="bt-exercise"> matcher
#
# Usage: bash scripts/tests/test_quarto_filter.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PASS=0
FAIL=0

ok() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
ko() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

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
  exit 0
fi

echo "Using render tool: $RENDER_TOOL"

# ---------------------------------------------------------------------------
# Structural guard — .qmd YAML declares explicit filter path (pins the fix)
# ---------------------------------------------------------------------------

echo "== Structural guard: .qmd YAML declares explicit filter path =="

# The filter is loaded via explicit file path in each .qmd YAML header:
#   filters: [../_extensions/blendtutor/blendtutor.lua]
# This bypasses Quarto extension discovery entirely. Previous approaches
# (filters: [blendtutor] by name, _quarto.yml extensions: [blendtutor]) failed
# in CI because Quarto never scanned _extensions/ for standalone .qmd documents.
# This guard catches the regression without needing quarto installed.
FILTER_QMD="$FIXTURE_DIR/filter.qmd"

if [ ! -f "$FILTER_QMD" ]; then
  ko "filter.qmd exists — file not found: $FILTER_QMD"
else
  if grep -qF 'filters: [../_extensions/blendtutor/blendtutor.lua]' "$FILTER_QMD"; then
    ok "filter path declared in filter.qmd YAML"
  else
    ko "filter path declared in filter.qmd YAML — filters: [../_extensions/blendtutor/blendtutor.lua] not found"
  fi
fi

# ---------------------------------------------------------------------------
# Render helpers
# ---------------------------------------------------------------------------

render_to_html() {
  local input="$1"
  local output="$2"
  if [ "$RENDER_TOOL" = "quarto" ]; then
    quarto render "$input" --to html 2>&1
  else
    pandoc "$input" --from markdown --to html \
      --lua-filter "$LUA_FILTER" -o "$output" 2>&1
  fi
}

render_to_latex() {
  local input="$1"
  local output="$2"
  if [ "$RENDER_TOOL" = "quarto" ]; then
    quarto render "$input" --to latex 2>&1
  else
    pandoc "$input" --from markdown --to latex \
      --lua-filter "$LUA_FILTER" -o "$output" 2>&1
  fi
}

# ---------------------------------------------------------------------------
# Assertions 1-6: HTML render + JSON contract
# ---------------------------------------------------------------------------

echo "== Assertions 1-6: HTML render + JSON contract =="

HTML_FILE="$FIXTURE_DIR/filter.html"
rm -f "$HTML_FILE"

RENDER_OUTPUT=$(render_to_html "$FIXTURE_DIR/filter.qmd" "$HTML_FILE") && RENDER_RC=0 || RENDER_RC=$?

if [ "$RENDER_RC" -ne 0 ]; then
  ko "render exits 0 — exit code $RENDER_RC"
  echo "  render output: $RENDER_OUTPUT" >&2
else
  ok "render exits 0"
fi

if [ ! -f "$HTML_FILE" ]; then
  ko "HTML output exists — not found: $HTML_FILE"
  ko ">=3 bt-exercise widgets — HTML missing"
  ko "all 9 keys present — HTML missing"
  ko "full exercise values — HTML missing"
  ko "IDs distinct — HTML missing"
else
  # Run Python JSON assertions (covers assertions 1-6)
  python3 scripts/tests/verify_filter_output.py "$HTML_FILE" 2>&1 && PY_RC=0 || PY_RC=$?
  if [ "$PY_RC" -eq 0 ]; then
    ok ">=3 bt-exercise widgets with all 9 keys"
    ok "full exercise values correct (prompt <code>, code_template <-, checks len 2, solution 'a + b', hints <-, gotchas null, packages [])"
    ok "minimal Python: packages parsed, all 9 keys present"
    ok "empty exercise: all 9 keys present (null/[] for absent)"
    ok "IDs distinct, titles non-empty"
    ok "llm_evaluation_prompt ABSENT"
  else
    ko "JSON contract assertions — see errors above"
  fi
fi

# ---------------------------------------------------------------------------
# Assertions 10-11: data-language emission (AC-1 clause 2-3 shell-level check)
# ---------------------------------------------------------------------------

echo "== Assertions 10-11: data-language emission =="

if [ -f "$HTML_FILE" ]; then
  HTML_CONTENT=$(cat "$HTML_FILE")
  if grep -q 'data-language="r"' <<< "$HTML_CONTENT"; then
    ok "data-language=r emitted"
  else
    ko "data-language=r emitted — not found in $HTML_FILE"
  fi

  if grep -q 'data-language="python"' <<< "$HTML_CONTENT"; then
    ok "data-language=python emitted"
  else
    ko "data-language=python emitted — not found in $HTML_FILE"
  fi

  DATA_LANG_COUNT=$(grep -o 'data-language="[rp]"' <<< "$HTML_CONTENT" | wc -l | tr -d ' ' || true)
  if [ "$DATA_LANG_COUNT" -ge 4 ]; then
    ok ">=4 data-language attrs emitted (found $DATA_LANG_COUNT)"
  else
    ko ">=4 data-language attrs emitted — found $DATA_LANG_COUNT"
  fi
else
  ko "data-language=r emitted — HTML missing"
  ko "data-language=python emitted — HTML missing"
  ko ">=4 data-language attrs emitted — HTML missing"
fi

# ---------------------------------------------------------------------------
# Assertions 12-13: static pins (AC-1 clauses 4-5)
# ---------------------------------------------------------------------------

echo "== Assertions 12-13: static pins (runtime read + verifier shape) =="

RUNTIME_JS="_extensions/blendtutor/assets/exercise-runtime.js"
if grep -qF 'entry.element.dataset.language ||' "$RUNTIME_JS"; then
  ok "runtime reads entry.element.dataset.language ||"
else
  ko "runtime reads entry.element.dataset.language || — pattern not found in $RUNTIME_JS"
fi

if grep -qF '<div class="bt-exercise">' scripts/tests/verify_filter_output.py; then
  ko "verifier dropped exact matcher — '<div class=\"bt-exercise\">' still present in verify_filter_output.py"
else
  ok "verifier dropped exact matcher — no <div class=\"bt-exercise\"> in verify_filter_output.py"
fi

# ---------------------------------------------------------------------------
# Assertion 14: Non-HTML format — no bt-exercise + warning
# ---------------------------------------------------------------------------

echo "== Assertion 14: non-HTML format skips widget emission =="

LATEX_FILE="$FIXTURE_DIR/filter.tex"
rm -f "$LATEX_FILE"

LATEX_OUTPUT=$(render_to_latex "$FIXTURE_DIR/filter.qmd" "$LATEX_FILE") && LATEX_RC=0 || LATEX_RC=$?

if [ "$LATEX_RC" -ne 0 ]; then
  ko "non-HTML render exits 0 — exit code $LATEX_RC"
  echo "  render output: $LATEX_OUTPUT" >&2
else
  ok "non-HTML render exits 0"
fi

if [ ! -f "$LATEX_FILE" ]; then
  ko "non-HTML output exists — not found: $LATEX_FILE"
  ko "no bt-exercise in non-HTML — file missing"
  ko "warning emitted for non-HTML — file missing"
else
  LATEX_CONTENT=$(cat "$LATEX_FILE")
  if grep -q 'bt-exercise' <<< "$LATEX_CONTENT"; then
    ko "no bt-exercise in non-HTML — found 'bt-exercise' in latex output"
  else
    ok "no bt-exercise in non-HTML output"
  fi

  if grep -qiE 'WARNING.*(html|format|skip)' <<< "$LATEX_OUTPUT"; then
    ok "warning emitted for non-HTML format"
  else
    ko "warning emitted for non-HTML format — no warning in stderr"
    echo "  stderr: $LATEX_OUTPUT" >&2
  fi
fi

# ---------------------------------------------------------------------------
# Assertion 8: Invalid language — warning + skip
# ---------------------------------------------------------------------------

echo "== Assertion 15: invalid language warning + skip =="

INVALID_HTML="$FIXTURE_DIR/filter-invalid-lang.html"
rm -f "$INVALID_HTML"

INVALID_OUTPUT=$(render_to_html "$FIXTURE_DIR/filter-invalid-lang.qmd" "$INVALID_HTML") && INVALID_RC=0 || INVALID_RC=$?

if [ "$INVALID_RC" -ne 0 ]; then
  ko "invalid-lang render exits 0 — exit code $INVALID_RC"
else
  ok "invalid-lang render exits 0"
fi

if [ ! -f "$INVALID_HTML" ]; then
  ko "invalid-lang HTML exists — not found: $INVALID_HTML"
  ko "no bt-exercise for invalid language — file missing"
  ko "warning for invalid language — file missing"
else
  INVALID_CONTENT=$(cat "$INVALID_HTML")
  if grep -q 'bt-exercise' <<< "$INVALID_CONTENT"; then
    ko "no bt-exercise for invalid language — found 'bt-exercise'"
  else
    ok "no bt-exercise for invalid language"
  fi

  if grep -qiE 'WARNING.*(ruby|unsupported|skip)' <<< "$INVALID_OUTPUT"; then
    ok "warning for invalid language"
  else
    ko "warning for invalid language — no warning in stderr"
    echo "  stderr: $INVALID_OUTPUT" >&2
  fi
fi

# ---------------------------------------------------------------------------
# Assertion 9: Missing language — warning + skip
# ---------------------------------------------------------------------------

echo "== Assertion 16: missing language warning + skip =="

MISSING_HTML="$FIXTURE_DIR/filter-missing-lang.html"
rm -f "$MISSING_HTML"

MISSING_OUTPUT=$(render_to_html "$FIXTURE_DIR/filter-missing-lang.qmd" "$MISSING_HTML") && MISSING_RC=0 || MISSING_RC=$?

if [ "$MISSING_RC" -ne 0 ]; then
  ko "missing-lang render exits 0 — exit code $MISSING_RC"
else
  ok "missing-lang render exits 0"
fi

if [ ! -f "$MISSING_HTML" ]; then
  ko "missing-lang HTML exists — not found: $MISSING_HTML"
  ko "no bt-exercise for missing language — file missing"
  ko "warning for missing language — file missing"
else
  MISSING_CONTENT=$(cat "$MISSING_HTML")
  if grep -q 'bt-exercise' <<< "$MISSING_CONTENT"; then
    ko "no bt-exercise for missing language — found 'bt-exercise'"
  else
    ok "no bt-exercise for missing language"
  fi

  if grep -qiE 'WARNING.*(missing|skip)' <<< "$MISSING_OUTPUT"; then
    ok "warning for missing language"
  else
    ko "warning for missing language — no warning in stderr"
    echo "  stderr: $MISSING_OUTPUT" >&2
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
