#!/usr/bin/env bash
# Executable spec for issue #114 — Opt-in cross-origin isolation via coi='true'.
#
# Verifies the 7-clause predicate from AC-9:
#   1. coi="true" activates — script tag for coi-serviceworker.js present in HTML
#   2. false/yes/empty rejected — no script tag for coi="false", coi="yes", coi=""
#   3. dedup — exactly one script tag with multiple coi="true" divs
#   4. byte-parity — vendored coi-serviceworker.js byte-identical to source
#   5. per-page isolation — each page gets correct COI state (no cross-page leak)
#   6. no R exercises still registers — coi="true" without R exercises activates
#   7. coi-yaml — document-level YAML coi: true activates
#
# Negative: Script src 404s (file missing at referenced path).
#           Filter hardcodes script tag (injects regardless of coi attribute).
#
# Usage: bash scripts/tests/test_coi_filter.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PASS=0
FAIL=0

ok() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
ko() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

LUA_FILTER="_extensions/blendtutor/blendtutor.lua"
FIXTURE_DIR="quarto-fixture"
COI_SCRIPT_SRC="coi-serviceworker.js"
COI_DEST="_extensions/blendtutor/assets/coi-serviceworker.js"
COI_SOURCE="crates/core/assets/shared/coi-serviceworker.js"

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

# Count occurrences of coi-serviceworker.js script tag in HTML content.
# Uses grep -c which returns count (0 = not found, with || true for pipefail).
count_coi_scripts() {
  local content="$1"
  printf '%s' "$content" | grep -c 'coi-serviceworker.js' || true
}

# Check if coi-serviceworker.js script tag is present.
has_coi_script() {
  local content="$1"
  printf '%s' "$content" | grep -qF "$COI_SCRIPT_SRC"
}

# ---------------------------------------------------------------------------
# Clause 1: coi="true" activates
# ---------------------------------------------------------------------------

echo "== Clause 1: coi=\"true\" activates =="

COI_TRUE_HTML="$FIXTURE_DIR/coi-true.html"
rm -f "$COI_TRUE_HTML"

COI_TRUE_OUTPUT=$(render_to_html "$FIXTURE_DIR/coi-true.qmd" "$COI_TRUE_HTML") && COI_TRUE_RC=0 || COI_TRUE_RC=$?

if [ "$COI_TRUE_RC" -ne 0 ]; then
  ko "coi-true.qmd render exits 0 — exit code $COI_TRUE_RC"
  echo "  render output: $COI_TRUE_OUTPUT" >&2
else
  ok "coi-true.qmd render exits 0"
fi

if [ ! -f "$COI_TRUE_HTML" ]; then
  ko "coi-true.html exists — not found"
  ko "coi=\"true\" activates — HTML missing"
else
  COI_TRUE_CONTENT=$(cat "$COI_TRUE_HTML")
  if has_coi_script "$COI_TRUE_CONTENT"; then
    ok "coi=\"true\" activates — script tag present"
  else
    ko "coi=\"true\" activates — script tag NOT found in HTML"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 2: false/yes/empty rejected
# ---------------------------------------------------------------------------

echo "== Clause 2: false/yes/empty rejected =="

# coi="false"
COI_FALSE_HTML="$FIXTURE_DIR/coi-false.html"
rm -f "$COI_FALSE_HTML"
render_to_html "$FIXTURE_DIR/coi-false.qmd" "$COI_FALSE_HTML" >/dev/null 2>&1 || true

if [ ! -f "$COI_FALSE_HTML" ]; then
  ko "coi=\"false\" rejected — HTML missing"
else
  COI_FALSE_CONTENT=$(cat "$COI_FALSE_HTML")
  if has_coi_script "$COI_FALSE_CONTENT"; then
    ko "coi=\"false\" rejected — script tag found (should be absent)"
  else
    ok "coi=\"false\" rejected — no script tag"
  fi
fi

# coi="yes"
COI_YES_HTML="$FIXTURE_DIR/coi-yes.html"
rm -f "$COI_YES_HTML"
render_to_html "$FIXTURE_DIR/coi-yes.qmd" "$COI_YES_HTML" >/dev/null 2>&1 || true

if [ ! -f "$COI_YES_HTML" ]; then
  ko "coi=\"yes\" rejected — HTML missing"
else
  COI_YES_CONTENT=$(cat "$COI_YES_HTML")
  if has_coi_script "$COI_YES_CONTENT"; then
    ko "coi=\"yes\" rejected — script tag found (should be absent)"
  else
    ok "coi=\"yes\" rejected — no script tag"
  fi
fi

# coi=""
COI_EMPTY_HTML="$FIXTURE_DIR/coi-empty.html"
rm -f "$COI_EMPTY_HTML"
render_to_html "$FIXTURE_DIR/coi-empty.qmd" "$COI_EMPTY_HTML" >/dev/null 2>&1 || true

if [ ! -f "$COI_EMPTY_HTML" ]; then
  ko "coi=\"\" rejected — HTML missing"
else
  COI_EMPTY_CONTENT=$(cat "$COI_EMPTY_HTML")
  if has_coi_script "$COI_EMPTY_CONTENT"; then
    ko "coi=\"\" rejected — script tag found (should be absent)"
  else
    ok "coi=\"\" rejected — no script tag"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 3: dedup — exactly one script tag with multiple coi="true" divs
# ---------------------------------------------------------------------------

echo "== Clause 3: dedup (one script tag for multiple coi=\"true\" divs) =="

COI_DOUBLE_HTML="$FIXTURE_DIR/coi-double.html"
rm -f "$COI_DOUBLE_HTML"
render_to_html "$FIXTURE_DIR/coi-double.qmd" "$COI_DOUBLE_HTML" >/dev/null 2>&1 || true

if [ ! -f "$COI_DOUBLE_HTML" ]; then
  ko "dedup — HTML missing"
  ko "dedup count — HTML missing"
else
  COI_DOUBLE_CONTENT=$(cat "$COI_DOUBLE_HTML")
  COI_DOUBLE_COUNT=$(count_coi_scripts "$COI_DOUBLE_CONTENT")
  if [ "$COI_DOUBLE_COUNT" -eq 1 ]; then
    ok "dedup — exactly one script tag ($COI_DOUBLE_COUNT found)"
  else
    ko "dedup — expected 1 script tag, found $COI_DOUBLE_COUNT"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 4: byte-parity — vendored coi-serviceworker.js byte-identical to source
# ---------------------------------------------------------------------------

echo "== Clause 4: byte-parity (vendored == source) =="

if [ -f "$COI_SOURCE" ] && [ -f "$COI_DEST" ]; then
  if cmp -s "$COI_SOURCE" "$COI_DEST"; then
    ok "byte-parity — vendored coi-serviceworker.js byte-identical to source"
  else
    ko "byte-parity — vendored coi-serviceworker.js differs from source"
  fi
else
  if [ ! -f "$COI_SOURCE" ]; then
    ko "byte-parity — source file missing: $COI_SOURCE"
  fi
  if [ ! -f "$COI_DEST" ]; then
    ko "byte-parity — vendored file missing: $COI_DEST"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 5: per-page isolation — each page gets correct COI state
# ---------------------------------------------------------------------------

echo "== Clause 5: per-page isolation =="

# Render chapter-coi.qmd (has coi="true") — script tag should be present.
COI_BOOK_COI_HTML="$FIXTURE_DIR/coi-book/chapter-coi.html"
rm -f "$COI_BOOK_COI_HTML"
render_to_html "$FIXTURE_DIR/coi-book/chapter-coi.qmd" "$COI_BOOK_COI_HTML" >/dev/null 2>&1 || true

if [ ! -f "$COI_BOOK_COI_HTML" ]; then
  ko "per-page isolation (coi chapter) — HTML missing"
else
  COI_BOOK_COI_CONTENT=$(cat "$COI_BOOK_COI_HTML")
  if has_coi_script "$COI_BOOK_COI_CONTENT"; then
    ok "per-page isolation — coi chapter has script tag"
  else
    ko "per-page isolation — coi chapter missing script tag"
  fi
fi

# Render chapter-no-coi.qmd (no coi) — script tag should be ABSENT.
COI_BOOK_NOCOI_HTML="$FIXTURE_DIR/coi-book/chapter-no-coi.html"
rm -f "$COI_BOOK_NOCOI_HTML"
render_to_html "$FIXTURE_DIR/coi-book/chapter-no-coi.qmd" "$COI_BOOK_NOCOI_HTML" >/dev/null 2>&1 || true

if [ ! -f "$COI_BOOK_NOCOI_HTML" ]; then
  ko "per-page isolation (no-coi chapter) — HTML missing"
else
  COI_BOOK_NOCOI_CONTENT=$(cat "$COI_BOOK_NOCOI_HTML")
  if has_coi_script "$COI_BOOK_NOCOI_CONTENT"; then
    ko "per-page isolation — no-coi chapter has script tag (should be absent)"
  else
    ok "per-page isolation — no-coi chapter has no script tag"
  fi
fi

# Render index.qmd (no coi) — script tag should be ABSENT.
COI_BOOK_INDEX_HTML="$FIXTURE_DIR/coi-book/index.html"
rm -f "$COI_BOOK_INDEX_HTML"
render_to_html "$FIXTURE_DIR/coi-book/index.qmd" "$COI_BOOK_INDEX_HTML" >/dev/null 2>&1 || true

if [ ! -f "$COI_BOOK_INDEX_HTML" ]; then
  ko "per-page isolation (index) — HTML missing"
else
  COI_BOOK_INDEX_CONTENT=$(cat "$COI_BOOK_INDEX_HTML")
  if has_coi_script "$COI_BOOK_INDEX_CONTENT"; then
    ko "per-page isolation — index has script tag (should be absent)"
  else
    ok "per-page isolation — index has no script tag"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 6: no R exercises still registers
# ---------------------------------------------------------------------------

echo "== Clause 6: no R exercises still registers =="

COI_PYTHON_HTML="$FIXTURE_DIR/coi-python-only.html"
rm -f "$COI_PYTHON_HTML"
render_to_html "$FIXTURE_DIR/coi-python-only.qmd" "$COI_PYTHON_HTML" >/dev/null 2>&1 || true

if [ ! -f "$COI_PYTHON_HTML" ]; then
  ko "no R exercises still registers — HTML missing"
else
  COI_PYTHON_CONTENT=$(cat "$COI_PYTHON_HTML")
  if has_coi_script "$COI_PYTHON_CONTENT"; then
    ok "no R exercises still registers — coi script tag present"
  else
    ko "no R exercises still registers — coi script tag NOT found"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 7: coi-yaml — document-level YAML coi: true activates
# ---------------------------------------------------------------------------

echo "== Clause 7: coi-yaml (document-level YAML activates) =="

COI_YAML_HTML="$FIXTURE_DIR/coi-yaml.html"
rm -f "$COI_YAML_HTML"
render_to_html "$FIXTURE_DIR/coi-yaml.qmd" "$COI_YAML_HTML" >/dev/null 2>&1 || true

if [ ! -f "$COI_YAML_HTML" ]; then
  ko "coi-yaml — HTML missing"
else
  COI_YAML_CONTENT=$(cat "$COI_YAML_HTML")
  if has_coi_script "$COI_YAML_CONTENT"; then
    ok "coi-yaml — script tag present (YAML coi: true activates)"
  else
    ko "coi-yaml — script tag NOT found (YAML coi: true should activate)"
  fi
fi

# ---------------------------------------------------------------------------
# Negative: Script src 404s — file must exist at referenced path
# ---------------------------------------------------------------------------

echo "== Negative: script src does NOT 404 =="

if [ -f "$COI_DEST" ]; then
  ok "script src resolves — coi-serviceworker.js exists at $COI_DEST"
else
  ko "script src resolves — coi-serviceworker.js NOT found at $COI_DEST (would 404)"
fi

# ---------------------------------------------------------------------------
# Negative: filter does NOT hardcode script tag (absent when no coi)
# ---------------------------------------------------------------------------

echo "== Negative: filter does NOT hardcode script tag =="

# Render r-only.qmd (no coi attribute at all) — script tag should be ABSENT.
R_ONLY_HTML="$FIXTURE_DIR/r-only.html"
rm -f "$R_ONLY_HTML"
render_to_html "$FIXTURE_DIR/r-only.qmd" "$R_ONLY_HTML" >/dev/null 2>&1 || true

if [ ! -f "$R_ONLY_HTML" ]; then
  ko "no hardcode — r-only HTML missing"
else
  R_ONLY_CONTENT=$(cat "$R_ONLY_HTML")
  if has_coi_script "$R_ONLY_CONTENT"; then
    ko "no hardcode — script tag found in r-only.qmd (filter hardcodes)"
  else
    ok "no hardcode — no script tag when coi attribute absent"
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
