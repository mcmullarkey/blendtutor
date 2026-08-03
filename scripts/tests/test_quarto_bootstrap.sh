#!/usr/bin/env bash
# Executable spec for issue #139 — Filter injects auto-bootstrap module script
# with per-language adapters + YAML opt-out (filter-runtime-bootstrap AC-3).
#
# Verifies the 9-clause predicate from AC-3 (clauses 1-7 + 9 here; clause 8 is
# a rodney runtime probe in rodney-probes/auto-bootstrap.js):
#   1. Bootstrap emitted once (mixed): quarto-fixture/mixed-lang.qmd → exactly
#      one <script type="module" data-bt-bootstrap="auto"> containing start(,
#      scanExercises, buildRegistry, createWebRAdapter, AND pyodideAdapter.
#      type="module" mandatory.
#   2. Per-language conditional imports: r-only.qmd → createWebRAdapter, NOT
#      pyodideAdapter; pyodide.qmd → inverse. Specifiers iff has_r/has_python.
#   3. start() wiring: calls start(buildRegistry(scanExercises()), <map>) with
#      keys {r, python}. No exercises → no bootstrap.
#   4. Opt-out suppresses entirely: webr.qmd with YAML bt-auto-bootstrap: false
#      → ZERO data-bt-bootstrap="auto"; hand-written bootstrap preserved.
#      NOT double-start-guard reliance.
#   5. Non-HTML gate: filter.qmd → latex → zero data-bt-bootstrap="auto".
#   6. Libs-URL specifiers + coi depth (AC-4 rewrite): bootstrap import
#      specifiers reference <stem>_files/libs/quarto-contrib/blendtutor-0.1.0/
#      (computed from quarto.doc.output_file), never _extensions/ source-tree
#      paths; coi-book/chapter-coi.qmd shows the coi-serviceworker.js src STILL
#      depth-correct ../.. _extensions/ (COI stays include_text — SW scope).
#   7. Error sink: script contains .catch( + console.error.
#   9. Static pins: blendtutor.lua has has_r = true in Div() (mirror has_python),
#      hasBootstrapDone guard (mirror hasCoiDone), bt-auto-bootstrap YAML read
#      in Pandoc() (mirror coi read).
#
# Negative cases killed here: classic script no type=module (1), hardcodes both
# adapters (2), opt-out via guard (4), hardcoded specifier (6), omits .catch (7),
# no has_r flag (9).
#
# Usage: bash scripts/tests/test_quarto_bootstrap.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PASS=0
FAIL=0

ok() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
ko() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

LUA_FILTER="_extensions/blendtutor/blendtutor.lua"
FIXTURE_DIR="quarto-fixture"
MARKER='data-bt-bootstrap="auto"'

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
# Render + assertion helpers
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

# Count occurrences of the filter-injected bootstrap marker.
count_bootstrap() {
  local content="$1"
  printf '%s' "$content" | grep -o "$MARKER" | wc -l | tr -d ' ' || true
}

has_token() {
  local content="$1"
  local token="$2"
  printf '%s' "$content" | grep -qF "$token"
}

# Extract the body of the filter-injected bootstrap script (between the marker
# opening tag and its </script>). Empty if no bootstrap present.
extract_bootstrap() {
  local content="$1"
  awk -v marker="<script type=\"module\" data-bt-bootstrap=\"auto\">" '
    index($0, marker) { flag = 1; next }
    flag && index($0, "</script>") { flag = 0; next }
    flag { print }
  ' <<< "$content"
}

# Extract the body of the page-owned hand-written bootstrap (the FIRST
# <script type="module"> that has no filter marker). The hand-written script
# is the one with the import of scanExercises; the filter-injected one always
# carries data-bt-bootstrap="auto" and is excluded by the marker prefix.
extract_hand_script() {
  local content="$1"
  awk '
    /<script type="module"[^>]*>/ && !index($0, "data-bt-bootstrap=\"auto\"") {
      flag = 1
      sub(/^.*<script type="module"[^>]*>/, "")
      print
      next
    }
    flag && /<\/script>/ {
      sub(/<\/script>.*$/, "")
      print
      flag = 0
      next
    }
    flag { print }
  ' <<< "$content"
}

# ---------------------------------------------------------------------------
# Clause 1: Bootstrap emitted once (mixed)
# ---------------------------------------------------------------------------

echo "== Clause 1: bootstrap emitted once (mixed-lang.qmd) =="

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
  ko "exactly one bootstrap — HTML missing"
  ko "type=module — HTML missing"
  ko "bootstrap body tokens — HTML missing"
else
  MIXED_CONTENT=$(cat "$MIXED_HTML")
  MIXED_BOOTSTRAP_COUNT=$(count_bootstrap "$MIXED_CONTENT")
  if [ "$MIXED_BOOTSTRAP_COUNT" -eq 1 ]; then
    ok "exactly one bootstrap script ($MIXED_BOOTSTRAP_COUNT found)"
  else
    ko "exactly one bootstrap script — expected 1, found $MIXED_BOOTSTRAP_COUNT"
  fi

  if grep -qF '<script type="module" data-bt-bootstrap="auto">' <<< "$MIXED_CONTENT"; then
    ok "type=\"module\" mandatory — present"
  else
    ko "type=\"module\" mandatory — opening tag missing or classic script"
  fi

  MIXED_BOOTSTRAP=$(extract_bootstrap "$MIXED_CONTENT")
  for token in "start(" "scanExercises" "buildRegistry" "createWebRAdapter" "pyodideAdapter"; do
    if has_token "$MIXED_BOOTSTRAP" "$token"; then
      ok "bootstrap body contains $token"
    else
      ko "bootstrap body contains $token — not found"
    fi
  done
fi

# ---------------------------------------------------------------------------
# Clause 2: Per-language conditional imports
# ---------------------------------------------------------------------------

echo "== Clause 2: per-language conditional imports =="

R_ONLY_HTML="$FIXTURE_DIR/r-only.html"
rm -f "$R_ONLY_HTML"
render_to_html "$FIXTURE_DIR/r-only.qmd" "$R_ONLY_HTML" >/dev/null 2>&1 || true

if [ ! -f "$R_ONLY_HTML" ]; then
  ko "r-only conditional imports — HTML missing"
  ko "r-only pyodideAdapter absent — HTML missing"
else
  R_ONLY_BOOTSTRAP=$(extract_bootstrap "$(cat "$R_ONLY_HTML")")
  if has_token "$R_ONLY_BOOTSTRAP" "createWebRAdapter"; then
    ok "r-only: createWebRAdapter imported (has_r)"
  else
    ko "r-only: createWebRAdapter imported — not found"
  fi
  if has_token "$R_ONLY_BOOTSTRAP" "pyodideAdapter"; then
    ko "r-only: pyodideAdapter ABSENT — imported despite no python"
  else
    ok "r-only: pyodideAdapter absent"
  fi
fi

PYODIDE_HTML="$FIXTURE_DIR/pyodide.html"
rm -f "$PYODIDE_HTML"
render_to_html "$FIXTURE_DIR/pyodide.qmd" "$PYODIDE_HTML" >/dev/null 2>&1 || true

if [ ! -f "$PYODIDE_HTML" ]; then
  ko "pyodide conditional imports — HTML missing"
  ko "pyodide createWebRAdapter absent — HTML missing"
else
  PYODIDE_BOOTSTRAP=$(extract_bootstrap "$(cat "$PYODIDE_HTML")")
  if has_token "$PYODIDE_BOOTSTRAP" "pyodideAdapter"; then
    ok "pyodide: pyodideAdapter imported (has_python)"
  else
    ko "pyodide: pyodideAdapter imported — not found"
  fi
  if has_token "$PYODIDE_BOOTSTRAP" "createWebRAdapter"; then
    ko "pyodide: createWebRAdapter ABSENT — imported despite no r"
  else
    ok "pyodide: createWebRAdapter absent"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 3: start() wiring + no-exercises gate
# ---------------------------------------------------------------------------

echo "== Clause 3: start() wiring + no-exercises gate =="

if [ -f "$MIXED_HTML" ]; then
  if has_token "$MIXED_BOOTSTRAP" "start(buildRegistry(scanExercises())"; then
    ok "calls start(buildRegistry(scanExercises()), <map>)"
  else
    ko "calls start(buildRegistry(scanExercises()), <map>) — pattern not found"
  fi
  if has_token "$MIXED_BOOTSTRAP" "r: createWebRAdapter()"; then
    ok "adapter map has key r (factory called)"
  else
    ko "adapter map has key r — not found"
  fi
  if has_token "$MIXED_BOOTSTRAP" "python: pyodideAdapter"; then
    ok "adapter map has key python (singleton used directly)"
  else
    ko "adapter map has key python — not found"
  fi
fi

# No exercises → no bootstrap. coi-book/chapter-no-coi.qmd has no blendtutor
# divs (filters path is the only "blendtutor" occurrence).
NOEX_HTML="$FIXTURE_DIR/coi-book/chapter-no-coi.html"
rm -f "$NOEX_HTML"
render_to_html "$FIXTURE_DIR/coi-book/chapter-no-coi.qmd" "$NOEX_HTML" >/dev/null 2>&1 || true

if [ ! -f "$NOEX_HTML" ]; then
  ko "no-exercises gate — HTML missing"
else
  NOEX_COUNT=$(count_bootstrap "$(cat "$NOEX_HTML")")
  if [ "$NOEX_COUNT" -eq 0 ]; then
    ok "no exercises → no bootstrap (0 found)"
  else
    ko "no exercises → no bootstrap — found $NOEX_COUNT"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 4: Opt-out suppresses entirely (ux/webr/feedback + fixture YAML pins)
#
# AC-6 (issue #144): generalizes the former webr-only render block to ALL
# THREE hand-written-bootstrap fixtures — closes the webr-only render gap
# (ux/feedback were YAML-side verified only). Per fixture:
#   - zero filter-injected data-bt-bootstrap="auto" markers (clause 1)
#   - exactly one scanExercises module script — the hand-written bootstrap
#     (clause 2)
#   - hand-written specifiers preserved as ../_extensions/... source-tree
#     paths, NOT libs-dir URLs (clause 3) — static grep is authoritative
#     (ensureAssetSymlink masks specifier rewrites at runtime)
#   - per-fixture adapter token preserved (clause 4)
# ---------------------------------------------------------------------------

echo "== Clause 4: opt-out suppresses entirely (ux/webr/feedback) =="

for opt_fixture in ux.qmd webr.qmd feedback.qmd; do
  if grep -qF 'bt-auto-bootstrap: false' "$FIXTURE_DIR/$opt_fixture"; then
    ok "$opt_fixture declares YAML bt-auto-bootstrap: false"
  else
    ko "$opt_fixture declares YAML bt-auto-bootstrap: false — missing"
  fi
done

# fixture stem → expected hand-written bootstrap token (the adapter seam each
# fixture owns: mock adapter, webr adapter, feedback mount).
for opt_entry in "ux:__btTestAdapter" "webr:__btWebRAdapter" "feedback:mountAllFeedback"; do
  opt_stem="${opt_entry%%:*}"
  opt_token="${opt_entry##*:}"

  OPT_HTML="$FIXTURE_DIR/$opt_stem.html"
  rm -f "$OPT_HTML"
  render_to_html "$FIXTURE_DIR/$opt_stem.qmd" "$OPT_HTML" >/dev/null 2>&1 || true

  if [ ! -f "$OPT_HTML" ]; then
    ko "$opt_stem opt-out — HTML missing"
    ko "$opt_stem hand-written bootstrap preserved — HTML missing"
    ko "$opt_stem exactly-one scanExercises script — HTML missing"
    ko "$opt_stem specifiers preserved — HTML missing"
    continue
  fi

  OPT_CONTENT=$(cat "$OPT_HTML")

  # Clause 1: zero filter-injected auto-bootstrap.
  OPT_COUNT=$(count_bootstrap "$OPT_CONTENT")
  if [ "$OPT_COUNT" -eq 0 ]; then
    ok "$opt_stem opt-out — zero auto bootstrap ($OPT_COUNT found)"
  else
    ko "$opt_stem opt-out — expected 0 auto bootstrap, found $OPT_COUNT"
  fi

  # Clause 2: exactly one scanExercises module script (hand-written).
  OPT_SCAN_COUNT=$(printf '%s' "$OPT_CONTENT" | grep -c 'import { scanExercises' || true)
  if [ "$OPT_SCAN_COUNT" -eq 1 ]; then
    ok "$opt_stem exactly-one scanExercises module script ($OPT_SCAN_COUNT found)"
  else
    ko "$opt_stem exactly-one scanExercises module script — expected 1, found $OPT_SCAN_COUNT"
  fi

  # Clause 3: hand-written specifiers preserved — ../_extensions/, NOT
  # libs-dir URLs (static grep authoritative; ensureAssetSymlink masks at
  # runtime).
  OPT_SCRIPT=$(extract_hand_script "$OPT_CONTENT")
  if has_token "$OPT_SCRIPT" 'from "../_extensions/blendtutor/assets/' \
    && ! has_token "$OPT_SCRIPT" '_files/libs/'; then
    ok "$opt_stem hand-written specifiers preserved (../_extensions/, no libs-dir)"
  else
    ko "$opt_stem hand-written specifiers preserved — libs rewrite leaked or source specifier missing"
  fi

  # Clause 4: per-fixture adapter token preserved in the hand-written script.
  if has_token "$OPT_SCRIPT" "$opt_token"; then
    ok "$opt_stem hand-written bootstrap preserved ($opt_token)"
  else
    ko "$opt_stem hand-written bootstrap preserved — $opt_token not found"
  fi
done

# ---------------------------------------------------------------------------
# Clause 5: Non-HTML gate — latex output has no bootstrap
# ---------------------------------------------------------------------------

echo "== Clause 5: non-HTML gate (filter.qmd → latex) =="

LATEX_FILE="$FIXTURE_DIR/filter.tex"
rm -f "$LATEX_FILE"
render_to_latex "$FIXTURE_DIR/filter.qmd" "$LATEX_FILE" >/dev/null 2>&1 || true

if [ ! -f "$LATEX_FILE" ]; then
  ko "non-HTML gate — tex output missing"
else
  LATEX_COUNT=$(count_bootstrap "$(cat "$LATEX_FILE")")
  if [ "$LATEX_COUNT" -eq 0 ]; then
    ok "non-HTML gate — zero bootstrap in latex ($LATEX_COUNT found)"
  else
    ko "non-HTML gate — expected 0, found $LATEX_COUNT"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 6: Libs-URL specifiers + coi depth
# ---------------------------------------------------------------------------

echo "== Clause 6: libs-URL specifiers + coi depth =="

if [ -f "$MIXED_HTML" ]; then
  LIBS_PREFIX='mixed-lang_files/libs/quarto-contrib/blendtutor-0.1.0'
  if has_token "$MIXED_BOOTSTRAP" "$LIBS_PREFIX/exercise-runtime.js" \
    && has_token "$MIXED_BOOTSTRAP" "$LIBS_PREFIX/webr-adapter.js" \
    && has_token "$MIXED_BOOTSTRAP" "$LIBS_PREFIX/pyodide-adapter.js"; then
    ok "specifiers are libs URLs (mixed-lang_files/libs/quarto-contrib/blendtutor-0.1.0/)"
  else
    ko "specifiers are libs URLs — $LIBS_PREFIX/ not found in bootstrap"
  fi

  if printf '%s' "$MIXED_BOOTSTRAP" | grep -qF '_extensions/'; then
    ko "no _extensions/ substring in bootstrap — source-tree specifier remains"
  else
    ok "no _extensions/ substring in bootstrap specifiers"
  fi
fi

# Depth-correct ../.. coi src at coi-book/ depth (chapter-coi.qmd — COI stays
# include_text + resolve_asset_path under AC-4; service-worker scope = script
# URL dir, so it must NOT move to a libs dir).
COI_BOOK_HTML="$FIXTURE_DIR/coi-book/chapter-coi.html"
rm -f "$COI_BOOK_HTML"
render_to_html "$FIXTURE_DIR/coi-book/chapter-coi.qmd" "$COI_BOOK_HTML" >/dev/null 2>&1 || true

if [ ! -f "$COI_BOOK_HTML" ]; then
  ko "depth-correct ../.. specifier — HTML missing"
else
  COI_BOOK_CONTENT=$(cat "$COI_BOOK_HTML")
  if has_token "$COI_BOOK_CONTENT" 'src="../../_extensions/blendtutor/assets/coi-serviceworker.js"'; then
    ok "depth-correct ../.. specifier at coi-book depth"
  else
    ko "depth-correct ../.. specifier — ../../_extensions/... not found"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 7: Error sink
# ---------------------------------------------------------------------------

echo "== Clause 7: error sink =="

if [ -f "$MIXED_HTML" ]; then
  if has_token "$MIXED_BOOTSTRAP" ".catch(" && has_token "$MIXED_BOOTSTRAP" "console.error"; then
    ok ".catch( + console.error present"
  else
    ko ".catch( + console.error — missing one or both"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 9: Static pins (blendtutor.lua)
# ---------------------------------------------------------------------------

echo "== Clause 9: static pins (blendtutor.lua) =="

LUA_CONTENT=$(cat "$LUA_FILTER")
if has_token "$LUA_CONTENT" 'has_r = true'; then
  ok "has_r = true in Div() (mirror has_python)"
else
  ko "has_r = true in Div() — flag not set"
fi
if has_token "$LUA_CONTENT" 'hasBootstrapDone'; then
  ok "hasBootstrapDone guard (mirror hasCoiDone)"
else
  ko "hasBootstrapDone guard — missing"
fi
if has_token "$LUA_CONTENT" 'bt-auto-bootstrap'; then
  ok "bt-auto-bootstrap YAML read in Pandoc() (mirror coi read)"
else
  ko "bt-auto-bootstrap YAML read in Pandoc() — missing"
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
