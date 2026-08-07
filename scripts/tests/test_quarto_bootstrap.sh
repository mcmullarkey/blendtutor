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
#   3. start() wiring: hoists `const registry = buildRegistry(scanExercises())`
#      then calls start(registry, <map>) with keys {r, python}. No exercises →
#      no bootstrap.
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
# ISSUE #164 (byok-api-key AC-3) adds clauses 10-14:
#  10. Feedback + key-page imports (C5/C6): bootstrap imports mountAllFeedback
#      from libs exercise-feedback.js AND mountKeyPage from libs key-page.js.
#  11. Registry hoist + mount order (C7/C8/C9/C11): `const registry =
#      buildRegistry(scanExercises())` hoisted once, start(registry, ...) and
#      mountAllFeedback(registry) share it, mountAllFeedback sits INSIDE .then(
#      AFTER start( before .catch( (awk line-order), exactly one
#      mountAllFeedback( call, mountKeyPage(document.querySelector(".blendtutor-key"))
#      unconditional.
#  12. Key-only page (C12/C13/C14): quarto-fixture/key-only.qmd renders
#      <div class="blendtutor-key">, deploys bootstrap + mountKeyPage; static
#      pins — has_key = true set in Div() BEFORE the non-blendtutor early-return,
#      guards broadened to `has_blendtutor or has_key` at BOTH the
#      add_html_dependency and bootstrap-injection sites.
#  13. bt-feedback opt-out (C15/C16/C17): feedback-optout.qmd +
#      feedback-optout-string.qmd — bootstrap present, start( present,
#      mountKeyPage present, mountAllFeedback ABSENT (import AND call);
#      string "false" parity with boolean.
#  14. __btConfig keyPageUrl (C18/C19/C20/C21/C22): separate classic <head>
#      script `window.__btConfig = window.__btConfig || {}; window.__btConfig
#      .keyPageUrl = ...` on every has_blendtutor-or-has_key page REGARDLESS of
#      opt-outs (webr.qmd bt-auto-bootstrap: false still gets keyPageUrl, C18);
#      bt-key-page YAML honored (C20); default api-key.html (C21); merge pattern
#      never bare = {...} (C22).
#
# Negative cases killed here: classic script no type=module (1), hardcodes both
# adapters (2), opt-out via guard (4), hardcoded specifier (6), omits .catch (7),
# no has_r flag (9), mountAllFeedback before start resolves / double-mount /
# separate registries (11), key-only page dead (12), bt-feedback:false ignored
# or string treated as truthy (13), bare __btConfig clobber / keyPageUrl missing
# on opt-out pages / custom value ignored / emitted only inside module bootstrap
# (14).
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

# Static-pin source — read once at the top so ANY clause can reference it
# (clause 12 key-only pins + clause 15 static pins; set -u makes a late
# assignment fatal).
LUA_CONTENT=$(cat "$LUA_FILTER")

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
  grep -o "$MARKER" <<< "$content" | wc -l | tr -d ' ' || true
}

# Count occurrences of an arbitrary fixed token.
count_token() {
  local content="$1"
  local token="$2"
  grep -oF "$token" <<< "$content" | wc -l | tr -d ' ' || true
}

has_token() {
  local content="$1"
  local token="$2"
  grep -qF "$token" <<< "$content"
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
  # ISSUE #164 (AC-3 C9): registry is hoisted to a shared const first, then
  # start(registry, ...) — the OLD inline form start(buildRegistry(...)) was
  # replaced so mountAllFeedback() shares the SAME registry instance.
  if has_token "$MIXED_BOOTSTRAP" "const registry = buildRegistry(scanExercises())" \
    && has_token "$MIXED_BOOTSTRAP" "start(registry, {"; then
    ok "hoists registry then calls start(registry, <map>)"
  else
    ko "hoists registry then calls start(registry, <map>) — pattern not found"
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
  if grep -qF "__btConfig" "$LATEX_FILE"; then
    ko "non-HTML gate — zero __btConfig in latex (C23)"
  else
    ok "non-HTML gate — zero __btConfig in latex"
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

  if grep -qF '_extensions/' <<< "$MIXED_BOOTSTRAP"; then
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
# Clause 10 (issue #164): feedback + key-page imports (C5/C6)
# ---------------------------------------------------------------------------

echo "== Clause 10: bootstrap imports mountAllFeedback + mountKeyPage (C5/C6) =="

if [ -f "$MIXED_HTML" ]; then
  if has_token "$MIXED_BOOTSTRAP" 'import { mountAllFeedback } from "./mixed-lang_files/libs/quarto-contrib/blendtutor-0.1.0/exercise-feedback.js"'; then
    ok "bootstrap imports mountAllFeedback from libs exercise-feedback.js"
  else
    ko "bootstrap imports mountAllFeedback from libs exercise-feedback.js — not found"
  fi
  if has_token "$MIXED_BOOTSTRAP" 'import { mountKeyPage } from "./mixed-lang_files/libs/quarto-contrib/blendtutor-0.1.0/key-page.js"'; then
    ok "bootstrap imports mountKeyPage from libs key-page.js"
  else
    ko "bootstrap imports mountKeyPage from libs key-page.js — not found"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 11 (issue #164): registry hoist + mount order (C7/C8/C9/C11)
# ---------------------------------------------------------------------------

echo "== Clause 11: registry hoisted, mountAllFeedback after start, before catch (C7/C8/C9/C11) =="

if [ -f "$MIXED_HTML" ]; then
  if has_token "$MIXED_BOOTSTRAP" 'const registry = buildRegistry(scanExercises())'; then
    ok "registry hoisted to shared const (C9)"
  else
    ko "registry hoisted to shared const — 'const registry = buildRegistry(scanExercises())' not found"
  fi

  if has_token "$MIXED_BOOTSTRAP" 'start(registry, {'; then
    ok "start() receives the hoisted registry (C9)"
  else
    ko "start() receives the hoisted registry — 'start(registry,' not found"
  fi

  # C7: awk line-order — mountAllFeedback(registry) must sit after the start(
  # call and before the .catch( sink. || true guards set -e: greps legitimately
  # find nothing during the red phase.
  LINE_START=$(printf '%s\n' "$MIXED_BOOTSTRAP" | grep -n 'start(registry' | head -1 | cut -d: -f1 || true)
  LINE_MOUNT=$(printf '%s\n' "$MIXED_BOOTSTRAP" | grep -n 'mountAllFeedback(registry)' | head -1 | cut -d: -f1 || true)
  LINE_CATCH=$(printf '%s\n' "$MIXED_BOOTSTRAP" | grep -n '.catch(' | head -1 | cut -d: -f1 || true)
  if [ -n "$LINE_START" ] && [ -n "$LINE_MOUNT" ] && [ -n "$LINE_CATCH" ] \
    && [ "$LINE_START" -lt "$LINE_MOUNT" ] && [ "$LINE_MOUNT" -lt "$LINE_CATCH" ]; then
    ok "mountAllFeedback after start(, before .catch( (line order $LINE_START < $LINE_MOUNT < $LINE_CATCH)"
  else
    ko "mountAllFeedback after start(, before .catch( — order start=$LINE_START mount=$LINE_MOUNT catch=$LINE_CATCH"
  fi

  # C11: .then( and .catch( both present.
  if has_token "$MIXED_BOOTSTRAP" '.then(' && has_token "$MIXED_BOOTSTRAP" '.catch('; then
    ok ".then( and .catch( both present"
  else
    ko ".then( and .catch( — missing one or both"
  fi

  # C8: exactly one mountAllFeedback( call site.
  MOUNT_CALL_COUNT=$(count_token "$MIXED_BOOTSTRAP" 'mountAllFeedback(registry)')
  if [ "$MOUNT_CALL_COUNT" -eq 1 ]; then
    ok "exactly one mountAllFeedback(registry) call site ($MOUNT_CALL_COUNT found)"
  else
    ko "exactly one mountAllFeedback(registry) call site — expected 1, found $MOUNT_CALL_COUNT"
  fi

  # C10: mountKeyPage unconditional, selector on .blendtutor-key.
  if has_token "$MIXED_BOOTSTRAP" 'mountKeyPage(document.querySelector(".blendtutor-key"))'; then
    ok "mountKeyPage(document.querySelector('.blendtutor-key')) present, unconditional"
  else
    ko "mountKeyPage(document.querySelector('.blendtutor-key')) — not found"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 12 (issue #164): key-only page (C12/C13/C14)
# ---------------------------------------------------------------------------

echo "== Clause 12: key-only page renders div, deploys bootstrap + mountKeyPage (C12/C13/C14) =="

KEY_ONLY_HTML="$FIXTURE_DIR/key-only.html"
rm -f "$KEY_ONLY_HTML"
render_to_html "$FIXTURE_DIR/key-only.qmd" "$KEY_ONLY_HTML" >/dev/null 2>&1 || true

if [ ! -f "$KEY_ONLY_HTML" ]; then
  ko "key-only page — HTML missing"
  ko "key-only .blendtutor-key div — HTML missing"
  ko "key-only bootstrap injected — HTML missing"
  ko "key-only mountKeyPage called — HTML missing"
else
  KEY_ONLY_CONTENT=$(cat "$KEY_ONLY_HTML")

  if grep -qF '<div class="blendtutor-key">' <<< "$KEY_ONLY_CONTENT"; then
    ok ".blendtutor-key div renders as <div class=\"blendtutor-key\"> (C12)"
  else
    ko ".blendtutor-key div renders — <div class=\"blendtutor-key\"> not found"
  fi

  KEY_ONLY_BOOT_COUNT=$(count_bootstrap "$KEY_ONLY_CONTENT")
  if [ "$KEY_ONLY_BOOT_COUNT" -eq 1 ]; then
    ok "key-only page bootstrap injected (C14)"
  else
    ko "key-only page bootstrap injected — expected 1, found $KEY_ONLY_BOOT_COUNT"
  fi

  KEY_ONLY_BOOTSTRAP=$(extract_bootstrap "$KEY_ONLY_CONTENT")
  if has_token "$KEY_ONLY_BOOTSTRAP" 'mountKeyPage(document.querySelector(".blendtutor-key"))'; then
    ok "key-only page calls mountKeyPage (C14)"
  else
    ko "key-only page calls mountKeyPage — not found in bootstrap"
  fi
fi

# Static pins (C13): has_key set in Div() BEFORE the non-blendtutor early-return,
# guards broadened to has_blendtutor or has_key at ALL THREE emission sites —
# the add_html_dependency guard (C14), the bootstrap-injection guard (C14), and
# the __btConfig keyPageUrl head-script guard (C19).
# || true guards set -e: greps legitimately find nothing during the red phase.
KEY_LINE=$(grep -n 'has_key = true' "$LUA_FILTER" | head -1 | cut -d: -f1 || true)
EARLY_RETURN_LINE=$(grep -n 'if not div.classes:includes("blendtutor") then' "$LUA_FILTER" | head -1 | cut -d: -f1 || true)
if [ -n "$KEY_LINE" ] && [ -n "$EARLY_RETURN_LINE" ] && [ "$KEY_LINE" -lt "$EARLY_RETURN_LINE" ]; then
  ok "has_key = true set in Div() before non-blendtutor early-return (C13)"
else
  ko "has_key = true set in Div() before non-blendtutor early-return — key=$KEY_LINE early-return=$EARLY_RETURN_LINE"
fi

GUARD_COUNT=$(count_token "$LUA_CONTENT" 'has_blendtutor or has_key')
if [ "$GUARD_COUNT" -eq 3 ]; then
  ok "guards broadened to 'has_blendtutor or has_key' at all 3 emission sites (C14/C19)"
else
  ko "guards broadened to 'has_blendtutor or has_key' at all 3 sites — expected 3, found $GUARD_COUNT"
fi

# ---------------------------------------------------------------------------
# Clause 13 (issue #164): bt-feedback granular opt-out (C15/C16/C17)
# ---------------------------------------------------------------------------

echo "== Clause 13: bt-feedback opt-out — bootstrap kept, mountAllFeedback absent (C15/C16/C17) =="

for opt_entry in "feedback-optout:bt-feedback: false" "feedback-optout-string:bt-feedback: \"false\""; do
  opt_stem="${opt_entry%%:*}"
  opt_yaml="${opt_entry#*:}"

  if grep -qF "$opt_yaml" "$FIXTURE_DIR/$opt_stem.qmd"; then
    ok "$opt_stem declares YAML $opt_yaml"
  else
    ko "$opt_stem declares YAML $opt_yaml — missing"
  fi

  OPT_HTML="$FIXTURE_DIR/$opt_stem.html"
  rm -f "$OPT_HTML"
  render_to_html "$FIXTURE_DIR/$opt_stem.qmd" "$OPT_HTML" >/dev/null 2>&1 || true

  if [ ! -f "$OPT_HTML" ]; then
    ko "$opt_stem opt-out — HTML missing"
    continue
  fi

  OPT_CONTENT=$(cat "$OPT_HTML")
  OPT_BOOTSTRAP=$(extract_bootstrap "$OPT_CONTENT")

  OPT_BOOT_COUNT=$(count_bootstrap "$OPT_CONTENT")
  if [ "$OPT_BOOT_COUNT" -eq 1 ]; then
    ok "$opt_stem bootstrap present (granular opt-out keeps bootstrap)"
  else
    ko "$opt_stem bootstrap present — expected 1, found $OPT_BOOT_COUNT"
  fi

  if has_token "$OPT_BOOTSTRAP" 'start(registry'; then
    ok "$opt_stem start( present"
  else
    ko "$opt_stem start( present — not found"
  fi

  if has_token "$OPT_BOOTSTRAP" 'mountKeyPage(document.querySelector(".blendtutor-key"))'; then
    ok "$opt_stem mountKeyPage present"
  else
    ko "$opt_stem mountKeyPage present — not found"
  fi

  OPT_MOUNT_COUNT=$(count_token "$OPT_BOOTSTRAP" 'mountAllFeedback')
  if [ "$OPT_MOUNT_COUNT" -eq 0 ]; then
    ok "$opt_stem mountAllFeedback ABSENT (import + call suppressed)"
  else
    ko "$opt_stem mountAllFeedback ABSENT — found $OPT_MOUNT_COUNT occurrences"
  fi
done

# ---------------------------------------------------------------------------
# Clause 14 (issue #164): __btConfig keyPageUrl head script (C18-C22)
# ---------------------------------------------------------------------------

echo "== Clause 14: __btConfig keyPageUrl via separate head script (C18-C22) =="

# C21 default: mixed-lang.qmd (no bt-key-page) → api-key.html.
if [ -f "$MIXED_HTML" ]; then
  if has_token "$MIXED_CONTENT" 'window.__btConfig = window.__btConfig || {}'; then
    ok "merge pattern window.__btConfig = window.__btConfig || {} present (C22)"
  else
    ko "merge pattern window.__btConfig = window.__btConfig || {} — not found"
  fi

  if has_token "$MIXED_CONTENT" 'window.__btConfig.keyPageUrl = "api-key.html"'; then
    ok "keyPageUrl defaults to api-key.html (C21)"
  else
    ko "keyPageUrl defaults to api-key.html — not found"
  fi

  if grep -qF 'window.__btConfig = {' <<< "$MIXED_CONTENT"; then
    ko "NO bare window.__btConfig = { — clobber would drop maxFeedbackPerSession (C22)"
  else
    ok "no bare window.__btConfig = { assignment (merge only)"
  fi

  # C19: classic (non-module) head script, separate from the module bootstrap.
  if grep -qF '<script>window.__btConfig' <<< "$MIXED_CONTENT"; then
    ok "keyPageUrl emitted as separate classic <script> (not module bootstrap)"
  else
    ko "keyPageUrl emitted as separate classic <script> — not found"
  fi

  # C22: keyPageUrl must NOT live inside the module bootstrap (unreachable when
  # the bootstrap is opted out). Bootstrap body must be free of __btConfig.
  if has_token "$MIXED_BOOTSTRAP" '__btConfig'; then
    ko "bootstrap body has NO __btConfig emission (must be head script)"
  else
    ok "bootstrap body free of __btConfig emission"
  fi
fi

# C20: custom bt-key-page YAML honored.
KEY_META_HTML="$FIXTURE_DIR/key-page-meta.html"
rm -f "$KEY_META_HTML"
render_to_html "$FIXTURE_DIR/key-page-meta.qmd" "$KEY_META_HTML" >/dev/null 2>&1 || true

if [ ! -f "$KEY_META_HTML" ]; then
  ko "custom bt-key-page honored — HTML missing"
else
  if has_token "$(cat "$KEY_META_HTML")" 'window.__btConfig.keyPageUrl = "custom-key.html"'; then
    ok "custom bt-key-page value honored (C20)"
  else
    ko "custom bt-key-page value honored — custom-key.html not found"
  fi
fi

# C18: bt-auto-bootstrap: false → zero bootstrap but keyPageUrl head script
# STILL present (opt-out pages need the no-key link, AC-4).
WEBR_HTML="$FIXTURE_DIR/webr.html"
rm -f "$WEBR_HTML"
render_to_html "$FIXTURE_DIR/webr.qmd" "$WEBR_HTML" >/dev/null 2>&1 || true

if [ ! -f "$WEBR_HTML" ]; then
  ko "bt-auto-bootstrap: false → zero bootstrap + keyPageUrl present — HTML missing"
else
  WEBR_CONTENT=$(cat "$WEBR_HTML")
  WEBR_BOOT_COUNT=$(count_bootstrap "$WEBR_CONTENT")
  if [ "$WEBR_BOOT_COUNT" -eq 0 ]; then
    ok "bt-auto-bootstrap: false → zero auto bootstrap (C18 regression)"
  else
    ko "bt-auto-bootstrap: false → expected 0 auto bootstrap, found $WEBR_BOOT_COUNT"
  fi
  if has_token "$WEBR_CONTENT" 'window.__btConfig.keyPageUrl'; then
    ok "keyPageUrl head script STILL present on bt-auto-bootstrap:false page (C18/C19)"
  else
    ko "keyPageUrl head script STILL present on bt-auto-bootstrap:false page — not found"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 15: Static pins (blendtutor.lua)
# ---------------------------------------------------------------------------

echo "== Clause 15: static pins (blendtutor.lua) =="

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
