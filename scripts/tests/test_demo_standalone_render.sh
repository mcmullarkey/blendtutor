#!/usr/bin/env bash
# Executable spec for issue #150 — standalone interactive demo (demo-standalone/)
# with page-covering COI service worker for R + Python exercises.
#
# Verifies the 8-clause compound predicate from AC-1:
#   1. demo-standalone/_quarto.yml pinned type: default, filter reference
#      [../_extensions/blendtutor/blendtutor.lua], NO output-dir
#      (book mode kills COI per AC-5; output-dir breaks resolve_asset_path)
#   2. quarto render demo-standalone --to html exits 0 → index.html exists
#   3. filter loaded: exactly 2 bt-exercise divs — 1 data-language="r" +
#      1 data-language="python" (load-proving guard; pinned counts for AC-3)
#   4. libs deployed standalone-form at
#      index_files/libs/quarto-contrib/blendtutor-0.1.0/ with both adapters
#      (exercise-runtime.js, codemirror.js, styles.css, webr-adapter.js,
#      pyodide-adapter.js — page has both languages)
#   5. bootstrap module <script type="module" data-bt-bootstrap="auto"> with
#      ./index_files/libs/... specifiers; NO site_libs/, NO _extensions/
#      substring inside the module (book discriminator leak trap, issue #143)
#   6. COI activated: exactly ONE coi-serviceworker.js script TAG in HTML
#      (YAML coi: true → has_coi → injection; exactly-one pins hasCoiDone dedup)
#   7. COI scope coverage (SCOPE RESOLUTION): after running
#      scripts/fix-demo-coi-scope.sh demo-standalone — post-processed coi src is
#      EXACTLY ./coi-serviceworker.js (page-root relative, NOT ../-prefixed, NOT
#      _extensions/... subdir); demo-standalone/coi-serviceworker.js exists,
#      is non-empty, is byte-identical to the vendored shim, and carries the
#      load-bearing register(currentScript.src) call — the NO-scope
#      registration whose default scope this post-process exists to fix
#      (verified coi-serviceworker.js:103; the vendored shim is minified, so
#      "contains navigator.serviceWorker" reads as n.serviceWorker here).
#      Re-running the post-process is a no-op (idempotence).
#   8. CI wiring: .github/workflows/ci.yml quarto-render job runs this script.
#
# Negative cases (from the spec):
#   - raw filter emission leaves src as ../_extensions/blendtutor/assets/
#     coi-serviceworker.js → SW URL /_extensions/.../assets/ → scope covers the
#     assets dir only, page never controlled → webR dead despite a green "coi
#     present" grep. Killed by clause 7 exact-./coi-serviceworker.js match.
#   - post-process rewrites src but forgets the file copy (or copies a
#     zero-byte/wrong file) → 404 or silent SW registration failure. Killed by
#     clause 7 (exists + non-empty + byte-identical to vendored shim).
#   - render exits 0 but filter never loaded → raw content, no bt-exercise
#     widget. Killed by clause 3.
#   - book discriminator leak → bootstrap ./site_libs/... (issue #143 trap) →
#     404 on Pages. Killed by clauses 4-5.
#   - hasCoiDone dedup broken → duplicate coi script tags → double SW
#     registration + reload loop. Killed by clause 6 exactly-one.
#   - type: book regression or output-dir set → COI inert / emitted src
#     mismatches output location. Killed by clause 1 static pin.
#   - Python exercise dropped → page R-only, AC says R + Python. Killed by
#     clause 3 data-language="python" pin.
#
# Usage: bash scripts/tests/test_demo_standalone_render.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PASS=0
FAIL=0

ok() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
ko() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

DEMO_DIR="demo-standalone"
HTML_FILE="$DEMO_DIR/index.html"
LIBS_DIR="$DEMO_DIR/index_files/libs/quarto-contrib/blendtutor-0.1.0"
POST_PROCESS="scripts/fix-demo-coi-scope.sh"

# ---------------------------------------------------------------------------
# Assertion 1 — _quarto.yml pinned type: default, explicit filter, no output-dir
# ---------------------------------------------------------------------------

echo "== Assertion 1: demo-standalone/_quarto.yml config pin =="

QMD_YML="$DEMO_DIR/_quarto.yml"

if [ ! -f "$QMD_YML" ]; then
  ko "_quarto.yml exists — file not found: $QMD_YML"
else
  if grep -qE 'type:\s+default' "$QMD_YML"; then
    ok "project type: default (book mode kills COI — pinned)"
  else
    ko "project type: default — 'type: default' not found in $QMD_YML"
  fi

  if grep -qF 'filters: [../_extensions/blendtutor/blendtutor.lua]' "$QMD_YML"; then
    ok "filter reference ../_extensions/blendtutor/blendtutor.lua declared"
  else
    ko "filter reference ../_extensions/blendtutor/blendtutor.lua declared — not found in $QMD_YML"
  fi

  if ! grep -qE 'output-dir' "$QMD_YML"; then
    ok "no output-dir (render beside qmd, deterministic paths)"
  else
    ko "no output-dir — output-dir found in $QMD_YML"
  fi
fi

# ---------------------------------------------------------------------------
# Assertion 8 — CI wiring: quarto-render job runs this test
# ---------------------------------------------------------------------------

echo "== Assertion 8: CI runs the standalone demo render test =="

CI_FILE=".github/workflows/ci.yml"

if [ ! -f "$CI_FILE" ]; then
  ko "CI file exists — not found: $CI_FILE"
else
  # Pin the assertion to the quarto-render JOB BLOCK, not file-wide: the
  # step must live under quarto-render (clause 7), so a file-wide grep would
  # pass even if the step regressed to another job (e.g. quarto-distribution).
  # awk extracts the quarto-render block, ending at the next job header
  # (2-space-indented `name:`), and the header line itself is skipped so the
  # range cannot self-close on `quarto-render:`.
  if awk '/^  quarto-render:/{f=1;next} f&&/^  [a-z][a-z-]*:$/{f=0} f' "$CI_FILE" \
       | grep -qF 'scripts/tests/test_demo_standalone_render.sh'; then
    ok "CI quarto-render job runs test_demo_standalone_render.sh"
  else
    ko "CI quarto-render job runs test_demo_standalone_render.sh — not found in quarto-render job block"
  fi
fi

# ---------------------------------------------------------------------------
# Assertions 2-7 — quarto render + content + libs + bootstrap + COI scope
# ---------------------------------------------------------------------------

echo "== Assertions 2-7: quarto render + filter load + libs + bootstrap + COI scope =="

if [ ! -f "$DEMO_DIR/index.qmd" ]; then
  ko "demo-standalone/index.qmd exists — file not found: $DEMO_DIR/index.qmd"
  ko "quarto render exits 0 — index.qmd missing"
  ko "index.html exists — index.qmd missing"
else
  if ! command -v quarto &>/dev/null; then
    echo "  SKIP: quarto not installed locally — render assertions skipped"
    echo "  (CI installs quarto via quarto-dev/quarto-actions/setup@v2)"
    ko "quarto render exits 0 — quarto not installed"
  else
    # Clean previous render output (HTML + standalone libs + copied SW).
    rm -f "$DEMO_DIR"/*.html "$DEMO_DIR"/coi-serviceworker.js
    rm -rf "$DEMO_DIR"/*_files/ "$DEMO_DIR"/.quarto/

    RENDER_OUTPUT=$(quarto render "$DEMO_DIR" --to html 2>&1) && RENDER_RC=0 || RENDER_RC=$?

    if [ "$RENDER_RC" -eq 0 ]; then
      ok "quarto render demo-standalone exits 0"
    else
      ko "quarto render demo-standalone exits 0 — exit code $RENDER_RC"
      echo "  render output: $RENDER_OUTPUT" >&2
    fi

    if [ -f "$HTML_FILE" ]; then
      ok "index.html exists at $HTML_FILE"
    else
      ko "index.html exists — not found: $HTML_FILE"
    fi
  fi
fi

# Only run content assertions if the render produced HTML (or the qmd exists
# and quarto is absent — then we cannot assert, so report SKIP-style failures).
if [ ! -f "$HTML_FILE" ]; then
  echo "  NOTE: index.html absent — skipping clauses 3-7 content assertions"
  ko "clauses 3-7 content assertions — index.html not rendered"
else
  HTML_CONTENT=$(cat "$HTML_FILE")

  # --- Clause 3: exactly 2 bt-exercise divs, 1 r + 1 python ---
  BT_COUNT=$(grep -oF '<div class="bt-exercise"' "$HTML_FILE" | wc -l || true)
  BT_R=$(grep -oF '<div class="bt-exercise" data-language="r"' "$HTML_FILE" | wc -l || true)
  BT_PY=$(grep -oF '<div class="bt-exercise" data-language="python"' "$HTML_FILE" | wc -l || true)

  if [ "$BT_COUNT" -eq 2 ]; then
    ok "exactly 2 bt-exercise divs (filter loaded)"
  else
    ko "exactly 2 bt-exercise divs — found $BT_COUNT"
  fi

  if [ "$BT_R" -eq 1 ]; then
    ok "exactly 1 R exercise (data-language=\"r\")"
  else
    ko "exactly 1 R exercise (data-language=\"r\") — found $BT_R"
  fi

  if [ "$BT_PY" -eq 1 ]; then
    ok "exactly 1 Python exercise (data-language=\"python\")"
  else
    ko "exactly 1 Python exercise (data-language=\"python\") — found $BT_PY"
  fi

  # --- Clause 4: standalone-form libs deployed with both adapters ---
  LIBS_OK=0
  for f in exercise-runtime.js codemirror.js styles.css webr-adapter.js pyodide-adapter.js; do
    if [ -f "$LIBS_DIR/$f" ]; then
      LIBS_OK=$((LIBS_OK + 1))
    else
      ko "lib file deployed: $LIBS_DIR/$f — missing"
    fi
  done
  if [ "$LIBS_OK" -eq 5 ]; then
    ok "standalone-form libs deployed (5 files, both adapters)"
  else
    ko "standalone-form libs deployed — only $LIBS_OK/5 present"
  fi

  if [ -d "$DEMO_DIR/index_files/libs/quarto-contrib" ]; then
    ok "libs under index_files/libs/quarto-contrib (standalone form)"
  else
    ko "libs under index_files/libs/quarto-contrib — standalone libs dir not found"
  fi

  if grep -qF 'site_libs' "$HTML_FILE"; then
    ko "no site_libs/ anywhere in HTML (book discriminator leak)"
  else
    ok "no site_libs/ substring in HTML"
  fi

  # --- Clause 5: bootstrap module with ./index_files/libs/ specifiers ---
  MARKER='<script type="module" data-bt-bootstrap="auto">'
  BOOTSTRAP=$(awk -v marker="$MARKER" '
    index($0, marker) { in_block=1; print; next }
    in_block && /<\/script>/ { print; in_block=0; next }
    in_block { print }
  ' "$HTML_FILE")

  if [ -n "$BOOTSTRAP" ]; then
    ok "bootstrap module emitted (data-bt-bootstrap=\"auto\")"
  else
    ko "bootstrap module emitted — marker not found in HTML"
  fi

  if grep -qF './index_files/libs/' <<< "$BOOTSTRAP"; then
    ok "bootstrap specifiers use ./index_files/libs/ (standalone form)"
  else
    ko "bootstrap specifiers use ./index_files/libs/ — not found in module"
  fi

  if ! grep -qF 'site_libs/' <<< "$BOOTSTRAP"; then
    ok "no site_libs/ in bootstrap specifiers"
  else
    ko "no site_libs/ in bootstrap specifiers — book path leaked into module"
  fi

  if ! grep -qF '_extensions/' <<< "$BOOTSTRAP"; then
    ok "no _extensions/ in bootstrap specifiers (libs-dir deploy)"
  else
    ko "no _extensions/ in bootstrap specifiers — source-tree path leaked into module"
  fi

  # --- Clause 6: exactly ONE coi-serviceworker.js script src ---
  # Count SCRIPT TAGS referencing the shim (the page's own prose may mention
  # the filename as documentation — raw occurrence counting would double-count).
  # Exactly-one pins the hasCoiDone dedup guard (duplicate coi script tags →
  # double SW registration + reload loop).
  COI_COUNT=$(grep -oE '<script[^>]*coi-serviceworker\.js[^>]*>' "$HTML_FILE" | wc -l || true)

  if [ "$COI_COUNT" -eq 1 ]; then
    ok "exactly one coi-serviceworker.js script tag (hasCoiDone dedup)"
  else
    ko "exactly one coi-serviceworker.js script tag — found $COI_COUNT"
  fi

  # --- Clause 7: COI scope post-process ---
  if [ -f "$POST_PROCESS" ]; then
    bash "$POST_PROCESS" "$DEMO_DIR"

    if grep -qF 'src="./coi-serviceworker.js"' "$HTML_FILE"; then
      ok "post-processed coi src is exactly ./coi-serviceworker.js"
    else
      ko "post-processed coi src is exactly ./coi-serviceworker.js — not found"
    fi

    if grep -qF '_extensions/blendtutor/assets/coi-serviceworker.js' "$HTML_FILE"; then
      ko "raw ../_extensions/ coi src fully replaced (SW subdir scope trap)"
    else
      ok "raw ../_extensions/ coi src fully replaced"
    fi

    if [ -f "$DEMO_DIR/coi-serviceworker.js" ]; then
      ok "coi-serviceworker.js shim exists at page root"
    else
      ko "coi-serviceworker.js shim exists at page root — missing"
    fi

    if [ -s "$DEMO_DIR/coi-serviceworker.js" ]; then
      ok "coi-serviceworker.js shim is non-empty"
    else
      ko "coi-serviceworker.js shim is non-empty — zero-byte file"
    fi

    # The vendored shim is minified (n = navigator alias), so the upstream
    # "contains navigator.serviceWorker" wording becomes: byte-identical to the
    # vendored source (strongest wrong-file kill) AND carries the load-bearing
    # register(currentScript.src) call — the NO-scope registration whose default
    # scope this post-process exists to fix.
    SHIM_SUM=$(cksum "$DEMO_DIR/coi-serviceworker.js" | cut -d' ' -f1)
    SHIM_SRC_SUM=$(cksum "_extensions/blendtutor/assets/coi-serviceworker.js" | cut -d' ' -f1)
    if [ "$SHIM_SUM" = "$SHIM_SRC_SUM" ]; then
      ok "shim byte-identical to vendored coi-serviceworker.js (real SW code, not stub)"
    else
      ko "shim byte-identical to vendored coi-serviceworker.js — wrong file copied (sum $SHIM_SUM != $SHIM_SRC_SUM)"
    fi

    if grep -qF 'register(window.document.currentScript.src)' "$DEMO_DIR/coi-serviceworker.js"; then
      ok "shim carries register(currentScript.src) — the no-scope registration call"
    else
      ko "shim carries register(currentScript.src) — no-scope registration call missing"
    fi

    # Idempotence: re-running the post-process must be a no-op — still exactly
    # one coi script tag, still ./coi-serviceworker.js, shim unchanged.
    BEFORE_SUM=$(cksum "$DEMO_DIR/coi-serviceworker.js" | cut -d' ' -f1)
    bash "$POST_PROCESS" "$DEMO_DIR"
    AFTER_SUM=$(cksum "$DEMO_DIR/coi-serviceworker.js" | cut -d' ' -f1)

    COI_COUNT_2=$(grep -oE '<script[^>]*coi-serviceworker\.js[^>]*>' "$HTML_FILE" | wc -l || true)
    if [ "$COI_COUNT_2" -eq 1 ] && grep -qF 'src="./coi-serviceworker.js"' "$HTML_FILE" \
        && [ "$BEFORE_SUM" = "$AFTER_SUM" ]; then
      ok "post-process idempotent (re-run is a no-op)"
    else
      ko "post-process idempotent — re-run changed output (count=$COI_COUNT_2, sum $BEFORE_SUM->$AFTER_SUM)"
    fi
  else
    ko "post-process script exists — not found: $POST_PROCESS"
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
