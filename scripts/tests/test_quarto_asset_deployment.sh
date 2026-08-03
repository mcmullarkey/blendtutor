#!/usr/bin/env bash
# Executable spec for issue #141 — Deploy extension assets to render output
# via quarto.doc.add_html_dependency + rewrite bootstrap specifiers
# (filter-runtime-bootstrap AC-4).
#
# Verifies the 10-clause predicate from the AC-4 executable spec (clauses 11-13
# live in rodney-probes/auto-bootstrap.js, sync-quarto-assets.sh +
# verify_asset_scoping.py, and the migrated existing suites):
#   1. Mechanism pin: blendtutor.lua contains exactly ONE
#      quarto.doc.add_html_dependency({ name="blendtutor", version=BT_DEP_VERSION,
#      stylesheets={"assets/styles.css"}, resources={...} }) call. NO scripts=
#      key. NO _extension.yml resources: key. NO old css_link include_text for
#      styles.css remains (STYLES_CSS_PATH removed, not dual-injected).
#   2. Conditional JS resources: resources table built conditionally —
#      assets/exercise-runtime.js + assets/codemirror.js always;
#      assets/webr-adapter.js iff has_r; assets/pyodide-adapter.js iff
#      has_python.
#   3. Deployed to libs: files physically exist at
#      <stem>_files/libs/quarto-contrib/blendtutor-0.1.0/ — exercise-runtime.js,
#      codemirror.js, styles.css always; webr-adapter.js present iff page has R
#      exercises, ABSENT otherwise (same for pyodide/python).
#   4. CSS via Quarto link: rendered HTML contains exactly one
#      <link ... blendtutor-0.1.0/styles.css>; no _extensions/.../assets/styles.css
#      link present.
#   5. Bootstrap specifiers rewritten: data-bt-bootstrap="auto" module's import
#      specifiers reference <stem>_files/libs/quarto-contrib/blendtutor-0.1.0/<file>.js,
#      computed from quarto.doc.output_file stem; NO _extensions/ substring and
#      NO resolve_asset_path output in any specifier.
#   6. No classic runtime script tag: rendered HTML contains NO
#      <script src="...exercise-runtime.js"></script> (ES modules SyntaxError
#      as classic scripts).
#   7. COI boundary: coi-serviceworker.js stays include_text + resolve_asset_path
#      (SW scope = script URL dir) — NOT in any libs dir.
#   8. Stem correctness, nested: hermetic pages/ subdir render yields libs at
#      pages/index_files/libs/... and bootstrap specifier index_files/...
#      (document-relative); coi-book multi-page render exits 0.
#   8b. Book-aware libs_url (issue #143): BOOK project → ./site_libs/...
#      specifiers + files at _output/site_libs/...; standalone + output-dir
#      keeps <stem>_files/... (discriminator pin).
#   9. Non-HTML gate: hermetic latex render → zero *_files/libs/ dirs created,
#      zero bootstrap injection.
#  10. Version pin single-sourced: BT_DEP_VERSION = "0.1.0" Lua constant equals
#      _extension.yml:3 version AND used in BOTH dependency declaration and
#      emitted libs URL string.
#
# Negative cases killed here: _extension.yml resources (1), scripts= classic
# tag (1+6), URL rewritten but files not deployed (3 file-existence), hardcoded
# stem (8), version drift (10), dual CSS injection (4), coi in libs (7),
# unconditional adapter deployment (3 absence), no is_html_format guard (9).
#
# Usage: bash scripts/tests/test_quarto_asset_deployment.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PASS=0
FAIL=0

ok() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
ko() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

LUA_FILTER="_extensions/blendtutor/blendtutor.lua"
EXTENSION_YML="_extensions/blendtutor/_extension.yml"
FIXTURE_DIR="quarto-fixture"
MARKER='data-bt-bootstrap="auto"'
LIB_VERSION="0.1.0"
LIBS_REL="libs/quarto-contrib/blendtutor-$LIB_VERSION"

if ! command -v quarto &>/dev/null; then
  echo "SKIP: quarto not installed (CI installs via quarto-dev/quarto-actions/setup@v2)"
  exit 0
fi

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

count_occurrences() {
  local content="$1" token="$2"
  printf '%s' "$content" | grep -oF "$token" | wc -l | tr -d ' ' || true
}

has_token() {
  local content="$1" token="$2"
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

# Copy the whole extension dir (lua + assets) into a hermetic TMP project at
# the org/repo install path (matches the P2 convention in
# test_quarto_install_render.sh — the only permitted copy target).
setup_extension() {
  local tmpdir="$1"
  mkdir -p "$tmpdir/_extensions/mcmullarkey/blendtutor"
  cp -r _extensions/blendtutor/assets "$tmpdir/_extensions/mcmullarkey/blendtutor/assets"
  cp "$LUA_FILTER" "$tmpdir/_extensions/mcmullarkey/blendtutor/blendtutor.lua"
}

# ---------------------------------------------------------------------------
# Clause 1: Mechanism pin (static — blendtutor.lua + _extension.yml)
# ---------------------------------------------------------------------------

echo "== Clause 1: mechanism pin (single add_html_dependency, no scripts=, no yml resources) =="

LUA_CONTENT=$(cat "$LUA_FILTER")
YML_CONTENT=$(cat "$EXTENSION_YML")

ADEP_COUNT=$(count_occurrences "$LUA_CONTENT" "quarto.doc.add_html_dependency({")
if [ "$ADEP_COUNT" -eq 1 ]; then
  ok "exactly one quarto.doc.add_html_dependency call ($ADEP_COUNT found)"
else
  ko "exactly one quarto.doc.add_html_dependency call — expected 1, found $ADEP_COUNT"
fi

if has_token "$LUA_CONTENT" 'name = "blendtutor"' \
  && has_token "$LUA_CONTENT" "version = BT_DEP_VERSION" \
  && has_token "$LUA_CONTENT" "stylesheets = {" \
  && has_token "$LUA_CONTENT" "resources = {"; then
  ok "dependency table pins {name, version=BT_DEP_VERSION, stylesheets, resources}"
else
  ko "dependency table pins {name, version=BT_DEP_VERSION, stylesheets, resources} — missing one or more keys"
fi

if has_token "$LUA_CONTENT" "scripts ="; then
  ko "NO scripts= key — found scripts= in blendtutor.lua (classic-tag trap)"
else
  ok "no scripts= key in blendtutor.lua"
fi

if has_token "$YML_CONTENT" "resources:"; then
  ko "NO _extension.yml resources: key — found resources: in _extension.yml"
else
  ok "no resources: key in _extension.yml"
fi

if has_token "$LUA_CONTENT" "STYLES_CSS_PATH" || has_token "$LUA_CONTENT" "css_link"; then
  ko "old css_link include_text removed — STYLES_CSS_PATH/css_link still present in blendtutor.lua"
else
  ok "old css_link include_text removed (no STYLES_CSS_PATH / css_link)"
fi

# ---------------------------------------------------------------------------
# Clause 2: Conditional JS resources (static pin — mirrors AC-3 imports)
# ---------------------------------------------------------------------------

echo "== Clause 2: conditional JS resources (static pin) =="

if has_token "$LUA_CONTENT" '"assets/exercise-runtime.js"' \
  && has_token "$LUA_CONTENT" '"assets/codemirror.js"'; then
  ok "exercise-runtime.js + codemirror.js always in resources"
else
  ko "exercise-runtime.js + codemirror.js always in resources — missing one or both"
fi

if has_token "$LUA_CONTENT" '"assets/webr-adapter.js"' && has_token "$LUA_CONTENT" "has_r"; then
  ok "webr-adapter.js conditionally included (has_r)"
else
  ko "webr-adapter.js conditionally included — missing file or has_r condition"
fi

if has_token "$LUA_CONTENT" '"assets/pyodide-adapter.js"' && has_token "$LUA_CONTENT" "has_python"; then
  ok "pyodide-adapter.js conditionally included (has_python)"
else
  ko "pyodide-adapter.js conditionally included — missing file or has_python condition"
fi

# ---------------------------------------------------------------------------
# Clause 3: Deployed to libs + conditional adapters (behavioral, fixtures)
# ---------------------------------------------------------------------------

echo "== Clause 3: files exist on disk at <stem>_files/libs/... (conditional adapters) =="

render_to_html() {
  local input="$1"
  quarto render "$input" --to html 2>&1
}

LIBS_DIR="$FIXTURE_DIR/mixed-lang_files/$LIBS_REL"
rm -rf "$FIXTURE_DIR/mixed-lang_files"
render_to_html "$FIXTURE_DIR/mixed-lang.qmd" >/dev/null 2>&1 || true

if [ ! -d "$LIBS_DIR" ]; then
  ko "mixed-lang libs dir exists — missing: $LIBS_DIR"
else
  for f in exercise-runtime.js codemirror.js styles.css webr-adapter.js pyodide-adapter.js; do
    if [ -f "$LIBS_DIR/$f" ]; then
      ok "mixed-lang libs contains $f"
    else
      ko "mixed-lang libs contains $f — missing on disk (deployment broke or silently skipped)"
    fi
  done
fi

rm -rf "$FIXTURE_DIR/r-only_files"
render_to_html "$FIXTURE_DIR/r-only.qmd" >/dev/null 2>&1 || true
R_LIBS="$FIXTURE_DIR/r-only_files/$LIBS_REL"
if [ ! -d "$R_LIBS" ]; then
  ko "r-only libs dir exists — missing: $R_LIBS"
else
  if [ -f "$R_LIBS/webr-adapter.js" ]; then
    ok "r-only libs contains webr-adapter.js (has_r)"
  else
    ko "r-only libs contains webr-adapter.js — missing"
  fi
  if [ -f "$R_LIBS/pyodide-adapter.js" ]; then
    ko "r-only libs ABSENT pyodide-adapter.js — deployed despite no python"
  else
    ok "r-only libs absent pyodide-adapter.js (no python)"
  fi
fi

rm -rf "$FIXTURE_DIR/pyodide_files"
render_to_html "$FIXTURE_DIR/pyodide.qmd" >/dev/null 2>&1 || true
P_LIBS="$FIXTURE_DIR/pyodide_files/$LIBS_REL"
if [ ! -d "$P_LIBS" ]; then
  ko "pyodide libs dir exists — missing: $P_LIBS"
else
  if [ -f "$P_LIBS/pyodide-adapter.js" ]; then
    ok "pyodide libs contains pyodide-adapter.js (has_python)"
  else
    ko "pyodide libs contains pyodide-adapter.js — missing"
  fi
  if [ -f "$P_LIBS/webr-adapter.js" ]; then
    ko "pyodide libs ABSENT webr-adapter.js — deployed despite no r"
  else
    ok "pyodide libs absent webr-adapter.js (no r)"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 4: CSS via Quarto link (exactly one, libs-dir href)
# ---------------------------------------------------------------------------

echo "== Clause 4: CSS via Quarto link (exactly one, no source-tree link) =="

if [ ! -f "$FIXTURE_DIR/mixed-lang.html" ]; then
  ko "exactly one styles.css link — HTML missing"
  ko "no _extensions styles.css link — HTML missing"
else
  MIXED_CONTENT=$(cat "$FIXTURE_DIR/mixed-lang.html")
  CSS_LIB_HREF="mixed-lang_files/$LIBS_REL/styles.css"
  CSS_LINK_COUNT=$(count_occurrences "$MIXED_CONTENT" "$CSS_LIB_HREF")
  if [ "$CSS_LINK_COUNT" -eq 1 ]; then
    ok "exactly one libs styles.css link ($CSS_LINK_COUNT found)"
  else
    ko "exactly one libs styles.css link — expected 1, found $CSS_LINK_COUNT"
  fi
  if has_token "$MIXED_CONTENT" '_extensions/blendtutor/assets/styles.css'; then
    ko "no _extensions styles.css link — found source-tree link"
  else
    ok "no _extensions/blendtutor/assets/styles.css link in HTML"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 5: Bootstrap specifiers rewritten to libs URLs
# ---------------------------------------------------------------------------

echo "== Clause 5: bootstrap specifiers are libs URLs from output_file stem =="

if [ ! -f "$FIXTURE_DIR/mixed-lang.html" ]; then
  ko "bootstrap specifiers libs URLs — HTML missing"
else
  MIXED_BOOTSTRAP=$(extract_bootstrap "$(cat "$FIXTURE_DIR/mixed-lang.html")")
  LIBS_PREFIX="mixed-lang_files/$LIBS_REL"
  for f in exercise-runtime.js webr-adapter.js pyodide-adapter.js; do
    if has_token "$MIXED_BOOTSTRAP" "$LIBS_PREFIX/$f"; then
      ok "bootstrap imports $f from libs URL"
    else
      ko "bootstrap imports $f from libs URL — $LIBS_PREFIX/$f not found"
    fi
  done
  # ES module specifiers MUST start with ./ (bare relative references throw
  # "Relative references must start with /, ./, or ../" at import time —
  # rodney clause 11 caught this; <link href> tolerates bare paths, modules
  # do not).
  if has_token "$MIXED_BOOTSTRAP" 'from "./mixed-lang_files/libs/quarto-contrib/blendtutor-0.1.0/exercise-runtime.js"'; then
    ok "runtime specifier is ES-module-safe (./ prefix)"
  else
    ko "runtime specifier is ES-module-safe — missing ./ prefix"
  fi
  if has_token "$MIXED_BOOTSTRAP" "_extensions/"; then
    ko "no _extensions/ substring in bootstrap — found source-tree specifier"
  else
    ok "no _extensions/ substring in bootstrap specifiers"
  fi
  if printf '%s' "$MIXED_BOOTSTRAP" | grep -qE "assets/(exercise-runtime|webr-adapter|pyodide-adapter)\.js"; then
    ko "no resolve_asset_path output in specifiers — bare assets/ path found"
  else
    ok "no resolve_asset_path output in specifiers"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 6: No classic runtime script tag
# ---------------------------------------------------------------------------

echo "== Clause 6: no classic runtime script tag =="

if [ ! -f "$FIXTURE_DIR/mixed-lang.html" ]; then
  ko "no classic runtime script tag — HTML missing"
else
  if printf '%s' "$(cat "$FIXTURE_DIR/mixed-lang.html")" | grep -qE '<script src="[^"]*exercise-runtime\.js"'; then
    ko "no classic runtime script tag — classic <script src=...exercise-runtime.js> found"
  else
    ok "no classic <script src=...exercise-runtime.js> tag"
  fi
fi

# ---------------------------------------------------------------------------
# Clause 7: COI boundary (coi-serviceworker.js stays source-tree include_text)
# ---------------------------------------------------------------------------

echo "== Clause 7: coi-serviceworker.js stays source-tree (NOT in libs) =="

TMP_COI=$(mktemp -d)
trap 'rm -rf "$TMP_COI"' EXIT
setup_extension "$TMP_COI"

cat > "$TMP_COI/coi.qmd" <<'QMD'
---
title: COI deployment fixture
filters: [_extensions/mcmullarkey/blendtutor/blendtutor.lua]
---

::: {.blendtutor language="r" coi="true"}
Write a function `add(a, b)`.

```r
add <- function(a, b) { a + b }
```
:::
QMD

( cd "$TMP_COI" && quarto render coi.qmd --to html ) >/dev/null 2>&1 || true

COI_HTML="$TMP_COI/coi.html"
COI_LIBS="$TMP_COI/coi_files/$LIBS_REL"
if [ ! -f "$COI_HTML" ]; then
  ko "coi stays source-tree — HTML missing"
  ko "coi libs absent coi-serviceworker.js — HTML missing"
else
  if has_token "$(cat "$COI_HTML")" '_extensions/mcmullarkey/blendtutor/assets/coi-serviceworker.js'; then
    ok "coi script src still source-tree resolve_asset_path path"
  else
    ko "coi script src still source-tree — _extensions/.../coi-serviceworker.js not found"
  fi
  if [ -d "$COI_LIBS" ] && [ -f "$COI_LIBS/coi-serviceworker.js" ]; then
    ko "coi-serviceworker.js NOT in libs — found in libs dir (SW scope break)"
  else
    ok "coi-serviceworker.js not in libs dir"
  fi
fi
rm -rf "$TMP_COI"

# ---------------------------------------------------------------------------
# Clause 8: Stem correctness — nested pages/ hermetic render + multi-page book
# ---------------------------------------------------------------------------

echo "== Clause 8: nested pages/ stem + multi-page book render =="

TMP_NEST=$(mktemp -d)
setup_extension "$TMP_NEST"
mkdir -p "$TMP_NEST/pages"

cat > "$TMP_NEST/pages/index.qmd" <<'QMD'
---
title: Nested deployment fixture
filters: [../_extensions/mcmullarkey/blendtutor/blendtutor.lua]
---

::: {.blendtutor language="r"}
Write a function `add(a, b)`.

```r
add <- function(a, b) { a + b }
```
:::
QMD

NEST_OUTPUT=$( ( cd "$TMP_NEST" && quarto render pages/index.qmd --to html ) 2>&1 ) && NEST_RC=0 || NEST_RC=$?
if [ "$NEST_RC" -eq 0 ]; then
  ok "nested pages/ render exits 0"
else
  ko "nested pages/ render exits 0 — exit code $NEST_RC"
  echo "  render output: $NEST_OUTPUT" >&2
fi

NEST_LIBS="$TMP_NEST/pages/index_files/$LIBS_REL"
if [ -d "$NEST_LIBS" ] && [ -f "$NEST_LIBS/exercise-runtime.js" ]; then
  ok "nested libs at pages/index_files/$LIBS_REL (stem + subdir correct)"
else
  ko "nested libs at pages/index_files/$LIBS_REL — missing"
fi

if [ -f "$TMP_NEST/pages/index.html" ]; then
  NEST_BOOTSTRAP=$(extract_bootstrap "$(cat "$TMP_NEST/pages/index.html")")
  if has_token "$NEST_BOOTSTRAP" "./index_files/$LIBS_REL/exercise-runtime.js"; then
    ok "nested bootstrap specifier document-relative (./index_files/...)"
  else
    ko "nested bootstrap specifier document-relative — ./index_files/... not found"
  fi
  if has_token "$NEST_BOOTSTRAP" "pages/index_files"; then
    ko "nested bootstrap specifier has no pages/ prefix"
  else
    ok "nested bootstrap specifier has no pages/ prefix"
  fi
fi
rm -rf "$TMP_NEST"

BOOK_OUTPUT=$( ( quarto render "$FIXTURE_DIR/coi-book" --to html ) 2>&1 ) && BOOK_RC=0 || BOOK_RC=$?
if [ "$BOOK_RC" -eq 0 ]; then
  ok "coi-book multi-page render exits 0"
else
  ko "coi-book multi-page render exits 0 — exit code $BOOK_RC"
  echo "  render output: $BOOK_OUTPUT" >&2
fi

# ---------------------------------------------------------------------------
# Clause 8b (issue #143): BOOK project → site_libs URLs; standalone +
# output-dir → <stem>_files form UNCHANGED (discriminator pin).
# Quarto book projects (type: book, output-dir) consolidate ALL
# add_html_dependency resources into the SHARED _output/site_libs/ dir
# (bookProjectType.libDir = "site_libs", verified quarto 1.10.18) — the
# filter's libs_url() must emit ./site_libs/... specifiers there. Standalone
# renders (type: default) keep <stem>_files/libs/... even when output-dir is
# set — quarto.project.output_directory == directory for standalone, and the
# deployed libs land beside the output document, never in site_libs.
# ---------------------------------------------------------------------------

echo "== Clause 8b: book project → site_libs URLs; standalone+output-dir → _files =="

TMP_BOOK=$(mktemp -d)
setup_extension "$TMP_BOOK"
cat > "$TMP_BOOK/_quarto.yml" <<'YML'
project:
  type: book
  output-dir: _output
book:
  title: Book fixture
  chapters:
    - index.qmd
YML
cat > "$TMP_BOOK/index.qmd" <<'QMD'
---
title: R chapter
filters: [_extensions/mcmullarkey/blendtutor/blendtutor.lua]
---

::: {.blendtutor language="r"}
Write `add(a, b)`.

```r
add <- function(a, b) { a + b }
```
:::
QMD
BOOK2_OUTPUT=$( ( cd "$TMP_BOOK" && quarto render --to html ) 2>&1 ) && BOOK2_RC=0 || BOOK2_RC=$?
if [ "$BOOK2_RC" -eq 0 ]; then
  ok "book fixture render exits 0"
else
  ko "book fixture render exits 0 — exit code $BOOK2_RC"
  echo "  render output: $BOOK2_OUTPUT" >&2
fi

BOOK_PAGE="$TMP_BOOK/_output/index.html"
# Book mode has NO <stem>_files/libs/ segment — libs deploy directly under
# _output/site_libs/quarto-contrib/<name>-<version>/.
BOOK_LIBS_REL="quarto-contrib/blendtutor-$LIB_VERSION"
BOOK_SITE_LIBS="$TMP_BOOK/_output/site_libs/$BOOK_LIBS_REL"
if [ -f "$BOOK_PAGE" ]; then
  BOOK_BOOTSTRAP=$(extract_bootstrap "$(cat "$BOOK_PAGE")")
  if has_token "$BOOK_BOOTSTRAP" "./site_libs/$BOOK_LIBS_REL/exercise-runtime.js"; then
    ok "book bootstrap specifier uses ./site_libs/... (book-aware libs_url)"
  else
    ko "book bootstrap specifier uses ./site_libs/... — not found: $BOOK_BOOTSTRAP"
  fi
  if printf '%s' "$BOOK_BOOTSTRAP" | grep -qF "_files/libs/"; then
    ko "book bootstrap has NO <stem>_files/ specifier — found _files/libs/ path"
  else
    ok "book bootstrap specifier has no <stem>_files/ path"
  fi
  if [ -d "$BOOK_SITE_LIBS" ]; then
    for f in exercise-runtime.js codemirror.js styles.css webr-adapter.js; do
      if [ -f "$BOOK_SITE_LIBS/$f" ]; then
        ok "book site_libs contains $f"
      else
        ko "book site_libs contains $f — missing"
      fi
    done
    if find "$TMP_BOOK/_output" -maxdepth 1 -type d -name '*_files' | grep -q .; then
      ko "book render creates NO <stem>_files dirs under _output — found *_files dir"
    else
      ok "book render creates no <stem>_files dirs under _output (site_libs only)"
    fi
  else
    ko "book site_libs dir exists — missing: $BOOK_SITE_LIBS"
  fi
else
  ko "book page rendered — _output/index.html missing"
fi
rm -rf "$TMP_BOOK"

TMP_OUTDIR=$(mktemp -d)
setup_extension "$TMP_OUTDIR"
cat > "$TMP_OUTDIR/index.qmd" <<'QMD'
---
project:
  type: default
  output-dir: build-out
title: Standalone with output-dir fixture
filters: [_extensions/mcmullarkey/blendtutor/blendtutor.lua]
---

::: {.blendtutor language="r"}
Write `add(a, b)`.

```r
add <- function(a, b) { a + b }
```
:::
QMD
OUTDIR_OUTPUT=$( ( cd "$TMP_OUTDIR" && quarto render index.qmd --to html ) 2>&1 ) && OUTDIR_RC=0 || OUTDIR_RC=$?
if [ "$OUTDIR_RC" -eq 0 ]; then
  ok "standalone+output-dir render exits 0"
else
  ko "standalone+output-dir render exits 0 — exit code $OUTDIR_RC"
  echo "  render output: $OUTDIR_OUTPUT" >&2
fi

if [ -f "$TMP_OUTDIR/index.html" ]; then
  OUTDIR_BOOTSTRAP=$(extract_bootstrap "$(cat "$TMP_OUTDIR/index.html")")
  if has_token "$OUTDIR_BOOTSTRAP" "./index_files/$LIBS_REL/exercise-runtime.js"; then
    ok "standalone+output-dir keeps <stem>_files specifier (discriminator: not site_libs)"
  else
    ko "standalone+output-dir keeps <stem>_files specifier — got: $OUTDIR_BOOTSTRAP"
  fi
  if printf '%s' "$OUTDIR_BOOTSTRAP" | grep -qF "site_libs/"; then
    ko "standalone+output-dir has NO site_libs specifier — found site_libs/ path"
  else
    ok "standalone+output-dir specifier has no site_libs/ path"
  fi
  if [ -d "$TMP_OUTDIR/index_files/$LIBS_REL" ]; then
    ok "standalone+output-dir libs deployed at index_files/libs (unchanged)"
  else
    ko "standalone+output-dir libs deployed at index_files/libs — missing"
  fi
else
  ko "standalone+output-dir page rendered — index.html missing"
fi
rm -rf "$TMP_OUTDIR"

# ---------------------------------------------------------------------------
# Clause 9: Non-HTML gate — hermetic latex render
# ---------------------------------------------------------------------------

echo "== Clause 9: non-HTML gate (latex — zero libs dirs, zero bootstrap) =="

TMP_LATEX=$(mktemp -d)
setup_extension "$TMP_LATEX"

cat > "$TMP_LATEX/index.qmd" <<'QMD'
---
title: Latex gate fixture
filters: [_extensions/mcmullarkey/blendtutor/blendtutor.lua]
---

::: {.blendtutor language="r"}
Write a function `add(a, b)`.

```r
add <- function(a, b) { a + b }
```
:::
QMD

LATEX_OUTPUT=$( ( cd "$TMP_LATEX" && quarto render index.qmd --to latex ) 2>&1 ) && LATEX_RC=0 || LATEX_RC=$?
if [ "$LATEX_RC" -eq 0 ]; then
  ok "latex render exits 0"
else
  ko "latex render exits 0 — exit code $LATEX_RC"
  echo "  render output: $LATEX_OUTPUT" >&2
fi

if find "$TMP_LATEX" -type d -name "libs" | grep -q .; then
  ko "zero libs dirs on latex — found *_files/libs/ dir"
else
  ok "zero libs dirs created on latex render"
fi

if [ -f "$TMP_LATEX/index.tex" ]; then
  if grep -qF "$MARKER" "$TMP_LATEX/index.tex"; then
    ko "zero bootstrap injection on latex — found data-bt-bootstrap in tex"
  else
    ok "zero bootstrap injection on latex"
  fi
fi
rm -rf "$TMP_LATEX"

# ---------------------------------------------------------------------------
# Clause 10: Version pin single-sourced
# ---------------------------------------------------------------------------

echo "== Clause 10: BT_DEP_VERSION single-sourced =="

if has_token "$LUA_CONTENT" 'BT_DEP_VERSION = "0.1.0"'; then
  ok "BT_DEP_VERSION = \"0.1.0\" constant in blendtutor.lua"
else
  ko "BT_DEP_VERSION = \"0.1.0\" — constant missing or drifted"
fi

if has_token "$LUA_CONTENT" 'version = BT_DEP_VERSION'; then
  ok "dependency declaration uses BT_DEP_VERSION"
else
  ko "dependency declaration uses BT_DEP_VERSION — version not tied to constant"
fi

if has_token "$LUA_CONTENT" 'quarto-contrib/blendtutor-" .. BT_DEP_VERSION'; then
  ok "emitted libs URL uses BT_DEP_VERSION"
else
  ko "emitted libs URL uses BT_DEP_VERSION — URL not tied to constant"
fi

if [ "$(sed -n '3p' "$EXTENSION_YML" | tr -d ' ')" = "version:0.1.0" ]; then
  ok "_extension.yml line 3 version = 0.1.0 (parity with BT_DEP_VERSION)"
else
  ko "_extension.yml line 3 version = 0.1.0 — got: $(sed -n '3p' "$EXTENSION_YML")"
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
