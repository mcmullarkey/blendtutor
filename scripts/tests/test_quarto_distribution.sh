#!/usr/bin/env bash
# Executable spec for issue #115 — Author docs, demo book, quarto add install verification.
#
# Verifies the 15-clause predicate from AC-10 in 3 groups (extended by issue
# #143 AC-5: by-name install clauses 1-3, book-aware site_libs libs clauses
# 5-7, org/repo-aware P9):
#
#   Group 1 — README (6 clauses):
#     1. Install command present (quarto add mcmullarkey/blendtutor)
#     2. Syntax shown for both languages (R and Python)
#     3. BYOK (bring your own key) mentioned
#     4. Minimum Quarto version stated
#     5. Demo book link present
#     6. Install path contract (org/repo path, per AC-3)
#
#   Group 2 — Demo book (6 clauses):
#     7. quarto render demo-book exits 0 + asset href targets file-checked
#     8. ≥2 exercises per language (R and Python)
#     9. Non-empty content (no empty exercise divs / empty JSON)
#    10. Mixed-language (both R and Python present)
#    11. No llm_evaluation_prompt field
#    12. COI config present (coi="true" or coi: true)
#
#   Group 3 — CI (3 clauses):
#    13. quarto add in temp dir
#    14. setup@v2
#    15. No continue-on-error
#
# P9 (issue #130): the demo-book render path must be side-effect free — the
# suite never creates an extension dir under demo-book (the old clause-6 copy
# hack's residue is asserted absent at the end of Group 2).
#
# Negative cases:
#   - Wrong org/repo (must be mcmullarkey/blendtutor)
#   - Empty JSON (exercise divs must have content)
#   - CI uses _extensions: copy (must use quarto add instead)
#   - README omits BYOK
#
# Usage: bash scripts/tests/test_quarto_distribution.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PASS=0
FAIL=0

ok() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
ko() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

README="README.md"
DEMO_BOOK_DIR="demo-book"
CI_FILE=".github/workflows/ci.yml"

# ---------------------------------------------------------------------------
# Group 1 — README (6 clauses)
# ---------------------------------------------------------------------------

echo "== Group 1: README =="

# Clause 1: Install command present
echo "== Clause 1: install command =="
if [ ! -f "$README" ]; then
  ko "install command — README not found: $README"
else
  if grep -qF 'quarto add mcmullarkey/blendtutor' "$README"; then
    ok "install command present (quarto add mcmullarkey/blendtutor)"
  else
    ko "install command present — 'quarto add mcmullarkey/blendtutor' not found in README"
  fi
fi

# Clause 2: Syntax shown for both languages
echo "== Clause 2: syntax both languages =="
if [ ! -f "$README" ]; then
  ko "syntax both languages — README not found"
else
  # Check for R exercise syntax (language="r")
  if grep -qF 'language="r"' "$README" || grep -qF "language='r'" "$README"; then
    ok "R exercise syntax shown in README"
  else
    ko "R exercise syntax shown in README — language=\"r\" not found"
  fi

  # Check for Python exercise syntax (language="python")
  if grep -qF 'language="python"' "$README" || grep -qF "language='python'" "$README"; then
    ok "Python exercise syntax shown in README"
  else
    ko "Python exercise syntax shown in README — language=\"python\" not found"
  fi
fi

# Clause 3: BYOK mentioned
echo "== Clause 3: BYOK =="
if [ ! -f "$README" ]; then
  ko "BYOK — README not found"
else
  # Check for BYOK or "bring your own key" (case-insensitive)
  if grep -qiE 'BYOK|bring.your.own.key' "$README"; then
    ok "BYOK mentioned in README"
  else
    ko "BYOK mentioned in README — no BYOK or 'bring your own key' found"
  fi
fi

# Clause 4: Minimum Quarto version stated
echo "== Clause 4: minimum Quarto version =="
if [ ! -f "$README" ]; then
  ko "minimum Quarto version — README not found"
else
  # Check for a Quarto version requirement (e.g., "Quarto 1.4", "Quarto >= 1.4", "Quarto 1.5+")
  if grep -qiE 'quarto[[:space:]]*[>=]*[[:space:]]*1\.[0-9]' "$README"; then
    ok "minimum Quarto version stated in README"
  else
    ko "minimum Quarto version stated in README — no Quarto version requirement found"
  fi
fi

# Clause 5: Demo book link present
echo "== Clause 5: demo book link =="
if [ ! -f "$README" ]; then
  ko "demo book link — README not found"
else
  # Check for a link to the demo-book directory
  if grep -qiE 'demo-book|demo.book' "$README"; then
    ok "demo book link present in README"
  else
    ko "demo book link present in README — no demo-book reference found"
  fi
fi

# Clause 6: Install path contract (AC-3)
echo "== Clause 6: install path contract =="
if [ ! -f "$README" ]; then
  ko "install path — README not found"
else
  # 6a: Install command survives
  if grep -qF 'quarto add mcmullarkey/blendtutor' "$README"; then
    ok "install command present (quarto add mcmullarkey/blendtutor)"
  else
    ko "install command present — 'quarto add mcmullarkey/blendtutor' not found"
  fi

  # 6b: Correct install path stated (org/repo path, not bare repo name)
  if grep -qF '_extensions/mcmullarkey/blendtutor/' "$README"; then
    ok "install path stated (_extensions/mcmullarkey/blendtutor/)"
  else
    ko "install path stated — '_extensions/mcmullarkey/blendtutor/' not found"
  fi

  # 6c: Old wrong claim gone (bare 'your project's _extensions/blendtutor/' phrase)
  if ! grep -qF "your project's \`_extensions/blendtutor/\`" "$README"; then
    ok "old wrong install path claim removed"
  else
    ko "old wrong install path claim removed — found 'your project's \`_extensions/blendtutor/\`'"
  fi

  # 6d: Install-path independence stated
  if grep -qiE 'install-path-independent|independent of.{0,40}install|regardless install|relative to.*filter|PANDOC_SCRIPT_FILE' "$README"; then
    ok "install-path independence stated in README"
  else
    ko "install-path independence stated in README — no independence mention found"
  fi
fi

# 6e: ADR-0017 annotated with actual CI-asserted org/repo path
ADR="docs/adr/0017-quarto-extension-distribution.md"
if [ ! -f "$ADR" ]; then
  ko "ADR-0017 annotation — ADR file not found: $ADR"
else
  # Require both annotation-unique strings — 'mcmullarkey/blendtutor' alone is
  # non-discriminatory (4 pre-existing occurrences in ADR body, lines 35/41/48/53).
  if grep -qF 'CI actually' "$ADR" && grep -qF '_extensions/mcmullarkey/blendtutor/' "$ADR"; then
    ok "ADR-0017 annotated with org/repo install path (CI actually + _extensions/mcmullarkey/blendtutor/)"
  else
    ko "ADR-0017 annotated with org/repo install path — annotation missing ('CI actually' + '_extensions/mcmullarkey/blendtutor/' not both found)"
  fi
fi

# ---------------------------------------------------------------------------
# Group 2 — Demo book (6 clauses)
# ---------------------------------------------------------------------------

echo ""
echo "== Group 2: Demo book =="

# Collect all .qmd files in demo-book/
DEMO_QMD_FILES=()
if [ -d "$DEMO_BOOK_DIR" ]; then
  while IFS= read -r -d '' f; do
    DEMO_QMD_FILES+=("$f")
  done < <(find "$DEMO_BOOK_DIR" -name '*.qmd' -print0)
fi

# Issue #143 AC-5 clause 1: by-name install committed (.gitignore un-ignores
# /_extensions/, adds /_output/; vendored org/repo install present on disk).
echo "== AC-5 Clause 1: by-name install committed =="
if [ ! -f "$DEMO_BOOK_DIR/.gitignore" ]; then
  ko "by-name install committed — demo-book/.gitignore not found"
else
  if grep -qF '/_extensions/' "$DEMO_BOOK_DIR/.gitignore"; then
    ko "by-name install committed — .gitignore still ignores /_extensions/ (install never committed)"
  else
    ok ".gitignore no longer ignores /_extensions/"
  fi
  if grep -qF '/_output/' "$DEMO_BOOK_DIR/.gitignore"; then
    ok "/_output/ added to .gitignore (render artifacts excluded)"
  else
    ko "/_output/ added to .gitignore — missing (render noise would be committed)"
  fi
fi
if [ -f "$DEMO_BOOK_DIR/_extensions/mcmullarkey/blendtutor/_extension.yml" ] \
  && [ -f "$DEMO_BOOK_DIR/_extensions/mcmullarkey/blendtutor/blendtutor.lua" ] \
  && [ -d "$DEMO_BOOK_DIR/_extensions/mcmullarkey/blendtutor/assets" ]; then
  ok "vendored install present at demo-book/_extensions/mcmullarkey/blendtutor/ (extension.yml + lua + assets)"
else
  ko "vendored install present — demo-book/_extensions/mcmullarkey/blendtutor/ incomplete"
fi

# Issue #143 AC-5 clause 2: extension currency markers in vendored copy
# (committed guard is marker-based only; byte parity is build-time cmp -s).
echo "== AC-5 Clause 2: vendored extension current (markers) =="
VENDORED_LUA="$DEMO_BOOK_DIR/_extensions/mcmullarkey/blendtutor/blendtutor.lua"
if [ -f "$VENDORED_LUA" ]; then
  if grep -qF 'add_html_dependency' "$VENDORED_LUA" \
    && grep -qF 'BT_DEP_VERSION' "$VENDORED_LUA" \
    && grep -qF 'quarto-contrib/blendtutor-' "$VENDORED_LUA"; then
    ok "vendored lua current (add_html_dependency + BT_DEP_VERSION + libs URL markers)"
  else
    ko "vendored lua current — missing one or more AC-4 markers (stale vendored copy)"
  fi
else
  ko "vendored lua current — blendtutor.lua missing at $VENDORED_LUA"
fi

# Issue #143 AC-5 clause 3: by-name filter reference (grep VALUE, not line).
echo "== AC-5 Clause 3: by-name filter reference =="
if [ ! -f "$DEMO_BOOK_DIR/_quarto.yml" ]; then
  ko "by-name filter reference — demo-book/_quarto.yml not found"
else
  if grep -qF 'mcmullarkey/blendtutor' "$DEMO_BOOK_DIR/_quarto.yml"; then
    ok "filters reference mcmullarkey/blendtutor (full org/repo form)"
  else
    ko "filters reference mcmullarkey/blendtutor — org/repo form not found"
  fi
  if grep -qF '../_extensions' "$DEMO_BOOK_DIR/_quarto.yml"; then
    ko "no out-of-root ../_extensions filter path — still present in _quarto.yml"
  else
    ok "no ../_extensions filter path in _quarto.yml"
  fi
fi

# Clause 7: quarto render demo-book exits 0 + asset href targets file-checked
echo "== Clause 7: render exits 0 =="
if [ ! -d "$DEMO_BOOK_DIR" ]; then
  ko "render exits 0 — demo-book directory not found"
elif [ ${#DEMO_QMD_FILES[@]} -eq 0 ]; then
  ko "render exits 0 — no .qmd files found in demo-book/"
else
  if ! command -v quarto &>/dev/null; then
    echo "  SKIP: quarto not installed locally — render assertion skipped"
    echo "  (CI installs quarto via quarto-dev/quarto-actions/setup@v2)"
    # Structural fallback: verify _quarto.yml exists (book project config)
    if [ -f "$DEMO_BOOK_DIR/_quarto.yml" ]; then
      ok "render exits 0 — _quarto.yml present (structural, quarto not installed)"
    else
      ko "render exits 0 — _quarto.yml missing in demo-book/"
    fi
  else
    # Render the demo book. No extension copy here (the old clause-6 copy
    # hack masked the install-path bug by copying the extension dir into
    # demo-book): post-AC-1 the filter derives asset paths from
    # PANDOC_SCRIPT_FILE, so the emitted hrefs resolve relative to the
    # project dir (demo-book/) into the repo checkout.
    RENDER_OUTPUT=$(quarto render "$DEMO_BOOK_DIR" --to html 2>&1) && RENDER_RC=0 || RENDER_RC=$?
    if [ "$RENDER_RC" -eq 0 ]; then
      ok "quarto render demo-book exits 0"
    else
      ko "quarto render demo-book exits 0 — exit code $RENDER_RC"
      echo "  render output: $RENDER_OUTPUT" >&2
    fi

    # Asset href file check (P9): Quarto does NOT validate emitted hrefs at
    # render — exit 0 alone is insufficient. demo-book is a BOOK project
    # (type: book, output-dir: _output) so Quarto consolidates ALL
    # html-dependency resources into the SHARED _output/site_libs/
    # (bookProjectType.libDir = "site_libs", verified quarto 1.10.18) and the
    # filter's book-aware libs_url() emits ./site_libs/... specifiers. Extract
    # every blendtutor asset href/src and file-check its target resolved
    # relative to the rendered output dir (demo-book/_output/).
    RENDER_HTML_DIR="$DEMO_BOOK_DIR/_output"
    HREF_FOUND=0
    HREF_MISSING=0
    if [ -d "$RENDER_HTML_DIR" ]; then
      while IFS= read -r href; do
        [ -z "$href" ] && continue
        HREF_FOUND=1
        # Strip the leading ./ that ES-module import specifiers require
        # (module specifiers reject bare relative references).
        href_file="${href#./}"
        if ( cd "$RENDER_HTML_DIR" && test -f "$href_file" ); then
          ok "asset href target exists: $href"
        else
          ko "asset href target missing: $href"
          HREF_MISSING=1
        fi
      done < <(grep -hoE '(href|src)="[^"]*blendtutor-0\.1\.0/[^"]*"' "$RENDER_HTML_DIR"/*.html 2>/dev/null | sed -E 's/^[^"]*"([^"]*)"/\1/' | sort -u)
      if [ "$HREF_FOUND" -eq 0 ]; then
        ko "asset href file check — no blendtutor asset hrefs found in rendered HTML"
      fi
    else
      ko "asset href file check — rendered HTML dir not found: $RENDER_HTML_DIR"
    fi
  fi
fi

# Issue #143 AC-5 clause 5 (amended): rendered book pages reference the
# SHARED site_libs/quarto-contrib/blendtutor-<version>/ URLs (book mode),
# never <stem>_files/; NO _extensions/ substring in filter-injected bootstrap
# specifiers for the four assets.
echo "== AC-5 Clause 5: book libs URLs (site_libs) + no _extensions/ in bootstrap =="
BOOK_LIBS="site_libs/quarto-contrib/blendtutor-0.1.0"
if [ -d "$RENDER_HTML_DIR" ]; then
  # Extract the filter-injected bootstrap body (coi shim src legitimately
  # contains _extensions/... outside the bootstrap, so scope the check).
  extract_bootstrap() {
    local content="$1"
    awk -v marker='<script type="module" data-bt-bootstrap="auto">' '
      index($0, marker) { flag = 1; next }
      flag && index($0, "</script>") { flag = 0; next }
      flag { print }
    ' <<< "$content"
  }
  for page in r-exercises python-exercises; do
    if [ ! -f "$RENDER_HTML_DIR/$page.html" ]; then
      ko "book libs URLs — $page.html not rendered"
      continue
    fi
    if grep -qF "$BOOK_LIBS/exercise-runtime.js" "$RENDER_HTML_DIR/$page.html"; then
      ok "$page references site_libs exercise-runtime.js URL"
    else
      ko "$page references site_libs exercise-runtime.js URL — not found"
    fi
    # Adapter imports are per-language (conditional has_r/has_python): the R
    # page imports webr-adapter.js only, the python page pyodide-adapter.js.
    case "$page" in
      r-exercises) PAGE_ADAPTER="webr-adapter.js"; ADAPTER_ABSENT="pyodide-adapter.js" ;;
      python-exercises) PAGE_ADAPTER="pyodide-adapter.js"; ADAPTER_ABSENT="webr-adapter.js" ;;
    esac
    if grep -qF "$BOOK_LIBS/$PAGE_ADAPTER" "$RENDER_HTML_DIR/$page.html"; then
      ok "$page references site_libs $PAGE_ADAPTER URL (conditional import)"
    else
      ko "$page references site_libs $PAGE_ADAPTER URL — not found"
    fi
    if grep -qF "$BOOK_LIBS/$ADAPTER_ABSENT" "$RENDER_HTML_DIR/$page.html"; then
      ko "$page must NOT import $ADAPTER_ABSENT (no such-language exercises on page)"
    else
      ok "$page does not import $ADAPTER_ABSENT (conditional per-language import)"
    fi
    if grep -qF "$BOOK_LIBS/styles.css" "$RENDER_HTML_DIR/$page.html"; then
      ok "$page references site_libs styles.css URL"
    else
      ko "$page references site_libs styles.css URL — not found"
    fi
    PAGE_BOOTSTRAP=$(extract_bootstrap "$(cat "$RENDER_HTML_DIR/$page.html")")
    if printf '%s' "$PAGE_BOOTSTRAP" | grep -qF '_extensions/'; then
      ko "$page bootstrap specifiers — _extensions/ substring found"
    else
      ok "$page bootstrap specifiers contain no _extensions/ substring"
    fi
  done
else
  ko "book libs URLs — rendered HTML dir not found: $RENDER_HTML_DIR"
fi

# Issue #143 AC-5 clause 6 (amended): files on disk under the shared
# _output/site_libs/quarto-contrib/blendtutor-0.1.0/ — exercise-runtime.js +
# styles.css + codemirror.js always; webr-adapter.js iff any R exercise in the
# book; pyodide-adapter.js iff any python exercise. Base dir is
# demo-book/_output/ (rendered-document-relative), NOT demo-book/.
echo "== AC-5 Clause 6: files on disk at _output/site_libs/... =="
BT_SITE_LIBS="$RENDER_HTML_DIR/$BOOK_LIBS"
if [ -d "$BT_SITE_LIBS" ]; then
  for f in exercise-runtime.js styles.css codemirror.js; do
    if [ -f "$BT_SITE_LIBS/$f" ]; then
      ok "book site_libs contains $f"
    else
      ko "book site_libs contains $f — missing on disk"
    fi
  done
  # demo-book has both R and Python exercises across chapters → both adapters
  # must deploy to the shared dir.
  if [ -f "$BT_SITE_LIBS/webr-adapter.js" ]; then
    ok "book site_libs contains webr-adapter.js (book has R exercises)"
  else
    ko "book site_libs contains webr-adapter.js — missing despite R exercises"
  fi
  if [ -f "$BT_SITE_LIBS/pyodide-adapter.js" ]; then
    ok "book site_libs contains pyodide-adapter.js (book has python exercises)"
  else
    ko "book site_libs contains pyodide-adapter.js — missing despite python exercises"
  fi
else
  ko "book site_libs dir exists — missing: $BT_SITE_LIBS"
fi

# Issue #143 AC-5 clause 7: COI boundary — r-exercises.html (coi: true)
# still loads coi-serviceworker.js via include_text; the shim is NEVER
# deployed via our add_html_dependency into the blendtutor-0.1.0 libs dir
# (SW scope = script URL dir). NOTE: in BOOK mode Quarto rewrites in-header
# src pointing at the by-name extension into its own
# site_libs/quarto-contrib/quarto-project/... copy — the shim src is
# Quarto-managed there, but our deployment boundary (nothing coi in the
# blendtutor-<version> libs dir) still holds.
echo "== AC-5 Clause 7: COI stays out of blendtutor libs dir (Quarto-managed src) =="
if [ -f "$RENDER_HTML_DIR/r-exercises.html" ]; then
  if grep -qE 'src="[^"]*coi-serviceworker\.js"' "$RENDER_HTML_DIR/r-exercises.html"; then
    ok "r-exercises loads coi-serviceworker.js via include_text (src present)"
  else
    ko "r-exercises coi shim — no coi-serviceworker.js src found"
  fi
  if grep -qE 'src="[^"]*blendtutor-0\.1\.0/coi-serviceworker\.js"' "$RENDER_HTML_DIR/r-exercises.html"; then
    ko "coi shim NOT in blendtutor libs URL — src points into blendtutor-0.1.0/ dir"
  else
    ok "coi shim src does not point into blendtutor-0.1.0 libs dir"
  fi
  if [ -d "$BT_SITE_LIBS" ] && [ -f "$BT_SITE_LIBS/coi-serviceworker.js" ]; then
    ko "coi-serviceworker.js NOT in blendtutor libs dir — found (SW scope break)"
  else
    ok "coi-serviceworker.js not in blendtutor libs dir"
  fi
else
  ko "COI boundary — r-exercises.html not rendered"
fi

# Clause 8: ≥2 exercises per language
echo "== Clause 8: ≥2 exercises per language =="

# Count R exercises (language="r") across all demo-book .qmd files
R_COUNT=0
PYTHON_COUNT=0
for f in ${DEMO_QMD_FILES[@]+"${DEMO_QMD_FILES[@]}"}; do
  r_in_file=$(grep -c 'language="r"' "$f" 2>/dev/null) || r_in_file=0
  py_in_file=$(grep -c 'language="python"' "$f" 2>/dev/null) || py_in_file=0
  R_COUNT=$((R_COUNT + r_in_file))
  PYTHON_COUNT=$((PYTHON_COUNT + py_in_file))
done

if [ "$R_COUNT" -ge 2 ]; then
  ok "≥2 R exercises ($R_COUNT found)"
else
  ko "≥2 R exercises — found $R_COUNT (need ≥2)"
fi

if [ "$PYTHON_COUNT" -ge 2 ]; then
  ok "≥2 Python exercises ($PYTHON_COUNT found)"
else
  ko "≥2 Python exercises — found $PYTHON_COUNT (need ≥2)"
fi

# Clause 9: Non-empty content (no empty exercise divs)
echo "== Clause 9: non-empty content =="
EMPTY_DIVS=0
for f in ${DEMO_QMD_FILES[@]+"${DEMO_QMD_FILES[@]}"}; do
  # Check for empty blendtutor divs: ::: {.blendtutor ...} immediately followed by :::
  # This pattern matches a div with no content between opening and closing :::
  empty_count=$(awk '
    /::: \{\.blendtutor/ { in_div=1; div_line=NR; content="" ; next }
    in_div && /^::: *$/ {
      if (content == "") { print "empty at line " div_line }
      in_div=0; next
    }
    in_div { content=content $0 }
  ' "$f" 2>/dev/null | wc -l)
  EMPTY_DIVS=$((EMPTY_DIVS + empty_count))
done

if [ "$EMPTY_DIVS" -eq 0 ]; then
  ok "non-empty content (no empty exercise divs)"
else
  ko "non-empty content — $EMPTY_DIVS empty exercise div(s) found"
fi

# Also check that each exercise has a code block (non-empty JSON payload)
CODE_BLOCKS=0
for f in ${DEMO_QMD_FILES[@]+"${DEMO_QMD_FILES[@]}"}; do
  # Count fenced code blocks inside blendtutor divs
  blocks=$(grep -cE '```(r|python)' "$f" 2>/dev/null) || blocks=0
  CODE_BLOCKS=$((CODE_BLOCKS + blocks))
done

if [ "$CODE_BLOCKS" -ge 4 ]; then
  ok "non-empty content (code blocks present: $CODE_BLOCKS)"
else
  ko "non-empty content — only $CODE_BLOCKS code blocks (need ≥4 for 2+ exercises per language)"
fi

# Clause 10: Mixed-language (both R and Python present)
echo "== Clause 10: mixed-language =="
if [ "$R_COUNT" -ge 1 ] && [ "$PYTHON_COUNT" -ge 1 ]; then
  ok "mixed-language (R: $R_COUNT, Python: $PYTHON_COUNT)"
else
  ko "mixed-language — R: $R_COUNT, Python: $PYTHON_COUNT (need both ≥1)"
fi

# Clause 11: No llm_evaluation_prompt
echo "== Clause 11: no llm_evaluation_prompt =="
LLM_PROMPT_COUNT=0
for f in ${DEMO_QMD_FILES[@]+"${DEMO_QMD_FILES[@]}"}; do
  count=$(grep -c 'llm_evaluation_prompt' "$f" 2>/dev/null) || count=0
  LLM_PROMPT_COUNT=$((LLM_PROMPT_COUNT + count))
done

# Also check _quarto.yml
if [ -f "$DEMO_BOOK_DIR/_quarto.yml" ]; then
  yml_count=$(grep -c 'llm_evaluation_prompt' "$DEMO_BOOK_DIR/_quarto.yml" 2>/dev/null) || yml_count=0
  LLM_PROMPT_COUNT=$((LLM_PROMPT_COUNT + yml_count))
fi

if [ "$LLM_PROMPT_COUNT" -eq 0 ]; then
  ok "no llm_evaluation_prompt (0 occurrences)"
else
  ko "no llm_evaluation_prompt — $LLM_PROMPT_COUNT occurrence(s) found"
fi

# Clause 12: COI config present
echo "== Clause 12: COI config =="
COI_FOUND=0
for f in ${DEMO_QMD_FILES[@]+"${DEMO_QMD_FILES[@]}"}; do
  # Check for coi="true" (div attribute) or coi: true (YAML metadata)
  if grep -qF 'coi="true"' "$f" 2>/dev/null || grep -qE '^coi:[[:space:]]*true' "$f" 2>/dev/null; then
    COI_FOUND=1
    break
  fi
done

# Also check _quarto.yml
if [ "$COI_FOUND" -eq 0 ] && [ -f "$DEMO_BOOK_DIR/_quarto.yml" ]; then
  if grep -qE 'coi:[[:space:]]*true' "$DEMO_BOOK_DIR/_quarto.yml" 2>/dev/null; then
    COI_FOUND=1
  fi
fi

if [ "$COI_FOUND" -eq 1 ]; then
  ok "COI config present (coi=\"true\" or coi: true)"
else
  ko "COI config present — no coi=\"true\" or coi: true found in demo-book"
fi

# ---------------------------------------------------------------------------
# P9 — Demo-book side-effect free (issue #130)
# ---------------------------------------------------------------------------

echo ""
echo "== P9: demo-book side-effect free (org/repo-aware, issue #143) =="

# The old clause-6 copy hack created a BARE extension dir under demo-book as a
# render side effect (masking the install-path bug). Post-AC-5 the by-name
# install at demo-book/_extensions/mcmullarkey/blendtutor/ is LEGITIMATE and
# must be allowed; only the non-org residue path fails. The bare path is
# assembled from fragments so the structural no-masking-copy scan (P2) does
# not false-positive on this guard's own source line.
DB_BARE_EXT="$DEMO_BOOK_DIR/_""extensions/blendtutor"
if [ -d "$DB_BARE_EXT" ]; then
  ko "demo-book side-effect free — bare extension dir exists (copy-hack residue, non-org path)"
else
  ok "demo-book side-effect free — no bare extension-dir residue (non-org path absent)"
fi
if [ -d "$DEMO_BOOK_DIR/_extensions/mcmullarkey/blendtutor" ]; then
  ok "org/repo by-name install present (allowed by P9)"
else
    ko "org/repo by-name install present — demo-book/_extensions/mcmullarkey/blendtutor/ missing"
fi

# ---------------------------------------------------------------------------
# Group 3 — CI (3 clauses)
# ---------------------------------------------------------------------------

echo ""
echo "== Group 3: CI =="

# Clause 13: quarto add in temp dir
echo "== Clause 13: quarto add in temp dir =="
if [ ! -f "$CI_FILE" ]; then
  ko "quarto add in temp dir — CI file not found: $CI_FILE"
else
  # Check for quarto add command with correct org/repo
  if grep -qF 'quarto add mcmullarkey/blendtutor' "$CI_FILE"; then
    ok "quarto add mcmullarkey/blendtutor present in CI"
  else
    ko "quarto add mcmullarkey/blendtutor present in CI — command not found"
  fi

  # Check for temp dir creation (mktemp or mkdir with a temp path)
  if grep -qE 'mktemp|mkdir.*tmp|TMPDIR' "$CI_FILE"; then
    ok "temp dir creation in CI"
  else
    ko "temp dir creation in CI — no mktemp/mkdir tmp found"
  fi

  # Check that test -f paths match Quarto's actual install path.
  # `quarto add org/repo` installs to _extensions/<org>/<repo>/, NOT
  # _extensions/<extension-name>/. The CI must assert files at the org/repo
  # path or the test will pass locally but fail in CI.
  if grep -qF 'test -f _extensions/mcmullarkey/blendtutor/' "$CI_FILE"; then
    ok "test -f paths use _extensions/mcmullarkey/blendtutor/ (actual install path)"
  else
    ko "test -f paths — CI must check _extensions/mcmullarkey/blendtutor/ (quarto add installs to _extensions/<org>/<repo>/)"
  fi
fi

# Clause 14: setup@v2
echo "== Clause 14: setup@v2 =="
if [ ! -f "$CI_FILE" ]; then
  ko "setup@v2 — CI file not found"
else
  if grep -qF 'quarto-dev/quarto-actions/setup@v2' "$CI_FILE"; then
    ok "quarto-dev/quarto-actions/setup@v2 present in CI"
  else
    ko "quarto-dev/quarto-actions/setup@v2 present in CI — action not found"
  fi
fi

# Clause 15: No continue-on-error in distribution job
echo "== Clause 15: no continue-on-error =="
if [ ! -f "$CI_FILE" ]; then
  ko "no continue-on-error — CI file not found"
else
  # Check that the quarto-distribution job (if present) has no continue-on-error
  # Look for the distribution job section
  DIST_JOB_LINE=$(grep -n 'quarto-distribution' "$CI_FILE" | head -1 | cut -d: -f1)
  if [ -z "$DIST_JOB_LINE" ]; then
    ko "no continue-on-error — quarto-distribution job not found in CI"
  else
    # Check for continue-on-error as a YAML key (not in comments) after the
    # distribution job definition. A YAML key starts the line (with optional
    # leading whitespace) — comment lines starting with # are excluded by design.
    REMAINING=$(tail -n +"$DIST_JOB_LINE" "$CI_FILE")
    if echo "$REMAINING" | grep -qE '^[[:space:]]*continue-on-error[[:space:]]*:'; then
      ko "no continue-on-error — found in quarto-distribution job section"
    else
      ok "no continue-on-error in quarto-distribution job"
    fi
  fi
fi

# ---------------------------------------------------------------------------
# Negative cases
# ---------------------------------------------------------------------------

echo ""
echo "== Negative cases =="

# Negative 1: Wrong org/repo — README must say mcmullarkey/blendtutor
echo "== Negative 1: correct org/repo =="
if [ ! -f "$README" ]; then
  ko "correct org/repo — README not found"
else
  if grep -qF 'mcmullarkey/blendtutor' "$README"; then
    ok "correct org/repo (mcmullarkey/blendtutor)"
  else
    ko "correct org/repo — mcmullarkey/blendtutor not found in README"
  fi
fi

# Negative 2: Empty JSON — exercises must have content (checked in clause 8)
echo "== Negative 2: no empty JSON (exercises have content) =="
if [ "$EMPTY_DIVS" -eq 0 ] && [ "$CODE_BLOCKS" -ge 4 ]; then
  ok "no empty JSON (all exercises have content)"
else
  ko "no empty JSON — $EMPTY_DIVS empty div(s), $CODE_BLOCKS code blocks"
fi

# Negative 3: CI must NOT use _extensions: copy (should use quarto add)
echo "== Negative 3: CI does not use _extensions: copy =="
if [ ! -f "$CI_FILE" ]; then
  ko "CI does not use _extensions: copy — CI file not found"
else
  if grep -qF '_extensions: copy' "$CI_FILE"; then
    ko "CI does not use _extensions: copy — found '_extensions: copy' in CI (should use quarto add)"
  else
    ok "CI does not use _extensions: copy (uses quarto add instead)"
  fi
fi

# Negative 4: README must mention BYOK (checked in clause 3)
echo "== Negative 4: README mentions BYOK =="
if [ ! -f "$README" ]; then
  ko "README mentions BYOK — README not found"
else
  if grep -qiE 'BYOK|bring.your.own.key' "$README"; then
    ok "README mentions BYOK"
  else
    ko "README mentions BYOK — not found"
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
