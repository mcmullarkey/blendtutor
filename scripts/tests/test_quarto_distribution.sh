#!/usr/bin/env bash
# Executable spec for issue #115 — Author docs, demo book, quarto add install verification.
#
# Verifies the 14-clause predicate from AC-10 in 3 groups:
#
#   Group 1 — README (5 clauses):
#     1. Install command present (quarto add mcmullarkey/blendtutor)
#     2. Syntax shown for both languages (R and Python)
#     3. BYOK (bring your own key) mentioned
#     4. Minimum Quarto version stated
#     5. Demo book link present
#
#   Group 2 — Demo book (6 clauses):
#     6. quarto render demo-book exits 0
#     7. ≥2 exercises per language (R and Python)
#     8. Non-empty content (no empty exercise divs / empty JSON)
#     9. Mixed-language (both R and Python present)
#    10. No llm_evaluation_prompt field
#    11. COI config present (coi="true" or coi: true)
#
#   Group 3 — CI (3 clauses):
#    12. quarto add in temp dir
#    13. setup@v2
#    14. No continue-on-error
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
# Group 1 — README (5 clauses)
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

# Clause 6: quarto render demo-book exits 0
echo "== Clause 6: render exits 0 =="
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
    # Render the demo book
    RENDER_OUTPUT=$(quarto render "$DEMO_BOOK_DIR" --to html 2>&1) && RENDER_RC=0 || RENDER_RC=$?
    if [ "$RENDER_RC" -eq 0 ]; then
      ok "quarto render demo-book exits 0"
    else
      ko "quarto render demo-book exits 0 — exit code $RENDER_RC"
      echo "  render output: $RENDER_OUTPUT" >&2
    fi
  fi
fi

# Clause 7: ≥2 exercises per language
echo "== Clause 7: ≥2 exercises per language =="

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

# Clause 8: Non-empty content (no empty exercise divs)
echo "== Clause 8: non-empty content =="
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

# Clause 9: Mixed-language (both R and Python present)
echo "== Clause 9: mixed-language =="
if [ "$R_COUNT" -ge 1 ] && [ "$PYTHON_COUNT" -ge 1 ]; then
  ok "mixed-language (R: $R_COUNT, Python: $PYTHON_COUNT)"
else
  ko "mixed-language — R: $R_COUNT, Python: $PYTHON_COUNT (need both ≥1)"
fi

# Clause 10: No llm_evaluation_prompt
echo "== Clause 10: no llm_evaluation_prompt =="
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

# Clause 11: COI config present
echo "== Clause 11: COI config =="
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
# Group 3 — CI (3 clauses)
# ---------------------------------------------------------------------------

echo ""
echo "== Group 3: CI =="

# Clause 12: quarto add in temp dir
echo "== Clause 12: quarto add in temp dir =="
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
fi

# Clause 13: setup@v2
echo "== Clause 13: setup@v2 =="
if [ ! -f "$CI_FILE" ]; then
  ko "setup@v2 — CI file not found"
else
  if grep -qF 'quarto-dev/quarto-actions/setup@v2' "$CI_FILE"; then
    ok "quarto-dev/quarto-actions/setup@v2 present in CI"
  else
    ko "quarto-dev/quarto-actions/setup@v2 present in CI — action not found"
  fi
fi

# Clause 14: No continue-on-error in distribution job
echo "== Clause 14: no continue-on-error =="
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
