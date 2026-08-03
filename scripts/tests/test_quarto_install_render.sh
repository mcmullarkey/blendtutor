#!/usr/bin/env bash
# Executable spec for issue #130 (AC-2) — remove clause-6 copy hack, add a
# hermetic working-tree install-path render test.
#
# Verifies the P1–P10 predicate from the AC-2 executable spec:
#
#   P1  Hack removed: no masking copy of the extension dir into demo-book
#       anywhere in test_quarto_distribution.sh
#   P2  No masking copy anywhere in the render path (scripts/tests +
#       .github/workflows/ci.yml) — the ONLY permitted copy is the working-tree
#       install simulation into the org/repo path
#       (_extensions/mcmullarkey/blendtutor/)
#   P3  Working-tree source: this test derives the extension from the repo
#       checkout; it must NOT run the quarto add install command in the
#       render path (quarto add installs GitHub main, not the PR's own code)
#   P4  Real install-path render: minimal index.qmd in $TMP with
#       filters: [_extensions/mcmullarkey/blendtutor/blendtutor.lua];
#       quarto render --to html exits 0
#   P5  Filter ran: output HTML contains bt-exercise
#   P6  Asset resolved, not exit-0 alone: HTML references
#       index_files/libs/quarto-contrib/blendtutor-0.1.0/styles.css (deployed
#       via add_html_dependency under AC-4) AND
#       test -f "$TMP/index_files/libs/quarto-contrib/blendtutor-0.1.0/styles.css"
#   P7  No old-path leak: HTML does NOT contain _extensions/blendtutor/assets
#       (non-org prefix)
#   P8  COI path covered: minimal .qmd div sets coi="true"; HTML references
#       _extensions/mcmullarkey/blendtutor/assets/coi-serviceworker.js AND
#       the file exists in $TMP
#   P9  (asserted by test_quarto_distribution.sh) demo-book side-effect free:
#       no extension-dir residue in demo-book after the distribution suite runs
#   P10 CI wiring: quarto-distribution job runs this test; `quarto add`
#       temp-dir step retained (distribution-only proof); `Render demo book`
#       step retained; no continue-on-error / || true on render steps
#
# Negative cases:
#   - Render inside clause-12's GitHub temp dir → tests published code,
#     not working tree → P3 + P7 catch
#   - Hack reintroduced at non-org path → P2 / P7 (and P1 for the
#     distribution script)
#   - Pre-AC-1 hardcoded asset paths → P6 / P8 RED
#   - Filter not loaded → no bt-exercise → P5 fails
#   - Exit-0-only assertions → P6 / P8 file checks catch (Quarto does not
#     validate emitted hrefs at render)
#
# Usage: bash scripts/tests/test_quarto_install_render.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PASS=0
FAIL=0
SKIP=0

ok() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
ko() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }
skip_note() { echo "  SKIP: $1"; SKIP=$((SKIP + 1)); }

# ---------------------------------------------------------------------------
# P1 — Hack removed from test_quarto_distribution.sh
# ---------------------------------------------------------------------------

echo "== P1: copy hack removed from test_quarto_distribution.sh =="

DIST_SCRIPT="scripts/tests/test_quarto_distribution.sh"

# Patterns are assembled from fragments so this test file itself never
# contains the literal regex or install command — otherwise the structural
# self-scans (P1/P2/P3) would false-positive on their own source lines.
CP_PAT="cp .*_""extensions"
DB_PAT="demo-book/_""extensions"
QADD_PAT="quarto add mcmullarkey/""blendtutor"

if [ ! -f "$DIST_SCRIPT" ]; then
  ko "hack removed — distribution test script not found: $DIST_SCRIPT"
else
  if grep -qE "$CP_PAT|$DB_PAT" "$DIST_SCRIPT"; then
    ko "hack removed — masking copy still present in $DIST_SCRIPT"
  else
    ok "hack removed — no masking copy in $DIST_SCRIPT"
  fi
fi

# ---------------------------------------------------------------------------
# P2 — No masking copy anywhere in the render path
# ---------------------------------------------------------------------------

echo "== P2: no masking copy in render path (only org/repo install simulation) =="

# rg exits 1 when nothing matches — that IS the success case; the '|| true'
# here only guards empty input and is not a render step (P10).
P2_MATCHES=$(rg -n "${CP_PAT}|${DB_PAT}" scripts/tests .github/workflows/ci.yml || true)

if [ -z "$P2_MATCHES" ]; then
  ok "no masking copy in render path"
else
  P2_BAD=0
  while IFS= read -r line; do
    if ! grep -qF '_extensions/mcmullarkey/blendtutor/' <<< "$line"; then
      ko "no masking copy — disallowed match: $line"
      P2_BAD=1
    fi
  done <<< "$P2_MATCHES"
  if [ "$P2_BAD" -eq 0 ]; then
    ok "no masking copy in render path (only the working-tree install simulation)"
  fi
fi

# ---------------------------------------------------------------------------
# P3 — Working-tree source (no quarto add in the render path)
# ---------------------------------------------------------------------------

echo "== P3: install-render test sources from working tree =="

if grep -qF "$QADD_PAT" "$0"; then
  ko "working-tree source — this test must not run the quarto add install command"
else
  ok "working-tree source — extension copied from repo checkout, no quarto add"
fi

# ---------------------------------------------------------------------------
# P10 — CI wiring (always checked)
# ---------------------------------------------------------------------------

echo "== P10: CI wiring (quarto-distribution job) =="

CI_FILE=".github/workflows/ci.yml"

if [ ! -f "$CI_FILE" ]; then
  ko "CI wiring — CI file not found: $CI_FILE"
else
  if grep -qF 'bash scripts/tests/test_quarto_install_render.sh' "$CI_FILE"; then
    ok "CI wiring — quarto-distribution job runs install render test"
  else
    ko "CI wiring — quarto-distribution job must run test_quarto_install_render.sh"
  fi

  if grep -qF "$QADD_PAT" "$CI_FILE"; then
    ok "CI wiring — quarto add step retained (distribution-only proof)"
  else
    ko "CI wiring — quarto add step missing from CI"
  fi

  if grep -qF 'Render demo book' "$CI_FILE"; then
    ok "CI wiring — Render demo book step retained"
  else
    ko "CI wiring — Render demo book step missing from CI"
  fi

  # No continue-on-error in the quarto-distribution job section.
  DIST_JOB_LINE=$(grep -n 'quarto-distribution' "$CI_FILE" | head -1 | cut -d: -f1)
  if [ -z "$DIST_JOB_LINE" ]; then
    ko "CI wiring — quarto-distribution job not found in CI"
  else
    REMAINING=$(tail -n +"$DIST_JOB_LINE" "$CI_FILE")
    if echo "$REMAINING" | grep -qE '^[[:space:]]*continue-on-error[[:space:]]*:'; then
      ko "CI wiring — continue-on-error found in quarto-distribution job"
    else
      ok "CI wiring — no continue-on-error in quarto-distribution job"
    fi
  fi

  # No '|| true' on render steps — only real run: lines count, not comments.
  if grep -E '\|\|[[:space:]]*true' "$CI_FILE" | grep -vE '^[[:space:]]*#' | grep -q .; then
    ko "CI wiring — '|| true' found in CI (must fail loudly on render steps)"
  else
    ok "CI wiring — no '|| true' on render steps in CI"
  fi
fi

# ---------------------------------------------------------------------------
# P4–P8 — Hermetic real install-path render (requires quarto)
# ---------------------------------------------------------------------------

echo ""
echo "== P4-P8: hermetic working-tree install-path render =="

if ! command -v quarto &>/dev/null; then
  skip_note "quarto not installed locally — render assertions skipped"
  skip_note "(CI installs quarto via quarto-dev/quarto-actions/setup@v2)"
else
  # Hermetic fixture: simulate the quarto add org/repo install layout
  # from the PR's OWN working tree (the repo checkout), NOT GitHub
  # main. Quarto installs GitHub-sourced extensions to
  # _extensions/<org>/<repo>/, so we copy to _extensions/mcmullarkey/blendtutor/.
  TMP_DIR=$(mktemp -d)
  trap 'rm -rf "$TMP_DIR"' EXIT

  mkdir -p "$TMP_DIR/_extensions/mcmullarkey"
  cp -r _extensions/blendtutor "$TMP_DIR/_extensions/mcmullarkey/blendtutor/"

  # P4 fixture: minimal index.qmd exercising the real install-path filter and
  # a blendtutor div with language="r" and coi="true" (P8).
  cat > "$TMP_DIR/index.qmd" <<'QMD'
---
title: blendtutor install render fixture
filters: [_extensions/mcmullarkey/blendtutor/blendtutor.lua]
---

## Install path render test

::: {.blendtutor language="r" coi="true"}
Write a function `add(a, b)` that returns the sum.

```r
add <- function(a, b) { a + b }
```

```{.r .checks}
stopifnot(add(1, 2) == 3)
```
:::
QMD

  # P4 — quarto render --to html exits 0
  RENDER_OUTPUT=$( (cd "$TMP_DIR" && quarto render index.qmd --to html) 2>&1 ) && RENDER_RC=0 || RENDER_RC=$?

  if [ "$RENDER_RC" -eq 0 ]; then
    ok "P4: quarto render --to html exits 0 (org/repo install path)"
  else
    ko "P4: quarto render --to html exits 0 — exit code $RENDER_RC"
    echo "  render output: $RENDER_OUTPUT" >&2
  fi

  HTML_FILE="$TMP_DIR/index.html"

  if [ ! -f "$HTML_FILE" ]; then
    ko "P5: filter ran — HTML output not found: $HTML_FILE"
    ko "P6: styles.css resolved — HTML output not found"
    ko "P7: no old-path leak — HTML output not found"
    ko "P8: coi-serviceworker.js resolved — HTML output not found"
  else
    HTML_CONTENT=$(cat "$HTML_FILE")

    # P5 — filter actually loaded: bt-exercise widget is only emitted by the
    # loaded blendtutor filter. Content-survival checks pass trivially when
    # the filter never runs, so this is the load-proving guard.
    if grep -qF 'bt-exercise' <<< "$HTML_CONTENT"; then
      ok "P5: filter ran — bt-exercise widget present in HTML"
    else
      ko "P5: filter ran — bt-exercise not found; filter never loaded"
    fi

    # P6 — styles.css deployed to the libs dir AND the file exists
    # (exit-0 alone is insufficient: Quarto does not validate emitted hrefs;
    # add_html_dependency can silently skip a missing file — assert on disk).
    CSS_REF='index_files/libs/quarto-contrib/blendtutor-0.1.0/styles.css'
    CSS_FILE="$TMP_DIR/index_files/libs/quarto-contrib/blendtutor-0.1.0/styles.css"
    if grep -qF "$CSS_REF" <<< "$HTML_CONTENT"; then
      if [ -f "$CSS_FILE" ]; then
        ok "P6: styles.css resolved — HTML references $CSS_REF and file exists"
      else
        ko "P6: styles.css resolved — HTML references $CSS_REF but file missing: $CSS_FILE"
      fi
    else
      ko "P6: styles.css resolved — HTML does not reference $CSS_REF"
    fi

    # P7 — no old-path leak (non-org _extensions/blendtutor/assets prefix).
    if grep -qF '_extensions/blendtutor/assets' <<< "$HTML_CONTENT"; then
      ko "P7: no old-path leak — HTML contains _extensions/blendtutor/assets"
    else
      ok "P7: no old-path leak — no _extensions/blendtutor/assets in HTML"
    fi

    # P8 — coi-serviceworker.js referenced at the org/repo path AND file exists.
    COI_REF='_extensions/mcmullarkey/blendtutor/assets/coi-serviceworker.js'
    COI_FILE="$TMP_DIR/_extensions/mcmullarkey/blendtutor/assets/coi-serviceworker.js"
    if grep -qF "$COI_REF" <<< "$HTML_CONTENT"; then
      if [ -f "$COI_FILE" ]; then
        ok "P8: coi-serviceworker.js resolved — HTML references $COI_REF and file exists"
      else
        ko "P8: coi-serviceworker.js resolved — HTML references $COI_REF but file missing: $COI_FILE"
      fi
    else
      ko "P8: coi-serviceworker.js resolved — HTML does not reference $COI_REF (coi=\"true\" not honored)"
    fi
  fi
fi

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

echo ""
echo "========================================="
echo "  Results: $PASS passed, $FAIL failed, $SKIP skipped"
echo "========================================="

if [ "$FAIL" -gt 0 ]; then
  exit 1
fi

echo "All tests passed."
