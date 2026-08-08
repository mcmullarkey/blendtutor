#!/usr/bin/env bash
# Executable spec for issue #152 — deploy demo-book + demo-standalone to the
# GitHub Pages artifact (/demo-book/ + /demo/) with a root .nojekyll.
#
# Verifies the 11-clause compound predicate from AC-2:
#   1. build job block has quarto setup (quarto-dev/quarto-actions/setup@v2)
#      — docs.yml has no quarto setup today, so a clean runner render fails
#   2. build job renders demo-book (quarto render demo-book --to html)
#   3. build job renders demo-standalone (quarto render demo-standalone --to html)
#   4. scripts/fix-demo-coi-scope.sh demo-standalone runs AFTER the standalone
#      render step and BEFORE the artifact-copy steps + upload (line-order pin)
#   5. demo-book assembled via DOT-COPY: cp -R demo-book/_output/. — a bare cp
#      would create a demo-book/_output/ layer (demo-book/_quarto.yml:3 pins
#      output-dir: _output) and /demo-book/ would 404
#   6. demo-standalone assembled SELECTIVELY into /demo/: index.html +
#      index_files/ + coi-serviceworker.js only — no .qmd / _quarto.yml /
#      _extensions/ leak into the public artifact
#   7. explicit workflow step creates docs/book/book/.nojekyll at artifact ROOT
#      (must not rely on the local untracked file)
#   8. ALL new steps appear BEFORE actions/upload-pages-artifact@v5; NO || true,
#      NO continue-on-error: true; render/copy steps in the build job block
#      only, NOT the deploy job block (deploy has no checkout/quarto)
#   9. scripts/check-docs.sh mirrors the new steps (mirror contract) and its
#      render/assemble/assert section enforces the assembled-layout predicates
#      (clauses 9-10 asserted by Phase 2's end-to-end check-docs.sh run; the
#      structural greps here pin the mirror contract)
#  10. existing artifact survives (mdBook index.html, api/, examples/) — no
#      rm -rf docs/book/book/* clobber (asserted by check-docs.sh Phase 2)
#  11. ci.yml quarto-render job runs this test (awk job-block pin, not file-wide)
#
# AC-6 of smevals-eval-report (#199) adds a 12th concern — evals publishing:
#  12. build job gains a guarded evals assemble step: if/then/fi guard with the
#      literal `if [ -d docs/evals ]` AND the `&&` shorthand `[ -d docs/evals ]
#      &&` ABSENT anywhere in the workflow (Actions runs steps under
#      bash -eo pipefail — a trailing && chain exits 1 when the dir is missing,
#      reddening CI before AC-5 commits any report)
#  13. within the step, line order is guard < `rm -rf docs/book/book/evals` <
#      `mkdir -p docs/book/book/evals` < literal DOT-COPY `cp -R docs/evals/.
#      docs/book/book/evals/` (trailing /. — a bare cp double-nests to
#      /evals/evals/<lesson>/ → 404); mkdir INSIDE the guard (no empty /evals/
#      nest published pre-AC-5); step before .nojekyll and before upload
#  14. deploy job block free of `evals`/`docs/evals` needles (deploy has no
#      checkout); check-docs.sh mirrors the guarded assemble + double-nest
#      assert against $book_out; fixture sub-phase (Phase 3) proves the cp
#      semantics (no double-nest) + the guard exits 0 when docs/evals absent
#
# Negative cases (from the spec):
#   - quarto setup step missing → render fails on clean runner → clause 1
#   - || true / continue-on-error: true → silent-failure deploy of a broken
#     artifact → clause 8 refusal-arm pin
#   - cp -R demo-book/_output dst (no trailing /.) → extra _output/ layer →
#     /demo-book/ 404 → clause 5 + check-docs demo-book/index.html existence
#   - COI post-process skipped or run after copy → SW subdir scope → webR dead
#     despite a green "coi present" grep → clause 4 order pin + check-docs
#     exact ./coi-serviceworker.js src on the ASSEMBLED artifact
#   - .nojekyll omitted from workflow (relying on local untracked file) or
#     nested path → Quarto *_files/ dirs Jekyll-filtered in branch mode →
#     clause 7 step pin + check-docs root-existence assert
#   - new steps placed AFTER upload → artifact uploaded without demos →
#     clause 8 line-order pin
#   - render/copy steps in deploy job (no checkout, no quarto) → clause 8
#     build-job-block scoping + deploy-block absence assert
#   - rm -rf docs/book/book/* during assembly → mdBook/rustdoc/examples
#     clobbered → clause 10 survival asserts in check-docs.sh
#   - demo-standalone copied wholesale → .qmd/_quarto.yml/_extensions/ leak →
#     clause 6 selective-copy pin + check-docs no-_extensions/ assert
#   - check-docs.sh not updated → mirror contract silently broken → clause 9
#     structural grep in Phase 1
#
# Usage: bash scripts/tests/test_docs_pages_artifact.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PASS=0
FAIL=0

ok() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
ko() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

DOCS_FILE=".github/workflows/docs.yml"
CI_FILE=".github/workflows/ci.yml"
CHECK_DOCS="scripts/check-docs.sh"

# ---------------------------------------------------------------------------
# Helpers — job-block extraction + relative line numbers
# ---------------------------------------------------------------------------

# Extract one job block from a workflow: starts at the 2-space-indented job
# header (the header line itself is skipped so the range cannot self-close),
# ends at the next 2-space-indented job header. Mirrors the AC-1 cycle-2
# pattern — a file-wide grep is insufficient because a step that regressed to
# the wrong job (e.g. deploy) would still pass it.
job_block() {
  local file="$1" job="$2"
  awk -v job="^  $job:" '$0 ~ job {f=1;next} f&&/^  [a-z][a-z-]*:$/{f=0} f' "$file"
}

# Relative line number of the first occurrence of $2 in block $1 (empty if
# none). Used for the clause-4/8 ordering pins within the build job block.
block_line() {
  grep -nF "$2" <<< "$1" | head -1 | cut -d: -f1 || true
}

# ---------------------------------------------------------------------------
# Phase 1 — structural pins on docs.yml / ci.yml / check-docs.sh
# ---------------------------------------------------------------------------

echo "== Phase 1: structural pins (docs.yml build job) =="

BUILD_BLOCK="$(job_block "$DOCS_FILE" build || true)"
DEPLOY_BLOCK="$(job_block "$DOCS_FILE" deploy || true)"

L_SETUP="$(block_line "$BUILD_BLOCK" 'quarto-dev/quarto-actions/setup@v2')"
L_RENDER_BOOK="$(block_line "$BUILD_BLOCK" 'quarto render demo-book')"
L_RENDER_SA="$(block_line "$BUILD_BLOCK" 'quarto render demo-standalone')"
L_FIX="$(block_line "$BUILD_BLOCK" 'scripts/fix-demo-coi-scope.sh demo-standalone')"
L_COPY_BOOK="$(block_line "$BUILD_BLOCK" 'demo-book/_output/.')"
L_COPY_DEMO="$(block_line "$BUILD_BLOCK" 'cp demo-standalone/index.html')"
L_NOJEKYLL="$(block_line "$BUILD_BLOCK" 'docs/book/book/.nojekyll')"
L_UPLOAD="$(block_line "$BUILD_BLOCK" 'actions/upload-pages-artifact@v5')"
L_EVALS_GUARD="$(block_line "$BUILD_BLOCK" 'if [ -d docs/evals ]')"
L_EVALS_RM="$(block_line "$BUILD_BLOCK" 'rm -rf docs/book/book/evals')"
L_EVALS_MKDIR="$(block_line "$BUILD_BLOCK" 'mkdir -p docs/book/book/evals')"
L_EVALS_CP="$(block_line "$BUILD_BLOCK" 'cp -R docs/evals/. docs/book/book/evals/')"

# Clause 1 — quarto setup in the build job block (absent today: a clean
# runner has no quarto and the render steps would fail).
if [ -n "$L_SETUP" ]; then
  ok "quarto setup in build job (quarto-dev/quarto-actions/setup@v2)"
else
  ko "quarto setup in build job (quarto-dev/quarto-actions/setup@v2) — missing"
fi

# Clause 2 — build job renders demo-book.
if [ -n "$L_RENDER_BOOK" ]; then
  ok "build job renders demo-book (quarto render demo-book --to html)"
else
  ko "build job renders demo-book (quarto render demo-book --to html) — missing"
fi

# Clause 3 — build job renders demo-standalone.
if [ -n "$L_RENDER_SA" ]; then
  ok "build job renders demo-standalone (quarto render demo-standalone --to html)"
else
  ko "build job renders demo-standalone (quarto render demo-standalone --to html) — missing"
fi

# Clause 4 — COI post-process present, after the standalone render, before copy.
if [ -n "$L_FIX" ]; then
  ok "COI post-process present (scripts/fix-demo-coi-scope.sh demo-standalone)"
else
  ko "COI post-process present (scripts/fix-demo-coi-scope.sh demo-standalone) — missing"
fi

# Clause 5 — dot-copy literal (trailing /.) for demo-book/_output.
if [ -n "$L_COPY_BOOK" ]; then
  ok "demo-book DOT-COPY literal (cp -R demo-book/_output/. — no _output/ layer)"
else
  ko "demo-book DOT-COPY literal (cp -R demo-book/_output/. — no _output/ layer) — missing"
fi

# Clause 6 — selective demo copy: index.html + index_files/ + coi-serviceworker.js.
SELECTIVE_OK=0
for needle in \
  'cp demo-standalone/index.html' \
  'cp -R demo-standalone/index_files' \
  'cp demo-standalone/coi-serviceworker.js' \
  'docs/book/book/demo'; do
  grep -qF "$needle" <<< "$BUILD_BLOCK" && SELECTIVE_OK=$((SELECTIVE_OK + 1))
done
if [ "$SELECTIVE_OK" -eq 4 ]; then
  ok "selective demo copy (index.html + index_files/ + coi-serviceworker.js → /demo/)"
else
  ko "selective demo copy — only $SELECTIVE_OK/4 selective-copy commands found in build job"
fi

# Clause 7 — explicit .nojekyll step at artifact ROOT.
if [ -n "$L_NOJEKYLL" ]; then
  ok ".nojekyll step at artifact root (touch docs/book/book/.nojekyll)"
else
  ko ".nojekyll step at artifact root (touch docs/book/book/.nojekyll) — missing"
fi

# Clause 8 — ordering: setup → demo-book render → demo-standalone render →
# fix-coi → dot-copy demo-book → selective demo copy → .nojekyll → upload.
ORDER_OK=1
for ln in "$L_SETUP" "$L_RENDER_BOOK" "$L_RENDER_SA" "$L_FIX" "$L_COPY_BOOK" \
          "$L_COPY_DEMO" "$L_EVALS_GUARD" "$L_NOJEKYLL" "$L_UPLOAD"; do
  [ -n "$ln" ] || ORDER_OK=0
done
if [ "$ORDER_OK" -eq 1 ] \
    && [ "$L_SETUP" -lt "$L_RENDER_BOOK" ] \
    && [ "$L_RENDER_BOOK" -lt "$L_RENDER_SA" ] \
    && [ "$L_RENDER_SA" -lt "$L_FIX" ] \
    && [ "$L_FIX" -lt "$L_COPY_BOOK" ] \
    && [ "$L_COPY_BOOK" -lt "$L_COPY_DEMO" ] \
    && [ "$L_COPY_DEMO" -lt "$L_EVALS_GUARD" ] \
    && [ "$L_EVALS_GUARD" -lt "$L_NOJEKYLL" ] \
    && [ "$L_NOJEKYLL" -lt "$L_UPLOAD" ]; then
  ok "ordering: setup → renders → fix-coi → copies → evals guard → .nojekyll all BEFORE upload (clauses 4, 8)"
else
  ko "ordering: all new steps BEFORE upload with fix-coi after render, before copy — setup=$L_SETUP render_book=$L_RENDER_BOOK render_sa=$L_RENDER_SA fix=$L_FIX copy_book=$L_COPY_BOOK copy_demo=$L_COPY_DEMO evals=$L_EVALS_GUARD nojekyll=$L_NOJEKYLL upload=$L_UPLOAD"
fi

# Clause 8 — refusal arms: no || true, no continue-on-error on any step.
if grep -qE '\|\|\s*true' <<< "$BUILD_BLOCK"; then
  ko "no '|| true' on build steps (silent-failure trap)"
else
  ok "no '|| true' on build steps"
fi

if grep -qF 'continue-on-error: true' <<< "$BUILD_BLOCK"; then
  ko "no 'continue-on-error: true' on build steps (silent-failure trap)"
else
  ok "no 'continue-on-error: true' on build steps"
fi

# AC-6 (#199) — clause 1: evals guard present in if/then/fi form, and the
# `[ -d docs/evals ] &&` shorthand ABSENT anywhere in the workflow. Actions
# runs steps under `bash -eo pipefail`, so a trailing && chain would exit 1
# (and redden CI) whenever docs/evals does not exist — before AC-5 has
# committed any report. Grep the whole file, not just the build block.
if [ -n "$L_EVALS_GUARD" ]; then
  ok "evals guard present (if [ -d docs/evals ])"
else
  ko "evals guard present (if [ -d docs/evals ]) — missing"
fi

if grep -qF '[ -d docs/evals ] &&' "$DOCS_FILE"; then
  ko "no '&&' shorthand guard (trailing && exits 1 under bash -eo pipefail when docs/evals missing)"
else
  ok "no '&&' shorthand guard (if/then/fi only — CI green before AC-5)"
fi

# AC-6 (#199) — clause 2: within-step line order guard < rm < mkdir < dot-copy.
# mkdir INSIDE the guard: no empty /evals/ nest published pre-AC-5. The
# trailing /. on the copy is the double-nest trap (bare cp → /evals/evals/).
EVALS_ORDER_OK=1
for ln in "$L_EVALS_GUARD" "$L_EVALS_RM" "$L_EVALS_MKDIR" "$L_EVALS_CP"; do
  [ -n "$ln" ] || EVALS_ORDER_OK=0
done
if [ "$EVALS_ORDER_OK" -eq 1 ] \
    && [ "$L_EVALS_GUARD" -lt "$L_EVALS_RM" ] \
    && [ "$L_EVALS_RM" -lt "$L_EVALS_MKDIR" ] \
    && [ "$L_EVALS_MKDIR" -lt "$L_EVALS_CP" ]; then
  ok "evals step order: guard < rm < mkdir < dot-copy (clause 2, mkdir inside guard)"
else
  ko "evals step order: guard < rm < mkdir < dot-copy — guard=$L_EVALS_GUARD rm=$L_EVALS_RM mkdir=$L_EVALS_MKDIR cp=$L_EVALS_CP"
fi

# AC-6 (#199) — clause 4: no || true on the evals step.
if [ -z "$L_EVALS_GUARD" ] || [ -z "$L_EVALS_CP" ]; then
  ko "no '|| true' on evals step — evals step missing (clause 1 already fails)"
elif grep -qE '\|\|\s*true' <<< "$(sed -n "$L_EVALS_GUARD,$((L_EVALS_CP + 2))p" <<< "$BUILD_BLOCK")"; then
  ko "no '|| true' on evals step (silent-failure trap)"
else
  ok "no '|| true' on evals step"
fi

# Clause 8 — build-job-block scoping: no render/copy steps leak into deploy.
DEPLOY_LEAK=""
for needle in 'quarto render' 'fix-demo-coi-scope' 'demo-book' 'demo-standalone' '.nojekyll' 'evals'; do
  if grep -qF "$needle" <<< "$DEPLOY_BLOCK"; then
    DEPLOY_LEAK="$DEPLOY_LEAK $needle"
  fi
done
if [ -z "$DEPLOY_LEAK" ]; then
  ok "no render/copy steps in deploy job block"
else
  ko "no render/copy steps in deploy job block — leaked:$DEPLOY_LEAK"
fi

echo "== Phase 1: structural pins (ci.yml quarto-render job) =="

# Clause 11 — ci.yml quarto-render job runs this test (awk job-block pin).
QR_BLOCK="$(job_block "$CI_FILE" quarto-render || true)"
if grep -qF 'scripts/tests/test_docs_pages_artifact.sh' <<< "$QR_BLOCK"; then
  ok "CI quarto-render job runs test_docs_pages_artifact.sh"
else
  ko "CI quarto-render job runs test_docs_pages_artifact.sh — not found in quarto-render job block"
fi

echo "== Phase 1: structural pins (check-docs.sh mirror contract) =="

# Clause 9 (structural half) — check-docs.sh mirrors the new build steps so
# the local mirror cannot silently diverge from CI. AC-6 (#199) adds the three
# evals needles (guard, dot-copy, double-nest assert) → 8 → 11.
MIRROR_OK=0
for needle in \
  'quarto render demo-book' \
  'quarto render demo-standalone' \
  'scripts/fix-demo-coi-scope.sh demo-standalone' \
  'demo-book/_output/.' \
  'cp demo-standalone/index.html' \
  'cp -R demo-standalone/index_files' \
  'cp demo-standalone/coi-serviceworker.js' \
  '.nojekyll' \
  'if [ -d docs/evals ]' \
  'cp -R docs/evals/. "$book_out/evals/"' \
  '"$book_out/evals/evals"'; do
  grep -qF "$needle" "$CHECK_DOCS" && MIRROR_OK=$((MIRROR_OK + 1))
done
if [ "$MIRROR_OK" -eq 11 ]; then
  ok "check-docs.sh mirrors the build steps incl. guarded evals assemble (11/11 commands found)"
else
  ko "check-docs.sh mirrors the build steps — only $MIRROR_OK/11 commands found"
fi

# ---------------------------------------------------------------------------
# Phase 2 — local render + assemble + assert (check-docs.sh end-to-end)
# ---------------------------------------------------------------------------

echo "== Phase 2: local render + assemble + assert (check-docs.sh) =="

# Clause 9-10 (assert half) live in check-docs.sh's render/assemble/assert
# section: full local build (mdBook + rustdoc + examples + demo renders) then
# assembled-layout asserts (demo-book/index.html not under _output/,
# demo/index.html coi src exactly ./coi-serviceworker.js with no _extensions/,
# shim byte-identical to vendored, root .nojekyll, existing artifact survives).
# SKIP (exit 0) when any required tool is absent — CI's quarto-render job only
# has quarto, so this phase is exercised locally and on dev machines.
if ! command -v quarto &>/dev/null || ! command -v mdbook &>/dev/null \
    || ! command -v cargo &>/dev/null; then
  echo "  SKIP: quarto/mdbook/cargo not all installed — Phase 2 skipped"
  echo "  (Phase 1 structural assertions are the CI-enforced half)"
else
  if bash "$CHECK_DOCS"; then
    ok "check-docs.sh renders + assembles + asserts the artifact layout (clauses 9-10)"
  else
    ko "check-docs.sh renders + assembles + asserts the artifact layout (clauses 9-10)"
  fi
fi

# ---------------------------------------------------------------------------
# Phase 3 — functional evals fixture sub-phase (AC-6, #199, clause 8)
# ---------------------------------------------------------------------------

echo "== Phase 3: functional evals fixture sub-phase =="

# Once AC-5 (#198) lands, docs/evals/ is COMMITTED SOURCE — this sub-phase must
# never destroy it (a `rm -rf docs/evals` cleanup would delete real reports
# from the working tree). If docs/evals pre-exists, its content is snapshotted
# and moved aside; the fixture sub-phase then runs against a fresh fixture tree
# and the pre-existing content is restored + verified byte-identical. If it does
# not pre-exist, a simulated committed report tree is created so the same
# safety path is always exercised (and cleaned up on exit).
SCRATCH="$(mktemp -d "${TMPDIR:-/tmp}/bt-evals-scratch.XXXXXX")"
EVALS_PREEXISTING=0
EVALS_BACKUP=""
FIXTURE_DIR="docs/evals/evals-fixture"
cleanup_evals() {
  # Restore pre-existing (committed) docs/evals if it was moved aside.
  if [ -n "$EVALS_BACKUP" ] && [ -d "$EVALS_BACKUP" ]; then
    rm -rf docs/evals
    mv "$EVALS_BACKUP" docs/evals
  fi
  # Temp fixture — inside the restored real tree (pre-existing case) OR the
  # simulated tree (non-pre-existing) — must not linger in either case.
  rm -rf "$FIXTURE_DIR"
  # Simulated committed tree (created by this sub-phase) must not linger.
  if [ "$EVALS_PREEXISTING" -eq 0 ]; then
    rm -rf docs/evals
  fi
  rm -rf "$SCRATCH"
}
trap cleanup_evals EXIT

if [ -d docs/evals ]; then
  EVALS_PREEXISTING=1
else
  # Simulate the post-AC-5 committed tree so the backup/restore path is always
  # exercised and the preservation pin is live even before AC-5 lands.
  mkdir -p docs/evals/committed-lesson
  printf '<!doctype html><html><body>committed report</body></html>\n' \
    > docs/evals/committed-lesson/report.html
fi

# The committed content file (real pre-existing or simulated), relative to the
# repo root — used to assert it nests alongside the fixture in clause 8a
# without hardcoding the simulated dir name (real AC-5 lessons vary).
COMMITTED_FILE="$(find docs/evals -type f | head -1 || true)"
COMMITTED_NESTED="${COMMITTED_FILE#docs/evals/}"

# Snapshot the committed tree's content BEFORE the temp fixture is added, so
# the preservation pin proves the committed content survived the sub-phase
# independent of the fixture (byte-identical compare, fixture excluded).
find docs/evals -type f -exec cksum {} \; | sort > "$SCRATCH/docs-evals-before.cksum"

# Temp fixture — simulates a report tree inside the (real or simulated)
# committed docs/evals, exactly the post-AC-5 shape (lessons + new report).
mkdir -p "$FIXTURE_DIR"
printf '<!doctype html><html><body>evals-fixture</body></html>\n' \
  > "$FIXTURE_DIR/index.html"

# The guarded assemble snippet — the docs.yml step body with docs/book/book
# replaced by the scratch dir, run under bash -euo pipefail.
evals_assemble() {
  bash -euo pipefail -c '
    if [ -d docs/evals ]; then
      rm -rf "$1/evals"
      mkdir -p "$1/evals"
      cp -R docs/evals/. "$1/evals/"
    fi
  ' _ "$SCRATCH"
}

# Clause 8a — fixture present: HTML lands at <scratch>/evals/evals-fixture/
# index.html byte-identical, committed lesson also nests, and no double-nest
# (<scratch>/evals/evals absent).
if evals_assemble; then
  if [ -f "$SCRATCH/evals/evals-fixture/index.html" ] \
      && cmp -s "$FIXTURE_DIR/index.html" "$SCRATCH/evals/evals-fixture/index.html"; then
    ok "fixture HTML lands at <scratch>/evals/evals-fixture/index.html, byte-identical (dot-copy)"
  else
    ko "fixture HTML not byte-identical at <scratch>/evals/evals-fixture/index.html"
  fi
  if [ -n "$COMMITTED_NESTED" ] && [ -f "$SCRATCH/evals/$COMMITTED_NESTED" ]; then
    ok "committed lesson nests alongside fixture (<scratch>/evals/$COMMITTED_NESTED)"
  else
    ko "committed lesson missing from assembled evals nest (expected <scratch>/evals/$COMMITTED_NESTED)"
  fi
  if [ ! -e "$SCRATCH/evals/evals" ]; then
    ok "no double-nest — <scratch>/evals/evals absent"
  else
    ko "double-nest — <scratch>/evals/evals present (bare cp, not dot-copy)"
  fi
else
  ko "guarded assemble exits non-zero with fixture present"
fi

# Clause 8b — docs/evals absent (as pre-AC-5): same snippet under bash -e
# exits 0 and creates no evals/ dir (CI stays green). The committed tree is
# moved aside first (restored below); scratch evals/ from 8a is cleared so
# "creates no evals dir" is a true negative.
mv docs/evals "$SCRATCH/docs-evals-real"
EVALS_BACKUP="$SCRATCH/docs-evals-real"
rm -rf "$SCRATCH/evals"
if bash -e -c '
  if [ -d docs/evals ]; then
    rm -rf "$1/evals"
    mkdir -p "$1/evals"
    cp -R docs/evals/. "$1/evals/"
  fi
' _ "$SCRATCH"; then
  if [ ! -e "$SCRATCH/evals" ]; then
    ok "guard exits 0 with docs/evals absent; no evals/ nest created (CI green pre-AC-5)"
  else
    ko "evals/ nest created despite docs/evals absent (mkdir outside guard)"
  fi
else
  ko "guard exits non-zero with docs/evals absent (would redden CI pre-AC-5)"
fi

# Restore the (real or simulated) committed tree and pin byte-identical
# preservation — a regression here deletes committed AC-5 reports. The temp
# fixture (which travelled inside docs/evals to the backup) is removed from the
# restored tree; the before-snapshot never contained it, so the pin compares
# committed content only. Guarded: if 8b never moved docs/evals aside (hard
# failure before it), there is nothing to restore.
if [ -n "$EVALS_BACKUP" ] && [ -d "$EVALS_BACKUP" ]; then
  rm -rf docs/evals
  mv "$EVALS_BACKUP" docs/evals
  EVALS_BACKUP=""
  rm -rf "$FIXTURE_DIR"
  if find docs/evals -type f -exec cksum {} \; | sort \
      | diff -q "$SCRATCH/docs-evals-before.cksum" - >/dev/null; then
    ok "committed docs/evals preserved byte-identical after fixture sub-phase"
  else
    ko "committed docs/evals NOT preserved (content changed or deleted)"
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
