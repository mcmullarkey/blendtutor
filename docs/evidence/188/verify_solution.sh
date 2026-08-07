#!/usr/bin/env bash
# E2E verification for issue #188 — rigorous R data-wrangling Exercise 3.
#
# Checks (in order):
#   1. Structural counts: 3 blendtutor language="r" divs, 3 plain ```r blocks,
#      3 {.r .solution} blocks, 2 {.r .checks} blocks (Exercise 3 omits),
#      1 "## Exercise 3:" heading.
#   2. No .checks block inside the Exercise 3 region.
#   3. Exercises 1-2 byte-identical (first 56 lines match HEAD).
#   4. Prompt prose is Para/Plain only: no bullet list, header, blockquote,
#      or non-template fence between the Exercise 3 opener and its first ```r.
#   5. Solution block (last {.r .solution}) runs via Rscript --vanilla, exit 0.
#   6. Solution stdout contains all six step labels "Step 1:" … "Step 6:".
#   7. Aggregate prints exactly 3 category rows: books 96, clothing 190,
#      electronics 172.5 (exact IEEE754 — all values are multiples of 0.5).
#   8. rodney-probes/demo-book-bootstrap.js R-page clause migrated 2 -> 3
#      (no "=== 2" in the R clause; python clause still "=== 2", AC-2 scope).
#
# Rscript may be absent locally — pinned arithmetic (96/190/172.5) is the
# contract; the structural checks still run.

set -euo pipefail

QMD="demo-book/r-exercises.qmd"
BOOT="rodney-probes/demo-book-bootstrap.js"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

fail() { echo "FAIL: $1"; exit 1; }

echo "=== 1. Structural counts ==="
DIVS="$(grep -c '::: {.blendtutor language="r"}' "$QMD")"
RBLOCKS="$(grep -c '^```r$' "$QMD")"
SOLS="$(grep -c '{.r .solution}' "$QMD")"
CHECKS="$(grep -c '{.r .checks}' "$QMD")"
HEADING="$(grep -c '^## Exercise 3:' "$QMD")"
echo "blendtutor language=\"r\" divs: $DIVS (expect 3)"
echo "plain \`\`\`r blocks:            $RBLOCKS (expect 3)"
echo "{.r .solution} blocks:          $SOLS (expect 3)"
echo "{.r .checks} blocks:            $CHECKS (expect 2)"
echo "\"## Exercise 3:\" headings:     $HEADING (expect 1)"
[ "$DIVS" -eq 3 ] || fail "div count $DIVS != 3"
[ "$RBLOCKS" -eq 3 ] || fail "r-block count $RBLOCKS != 3"
[ "$SOLS" -eq 3 ] || fail "solution count $SOLS != 3"
[ "$CHECKS" -eq 2 ] || fail "checks count $CHECKS != 2 (Exercise 3 must omit .checks)"
[ "$HEADING" -eq 1 ] || fail "heading count $HEADING != 1"

echo "=== 2. No .checks in Exercise 3 region ==="
awk '
  /^## Exercise 3:/{in3=1}
  in3 && /\.checks/{print "FAIL: .checks found at line " NR; bad=1}
  END{exit bad}
' "$QMD" || fail ".checks present in Exercise 3 region"
echo "OK"

echo "=== 3. Exercises 1-2 byte-identical (first 56 lines vs HEAD) ==="
git show HEAD:demo-book/r-exercises.qmd | head -56 > "$WORK/orig"
head -56 "$QMD" > "$WORK/now"
cmp "$WORK/orig" "$WORK/now" || fail "Exercises 1-2 (or YAML) altered"
echo "OK"

echo "=== 4. Prompt prose Para/Plain only (Ex3 opener -> first \`\`\`r) ==="
awk '
  /^::: \{.blendtutor language="r"\}/{n++}
  n==3 && !started {started=1; opener=NR}
  started && /^```r$/{exit 0}
  started && /^```/{print "FAIL: non-template fence before first ```r at line " NR; bad=1; exit}
  started && /^#{1,6} /{print "FAIL: header in prompt prose at line " NR; bad=1; exit}
  started && /^[-*+>] /{print "FAIL: bullet/blockquote in prompt prose at line " NR; bad=1; exit}
  END{exit bad}
' "$QMD" || fail "prompt prose contains dropped markdown constructs"
echo "OK"

echo "=== 5. Extract last {.r .solution} block and run via Rscript ==="
awk '
  /^```\{\.r \.solution\}/{buf=""; f=1; next}
  f && /^```$/{last=buf; f=0; next}
  f{buf=buf $0 ORS}
  END{printf "%s", last}
' "$QMD" > "$WORK/solution.R"
[ -s "$WORK/solution.R" ] || fail "no solution block extracted"

if command -v Rscript >/dev/null 2>&1; then
  if ! Rscript --vanilla "$WORK/solution.R" > "$WORK/solution.out" 2> "$WORK/solution.err"; then
    fail "Rscript exited non-zero (stderr: $(head -1 "$WORK/solution.err"))"
  fi
  echo "Rscript exited 0"

  echo "=== 6. Step labels ==="
  for i in 1 2 3 4 5 6; do
    grep -q "Step $i:" "$WORK/solution.out" || fail "missing \"Step $i:\" label in solution stdout"
  done
  echo "OK: all six \"Step N:\" labels present"

  echo "=== 7. Aggregate ==="
  awk '$2 ~ /^(books|clothing|electronics)$/ && $3 ~ /^[0-9.]+$/ {print $2, $3}' "$WORK/solution.out" > "$WORK/agg.txt"
  cat "$WORK/agg.txt"
  awk '
    {cat[$1]=$2+0; n++}
    END{
      ok = 1
      if (n != 3) {print "FAIL: expected 3 category rows, got " n; ok = 0}
      if (!(cat["books"] == 96))      {print "FAIL: books != 96"; ok = 0}
      if (!(cat["clothing"] == 190))  {print "FAIL: clothing != 190"; ok = 0}
      if (!(cat["electronics"] == 172.5)) {print "FAIL: electronics != 172.5"; ok = 0}
      exit (ok ? 0 : 1)
    }
  ' "$WORK/agg.txt" || fail "aggregate does not match pinned 96/190/172.5"
  echo "OK: aggregate matches books 96 / clothing 190 / electronics 172.5"
else
  echo "SKIP: Rscript unavailable — pinned arithmetic is the contract (structural checks only)"
fi

echo "=== 8. Bootstrap R-page clause migrated 2 -> 3 ==="
R_CLAUSE="$(awk '/Clause 8: demo-book\/_output\/r-exercises.html/{f=1} /Clause 8: demo-book\/_output\/python-exercises.html/{f=0} f' "$BOOT")"
PY_CLAUSE="$(awk '/Clause 8: demo-book\/_output\/python-exercises.html/{f=1} /Fix \(Part 2\)/{f=0} f' "$BOOT")"
PY_HEAD="$(git show HEAD:rodney-probes/demo-book-bootstrap.js | awk '/Clause 8: demo-book\/_output\/python-exercises.html/{f=1} /Fix \(Part 2\)/{f=0} f')"
if printf '%s' "$R_CLAUSE" | grep -q '=== 2'; then
  fail "bootstrap R clause still contains '=== 2'"
fi
printf '%s' "$R_CLAUSE" | grep -q '=== 3' || fail "bootstrap R clause lacks '=== 3'"
[ "$PY_CLAUSE" = "$PY_HEAD" ] || fail "python clause changed (AC-2 scope — must be untouched)"
echo "OK: R clause === 3, python clause byte-identical to HEAD"

echo "=== ALL VERIFICATIONS PASSED ==="
