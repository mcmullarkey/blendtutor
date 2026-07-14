#!/usr/bin/env bash
# Stub-based integration test for scripts/eval-course.sh.
#
# Verifies the 6-point compound predicate from AC-5:
#   1. Script exists, is executable, takes exactly one <course-dir> argument
#   2. Extracts ALL lesson paths from blendtutor.toml (not just first)
#   3. Runs blendtutor eval per lesson, aborts on non-zero exit (no report)
#   4. Aggregation is sum(matched)/sum(total), NOT average(accuracy)
#   5. Zero lessons/cases → {"accuracy": 0.0} (not NaN, not crash)
#   6. Writes <course-dir>/eval-report.json with {"accuracy": <0..1>}
#
# Also catches the 7 negative cheats:
#   1. Accuracy-averaging: unequal case counts (5/5 + 0/1 → 5/6=0.833, NOT 0.5)
#   2. First-lesson-only: stub call count == manifest lesson count
#   3. Silent-partial-failure: no eval-report.json after mid-run failure
#   4. Empty-course NaN: empty manifest → 0.0
#   5. Hardcoded-report: stub called N times (counter file)
#   6. Wrong-path: stub asserts it received .yaml file path, not slug id
#   7. TOML-grammar-fragile: fixture manifest has inline comments
#
# Usage: bash scripts/tests/eval-course.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

SCRIPT="scripts/eval-course.sh"
PASS=0
FAIL=0

ok() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
ko() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

assert_eq() {
  local label="$1" expected="$2" actual="$3"
  if [ "$expected" = "$actual" ]; then
    ok "$label (got: $actual)"
  else
    ko "$label — expected [$expected], got [$actual]"
  fi
}

assert_contains() {
  local label="$1" haystack="$2" needle="$3"
  if echo "$haystack" | grep -qF "$needle"; then
    ok "$label"
  else
    ko "$label — [$needle] not found in output"
  fi
}

assert_file_exists() {
  local label="$1" path="$2"
  if [ -f "$path" ]; then
    ok "$label"
  else
    ko "$label — file not found: $path"
  fi
}

assert_file_absent() {
  local label="$1" path="$2"
  if [ ! -f "$path" ]; then
    ok "$label"
  else
    ko "$label — file should NOT exist: $path"
  fi
}

# ---------------------------------------------------------------------------
# Setup: create a stub `blendtutor` that emits canned JSON per lesson path.
# ---------------------------------------------------------------------------

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

STUB_DIR="$TMPDIR/bin"
mkdir -p "$STUB_DIR"

# Counter file — tracks how many times the stub was called and what paths it saw.
COUNTER="$TMPDIR/calls.txt"
: > "$COUNTER"

cat > "$STUB_DIR/blendtutor" <<'STUB'
#!/usr/bin/env bash
# Stub blendtutor eval — emits canned JSON based on lesson file path.
# Records each call for assertion.
set -euo pipefail

COUNTER="${EVAL_COURSE_COUNTER:-/dev/null}"

if [ "$1" != "eval" ]; then
  echo "stub: unexpected command: $1" >&2
  exit 1
fi

lesson_path="$2"

# Record the call (path only, not the full args).
echo "$lesson_path" >> "$COUNTER"

# Negative #6 — wrong-path cheat: assert we got a .yaml file path, not a slug id.
case "$lesson_path" in
  *.yaml|*.yml) ;;
  *)
    echo "stub: expected .yaml file path, got slug: $lesson_path" >&2
    exit 1
    ;;
esac

# Emit canned JSON based on the lesson file name.
case "$(basename "$lesson_path")" in
  lesson_a.yaml)
    # 5 cases, all matched → accuracy 1.0
    echo '{"cases":[{"expected":"correct","actual":"correct","matched":true},{"expected":"correct","actual":"correct","matched":true},{"expected":"correct","actual":"correct","matched":true},{"expected":"correct","actual":"correct","matched":true},{"expected":"correct","actual":"correct","matched":true}],"accuracy":1.0}'
    ;;
  lesson_b.yaml)
    # 1 case, not matched → accuracy 0.0
    echo '{"cases":[{"expected":"incorrect","actual":"correct","matched":false}],"accuracy":0.0}'
    ;;
  lesson_fail.yaml)
    # Simulate eval failure.
    echo "stub: simulated eval failure for $lesson_path" >&2
    exit 1
    ;;
  lesson_empty.yaml)
    # Zero cases → accuracy 0.0
    echo '{"cases":[],"accuracy":0.0}'
    ;;
  *)
    echo "stub: unknown lesson: $lesson_path" >&2
    exit 1
    ;;
esac
STUB
chmod +x "$STUB_DIR/blendtutor"

export PATH="$STUB_DIR:$PATH"
export EVAL_COURSE_COUNTER="$COUNTER"

# ---------------------------------------------------------------------------
# Predicate 1 — script exists, is executable, takes exactly one argument.
# ---------------------------------------------------------------------------

echo "== Predicate 1: exists, executable, takes one arg =="

if [ -f "$SCRIPT" ]; then
  ok "script exists"
else
  ko "script exists — file not found: $SCRIPT"
fi

if [ -x "$SCRIPT" ]; then
  ok "script is executable"
else
  ko "script is executable — not executable: $SCRIPT"
fi

# No-arg → should fail with usage error.
if "$SCRIPT" > /dev/null 2>&1; then
  ko "no-arg exits non-zero — script accepted zero args"
else
  ok "no-arg exits non-zero"
fi

# Two-arg → should fail.
if "$SCRIPT" "$TMPDIR" "$TMPDIR" > /dev/null 2>&1; then
  ko "two-arg exits non-zero — script accepted two args"
else
  ok "two-arg exits non-zero"
fi

# ---------------------------------------------------------------------------
# Predicate 2 + 4 + 6 + Negative 1,2,5,6,7 — unequal case counts, all lessons,
# sum(matched)/sum(total), stub called N times, file paths not slugs, comments.
# ---------------------------------------------------------------------------

echo "== Predicate 2,4,6 + Negatives 1,2,5,6,7: unequal case counts =="

COURSE="$TMPDIR/course_unequal"
mkdir -p "$COURSE"

# TOML with inline comments (Negative #7 — catches fragile rg+sed parsers).
cat > "$COURSE/blendtutor.toml" <<'TOML'
# Course manifest with inline comments to catch fragile TOML parsers.
# Each lesson has an id (slug) and a path (file name).
[[lessons]]
id = "lesson-a"  # slug, NOT the path
path = "lesson_a.yaml"  # the file the script should pass to eval

[[lessons]]
id = "lesson-b"  # slug
path = "lesson_b.yaml"  # file path
TOML

# Create dummy lesson files so paths resolve.
touch "$COURSE/lesson_a.yaml" "$COURSE/lesson_b.yaml"

: > "$COUNTER"  # reset call counter

REPORT="$COURSE/eval-report.json"
rm -f "$REPORT"

"$SCRIPT" "$COURSE"

assert_file_exists "eval-report.json written" "$REPORT"

# Predicate 6 — report contains accuracy field.
if jq -e '.accuracy' "$REPORT" > /dev/null 2>&1; then
  ok "report has accuracy field"
else
  ko "report has accuracy field — .accuracy missing"
fi

# Predicate 4 + Negative 1 — sum(matched)/sum(total), NOT average(accuracy).
# Lesson A: 5/5 matched → accuracy 1.0
# Lesson B: 0/1 matched → accuracy 0.0
# sum(matched)/sum(total) = 5/6 = 0.8333...
# average(accuracy) = (1.0 + 0.0)/2 = 0.5  ← WRONG, must be caught
ACTUAL_ACC=$(jq '.accuracy' "$REPORT")
EXPECTED_APPROX="0.833"

# Check accuracy is approximately 5/6 (0.8333...), NOT 0.5.
if echo "$ACTUAL_ACC" | grep -qE '^0\.8333'; then
  ok "accuracy is 5/6=0.833 (sum formula, not average) — got: $ACTUAL_ACC"
else
  ko "accuracy is 5/6=0.833 — got: $ACTUAL_ACC (average cheat would give 0.5)"
fi

# Negative 1 — explicitly assert NOT 0.5 (the averaging cheat).
if echo "$ACTUAL_ACC" | grep -qE '^0\.5$'; then
  ko "accuracy is NOT 0.5 (averaging cheat detected)"
else
  ok "accuracy is NOT 0.5 (no averaging cheat)"
fi

# Predicate 2 + Negative 2 — ALL lessons processed (not just first).
CALL_COUNT=$(wc -l < "$COUNTER" | tr -d ' ')
assert_eq "stub called for ALL lessons (2)" "2" "$CALL_COUNT"

# Negative 6 — stub received file paths (.yaml), not slug ids.
if grep -q 'lesson_a\.yaml' "$COUNTER" && grep -q 'lesson_b\.yaml' "$COUNTER"; then
  ok "stub received file paths (not slug ids)"
else
  ko "stub received file paths — calls: $(cat "$COUNTER")"
fi

# Negative 5 — stub was actually called (not hardcoded report).
if [ "$CALL_COUNT" -gt 0 ]; then
  ok "stub was called (not hardcoded report)"
else
  ko "stub was called — zero calls, report may be hardcoded"
fi

# ---------------------------------------------------------------------------
# Predicate 3 + Negative 3 — aborts on non-zero eval exit, no partial report.
# ---------------------------------------------------------------------------

echo "== Predicate 3 + Negative 3: fail-fast on eval error =="

COURSE_FAIL="$TMPDIR/course_fail"
mkdir -p "$COURSE_FAIL"

cat > "$COURSE_FAIL/blendtutor.toml" <<'TOML'
# Course where the second lesson's eval fails.
[[lessons]]
id = "ok"
path = "lesson_a.yaml"

[[lessons]]
id = "boom"
path = "lesson_fail.yaml"
TOML

touch "$COURSE_FAIL/lesson_a.yaml" "$COURSE_FAIL/lesson_fail.yaml"

REPORT_FAIL="$COURSE_FAIL/eval-report.json"
rm -f "$REPORT_FAIL"

if "$SCRIPT" "$COURSE_FAIL" > /dev/null 2>&1; then
  ko "eval failure aborts — script exited 0 despite eval failure"
else
  ok "eval failure aborts (non-zero exit)"
fi

assert_file_absent "no partial report on failure" "$REPORT_FAIL"

# ---------------------------------------------------------------------------
# Predicate 5 + Negative 4 — zero lessons → {"accuracy": 0.0}, not NaN.
# ---------------------------------------------------------------------------

echo "== Predicate 5 + Negative 4: empty manifest → 0.0 =="

COURSE_EMPTY="$TMPDIR/course_empty"
mkdir -p "$COURSE_EMPTY"

cat > "$COURSE_EMPTY/blendtutor.toml" <<'TOML'
# Empty course — no lessons at all.
TOML

REPORT_EMPTY="$COURSE_EMPTY/eval-report.json"
rm -f "$REPORT_EMPTY"

"$SCRIPT" "$COURSE_EMPTY"

assert_file_exists "empty course writes report" "$REPORT_EMPTY"

EMPTY_ACC=$(jq '.accuracy' "$REPORT_EMPTY")
assert_eq "empty course accuracy is 0.0" "0.0" "$EMPTY_ACC"

# Negative 4 — explicitly NOT null/NaN.
if jq -e '.accuracy == 0' "$REPORT_EMPTY" > /dev/null 2>&1; then
  ok "empty course accuracy is 0.0 (not NaN/null)"
else
  ko "empty course accuracy is 0.0 — got: $EMPTY_ACC"
fi

# ---------------------------------------------------------------------------
# Predicate 5 (variant) — zero total cases (lessons exist but no cases).
# ---------------------------------------------------------------------------

echo "== Predicate 5 (variant): zero cases → 0.0 =="

COURSE_NOCASES="$TMPDIR/course_nocases"
mkdir -p "$COURSE_NOCASES"

cat > "$COURSE_NOCASES/blendtutor.toml" <<'TOML'
# Lessons exist but eval returns zero cases.
[[lessons]]
id = "empty"
path = "lesson_empty.yaml"
TOML

touch "$COURSE_NOCASES/lesson_empty.yaml"

REPORT_NOCASES="$COURSE_NOCASES/eval-report.json"
rm -f "$REPORT_NOCASES"

"$SCRIPT" "$COURSE_NOCASES"

assert_file_exists "zero-cases course writes report" "$REPORT_NOCASES"

NOCASES_ACC=$(jq '.accuracy' "$REPORT_NOCASES")
assert_eq "zero-cases accuracy is 0.0" "0.0" "$NOCASES_ACC"

# ---------------------------------------------------------------------------
# Predicate 6 — accuracy is finite f64 in [0.0, 1.0].
# ---------------------------------------------------------------------------

echo "== Predicate 6: accuracy in [0.0, 1.0] =="

# Re-check the unequal-course report.
ACC_VAL=$(jq '.accuracy' "$REPORT")
if jq -e '.accuracy >= 0 and .accuracy <= 1' "$REPORT" > /dev/null 2>&1; then
  ok "accuracy in [0.0, 1.0] — got: $ACC_VAL"
else
  ko "accuracy in [0.0, 1.0] — got: $ACC_VAL"
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
