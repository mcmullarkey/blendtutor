#!/usr/bin/env bash
# Stub-based integration test for scripts/smevals/run.sh + check_polarity.sh.
#
# Verifies the 17-point compound predicate from AC-3:
#   1. Env→argv wiring: stub blendtutor asserts `eval <lesson> --case N --format
#      json` built from SMEVALS_TASK_LESSON/SMEVALS_TASK_CASE; counter file
#      proves invocation (no canned output).
#   2. No `cargo run` anywhere in scripts/smevals/*.sh — blendtutor from PATH.
#   3. Header contract: line 1 == `verdict: correct|incorrect` (exact,
#      lowercase); lines 2+ == feedback_message byte-exact, multi-line kept.
#   4. Source-of-truth = .actual token, NOT .matched (inconsistent-JSON stub).
#   5. Four polarity combos → checker exit 0 on matches, non-zero on mismatches.
#   6. Line-1-only parsing: `verdict: ` in the body must not confuse the checker.
#   7. Fail-closed on malformed headers (maybe / empty / no-space / 0-byte).
#   8. Case-sensitive token: `verdict: Correct` fails closed.
#   9. Bounded transient-only retry: exit-1, empty stdout, malformed JSON all
#      retry (max 3, sleep 2); well-formed mismatch verdict does NOT retry.
#  10. Backoff wall-time >= 4s for a retry-then-fail run.
#  11. Per-call timeout: SMEVALS_TIMEOUT=2 + sleep-5 stub → 124 → transient retry.
#  12. Missing SMEVALS_TASK_LESSON/SMEVALS_TASK_CASE → non-zero naming the var.
#  13. FIREWORKS_API_KEY propagation (no env scrubbing in the runner).
#  14. Empty feedback_message → header only, zero following lines, exit 0.
#  15. jq absent from PATH → runner exits non-zero naming jq.
#  16. Both scripts start with `set -euo pipefail`.
#  17. Generator template checker path == real checker script path (cross-file).
#
# Also catches the negative cheat: inconsistent JSON (.actual=incorrect,
# .matched=true) whose message body contains `verdict: correct` on line 3 — a
# sneaky-pass runner reading .matched or grep-scanning for verdict: yields the
# wrong polarity and the checker wrongly exits 0.
#
# The stub-on-PATH + counter-file pattern follows scripts/tests/eval-course.sh
# (stub at :83-137). macOS lacks GNU coreutils `timeout`, so the test also puts
# a GNU-compatible `timeout` shim on PATH FIRST — deterministic on macOS and on
# CI ubuntu (which would otherwise use the real coreutils timeout).
#
# Usage: bash scripts/tests/test_smevals_runner.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

RUNNER="scripts/smevals/run.sh"
CHECKER="scripts/smevals/check_polarity.sh"
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

# ---------------------------------------------------------------------------
# Setup: temp dir + stub blendtutor + GNU-compatible timeout shim on PATH.
# ---------------------------------------------------------------------------

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

STUB_DIR="$TMPDIR/bin"
mkdir -p "$STUB_DIR"

# Counter file — proves the stub was invoked (predicate 1: no canned output).
COUNTER="$TMPDIR/calls.txt"
: > "$COUNTER"

cat > "$STUB_DIR/blendtutor" <<'STUB'
#!/usr/bin/env bash
# Stub blendtutor eval — asserts the AC-3 argv contract, records each call,
# and emits canned JSON selected by STUB_MODE.
set -euo pipefail

COUNTER="${SMEVALS_RUNNER_COUNTER:-/dev/null}"
MODE="${STUB_MODE:-correct}"

# Predicate 13 — FIREWORKS_API_KEY must reach the subprocess unscrubbed.
if [ "${STUB_ASSERT_API_KEY:-0}" = "1" ] && [ -z "${FIREWORKS_API_KEY:-}" ]; then
  echo "stub: FIREWORKS_API_KEY not visible in environment" >&2
  exit 1
fi

# Argv contract (predicate 1): eval <lesson> --case N --format json.
if [ "$1" != "eval" ] || [ "$3" != "--case" ] || [ "$5" != "--format" ] || [ "$6" != "json" ]; then
  echo "stub: unexpected argv: $*" >&2
  exit 1
fi
if [ "$2" != "$SMEVALS_TASK_LESSON" ] || [ "$4" != "$SMEVALS_TASK_CASE" ]; then
  echo "stub: argv lesson/case do not match env: $2 / $4" >&2
  exit 1
fi

echo "eval $2 --case $4 --format json" >> "$COUNTER"

case "$MODE" in
  correct)
    echo '{"cases":[{"expected":"correct","actual":"correct","matched":true,"feedback_message":"all good\nsecond line"}],"accuracy":1.0}'
    ;;
  incorrect)
    echo '{"cases":[{"expected":"correct","actual":"incorrect","matched":false,"feedback_message":"try again"}],"accuracy":0.0}'
    ;;
  inconsistent)
    # Negative cheat: .actual=incorrect but .matched=true, and the message body
    # contains `verdict: correct` on its second line (= output line 3).
    echo '{"cases":[{"expected":"correct","actual":"incorrect","matched":true,"feedback_message":"feedback one\nverdict: correct\ntail"}],"accuracy":0.0}'
    ;;
  empty-msg)
    echo '{"cases":[{"expected":"correct","actual":"correct","matched":true,"feedback_message":""}],"accuracy":1.0}'
    ;;
  fail)
    echo "stub: simulated eval failure" >&2
    exit 1
    ;;
  empty)
    # Exit 0 with no stdout — transient (empty output).
    exit 0
    ;;
  garbage)
    echo "this is not json at all"
    ;;
  slow)
    sleep 5
    echo '{"cases":[{"expected":"correct","actual":"correct","matched":true,"feedback_message":"slow"}],"accuracy":1.0}'
    ;;
  *)
    echo "stub: unknown mode: $MODE" >&2
    exit 1
    ;;
esac
STUB
chmod +x "$STUB_DIR/blendtutor"

cat > "$STUB_DIR/timeout" <<'STUB'
#!/usr/bin/env bash
# Minimal GNU-timeout-compatible shim (CI uses real coreutils; local macOS
# often lacks GNU timeout). Runs "$@" with SIGTERM to its process group after
# $1 seconds; exits 124 on timeout (GNU contract), else the command's own exit
# status. `set -m` puts the command in its own group so the group-kill takes
# the command AND its children down together — killing only the parent would
# orphan a child holding the caller's stdout pipe open.
set -euo pipefail
set -m
limit="$1"
shift
"$@" &
pid=$!
# The killer must not inherit the caller's stdout pipe: if it is orphaned, a
# held pipe would block the runner's command substitution until it expires.
( sleep "$limit"; kill -TERM -"$pid" 2>/dev/null || true ) >/dev/null 2>&1 &
killer=$!
status=0
wait "$pid" || status=$?
kill "$killer" 2>/dev/null || true
if [ "$status" -eq 143 ]; then
  exit 124
fi
exit "$status"
STUB
chmod +x "$STUB_DIR/timeout"

export PATH="$STUB_DIR:$PATH"
export SMEVALS_RUNNER_COUNTER="$COUNTER"

# ---------------------------------------------------------------------------
# Helpers: run the runner / checker with the standard smevals task env.
# ---------------------------------------------------------------------------

# run_runner_status <out> <err> [VAR=val overrides...] — sets RUNNER_STATUS.
run_runner_status() {
  local out="$1" err="$2"
  shift 2
  RUNNER_STATUS=0
  env STUB_MODE="${STUB_MODE:-correct}" \
      SMEVALS_MODEL="accounts/fireworks/models/deepseek-v4-flash-0731" \
      SMEVALS_PROMPT="prompt-placeholder" \
      SMEVALS_TASK_LESSON="lessons/alpha.yaml" \
      SMEVALS_TASK_CASE="2" \
      SMEVALS_TASK_EXPECTED="correct" \
      "$@" "$RUNNER" >"$out" 2>"$err" || RUNNER_STATUS=$?
}

# checker_status <expected> <output-file> — sets CHECKER_STATUS.
checker_status() {
  local expected="$1" file="$2"
  CHECKER_STATUS=0
  SMEVALS_TASK_EXPECTED="$expected" "$CHECKER" "$file" >/dev/null 2>&1 \
    || CHECKER_STATUS=$?
}

OUT="$TMPDIR/out.txt"
ERR="$TMPDIR/err.txt"

# ---------------------------------------------------------------------------
# Predicate 1 + 3 — env→argv wiring, counter proof, header contract.
# ---------------------------------------------------------------------------

echo "== Predicate 1 + 3: env→argv wiring + header contract =="

: > "$COUNTER"
run_runner_status "$OUT" "$ERR"
assert_eq "runner exits 0" "0" "$RUNNER_STATUS"
assert_eq "stub invoked exactly once" "1" "$(wc -l < "$COUNTER" | tr -d ' ')"
assert_eq "argv = eval <lesson> --case N --format json" \
  "eval lessons/alpha.yaml --case 2 --format json" "$(cat "$COUNTER")"
assert_eq "line 1 = verdict: correct" "verdict: correct" "$(sed -n '1p' "$OUT")"
assert_eq "message byte-exact, multi-line preserved" "all good
second line" "$(tail -n +2 "$OUT")"

# ---------------------------------------------------------------------------
# Predicate 2 — no `cargo run` in the smevals scripts.
# ---------------------------------------------------------------------------

echo "== Predicate 2: no cargo run =="

# grep -c exits 1 on zero matches — the group's `|| true` keeps the pipeline
# status 0 (pipefail would otherwise kill the test) without double-printing.
COUNT=$({ grep -c 'cargo run' scripts/smevals/run.sh scripts/smevals/check_polarity.sh \
  2>/dev/null || true; } | awk -F: '{s += $2} END {print s+0}')
assert_eq "grep -c 'cargo run' scripts/smevals/*.sh == 0" "0" "$COUNT"

# ---------------------------------------------------------------------------
# Predicate 4 + negative — .actual is the source of truth, not .matched.
# ---------------------------------------------------------------------------

echo "== Predicate 4 + negative: .actual source of truth, fail-closed =="

: > "$COUNTER"
STUB_MODE=inconsistent run_runner_status "$OUT" "$ERR"
assert_eq "runner exits 0 on well-formed JSON" "0" "$RUNNER_STATUS"
assert_eq "verdict from .actual (incorrect), not .matched" \
  "verdict: incorrect" "$(sed -n '1p' "$OUT")"

# Negative: expected=correct vs the sneaky-pass output → checker must fail.
checker_status "correct" "$OUT"
if [ "$CHECKER_STATUS" -ne 0 ]; then
  ok "negative: checker fails closed on inconsistent-JSON output"
else
  ko "negative: checker fails closed on inconsistent-JSON output — sneaky-pass!"
fi

# ---------------------------------------------------------------------------
# Predicate 5 — all four polarity combinations.
# ---------------------------------------------------------------------------

echo "== Predicate 5: four polarity combinations =="

printf 'verdict: correct\n' > "$TMPDIR/p5-correct.txt"
printf 'verdict: incorrect\n' > "$TMPDIR/p5-incorrect.txt"

checker_status "correct" "$TMPDIR/p5-correct.txt"
assert_eq "correct×correct → exit 0" "0" "$CHECKER_STATUS"
checker_status "incorrect" "$TMPDIR/p5-incorrect.txt"
assert_eq "incorrect×incorrect → exit 0" "0" "$CHECKER_STATUS"
checker_status "correct" "$TMPDIR/p5-incorrect.txt"
if [ "$CHECKER_STATUS" -ne 0 ]; then
  ok "correct×incorrect → non-zero"
else
  ko "correct×incorrect → non-zero"
fi
checker_status "incorrect" "$TMPDIR/p5-correct.txt"
if [ "$CHECKER_STATUS" -ne 0 ]; then
  ok "incorrect×correct → non-zero"
else
  ko "incorrect×correct → non-zero"
fi

# ---------------------------------------------------------------------------
# Predicate 6 — line-1-only parsing (a body `verdict:` must not confuse).
# ---------------------------------------------------------------------------

echo "== Predicate 6: line-1-only parsing =="

printf 'verdict: incorrect\nbody mentions verdict: correct\n' > "$TMPDIR/p6a.txt"
checker_status "incorrect" "$TMPDIR/p6a.txt"
assert_eq "line 1 wins over body mention" "0" "$CHECKER_STATUS"

printf 'plain body\nverdict: correct\n' > "$TMPDIR/p6b.txt"
checker_status "correct" "$TMPDIR/p6b.txt"
if [ "$CHECKER_STATUS" -ne 0 ]; then
  ok "body-only verdict: ignored (a grep scanner would pass here)"
else
  ko "body-only verdict: ignored (a grep scanner would pass here)"
fi

# ---------------------------------------------------------------------------
# Predicate 7 — fail-closed on malformed headers.
# ---------------------------------------------------------------------------

echo "== Predicate 7: fail-closed on malformed headers =="

printf 'verdict: maybe\n' > "$TMPDIR/p7-maybe.txt"
printf 'verdict:\n' > "$TMPDIR/p7-empty.txt"
printf 'verdict:correct\n' > "$TMPDIR/p7-nospace.txt"
: > "$TMPDIR/p7-zero.txt"

for name in p7-maybe p7-empty p7-nospace p7-zero; do
  checker_status "correct" "$TMPDIR/$name.txt"
  if [ "$CHECKER_STATUS" -ne 0 ]; then
    ok "malformed ($name) fails closed"
  else
    ko "malformed ($name) fails closed"
  fi
done

# ---------------------------------------------------------------------------
# Predicate 8 — case-sensitive token.
# ---------------------------------------------------------------------------

echo "== Predicate 8: case-sensitive token =="

printf 'verdict: Correct\n' > "$TMPDIR/p8.txt"
checker_status "correct" "$TMPDIR/p8.txt"
if [ "$CHECKER_STATUS" -ne 0 ]; then
  ok "verdict: Correct fails closed"
else
  ko "verdict: Correct fails closed"
fi

# ---------------------------------------------------------------------------
# Predicate 9 + 10 — bounded transient-only retry + backoff wall-time.
# ---------------------------------------------------------------------------

echo "== Predicate 9 + 10: bounded transient-only retry + backoff =="

for mode in fail empty garbage; do
  : > "$COUNTER"
  START=$(date +%s)
  STUB_MODE="$mode" run_runner_status "$OUT" "$ERR"
  ELAPSED=$(( $(date +%s) - START ))
  assert_eq "$mode: exactly 3 attempts (max)" "3" "$(wc -l < "$COUNTER" | tr -d ' ')"
  if [ "$RUNNER_STATUS" -ne 0 ]; then
    ok "$mode: runner exits non-zero after retries"
  else
    ko "$mode: runner exits non-zero after retries"
  fi
  if [ "$ELAPSED" -ge 4 ]; then
    ok "$mode: backoff wall-time >= 4s (${ELAPSED}s)"
  else
    ko "$mode: backoff wall-time >= 4s (${ELAPSED}s)"
  fi
done

# No-retry arm: a well-formed (mismatched) verdict must NOT be retried.
: > "$COUNTER"
STUB_MODE=incorrect run_runner_status "$OUT" "$ERR"
assert_eq "well-formed mismatch: exit 0" "0" "$RUNNER_STATUS"
assert_eq "well-formed mismatch: exactly 1 call (no retry)" \
  "1" "$(wc -l < "$COUNTER" | tr -d ' ')"

# ---------------------------------------------------------------------------
# Predicate 11 — per-call timeout (SMEVALS_TIMEOUT), exit 124 → transient.
# ---------------------------------------------------------------------------

echo "== Predicate 11: per-call timeout =="

: > "$COUNTER"
START=$(date +%s)
STUB_MODE=slow SMEVALS_TIMEOUT=2 run_runner_status "$OUT" "$ERR"
ELAPSED=$(( $(date +%s) - START ))
assert_eq "timeout (124) treated as transient → 3 attempts" \
  "3" "$(wc -l < "$COUNTER" | tr -d ' ')"
if [ "$RUNNER_STATUS" -ne 0 ]; then
  ok "runner exits non-zero after timeout retries"
else
  ko "runner exits non-zero after timeout retries"
fi
if [ "$ELAPSED" -lt 15 ]; then
  ok "timeout actually killed the sleeps (${ELAPSED}s < 15s)"
else
  ko "timeout actually killed the sleeps (${ELAPSED}s >= 15s)"
fi

# ---------------------------------------------------------------------------
# Predicate 12 — missing env → usage error naming the variable.
# ---------------------------------------------------------------------------

echo "== Predicate 12: missing env naming the variable =="

if env SMEVALS_MODEL=m SMEVALS_PROMPT=p SMEVALS_TASK_EXPECTED=correct \
    "$RUNNER" >/dev/null 2>"$ERR"; then
  ko "missing SMEVALS_TASK_LESSON exits non-zero"
else
  if grep -q 'SMEVALS_TASK_LESSON' "$ERR"; then
    ok "missing SMEVALS_TASK_LESSON named on stderr"
  else
    ko "missing SMEVALS_TASK_LESSON named on stderr — got: $(cat "$ERR")"
  fi
fi

if env SMEVALS_MODEL=m SMEVALS_PROMPT=p SMEVALS_TASK_EXPECTED=correct \
    SMEVALS_TASK_LESSON=lessons/x.yaml "$RUNNER" >/dev/null 2>"$ERR"; then
  ko "missing SMEVALS_TASK_CASE exits non-zero"
else
  if grep -q 'SMEVALS_TASK_CASE' "$ERR"; then
    ok "missing SMEVALS_TASK_CASE named on stderr"
  else
    ko "missing SMEVALS_TASK_CASE named on stderr — got: $(cat "$ERR")"
  fi
fi

# ---------------------------------------------------------------------------
# Predicate 13 — FIREWORKS_API_KEY reaches the subprocess unscrubbed.
# ---------------------------------------------------------------------------

echo "== Predicate 13: FIREWORKS_API_KEY propagation =="

: > "$COUNTER"
STUB_ASSERT_API_KEY=1 FIREWORKS_API_KEY=fw_dummy-key run_runner_status "$OUT" "$ERR"
assert_eq "stub saw the key → runner exit 0 (no env scrubbing)" "0" "$RUNNER_STATUS"

# ---------------------------------------------------------------------------
# Predicate 14 — empty feedback_message.
# ---------------------------------------------------------------------------

echo "== Predicate 14: empty feedback_message =="

: > "$COUNTER"
STUB_MODE=empty-msg run_runner_status "$OUT" "$ERR"
assert_eq "runner exits 0" "0" "$RUNNER_STATUS"
assert_eq "header line emitted" "verdict: correct" "$(sed -n '1p' "$OUT")"
assert_eq "zero following lines" "1" "$(wc -l < "$OUT" | tr -d ' ')"

# ---------------------------------------------------------------------------
# Predicate 15 — jq guard.
# ---------------------------------------------------------------------------

echo "== Predicate 15: jq absent → non-zero naming jq =="

NOJQ="$TMPDIR/nojq"
mkdir -p "$NOJQ"
ln -s "$(command -v bash)" "$NOJQ/bash"

if env PATH="$NOJQ" "$RUNNER" >/dev/null 2>"$ERR"; then
  ko "jq-absent runner exits non-zero"
else
  if grep -qi 'jq' "$ERR"; then
    ok "jq guard names jq"
  else
    ko "jq guard names jq — got: $(cat "$ERR")"
  fi
fi

# ---------------------------------------------------------------------------
# Predicate 16 — both scripts start with set -euo pipefail.
# ---------------------------------------------------------------------------

echo "== Predicate 16: set -euo pipefail hygiene =="

for script in "$RUNNER" "$CHECKER"; do
  assert_eq "$(basename "$script") starts with set -euo pipefail" \
    "set -euo pipefail" "$(sed -n '2p' "$script")"
done

# ---------------------------------------------------------------------------
# Predicate 17 — grader-template checker path == real checker script path.
# ---------------------------------------------------------------------------

echo "== Predicate 17: generator template path == actual script path =="

if grep -qF 'scripts/smevals/run.sh' crates/core/src/smevals_gen.rs; then
  ok "generator records scripts/smevals/run.sh"
else
  ko "generator records scripts/smevals/run.sh"
fi
if [ -f scripts/smevals/run.sh ]; then
  ok "runner exists at pinned path"
else
  ko "runner exists at pinned path"
fi
if grep -qF 'scripts/smevals/check_polarity.sh' crates/core/src/smevals_gen.rs; then
  ok "generator records scripts/smevals/check_polarity.sh"
else
  ko "generator records scripts/smevals/check_polarity.sh"
fi
if [ -f scripts/smevals/check_polarity.sh ]; then
  ok "checker exists at pinned path"
else
  ko "checker exists at pinned path"
fi
if grep -qF 'required: true' crates/core/src/smevals_gen.rs; then
  ok "graders template pins required: true"
else
  ko "graders template pins required: true"
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
