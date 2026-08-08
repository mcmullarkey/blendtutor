#!/usr/bin/env bash
set -euo pipefail

# scripts/smevals/check_polarity.sh — smevals polarity checker.
#
# Contract (AC-3):
#   - Reads the runner's output.txt: the output file is argv 1 when given
#     (test-hermetic), else `${SMEVALS_RUN_DIR}/output.txt` (the real smevals
#     contract — smevals sets SMEVALS_RUN_DIR per run).
#   - Takes the verdict from line 1 ONLY (head -1 / read -r line 1 — never a
#     grep scan, so a `verdict: ` mention in the feedback body cannot confuse
#     it) and matches the full-line grammar `verdict: correct|incorrect`
#     exactly. Any other line-1 content — `verdict: maybe`, an empty value,
#     a missing space, wrong case, a 0-byte file — fails closed.
#   - Compares the actual polarity against SMEVALS_TASK_EXPECTED EXACTLY
#     (case-sensitive) and emits the smevals grader JSON contract on stdout:
#     `{"score": 1.0, "notes": "polarity match"}` + exit 0 on match, or
#     `{"score": 0.0, ...}` + exit 1 on mismatch. graders/default.yaml wires
#     this entry `required: true`, so a mismatch halts grading before any
#     later (AC-4 judge) check runs.

: "${SMEVALS_TASK_EXPECTED:?SMEVALS_TASK_EXPECTED is required}"

output_file="${1:-}"
if [ -z "$output_file" ]; then
  if [ -n "${SMEVALS_RUN_DIR:-}" ]; then
    output_file="$SMEVALS_RUN_DIR/output.txt"
  else
    echo "check_polarity.sh: no output file (pass as argv 1 or set SMEVALS_RUN_DIR)" >&2
    exit 1
  fi
fi

if [ ! -r "$output_file" ]; then
  echo "check_polarity.sh: output file not readable: $output_file" >&2
  exit 1
fi

line1="$(head -n 1 "$output_file")"

actual=""
case "$line1" in
  "verdict: correct") actual="correct" ;;
  "verdict: incorrect") actual="incorrect" ;;
esac

if [ "$actual" = "$SMEVALS_TASK_EXPECTED" ]; then
  printf '{"score": 1.0, "notes": "polarity match"}\n'
  exit 0
fi
printf '{"score": 0.0, "notes": "polarity mismatch"}\n'
exit 1
