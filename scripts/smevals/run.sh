#!/usr/bin/env bash
set -euo pipefail

# scripts/smevals/run.sh — smevals runner: invoke `blendtutor eval` for one
# task case and emit the verdict + feedback to stdout.
#
# Contract (AC-3):
#   - Reads the smevals task env (SMEVALS_TASK_LESSON/SMEVALS_TASK_CASE build
#     the argv; SMEVALS_MODEL/SMEVALS_PROMPT/SMEVALS_TASK_EXPECTED are
#     validated fail-closed so a misconfigured eval dir fails loudly, but are
#     not consumed — blendtutor eval derives the model from the provider
#     default, single-sourced to the same value configs/default.yaml names,
#     and the submission from the eval suite file).
#   - Invokes `blendtutor eval <lesson> --case N --format json` (AC-1) under a
#     per-call timeout, resolving blendtutor from PATH (never an in-tree build
#     invocation).
#   - Emits `verdict: correct|incorrect` (line 1, from the JSON `.cases[0]`.
#     `actual` token — the source of truth, never `.matched`) followed by the
#     byte-exact feedback_message (lines 2+, raw via jq so multi-line content
#     and trailing whitespace survive). smevals captures stdout as output.txt;
#     check_polarity.sh parses line 1 only.
#   - Bounded transient-only retry (3 attempts, 2s backoff): blendtutor
#     exit-1, timeout (124), empty stdout, and malformed JSON retry; a
#     well-formed run never retries — the polarity checker decides mismatches.
#   - No env scrubbing: FIREWORKS_API_KEY and friends reach blendtutor.
#
# Per-call timeout default 120s (typical LLM latency 5-30s); override via
# SMEVALS_TIMEOUT so tests can exercise the timeout path in seconds.

command -v jq >/dev/null 2>&1 || {
  echo "run.sh: jq not found in PATH — required to parse blendtutor eval JSON" >&2
  exit 1
}

: "${SMEVALS_TASK_LESSON:?SMEVALS_TASK_LESSON is required}"
: "${SMEVALS_TASK_CASE:?SMEVALS_TASK_CASE is required}"
: "${SMEVALS_MODEL:?SMEVALS_MODEL is required}"
: "${SMEVALS_PROMPT:?SMEVALS_PROMPT is required}"
: "${SMEVALS_TASK_EXPECTED:?SMEVALS_TASK_EXPECTED is required}"

MAX_ATTEMPTS=3
BACKOFF_SECONDS=2

attempt=0
while [ "$attempt" -lt "$MAX_ATTEMPTS" ]; do
  attempt=$((attempt + 1))
  exit_status=0
  json=""
  json="$(timeout "${SMEVALS_TIMEOUT:-120}" blendtutor eval \
    "$SMEVALS_TASK_LESSON" --case "$SMEVALS_TASK_CASE" --format json)" \
    || exit_status=$?

  if [ "$exit_status" -eq 0 ] \
      && [ -n "$json" ] \
      && jq -e '.cases[0].actual' >/dev/null 2>&1 <<<"$json"; then
    # Well-formed run: emit the header from .actual, then the verbatim
    # message (skipped entirely when empty — header line only).
    printf 'verdict: %s\n' "$(jq -r '.cases[0].actual' <<<"$json")"
    if jq -e '.cases[0].feedback_message != ""' >/dev/null 2>&1 <<<"$json"; then
      jq -r '.cases[0].feedback_message' <<<"$json"
    fi
    exit 0
  fi

  # Transient only: exit-1 (pipeline hiccup), timeout-124, and exit-0 with
  # empty or malformed stdout (the jq check above failed). Anything else —
  # usage error, missing binary — is permanent and fails immediately.
  retryable=false
  case "$exit_status" in
    0|1|124) retryable=true ;;
  esac
  if [ "$retryable" = false ]; then
    echo "run.sh: blendtutor eval failed permanently (exit $exit_status)" >&2
    exit 1
  fi

  if [ "$attempt" -lt "$MAX_ATTEMPTS" ]; then
    sleep "$BACKOFF_SECONDS"
  fi
done

echo "run.sh: blendtutor eval failed after $MAX_ATTEMPTS attempts" >&2
exit 1
