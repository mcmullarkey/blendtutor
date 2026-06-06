#!/usr/bin/env bash
# Assert the mutation-testing CI workflow exists and is non-gating.
#
# Slice 2 (#2) AC2: cargo-mutants is slow, so it must never sit on the per-PR
# critical path. The workflow runs only on a schedule, on manual dispatch, or
# when a PR is explicitly given the `mutation` label, and it surfaces results to
# the run's job summary. This guard encodes those required markers. Confirming
# the job is absent from branch-protection required checks stays a manual step
# (it lives in GitHub settings, not in the file).
set -euo pipefail

wf=".github/workflows/mutants.yml"

if [[ ! -f "$wf" ]]; then
  echo "FAIL: $wf does not exist" >&2
  exit 1
fi

fail=0
require() {
  # $1 = extended-regex marker, $2 = human description
  if ! grep -Eq "$1" "$wf"; then
    echo "FAIL: $wf is missing $2 (/$1/)" >&2
    fail=1
  fi
}

require 'schedule:' 'a schedule trigger'
require 'workflow_dispatch:' 'a manual workflow_dispatch trigger'
require 'mutation' 'a mutation-label condition'
require 'GITHUB_STEP_SUMMARY' 'a step that writes the job summary'

if [[ "$fail" -ne 0 ]]; then
  exit 1
fi

echo "OK: $wf declares schedule + workflow_dispatch + mutation-label triggers and writes GITHUB_STEP_SUMMARY"
