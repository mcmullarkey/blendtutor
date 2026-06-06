#!/usr/bin/env bash
# Assert the mutation-testing CI workflow exists and is non-gating.
#
# Slice 2 (#2) AC2: cargo-mutants is slow, so it must never sit on the per-PR
# critical path. The workflow runs only on a schedule, on manual dispatch, or
# when a PR is explicitly given the `mutation` label, and it surfaces results to
# the run's job summary. Confirming the job is absent from branch-protection
# required checks stays a manual step (GitHub settings, not the file).
#
# The checks run against the *parsed* workflow, not the raw text. A structural
# assertion cannot be fooled by a commented-out trigger or an incidental mention
# of "mutation" in prose — the comment-evasion failure mode that a pile of
# per-line regex markers kept reintroducing one marker at a time. Parsing also
# lets us assert the real non-gating invariant: pull_request is restricted to
# the `labeled` event, so the job never fires on an ordinary PR push.
set -euo pipefail

wf=".github/workflows/mutants.yml"

if [[ ! -f "$wf" ]]; then
  echo "FAIL: $wf does not exist" >&2
  exit 1
fi

# pyyaml via uv keeps the guard dependency-light and matches this project's
# convention of running Python through uv.
uv run --no-project --quiet --with pyyaml python - "$wf" <<'PY'
import sys

import yaml

wf = sys.argv[1]
with open(wf) as f:
    doc = yaml.safe_load(f)

problems = []

# PyYAML (YAML 1.1) parses the bare `on:` key as the boolean True.
triggers = doc.get(True) or doc.get("on") or {}
for key in ("schedule", "workflow_dispatch", "pull_request"):
    if key not in triggers:
        problems.append(f"missing trigger: on.{key}")

# A label-only pull_request trigger keeps the job off ordinary PR pushes; an
# unconditional `pull_request: {}` would run it on every PR (the AC2 negative).
pr = triggers.get("pull_request")
if isinstance(pr, dict):
    if "labeled" not in (pr.get("types") or []):
        problems.append("on.pull_request must restrict types to [labeled]")
elif pr is not None:
    problems.append("on.pull_request must be a mapping scoped to types: [labeled]")

job = (doc.get("jobs") or {}).get("mutants", {})

# The job must be gated on the `mutation` label for pull_request events.
cond = str(job.get("if", ""))
if not ("contains(" in cond and "labels" in cond and "mutation" in cond):
    problems.append(
        "job.mutants.if must gate on the 'mutation' label "
        "(contains(... labels ..., 'mutation'))"
    )

# Some step must write the job summary via a real >> redirect.
runs = [str(s.get("run", "")) for s in (job.get("steps") or [])]
if not any(">>" in r and "GITHUB_STEP_SUMMARY" in r for r in runs):
    problems.append("no step writes results to $GITHUB_STEP_SUMMARY (>> redirect)")

if problems:
    print(f"FAIL: {wf} is not a valid non-gating mutation workflow:", file=sys.stderr)
    for p in problems:
        print(f"  - {p}", file=sys.stderr)
    sys.exit(1)

print(f"OK: {wf} is label-gated, schedule/dispatch-triggered, and writes the job summary")
PY
