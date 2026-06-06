#!/usr/bin/env bash
# Block a push when the pushed `core` changes leave a surviving mutant.
#
# Issue #24: mutation testing as a per-push gate. Runs `cargo mutants --in-diff`
# over the range being pushed (<base>...HEAD), so only the lines this branch
# adds/changes are mutated; `.cargo/mutants.toml` (#2) further scopes mutation to
# the `core` crate. A surviving mutant means a changed core branch that no test
# pins — the push is blocked (nonzero exit). `git push --no-verify` bypasses it.
#
# Thin enough to call from .githooks/pre-push, and runnable by hand:
#   scripts/check-mutants-diff.sh origin/staging/rewrite-in-rust-lol
set -euo pipefail

base="${1:?usage: check-mutants-diff.sh <base-ref>}"

diff="$(git diff --no-color "$base"...HEAD)"
if [ -z "$diff" ]; then
  echo "mutants(in-diff): no commits vs $base — nothing to check"
  exit 0
fi

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT
printf '%s\n' "$diff" >"$tmp"

echo "mutants(in-diff): mutating changed core lines since $base …"
# Exit status propagates: cargo-mutants returns nonzero when a mutant survives,
# which blocks the push. With no core mutants in the diff it returns 0 (fast).
cargo mutants --in-diff "$tmp"
