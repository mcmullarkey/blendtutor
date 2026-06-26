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

# Fail loudly (and block) on a bad base ref rather than emitting a raw git error
# or, worse, silently skipping the gate.
if ! git rev-parse --verify --quiet "$base^{commit}" >/dev/null; then
  echo "mutants(in-diff): base ref '$base' is not a valid commit" >&2
  exit 2
fi

# --no-ext-diff / --no-color keep the output a clean unified diff regardless of
# the user's diff.external or textconv config, which --in-diff must parse.
diff="$(git diff --no-color --no-ext-diff "$base"...HEAD)"
if [ -z "$diff" ]; then
  echo "mutants(in-diff): no commits vs $base — nothing to check"
  exit 0
fi

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT
printf '%s\n' "$diff" >"$tmp"

echo "mutants(in-diff): mutating changed core lines since $base …"
# Exit status propagates: cargo-mutants returns nonzero when a mutant survives,
# which blocks the push. When the diff touches no core mutants it returns 0
# (nothing to test — no per-mutant builds).
cargo mutants --in-diff "$tmp"
