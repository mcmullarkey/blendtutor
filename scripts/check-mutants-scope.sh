#!/usr/bin/env bash
# Assert cargo-mutants is scoped to the `core` crate.
#
# Slice 2 (#2) wires mutation testing as the §5.3 feedback signal for `core`
# logic only — the thin `cli` shell is out of scope. `.cargo/mutants.toml`
# enforces that scope; this script is its executable guard, runnable locally and
# from the mutation CI workflow. It uses the fast `--list` (no build), so it can
# gate cheaply.
set -euo pipefail

list="$(cargo mutants --list)"

# At least one mutant must live under the core crate.
if ! grep -Eq '^crates/core/' <<<"$list"; then
  echo "FAIL: cargo mutants --list enumerated no mutants under crates/core/" >&2
  echo "$list" >&2
  exit 1
fi

# Every enumerated mutant must be a core mutant — no crates/cli/ leakage. Allow
# blank lines and any "<n> mutants" summary line that some versions print.
if grep -Ev '^(crates/core/|[0-9]+ mutants| *$)' <<<"$list" >&2; then
  echo "FAIL: cargo mutants --list enumerated mutants outside crates/core/ (above)" >&2
  exit 1
fi

echo "OK: all $(grep -Ec '^crates/core/' <<<"$list") mutant(s) scoped to crates/core/"
