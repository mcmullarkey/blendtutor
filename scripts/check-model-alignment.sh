#!/usr/bin/env bash
# Assert the Fireworks default model id is identical across the two consumers
# that hardcode it — the Rust CLI (`provider.rs`) and the browser BYOK path
# (`feedback.js`) — and that the retired model id (`qwen3-vl-30b-a3b-instruct`)
# leaves no dead-code ghost anywhere in `crates/core/src/`.
#
# This is the executable spec for issue #71's cross-file alignment AC. The same
# predicates CI enforces, runnable by hand:
#   scripts/check-model-alignment.sh
#
# Uses `grep` (not `rg`) so CI needs no extra install — ubuntu-latest ships grep.
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

model="accounts/fireworks/models/deepseek-v4-flash"
rust_src="crates/core/src/llm/provider.rs"
js_src="crates/core/assets/shared/feedback.js"
retired="qwen3-vl-30b-a3b-instruct"

# AC1 — exact model id present in the Rust provider (the CLI default).
test -f "$rust_src" \
  || { echo "model-align: missing $rust_src" >&2; exit 1; }
grep -qF "$model" "$rust_src" \
  || { echo "model-align: $rust_src missing model id '$model'" >&2; exit 1; }

# AC2 — the same model id present in the JS BYOK fallback (browser default).
test -f "$js_src" \
  || { echo "model-align: missing $js_src" >&2; exit 1; }
grep -qF "$model" "$js_src" \
  || { echo "model-align: $js_src missing model id '$model'" >&2; exit 1; }

# AC3 (negative) — the retired model id appears NOWHERE in crates/core/src/,
# catching a dead-code ghost left in a comment or unreachable branch.
if grep -rqF "$retired" crates/core/src/; then
  echo "model-align: retired model id '$retired' still present in crates/core/src/:" >&2
  grep -rnF "$retired" crates/core/src/ >&2 || true
  exit 1
fi

echo "model-align: OK — Rust and JS agree on '$model', '$retired' absent"
