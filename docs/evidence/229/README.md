# Issue #229 — E2E evidence: grader feedback on eval mismatch + next-steps footer

Real `blendtutor eval` runs (built binary, real Rscript interpreter) against a
local OpenAI-compatible provider stub (`provider_stub.py`, mirroring the
wiremock mounts in `crates/cli/tests/eval.rs`). Fixture:
`crates/core/tests/fixtures/eval_command/demo_lesson.yaml` (alpha/beta/gamma,
expected polarities `[correct, incorrect, correct]`).

## Runs

| File | Command shape | Shows |
|---|---|---|
| `eval_human_mismatch.txt` | `eval demo_lesson.yaml` (stub: gamma→incorrect) | F1: `grader: gamma is off` indented under the `[mismatch]` row; F2: footer naming case 3, `--case N`, `llm_evaluation_prompt` + `solution`. Exit 0 (P3). |
| `eval_human_full_match.txt` | `eval demo_lesson.yaml` (stub: gamma→correct) | Negative arm: accuracy 3/3, NO footer, NO guidance lines, NO `grader:` lines. Exit 0. |
| `eval_json_mismatch.txt` | `eval demo_lesson.yaml --format json` (stub: gamma→incorrect) | P2: JSON byte-shape unchanged — `cases`/`accuracy`, per-case `expected`/`actual`/`matched`/`feedback_message`, no footer concept. Exit 0. |

`.stderr.txt` files are empty (no diagnostics mixed into stdout).

## Reproduce

```bash
cargo build -p blendtutor-cli
uv run python docs/evidence/229/provider_stub.py 18731 &  # mismatch stub
FIREWORKS_API_KEY=test-key BLENDTUTOR_PROVIDER_URL=http://127.0.0.1:18731 \
  target/debug/blendtutor eval crates/core/tests/fixtures/eval_command/demo_lesson.yaml
```

For the full-match run, patch the stub's `gamma` verdict to `(True, "gamma looks right")`.
