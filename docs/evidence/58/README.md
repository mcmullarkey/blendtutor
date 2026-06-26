# Issue #58 — BYOK feedback panel styling

## Summary
Appended BYOK/feedback CSS section to `styles.css` with 2 new tokens and rules for all `data-byok`/`data-correct`/`#byok-disclosure` selectors. Pure CSS — `feedback.js` byte-unchanged.

## Files changed
| File | Change |
|------|--------|
| `crates/core/assets/shared/styles.css` | +112 lines: 2 new `:root` tokens + BYOK/feedback rules |
| `crates/cli/tests/build.rs` | +158 lines: `build_webr_styles_byok_feedback_panel` test |

## Build verification
- **webr** — `cargo run -p blendtutor-cli -- build --target webr crates/core/tests/fixtures/r-course -o /tmp/bt-site-58-webr` → `built 2 lesson(s) for webr`
- **pyodide** — `cargo run -p blendtutor-cli -- build --target pyodide crates/core/tests/fixtures/python-course -o /tmp/bt-site-58-pyodide` → `built 1 lesson(s) for pyodide`

## Target parity
- `styles.css` byte-identical across targets (11280 bytes each)
- `feedback.js` byte-identical across targets (25540 bytes each)
- `feedback.js` unchanged from pre-AC-3 baseline — `git diff staging/rewrite-in-rust-lol -- crates/core/assets/shared/feedback.js` = empty

## Test results
`cargo test -p blendtutor-core -p blendtutor-cli -q` — all 155 pass:
- 109 core unit tests
- 11 build integration tests (including new `build_webr_styles_byok_feedback_panel`)
- 35 other tests (cli, cutover, eval, grade, init, list, new, readme, run, validate, llm, runner)

## What was tested
| Assertion | Status |
|-----------|--------|
| 11 BYOK selectors present in `/* === feedback / BYOK === */` section | ✅ |
| Each selector block has ≥1 `var(--bt-*)` ref | ✅ |
| `[data-byok="verdict"][data-correct="true"]` and `[data-correct="false"]` have different declarations | ✅ |
| True block uses `--bt-color-success-bg` | ✅ |
| False block uses `--bt-color-danger-bg` | ✅ |
| `#byok-disclosure` has `color` or `font-size` | ✅ |
| ≥1 new AC-3 token declared in `:root` | ✅ |
| ≥1 new AC-3 token used in BYOK section | ✅ |
| No hardcoded hex in BYOK rules | ✅ |
| `styles.css` byte-identical across targets | ✅ |
| `feedback.js` byte-identical across targets | ✅ |
| `feedback.js` unchanged from pre-AC-3 | ✅ |
