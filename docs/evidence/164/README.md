# Issue #164 — E2E Evidence

byok-api-key AC-3: deploy feedback+key-page assets and auto-mount feedback in
blendtutor.lua. Verification: code (shell render-and-assert scripts).

## Probe runs (all from `main`-based branch `164-blendtutor-key-page-mount`)

| Artifact | Command | Result |
|----------|---------|--------|
| test-quarto-bootstrap.log | `bash scripts/tests/test_quarto_bootstrap.sh` | **71 passed, 0 failed** (C5-C11, C15-C18, C19-C22 asserts) |
| test-quarto-asset-deployment.log | `bash scripts/tests/test_quarto_asset_deployment.sh` | **68 passed, 0 failed** (C1-C4, C14, C23) |
| test-quarto-render.log | `bash scripts/tests/test_quarto_render.sh` | **12 passed, 0 failed** (verify path — regression) |
| test-quarto-feedback.log | `uv run python scripts/tests/test_quarto_feedback.py` | **40 passed, 0 failed** (regression — __btConfig allow-list unaffected) |
| test-quarto-filter.log | `bash scripts/tests/test_quarto_filter.sh` | **27 passed, 0 failed** (regression — full CI matrix) |
| test-quarto-install-render.log | `bash scripts/tests/test_quarto_install_render.sh` | **13 passed, 0 failed** (regression — full CI matrix) |
| test-quarto-distribution.log | `bash scripts/tests/test_quarto_distribution.sh` | **78 passed, 0 failed** (regression — full CI matrix) |
| test-quarto-key-page.log | `uv run python scripts/tests/test_quarto_key_page.py` | **40 passed, 0 failed** (AC-2 suite regression) |

## Local-machine caveat: test_quarto_ux.py

`test_quarto_ux.py` hardcodes a 60 s subprocess timeout for the ux.qmd render
(`timeout=60`, pre-existing since #142). On this local machine a quarto-fixture
render takes ~80 s — verified INDEPENDENT of this issue by swapping in `main`'s
`blendtutor.lua` and re-timing: main's filter renders ux.qmd in 78 s, this
branch's in 83 s (both exceed the 60 s limit). The filter-code delta is not the
cause; the local quarto/deno render overhead is. CI runners (GH Actions) pass
the same script historically — this is an environment-speed artifact, not a
regression.

## Key assertions exercised

- **C1/C2** — `resources` static-pins `assets/exercise-feedback.js` + `assets/key-page.js`; both exist at libs dir post-render (mixed-lang + key-only hermetic).
- **C5-C9/C11** — bootstrap imports `mountAllFeedback` + `mountKeyPage` from libs URLs; registry hoisted to one shared const passed to both `start()` and `mountAllFeedback()`; mountAllFeedback line-order inside `.then(` after `start(` before `.catch(`; exactly one call site.
- **C12-C14** — `::: {.blendtutor-key}` renders `<div class="blendtutor-key">`; `has_key` set in `Div()` before the non-blendtutor early-return; guards broadened to `has_blendtutor or has_key` at all 3 emission sites; key-only page (quarto-fixture/key-only.qmd) deploys both assets + injects bootstrap + calls mountKeyPage.
- **C15-C18** — `bt-feedback: false` and `bt-feedback: "false"` (string parity via meta_string normalization) suppress ONLY mountAllFeedback; `bt-auto-bootstrap: false` → zero bootstrap but keyPageUrl head script still present.
- **C19-C22** — separate classic `<script>` head emission `window.__btConfig = window.__btConfig || {}; window.__btConfig.keyPageUrl = ...`; default `api-key.html`; custom `bt-key-page: custom-key.html` honored; no bare `= {...}` clobber.
- **C23** — hermetic latex render with exercise + key div: zero libs dirs, zero bootstrap, zero `__btConfig`, zero key-page.js in libs.

## Demo-book vendored sync (done-condition)

```
cmp IDENTICAL: _extensions/blendtutor/blendtutor.lua == demo-book/_extensions/mcmullarkey/blendtutor/blendtutor.lua
cmp IDENTICAL: _extensions/blendtutor/assets/key-page.js == demo-book/_extensions/mcmullarkey/blendtutor/assets/key-page.js
cmp IDENTICAL: _extensions/blendtutor/assets/exercise-feedback.js == demo-book/_extensions/mcmullarkey/blendtutor/assets/exercise-feedback.js
```

## Notes

- `test_sync_assets.sh` was NOT run (assertion 7 is destructive — `git checkout --` on drift; assets were already committed).
- rodney companion probe (runtime mount/config behavior) is AC-8's scope — not run here.
- exercise-feedback.js / key-page.js sources untouched (referenced/deployed only).
