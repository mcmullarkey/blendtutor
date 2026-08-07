# Issue #166 — E2E Evidence

byok-api-key AC-5: wire check output into the Get feedback LLM flow with a
pinned Fireworks model and collapsed model picker. Verification: code
(primary — static + Node behavioral; the rodney fetch-spy probe is AC-8's
domain and was not runnable here — pytest static+Node asserts are the CI gate).

## What changed (2 real deltas + regression guard)

1. **Model pin** — `FIREWORKS_MODEL` = `accounts/fireworks/models/deepseek-v4-flash-0731`
   at BOTH uses (`_extensions/blendtutor/assets/exercise-feedback.js`:
   `PROVIDERS.fireworks.fallbackModel` + module const). Request body model is
   the pinned id.
2. **Picker collapse** — `renderModelPicker`/`modelPickerPresent`/`selectedModel`
   (and the now-dead `listModels`) removed; `handleSubmitForExercise` collapses
   to key check → rate-limit check → fetch with the pinned model. Single click
   goes straight to `${providerBaseUrl("fireworks")}/chat/completions`.
3. **Regression guard (all 13 arms)** — no auto-fire (runtime has zero
   exercise-feedback refs; mounting triggers zero fetches), button sole
   trigger with exact "Get feedback" text, check output in prompt
   (<<<CAPTURED_OUTPUT>>> + this exercise's .bt-output exactly once),
   per-exercise scoping (no cross-exercise bleed), textContent-only verdict
   (XSS payload literal, onerror never runs), rate-limit/no-key refusal with
   zero fetches, error path still increments the counter, concurrent guard
   (2 clicks → 1 fetch), empty .bt-output tolerated (fetch fires).

## Evidence artifacts

| Artifact | Contents |
|----------|----------|
| `run.log` | Full `uv run python scripts/tests/test_quarto_feedback.py` output — **62 passed, 0 failed**, incl. the AC-5 fetch-spy wiring suite that drives the REAL `handleSubmitForExercise` through `mountFeedback` clicks against a recording `window.fetch` spy (arms 2,4,5,7,8,9,10,11,12,13) |
| `test-suite.log` | All CI-equivalent suites + fixture render + cmp results |

## Probe runs

| Command | Result |
|---------|--------|
| `uv run python scripts/tests/test_quarto_feedback.py` | **62 passed, 0 failed** (CI gate, ci.yml:82) |
| `bash scripts/tests/test_quarto_distribution.sh` | **89 passed, 0 failed** |
| `bash scripts/tests/test_quarto_render.sh` | **12 passed, 0 failed** |
| `bash scripts/tests/test_sync_assets.sh` | **12 passed, 0 failed** (asset parity) |
| `uv run pytest -q` | **8 passed** (pytest-collected suite) |
| `quarto render quarto-fixture/feedback.qmd` | rc 0; rendered HTML: `maxFeedbackPerSession` present, `model-picker` absent |

## Demo-book vendored sync (done-condition)

```
cmp IDENTICAL: _extensions/blendtutor/assets/exercise-feedback.js == demo-book/_extensions/mcmullarkey/blendtutor/assets/exercise-feedback.js
```

## Notes

- styles.css untouched (sync copy from crates; picker collapse is DOM-only).
- exercise-runtime.js verify-only (zero references to exercise-feedback asserted).
- crates/core untouched (its feedback.js keeps the picker — separate seam).
- rodney companion probe (fetch-spy, session-scoped) is AC-8's scope — not run.
- test_quarto_ux.py 60s render timeout is a pre-existing local flake (stash-verified
  ~57s render on main too) — not a regression.
