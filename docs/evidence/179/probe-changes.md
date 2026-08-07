# Issue #179 — feedback-probe.js changes

## What changed (commit 693715a)

1. **Removed the P5 manual config injection** (`rodneyJs("(() => { ... maxFeedbackPerSession = 100 ... })()")`).
   The demo-book render now carries `maxFeedbackPerSession` from
   `blendtutor.lua build_key_page_config_script` (issue #179), so the probe no
   longer papers over a production gap — it exercises the real render.

2. **P10 config guard now asserts the REAL rendered config** instead of the
   injected `=== 100`:

   ```js
   const cfgOk = rodneyJs(
     "(window.__btConfig && typeof window.__btConfig.maxFeedbackPerSession === 'number' && window.__btConfig.maxFeedbackPerSession >= 1)",
   );
   ```

   Deliberately loose on the exact number (`>= 1`, not `=== 20`): the probe
   pins the CONTRACT (a working non-zero rate limit), not the tuning constant.
   A regression to keyPageUrl-only emission → `undefined` → `0>=0===true` →
   this guard fails loudly instead of vacuous-passing.

3. **Comments updated** to state the provenance honestly: config comes from the
   real render (default 20, crates parity), NO probe-side injection.

## Verification (performed locally)

- `node --check rodney-probes/feedback-probe.js` → SYNTAX OK
- `rg "maxFeedbackPerSession" rodney-probes/feedback-probe.js` → only comment
  references + the P10 guard remain; no injection call site.
- Rodney EXECUTION is out of builder scope (rodney is builder-vision-probe /
  CI's domain). The CI `rodney-probes` job renders the demo-book fresh and runs
  both probes (`key-page-probe.js` unchanged, `feedback-probe.js` as above)
  with NO manual config injection — that job validates this change end-to-end.

## Rendered-value parity used by the probe

- Rendered demo-book r-exercises.html head script:
  `window.__btConfig = window.__btConfig || {};window.__btConfig.keyPageUrl = "api-key.html";window.__btConfig.maxFeedbackPerSession = window.__btConfig.maxFeedbackPerSession ?? 20;`
- The probe reads `window.__btConfig.maxFeedbackPerSession` on that page = `20`
  (number, ≥ 1) → P10 guard passes.
