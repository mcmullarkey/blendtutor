# Issue #169 — rodney probes evidence (AC-8)

## What was verified (E2E, real rendered pages + real localhost stub)

Both probes drive the REAL `quarto render demo-book` output over HTTP in a
headless Chrome (uvx rodney 0.4.0, Chrome routed through
`scripts/rodney-chrome.sh` — P13):

- **`key-page-probe.js` (P6-P9)** — 16 clauses, verdict `PROBES_PASS`:
  - P6 save flow: `type=password` + `autocomplete=off`, status line visible
    via `getComputedStyle` (display=block, height=25.5 — not presence-only),
    localStorage slot populated, input cleared, GET `/models` validation
    fired against the stub.
  - P7 clear flow: after Clear, `fireworks_api_key` AND `bt_feedback_count`
    both null (AC-1 P2 contract).
  - P8 invalid-key 401 (`/_config/auth` toggle): invalid-key error shown, GET
    `/models` 401 actually fired, key stored **advisory** per the MERGED AC-2
    contract (see deviation note below).
  - P9 network error (dead provider port): optimistic save + network-error
    status shown.
- **`feedback-probe.js` (P5/P10/P11)** — 16 clauses, verdict `PROBES_PASS`:
  - P5 cross-page persistence: key saved via the key-page UI on
    `api-key.html`, REAL `location.href` navigation to `r-exercises.html`
    (same origin, same rodney session — no eval pre-seed, no new tab), key
    persists, feedback proceeds past no-key state.
  - P10 verdict end-to-end: new `/chat/completions` round-trip through the
    real stub observed, verdict rendered, XSS payload
    `<img src=x onerror=window.__xss=1>` renders as literal text (in
    textContent, no element injected, `window.__xss === undefined`).
  - P11 no-key link: empty storage → `[data-byok="no-key"]` link
    (target=_blank, rel=noopener, href=api-key.html) AND ZERO
    `/chat/completions` fetches.

## Run logs / reports

- `key-page-probe-report.json` / `key-page-probe.log`
- `feedback-probe-report.json` / `feedback-probe.log`
- Screenshots `key-01..04`, `fb-01..03` per UI state.

## CI wiring (P2)

`.github/workflows/ci.yml` gains a PR-gating `rodney-probes` job (workflow
already triggers on `pull_request`) that renders `demo-book`, then runs BOTH
probes with `EVIDENCE_DIR=docs/evidence/169` and
`ROD_CHROME_BIN=scripts/rodney-chrome.sh`. No `continue-on-error`, no
`|| true`, no `if: always()` — verified by source grep in the spec probe.

## Source gates

- P4: `grep -c sessionStorage feedback-probe.js` == 0 (verified).
- P3: no synthetic-DOM generation in either probe (verified).
- P12: EVIDENCE_DIR env-parameterized, no hardcoded `docs/evidence/112`
  (verified).
- P1: both probes `process.exit(1)` on PROBES_FAIL (verified; exit 0 on PASS).

## Deviation note — P8 "key NOT stored" vs AC-2 advisory storage

AC-8 spec P8 says "key page shows invalid-key error AND key NOT stored
(matches AC-2 classifyValidation 401 → invalid-key)". The merged AC-2
implementation (key-page.js) stores the key optimistically BEFORE validation
and its own test asserts the opposite of the spec wording:
`scripts/tests/test_quarto_key_page.py:369` —
`assert(localStorageMap.get("fireworks_api_key") === "BAD-KEY-401",
"advisory: key stored even when validation rejects")`.

The probe asserts the REAL merged contract: 401 → invalid-key error shown +
GET /models fired + key stored (advisory). Implementing the literal "key NOT
stored" would permanently red the CI gate against production code. See
`.opencode/plans/byok-api-key/AC-8.md` Decision Log.

## Local tool-behavior research (github-pages-deploy retro)

Verified locally on macOS (arm64): rodney 0.4.0 + `scripts/rodney-chrome.sh`
launch (resolves `~/.cache/rod/browser/.../Chromium.orig`), both probes run to
PROBES_PASS with exit 0. The ubuntu-latest runner ships google-chrome, which
the wrapper resolves on Linux.
