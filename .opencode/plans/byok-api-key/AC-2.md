---
ac: 2
depends_on: 1
risk: medium
status: spec
---

**Predicate:** `key-page.js` is a mountable, import-not-duplicate key-management module. All assertions MUST hold (source-pattern checks in Python + behavioral checks via `NODE_TEST_SCRIPT` Node harness with recording DOM/localStorage/fetch mocks):
- **P1 (import contract):** `key-page.js` imports `{readKey, storeKey, clearKey, providerBaseUrl, PROVIDERS}` from `"./exercise-feedback.js"` and contains ZERO literals of `fireworks_api_key`, `anthropic_api_key`, `byok_provider`, `bt_feedback_count`, `api.fireworks.ai`.
- **P2 (host-gated validation):** validation URL built via `providerBaseUrl("fireworks")`; `?provider=http://localhost:8080` routes validation to stub; no hardcoded Fireworks host.
- **P3 (discriminated result):** `classifyValidation(status, threw)` → `{ok:true}` on 2xx; `{ok:false, reason:"invalid-key"}` on 401|403; `{ok:false, reason:"network"}` on thrown fetch; `{ok:false, reason:"empty"}` for empty key pre-fetch (no fetch issued).
- **P4:** `statusMessage(reason)` pure, exported, maps each reason to friendly copy.
- **P5 (key hygiene):** zero `console.` calls; zero `innerHTML`; runtime DOM-mock records all `textContent` writes — after saving `"SECRET-TOKEN-XYZ"`, key is not a substring of any recorded textContent; positive companion: at least one non-empty textContent after save.
- **P6:** password input has `type="password"` + `autocomplete="off"`.
- **P7 (save round-trip):** Save calls imported `storeKey("fireworks", value)` (NEW AC-1 signature) → `readKey("fireworks") === "fw_test_123"` (mock with separate Maps).
- **P8 (clear round-trip):** Clear calls `clearKey("fireworks")` → key slot AND `bt_feedback_count` both gone.
- **P9 (empty save no-op):** Save with empty input does NOT clear existing key and does NOT call fetch.
- **P10 (no-key / key-set mount states):** empty storage → password input + Save; key set → "key is set" status + Clear, input NOT pre-filled; after Clear, UI returns to empty input form.
- **P11 (idempotent mount):** mount twice → one save → fetch exactly once.
- **P12:** `mountKeyPage(null/undefined)` is no-op, no crash.
- **P13:** submit handler calls `preventDefault`.
- **P14:** input value reset to `""` immediately AFTER `storeKey` (no password-manager capture, no echo).
- **P15 (pure helpers):** `buildValidationUrl`, `classifyValidation`, `statusMessage` bodies contain no `fetch(`, `localStorage`, `document.`.
- **P16 (module discipline):** docstring header (WHAT/WHERE/NOT) + ≤5 public exports (`buildValidationUrl`, `classifyValidation`, `statusMessage`, `mountKeyPage`).

**Probe:**
```
uv run pytest scripts/tests/test_quarto_key_page.py -x -q
```
NEW test file: (a) Python source-pattern asserts — P1 import line + forbidden-literal scan, P6 attributes, P15 purity scan, P16 docstring; (b) `NODE_TEST_SCRIPT` behavioral harness — recording `createElement` DOM mock, separate-Map localStorage mock, configurable `mockFetch`; asserts P3 discriminated outcomes, P5 key-not-in-DOM + positive textContent, P7/P8 round-trips via imported store/clear with NEW signature, P9 no-op, P10 mount states, P11 fetch-once, P12 null-mount, P14 reset-after-store, P2 host-gating via `?provider=` seam.

**Negative:** key echoed into any DOM textContent or console; hardcoded `api.fireworks.ai`; `innerHTML` of status (XSS); Save writing `sessionStorage` or old slot names; Clear leaving `bt_feedback_count`; validation silently skipped or 401 collapsed into network error (must use discriminated `classifyValidation`, not `listModels`); empty save wiping stored key; key pre-filled into input on key-set mount; duplicate literal slot names instead of imported contract; fetch issued twice on double-mount.

**Verification:** code

**Fixture status:** NEW files — `_extensions/blendtutor/assets/key-page.js`, `scripts/tests/test_quarto_key_page.py`

**Rubric anchor:** §1.5, §2.1, §4.2, §5.1

**Design Intent:**
- **Types/interfaces (§1):** discriminated validation result `{ok:true} | {ok:false, reason:"invalid-key"|"network"|"empty"}`; key presence represented by storage state, never by DOM-hidden key copy; ≤5 named exports.
- **Pure/effectful (§2):** pure core — `buildValidationUrl`, `classifyValidation`, `statusMessage` (P15, no I/O in bodies); thin effectful shell — `mountKeyPage` (DOM wiring, fetch, store/clear calls, `?provider=` seam).
- **Boundary cuts (§3):** key-page.js consumes exercise-feedback.js storage/provider contract via imports only; zero slot-name literals duplicated across module boundary; model-pinned display stays in exercise-feedback.js (out of scope here).
- **Module responsibility (§4):** docstring header — WHAT: key-management UI (input/save/clear/status); WHERE: mounted into `.blendtutor-key` div by blendtutor.lua page; NOT: model listing, feedback submission, provider selection.
- **Function discipline (§5):** each helper one thing, Node-testable without patches; mountKeyPage decomposed into mount-state render + save handler + clear handler + validate-and-report.

**Technical Context:** files: NEW `_extensions/blendtutor/assets/key-page.js`, NEW `scripts/tests/test_quarto_key_page.py`; import contract — exercise-feedback.js MUST be co-deployed (AC-3 `add_html_dependency` deploys both assets); test is gated on AC-1 merge (storeKey signature flip to `storeKey(providerId, value)`, clearKey removes counter, readKey null-on-unavailable); security properties — key leaves browser only as `Authorization: Bearer` header to host-gated baseUrl, never logged/echoed/pre-filled, input cleared post-store; `?provider=` seam is localhost-only, carries no credentials; refusal arms: empty key (no-op), 401/403 (invalid-key msg), network throw (network msg), localStorage-unavailable (save handler catches storeKey throw → friendly status), absent mount target (no-op), key-already-set (Clear affordance).

**Dependencies:** depends-on: 1 | blocks: 3, 7, 8 | conflict set: NEW files only (no edits to `exercise-feedback.js` or `test_quarto_feedback.py` — AC-2 consumes AC-1 exports read-only) | notes: co-deployment requirement via AC-3; behavioral probe cannot pass until AC-1 merged (storeKey signature flip).

## Signature Resolution (2026-08-06, post-AC-1)
VERIFIED against PR #171 merged code: `storeKey` signature is **KEPT** as `(key, providerId)` — NOT flipped to `(providerId, value)`. AC-1's spec P1 prose was ambiguous; the behavioral contract (storage backend localStorage) is satisfied. key-page.js (this AC) MUST call `storeKey(value, "fireworks")` with the ACTUAL (key, providerId) order. The earlier spec text "AC-2's test cannot pass until AC-1 merged (storeKey signature flip)" is SUPERSEDED — AC-1 is merged, dependency satisfied as-is. clearKey(providerId) + readKey-null-guard are the AC-1 contracts key-page.js consumes.

**Clarifications resolved:**
- **Save order → OPTIMISTIC (store first, validation advisory).** Matches existing `renderKeyPrompt` pattern; key is learner's own; validate-first would block offline learners from ever saving a valid key; network-error-with-key-stored is harmless (status line reports it).
- **Key-set UI → NO pre-fill; show "Key is set" + Clear only.** Pre-fill echoes key into input value, violating P5 spirit and enabling password-manager/shoulder-surf capture. After Clear, UI returns to empty input form (P10).
- **Naming → B's trio.** `buildValidationUrl` (pure URL builder), `classifyValidation` (discriminated result), `statusMessage` (friendly string). Auth-header assembly lives in effectful mountKeyPage shell.
- **Model-info display → OUT OF SCOPE.** Pinned-model display is exercise-feedback.js's concern; key page shows key status only.

**needs-clarification:** NONE

### Progress
- [x] AC-2 spec resolved — 2026-08-06
- [ ] implementation — pending B2

### Decision Log
- 2026-08-06 — optimistic save (store then validate advisory); no key pre-fill on key-set mount; naming trio buildValidationUrl/classifyValidation/statusMessage; model display out of scope.

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run `uv run pytest scripts/tests/test_quarto_key_page.py -x -q` after interrupted edit.
- Rollback: git revert the PR; re-sync demo-book mirror via `scripts/sync-quarto-assets.sh`.
