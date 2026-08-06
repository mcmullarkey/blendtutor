---
ac: 2
depends_on: 1
risk: low
status: complete
---

# AC2 — key-page.js key-management UI module

## Executable Criterion

- **predicate:** `_extensions/blendtutor/assets/key-page.js` is a mountable, import-not-duplicate key-management module. P1-P16 (see issue #163): imports `{readKey, storeKey, clearKey, providerBaseUrl, PROVIDERS}` from `./exercise-feedback.js` with ZERO slot-name/host literals; host-gated validation via `providerBaseUrl("fireworks") + "/models"`; discriminated `classifyValidation` (2xx/invalid-key/network/empty — no `listModels` reuse); pure `buildValidationUrl`/`classifyValidation`/`statusMessage` + effectful `mountKeyPage` (≤5 exports); textContent-only rendering, key never logged/echoed/pre-filled; `storeKey(key, providerId)` AC-1 signature; Clear removes key + counter; empty save no-op; idempotent mount; null-mount no-op; `preventDefault`.
- **probe:** `uv run pytest scripts/tests/test_quarto_key_page.py -x -q`
- **negative:** key echoed into DOM textContent or console; hardcoded `api.fireworks.ai`; `innerHTML` of status (XSS); Save writing sessionStorage or old slot names; Clear leaving `bt_feedback_count`; 401 collapsed into network (listModels reuse); empty save wiping stored key; key pre-filled on key-set mount; duplicate literal slot names; fetch twice on double-mount.
- **verification:** code · pytest (Python source-pattern checks + Node.js behavioral harness with recording DOM/localStorage/fetch mocks).
- **fixture status:** NEW — `_extensions/blendtutor/assets/key-page.js`, `scripts/tests/test_quarto_key_page.py`.
- **rubric anchor:** §1.5 (host gate + discriminated result type), §2.1 (pure core / thin effectful shell), §4.2 (key management owns the BYOK key concern), §5.1 (one thing per helper).

## Design Intent

- **Types / interfaces (§1):** discriminated validation result `{ok:true} | {ok:false, reason:"invalid-key"|"network"|"empty"}`; key presence = storage state, never DOM-hidden copy; ≤5 named exports.
- **Pure / effectful (§2):** `buildValidationUrl`, `classifyValidation`, `statusMessage` pure (no fetch/localStorage/document in bodies); `mountKeyPage` thin effectful shell (DOM wiring, fetch, store/clear, `?provider=` seam).
- **Boundary cuts (§3):** consumes exercise-feedback.js contract via imports only; zero slot-name literals duplicated across module boundary; model-pinned display stays in exercise-feedback.js (out of scope).
- **Module responsibility (§4):** WHAT: key-management UI (input/save/clear/status); WHERE: mounted into `.blendtutor-key` div by blendtutor.lua page; NOT: model listing, feedback submission, provider selection.
- **Function discipline (§5):** `handleSave` → `validateAndReport` decomposition; each helper one thing, Node-testable without patches.

## Technical Context

- **Files touched:** `_extensions/blendtutor/assets/key-page.js` (NEW), `scripts/tests/test_quarto_key_page.py` (NEW). NO edits to `exercise-feedback.js` (AC-1 owned, read-only import).
- **Signature resolution (verified post-AC-1):** AC-1 did NOT flip `storeKey` — actual signature is `storeKey(key, providerId)` (key first). key-page.js calls `storeKey(value, "fireworks")`. `clearKey(providerId)` removes the provider key slot AND `bt_feedback_count`. `readKey` returns null when no key / localStorage unavailable. P7 in the original issue prose ("storeKey(\"fireworks\", value)") was stale — the executable test asserts the round-trip via the real imported storeKey, so it pins the ACTUAL signature.
- **Security properties:** key leaves browser only as `Authorization: Bearer` header to host-gated baseUrl; never logged/echoed/pre-filled; input cleared post-store; `?provider=` seam localhost-only (carries no credentials).
- **Test harness:** mirrors `test_quarto_feedback.py` — Python source-pattern checks + `NODE_TEST_SCRIPT` (temp .mjs in repo root) importing key-page.js → transitive import of exercise-feedback.js works under Node 22 ESM (proven in CI for AC-112). Recording `createElement` DOM mock, separate-Map localStorage mock, configurable `mockFetch`.

## Dependencies

- **Depends on:** AC-1 (storeKey/clearKey/readKey/providerBaseUrl exports + signature).
- **Blocks:** AC-3 (co-deploys key-page.js via add_html_dependency), AC-7, AC-8.
- **Conflict set:** NEW files only. No edits to `exercise-feedback.js`, `test_quarto_feedback.py`, sync script (key-page.js is an _extensions-only asset, not in the crates sync list).

### Progress
- [x] 2026-08-06 — Red: wrote `scripts/tests/test_quarto_key_page.py` (Python source checks P1/P3/P5/P6/P15/P16 + Node behavioral harness P2-P14) — confirmed fail (key-page.js missing)
- [x] 2026-08-06 — Inner loop: wrote `key-page.js` (import contract, pure core, effectful shell, WeakSet idempotent mount)
- [x] 2026-08-06 — Green: probe passes — `uv run pytest scripts/tests/test_quarto_key_page.py -x -q` → 8 passed; `python3` direct → 39 assertions, 0 failed
- [x] 2026-08-06 — Negative control: mutated classifyValidation (401→network collapse) → test FAILED; reverted → green. Harness catches the negative case.
- [x] 2026-08-06 — Regression: `test_quarto_feedback.py` (35 passed), `test_quarto_ux.py` (46 passed), `sync-quarto-assets.sh --check` (in sync)
- [x] 2026-08-06 — Committed (test red + feat), created PR

### Decision Log
- 2026-08-06 — No ADR (ADR-0019 slot reserved but unused): the module follows the exact established pattern of exercise-feedback.js (pure/effectful split, import-not-duplicate, docstring header) — same reasoning as AC-1's decision log. A new ADR would restate what the code documents.
- 2026-08-06 — Key-set mount state renders NO input (status + Clear only): the negative "key pre-filled into input on key-set mount" is then trivially impossible, and the AC's "empty storage → input+Save; key set → status+Clear" composition reads as two distinct states. After Clear, `renderKeyForm(container, "cleared")` returns the UI to the empty input form with the cleared status.
- 2026-08-06 — Save keeps the form (input reset + status line) rather than switching to key-set state: enables the empty-save no-op scenario (P9) within the same mount.
- 2026-08-06 — Plan file lives at `.opencode/plans/fireworks-byok/AC-2.md` (existing feature dir from AC-1) — the `byok-api-key` dir named in the issue Plan Reference does not exist.

### Surprises & Discoveries
- Node 22's ESM `import()` resolves `.js` files with export syntax even with no package.json `type` field — the transitive import of exercise-feedback.js from key-page.js works under the same harness AC-112 uses in CI. No extra config needed.
- The repo has NO pyproject.toml, so the spec's `uv run pytest` probe failed with "Failed to spawn: pytest" until `uv tool install pytest` placed pytest on PATH. CI runs these scripts via plain `python3` — the test file is dual-mode (pytest test_* functions + python3 main()).

### Idempotence & Recovery
- Safe retry: `uv run pytest scripts/tests/test_quarto_key_page.py -x -q` after any key-page.js edit.
- Rollback: `git revert` the feat commit (34a7e93).
