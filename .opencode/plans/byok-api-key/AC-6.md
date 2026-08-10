---
ac: 6
depends_on: 1
risk: medium
status: complete
---

**Predicate:** Six clauses, all required (AND):
- **C1 (SELECT GONE, not hidden):** jsdom render `renderKeyPrompt` → `container.querySelector('select[data-byok="provider"]') === null` AND `container.querySelectorAll('option').length === 0` AND source contains no `data-byok="provider"` literal. (`display:none`/`hidden` still DOM-present and a11y-exposed — assert ABSENCE, not concealment.)
- **C2 (readProvider round-trip preserved — NO semantic change in AC-6):** `storeProvider("anthropic")` → `readProvider() === "anthropic"`; `storeProvider("fireworks")` → `"fireworks"`; empty storage → `"fireworks"`. AC-6 does NOT make readProvider unconditional. Semantics stay default-when-absent via existing `Object.hasOwn` clamp (L146-149); `DEFAULT_PROVIDER` already `"fireworks"` (L76). Fireworks-only learner guarantee comes from REMOVING the writer (the select), not from changing the reader. Existing round-trip assertions (test L319-323) must stay green.
- **C3 (PROVIDERS.anthropic survives):** `Object.hasOwn(PROVIDERS, "anthropic")` AND `PROVIDERS.anthropic.factory === byokAnthropic` AND `PROVIDERS.anthropic.keySlot === "anthropic_api_key"` AND `PROVIDERS.anthropic.baseUrl === "https://api.anthropic.com"`. (Catches overzealous cleanup — AC explicitly orders the backend code path kept.)
- **C4 (?provider= seam preserved):** `?provider=http://localhost:8080` → `"http://localhost:8080"`; `?provider=https://attacker.example` → contains `fireworks.ai`, NOT `attacker.example`; `?provider=http://user:pass@localhost:8080` (credentialed) rejected. Existing assertions (test L325-338) stay green.
- **C5 (disclosure copy):** rendered fireworks disclosure contains `"localStorage"` NOT `"sessionStorage"`; BOTH `PROVIDER_DISCLOSURES` strings (L81-88, fireworks + anthropic) updated to localStorage wording.
- **C6 (no orphaned provSelect refs):** submit handler must not leave `const providerId = provSelect.value;` (dangling ReferenceError on submit after select removed); jsdom submit with non-empty key → no throw; `storeProvider` called with `"fireworks"`.

**Probe:**
```
uv run python scripts/tests/test_quarto_feedback.py
```
Test file extended: (a) jsdom clause rendering `renderKeyPrompt` into container asserting C1 absence queries + C5 rendered-disclosure string checks; (b) jsdom submit case C6 (non-empty key → no throw, storeProvider("fireworks")); (c) C3 object assertions on `PROVIDERS.anthropic` (factory/keySlot/baseUrl shape); (d) keep existing C2 (L319-323, post-AC-1 localStorage) and C4 (L325-338) assertions green. Source-level checks in `check_providers_map`/`check_provider_override` extended with `data-byok="provider"` absence check.

**Negative:**
- Select hidden via CSS/`hidden` attribute but still in DOM/source → C1 fails (assert absence, not visibility).
- `readProvider` rewritten unconditional `return DEFAULT_PROVIDER` → C2 fails (breaks `applyEmbeddedKey` anthropic embed L628→L580 + round-trip test).
- Over-scope deletion of `PROVIDERS.anthropic` / `byokAnthropic` / anthropic disclosure string → C3 fails.
- `?provider=` override removed, or localhost-only gate loosened (attacker.example / credentialed host accepted) → C4 fails (key exfil).
- Disclosure updated only in comment, or only the fireworks string updated while `.anthropic` still says sessionStorage → C5 fails.
- Select removed but submit handler still reads `provSelect.value` → ReferenceError on submit → C6 fails.
- Scope-creep: one-time migration branch or readProvider clamp added "for stale state" → reject (unneeded — see Clarifications; violates smallest-change discipline).
- AC-4 pre-emption: `renderKeyPrompt` body replaced with key-page link in AC-6 → reject; AC-6 keeps key-input + save button; AC-4 owns body replacement.

**Verification:** code

**Fixture status:** existing test file modified (`scripts/tests/test_quarto_feedback.py` extended in place).

**Rubric anchor:** §1.5 (disclosure + `?provider=` localhost gate enforced in code), §1 (illegal state unrepresentable — no select element means no anthropic option), §2.1 (pure layer — readProvider/providerBaseUrl remain Node-importable), §3 (cut UX joint while keeping seam joint), §4 (renderKeyPrompt = key-entry without provider choice), §5 (readProvider does one thing: valid-stored-or-default).

**Design Intent:**
- **Types/interfaces (§1):** `DEFAULT_PROVIDER = "fireworks"` (L76) + `Object.hasOwn` clamp (L146-149) type the "valid stored value or default" contract; removing the `<select>` makes "learner chose anthropic" unrepresentable in the learner path.
- **Pure/effectful (§2):** readProvider/storeProvider/providerBaseUrl remain pure (storage + URL parse); AC-6's select removal + disclosure copy are effectful-shell (renderKeyPrompt) + pure-string (PROVIDER_DISCLOSURES) changes.
- **Boundary cuts (§3):** cut the learner-facing UX joint (remove select) while KEEPING the seam joint (`PROVIDERS.anthropic` + `?provider=` localhost gate) intact for tests + embedded-key builds (`applyEmbeddedKey`).
- **Module responsibility (§4):** exercise-feedback.js = learner key entry WITHOUT provider choice + provider registry still carrying anthropic (for `?provider=` tests + embeds); key-page.js (AC-2) = dedicated key-management page.
- **Function discipline (§5):** delete select block L420-433; `updateDisclosure` provider-switching L444-449 removed; submit's `provSelect.value` L463 replaced with `"fireworks"`; honest readProvider diff = ZERO lines.

**Technical Context:**
- **Files touched:** `_extensions/blendtutor/assets/exercise-feedback.js` (renderKeyPrompt L413-470 — delete select block L420-433; updateDisclosure L444-449; submit L463-464 `provSelect.value` → `"fireworks"`; PROVIDER_DISCLOSURES L81-88 — reword BOTH strings to localStorage), `scripts/tests/test_quarto_feedback.py` (extend), `demo-book/_extensions/mcmullarkey/blendtutor/assets/exercise-feedback.js` (mirror sync — done-condition: `cmp` byte-identical).
- **Line refs verified:** DEFAULT_PROVIDER L76, disclosures L81-88, readProvider L146-149 (already default-when-absent + Object.hasOwn clamp), providerBaseUrl L161-179, select block L420-433, submit L463-464, readProvider consumer L580, factory dispatch L605, applyEmbeddedKey storeProvider L628.
- **Transient vs durable (AC-4 interaction):** AC-6's renderKeyPrompt edits are TRANSIENT — AC-4 replaces the renderKeyPrompt body with a key-page link later in batch. AC-6's DURABLE changes: disclosure copy (both strings), `PROVIDERS.anthropic` kept, submit/storeProvider("fireworks") semantics, test coverage. AC-6 must NOT pre-empt AC-4 (keeps key-input + save).
- **Three-copy propagation:** demo-book mirror must be byte-identical post-AC-6 (`cmp` done-condition, same convention as AC-1). `crates/core/assets/shared/feedback.js` NOT touched (separate Rust-built system).

**Dependencies:** depends-on: 1 | blocks: 4, 5 (hot file) | conflict set: `_extensions/blendtutor/assets/exercise-feedback.js`, `scripts/tests/test_quarto_feedback.py`, demo-book mirror | serialization: AC-1 → AC-6 → AC-4 → AC-5 on the hot file.

**Clarifications resolved:**
- **readProvider semantics → default-when-absent preserved; AC-6 readProvider diff is a NO-OP (zero lines).** It already returns DEFAULT_PROVIDER when storage empty/invalid (L76 + L146-149). Fireworks-only learner guarantee comes from removing the select (the writer), not from changing the reader.
- **Stale byok_provider state (NC-2) → ZERO-CODE.** Pre-AC-6 storage was sessionStorage; AC-1's clean break means old sessionStorage values NEVER migrate to localStorage — stale anthropic state is unreachable for real learners. No migration branch, no clamp, no clearKey amendment needed.
- **Anthropic disclosure string (NC-3) → keep + update both strings to localStorage wording.** Minimal risk, preserves symmetry, keeps the static-scan intent.
- **demo-book mirror (NC-4) → folded into done-condition via `cmp`**, same convention as AC-1.

**needs-clarification:** NONE

### Progress
- [x] AC-6 spec resolved — 2026-08-06
- [x] implementation — red: C1/C6 source + jsdom render/submit tests extended, 4 expected failures 2026-08-06
- [x] implementation — renderKeyPrompt select removed, disclosure static, submit hardcodes DEFAULT_PROVIDER, renderKeyPrompt exported for tests 2026-08-06
- [x] E2E evidence at docs/evidence/167/ — test-suite.log 40/40 + run.log, mirror cmp byte-identical, negative control proven
- [x] commit 28694c5 + PR #172 pushed — awaiting PR review

### Decision Log
- 2026-08-06 — readProvider is a no-op change (already default-when-absent); Fireworks-only from select removal; stale byok_provider state zero-code (AC-1 clean break); both disclosure strings updated.

### Surprises & Discoveries
- AC-6 (byok-api-key): C5's source-level clause was already satisfied by AC-1 — both PROVIDER_DISCLOSURES strings already say "localStorage". AC-6's C5 contribution is the TEST (rendered-disclosure assertion + BOTH-strings source check pinning the wording), not a copy change. Zero disclosure edits needed.
- AC-6 (byok-api-key): no jsdom package in the repo (no package.json) — the spec's "jsdom render" maps to a hand-rolled DOM mock inside NODE_TEST_SCRIPT (same pattern as the existing separate-Map storage mock). The mock's querySelector matches `select[data-byok="provider"]` structurally, so a display:none/hidden select would still be found → C1 asserts absence, not concealment, exactly as specced.
- AC-6 (byok-api-key): exporting renderKeyPrompt was REQUIRED for the probe (jsdom render calls it directly); the source comment at the old select block contained the literal `data-byok="provider"` — leaving the old comment would have tripped the C1 source-absence scan. Comment rewrite is mandatory, not cosmetic.
- AC-6 (byok-api-key): C6 negative control confirmed teeth — reverting submit's `DEFAULT_PROVIDER` back to `provSelect.value` produced ReferenceError + 4 failures (3 behavioral + source scan); restored → 40/40. The _feedbackRunning=true guard in the C6 mock entry prevents the submit's handleSubmitForExercise recursion from reaching fetch/network.
- AC-6 (byok-api-key): spec line refs (select block L420-433, submit L463-464) drifted from actual file (L438-451, L481) — structure matched, line numbers off; verified by grep, not trusted blindly.

### Idempotence & Recovery
- Safe retry: re-run `uv run python scripts/tests/test_quarto_feedback.py` after interrupted edit.
- Rollback: git revert the PR; re-sync demo-book mirror via `scripts/sync-quarto-assets.sh`.
