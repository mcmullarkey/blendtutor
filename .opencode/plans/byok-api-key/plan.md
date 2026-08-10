# Master Plan: byok-api-key

## Feature Goal

Upgrade the blendtutor Quarto extension's BYOK (bring-your-own-key) flow from
"manual opt-in, per-tab sessionStorage, Anthropic-or-Fireworks provider chooser,
per-exercise inline key prompt" to a **dedicated key-management page + shared
localStorage persistence + Fireworks-only + auto-mounted feedback** model:
the learner enters a Fireworks API key once on a designated demo-book page
(password input, save, clear, cheap validation via `GET
https://api.fireworks.ai/inference/v1/models`), the key persists in
`localStorage` across all book pages on the same origin, and every lesson page
gets LLM feedback wired in automatically with a graceful "enter your API key
first" link state when no key is present. Security model: browser-direct —
key only ever leaves the browser in the `Authorization: Bearer` header of
requests to `api.fireworks.ai`; never logged, never in HTML source, LLM output
rendered via `textContent` only.

## Dependency DAG

```
AC-1 ──→ AC-2 ──→ AC-3 ──→ AC-4 ──→ AC-5 ──→ AC-8
AC-1 ──→ AC-6 ────────────────────────────→ AC-8
AC-2 ──→ AC-7 ───────────────────────────→ AC-8
AC-3 ──→ AC-7
AC-7 ──→ AC-9
```

## Batch Schedule

- B1: AC-1 (sequential)
- B2: AC-2 ∥ AC-6 (parallel)
- B3: AC-3 (sequential)
- B4: AC-4 ∥ AC-7 (parallel)
- B5: AC-5 (sequential)
- B6: AC-8 (sequential)
- B7: AC-9 (sequential)

## Hot Conflict Files

- `_extensions/blendtutor/assets/exercise-feedback.js`: touched by AC-1, AC-4,
  AC-5, AC-6 — strictly serialize (AC-1 → AC-6 → AC-4 → AC-5 order; never two
  in one batch).
- `scripts/tests/test_quarto_feedback.py`: touched by AC-1, AC-4, AC-5, AC-6 —
  serialized with the same chain (same PRs).
- `_extensions/blendtutor/blendtutor.lua`: touched by AC-3 (major) and AC-4
  (small, `window.__btConfig.keyPageUrl` emission) — serialize AC-3 → AC-4.
- `rodney-probes/feedback-probe.js`: touched by AC-4 and AC-8.
- `demo-book/_quarto.yml`: AC-7 only.
- `demo-book/_extensions/mcmullarkey/blendtutor/` is a synced copy
  (`scripts/sync-quarto-assets.sh`, mode=copy) — never edit directly; any AC
  touching `_extensions/` assets must re-run the sync script (fold into each
  such AC's done-condition, not a separate AC).

---

# AC-1

---
ac: 1
depends_on: none
risk: medium
status: spec
---

**Predicate:**
- **P1 (round-trip, no zombie):** With a Node mock exposing `window.localStorage` and `sessionStorage` backed by **separate** Map instances: `storeKey("fireworks","k")` → `readKey("fireworks") === "k"`; pre-seeding `sessionStorage.fireworks_api_key="stale"` with localStorage empty → `readKey("fireworks") === null` (no sessionStorage fallback).
- **P2 (clearKey scoped + counter reset):** `clearKey("fireworks")` removes `fireworks_api_key` **and** `bt_feedback_count`; leaves `anthropic_api_key`, `byok_provider`, `quarto-reader-mode`, `quarto-persistent-tabsets-data` intact (no `localStorage.clear()`); after `clearKey`, `feedbackCount() === 0` and `readKey("fireworks") === null`.
- **P3 (counter migrated):** `incrementFeedbackCount()` → `feedbackCount() === 1` with value in `localStorage.bt_feedback_count`; pre-seeding `sessionStorage.bt_feedback_count=99` with localStorage empty → `feedbackCount() === 0` (catches bare-global `sessionStorage` at lines 370/383).
- **P4 (source-scan):** Zero occurrences of the token `sessionStorage` anywhere in `_extensions/blendtutor/assets/exercise-feedback.js` — key fns, counter fns, **and** `readProvider`/`storeProvider` all migrated (no exemptions).
- **P5 (half-migration fails):** Test mock backs `sessionStorage` and `localStorage` with separate Maps, so any single-backend find-replace leaves asserts failing.
- **P6 (comments/disclosures honest):** No comment or disclosure string in the module claims "sessionStorage" or "tab-scoped" (lines 3, 21, 54, 57, 82–87, 131, 362, 391, 412, 561, 621, 640, 672); disclosure strings state localStorage persistence accurately.
- **P7 (read-guard):** `readKey` returns `null` (does not throw) when `localStorage.getItem` throws (private mode / `file://`), protecting `handleSubmitForExercise` (~:580).

**Probe:**
```
python3 scripts/tests/test_quarto_feedback.py
cmp _extensions/blendtutor/assets/exercise-feedback.js demo-book/_extensions/mcmullarkey/blendtutor/assets/exercise-feedback.js
```
Pytest file updated with: separate-Map storage mock (:267–280 region), round-trip + P1 zombie-fallback assert, `clearKey` scoped-removal + counter-reset assert (P2), counter-on-localStorage assert (P3), a source-scan test asserting zero `sessionStorage` occurrences in the asset (P4), and `check_shared_session_storage` (:135–155) reworked to assert the shared localStorage contract. `cmp` proves the demo-book mirror is byte-identical post-sync.

**Negative:**
1. Find-replace on `window.sessionStorage` misses **bare** `sessionStorage` at :370/:383 — separate-Map mock + P3 counter assert fail it.
2. `clearKey` over-clear (`localStorage.clear()` or touching `byok_provider`/`anthropic_api_key`) — P2 fails it.
3. Zombie fallback reading sessionStorage when localStorage is empty — P1 fails it.
4. `clearKey` that removes the key but not `bt_feedback_count` permanently rate-locks a cap-hit learner (localStorage survives tab close) — P2 counter-reset clause fails it.
5. Editing only the canonical asset and skipping the mirror — `cmp` fails it.

**Verification:** code · pytest + Node mock + source-scan + `cmp`

**Fixture status:** `scripts/tests/test_quarto_feedback.py` :135–155 and :267–364 modified in place; `clearKey`/counter/source-scan asserts NEW in same file. Canonical source `_extensions/blendtutor/assets/exercise-feedback.js` — readKey :138–140, storeKey :142–144, +clearKey NEW, readProvider :146–149, storeProvider :151–153, feedbackCount :369–371, incrementFeedbackCount :382–384.

**Rubric anchor:** §1, §2, §4

**Design Intent:**
- **Types/interfaces (§1):** `PROVIDERS[id].keySlot` remains the single source for key slots (:63–70); storage contract becomes "raw strings in `window.localStorage`"; `clearKey(providerId)` added to the module's exported surface.
- **Pure/effectful (§2):** Storage fns stay the effectful shell; pure rendering/validation core untouched; `readKey` gains a 2-line try/catch returning `null` so effectful failure degrades to "no key" instead of a crash.
- **Boundary cuts (§3):** One storage boundary — the whole module speaks localStorage only; no half-and-half split; `?provider=` URL stub seam unaffected (runtime param, not storage).
- **Module responsibility (§4):** exercise-feedback.js owns learner key + provider selection + rate-limit counter, all in one backend; `crates/core/assets/shared/feedback.js` is a separate Rust-built system and is NOT touched.
- **Function discipline (§5):** `clearKey(providerId)` is a one-liner pair of `removeItem`s (keySlot + `bt_feedback_count`); no new abstraction layers.

**Technical Context:**
- **Files touched:** `_extensions/blendtutor/assets/exercise-feedback.js` (key fns :138–153, counter fns :362–384, comments/disclosures :3,:21,:54,:57,:82–87,:131,:362,:391,:412,:561,:621,:640,:672); `demo-book/_extensions/mcmullarkey/blendtutor/assets/exercise-feedback.js` (byte-identical mirror — run `scripts/sync-quarto-assets.sh`, else hand-sync; **done-condition**: `cmp` passes); `scripts/tests/test_quarto_feedback.py` (:135–155, :267–364 + NEW asserts); `docs/agent-notes/feedback.html` :61 and `docs/okf/interfaces/js-runtime-seam.html` :144 (storage-claim updates).
- **Decisions:** `byok_provider` migrates to localStorage with everything else; `bt_feedback_count` migrates to localStorage and `clearKey` resets it; `window.__btConfig.maxFeedbackPerSession` config key is **not renamed** (smallest correct move — comment at :376 updated to document the now-persistent per-browser cap); `readKey` gets the try/catch→null guard (P7), write paths left to surface errors naturally.
- **Not touched:** `crates/core/assets/shared/feedback.js` (Rust system, build.rs assertions), `coi-serviceworker.js` sessionStorage usage (unrelated COI reload state), `docs/adr/0009`, `docs/adr/0014` (historical records — not rewritten).
- **Unaffected tests:** `test_quarto_ux.py` (imports exercise-runtime.js), `test_quarto_distribution.sh` (greps README), build.rs (crates).
- **Hidden coupling flagged:** `rodney-probes/feedback-probe.js` :381 reads `sessionStorage.getItem('fireworks_api_key')` and :254 calls `sessionStorage.clear()` — stale after AC-1 and will false-negative AC-4/5/6. **Assigned to AC-8** (rodney AC): AC-8 must switch both lines to localStorage.

**Dependencies:** depends-on: none | blocks: AC-2, AC-4, AC-5, AC-6 (hot file, serialized AC-1→AC-6→AC-4→AC-5) | conflict set: `_extensions/blendtutor/assets/exercise-feedback.js`, `demo-book/.../exercise-feedback.js`, `scripts/tests/test_quarto_feedback.py` | assigned ownership: rodney probe → **AC-8**, demo-book mirror → **AC-1 done-condition** (sync + `cmp`)

**Clarifications resolved:**
- **byok_provider → localStorage (migrated).** Hidden chooser + `DEFAULT_PROVIDER=fireworks` make the value near-moot; a key-localStorage/provider-sessionStorage split is a consistency smell and forces P4 exemptions. Migrating yields the stronger invariant: zero `sessionStorage` in the module. Only behavioral delta — provider choice persisting across tabs — is harmless under Fireworks-only scope.
- **bt_feedback_count → localStorage; clearKey resets it.** AC text mandates consistent migration, so per-tab semantics die with sessionStorage. Without reset, a cap-hit learner is permanently locked (localStorage survives tab close — real regression). Reset-on-clear is acceptable: the rate limit is learner self-protection over their own key/costs, not a security boundary, so the clear-to-reset bypass costs nothing.
- **maxFeedbackPerSession → document, don't rename.** Renaming churns `window.__btConfig` producers (config.js/blendtutor.lua) for zero behavioral gain; comment at :376 updated to state the cap is now persistent per-browser.
- **localStorage guard → include, minimal.** 2-line try/catch in `readKey` returning `null`; prevents a crash regression at `handleSubmitForExercise` on `file://`/private-mode without scope-creeping into write-path error handling.

**needs-clarification:** NONE

### Progress
- [x] AC-1 spec resolved — 2026-08-06
- [ ] implementation — pending B1

### Decision Log
- 2026-08-06 — storage backend localStorage + zero-sessionStorage invariant: adopted B's P1-P6 spine; inverted B on byok_provider (migrate, no exemptions); clearKey resets rate-limit counter; maxFeedbackPerSession documented not renamed; readKey try/catch guard.

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run `python3 scripts/tests/test_quarto_feedback.py` after any interrupted edit.
- Rollback: git revert the PR; mirror re-sync via `scripts/sync-quarto-assets.sh`.

---

# AC-2

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

---

# AC-3

---
ac: 3
depends_on: 2
risk: high
status: spec
---

**Predicate:** Rendering the fixture set through the Quarto extension must satisfy C1–C23:

*Deployment:* **C1** `resources` static-pins both `exercise-feedback.js` and `key-page.js` in blendtutor.lua; **C2** both files exist at libs dir post-render; **C3** no `scripts=` regression (dependency mechanism unchanged); **C4** book render lands assets under site_libs.

*Bootstrap:* **C5** bootstrap imports `mountAllFeedback` from exercise-feedback.js; **C6** imports `mountKeyPage` from key-page.js; **C7** `mountAllFeedback(registry)` sits inside `.then(` AFTER `start(...)` resolves, BEFORE `.catch(` (awk line-order assert); **C8** exactly one `mountAllFeedback(` call site; **C9** same hoisted `registry` const passed to both `start()` and `mountAllFeedback()`; **C10** bootstrap calls `mountKeyPage(document.querySelector(".blendtutor-key"))` — unconditional, no guard (AC-2 P12: null is no-op) — and key-page auto-mount is filter-driven, NOT hand-wired in api-key.qmd; **C11** `.then(` and `.catch(` both present.

*Div + flags:* **C12** `::: {.blendtutor-key}` renders element with class `blendtutor-key`; **C13** `has_key` set in `Div()` BEFORE any early-return, reset in `Pandoc()`; **C14** CRITICAL — key-only page (only `.blendtutor-key`, zero exercises) deploys both assets AND injects bootstrap AND calls mountKeyPage. Guards at blendtutor.lua:681 (`build_html_dependency()`) and :695 (bootstrap injection) MUST both broaden to `has_blendtutor or has_key`.

*Opt-out:* **C15** filter reads `bt-feedback` accepting both `false` and `"false"`; **C16** `bt-feedback: false` → bootstrap present, `start()` present, `mountKeyPage` present, `mountAllFeedback` ABSENT; **C17** string `"false"` parity with boolean; **C18** regression: `bt-auto-bootstrap: false` → bootstrap injection count == 0 (but keyPageUrl head script STILL present, see C19).

*__btConfig:* **C19** rendered HTML `<head>` contains `window.__btConfig` with `keyPageUrl` — emitted via SEPARATE `include_text` classic (non-module) `<script>` in `<head>` on every page where `has_blendtutor or has_key`, REGARDLESS of `bt-auto-bootstrap` / `bt-feedback` opt-outs (opt-out pages still need keyPageUrl for AC-4's no-key link); **C20** custom `bt-key-page` YAML value honored; **C21** default `api-key.html`; **C22** merge pattern `window.__btConfig = window.__btConfig || {}` then property assignment — NEVER bare `= {...}` (config.js crates/core/src/site/mod.rs:321 sets `maxFeedbackPerSession` on the same object; clobber breaks rate limiting at exercise-feedback.js:376).

*Non-HTML gate:* **C23** latex render: zero asset/bootstrap/config leak.

**Probe:**
```
bash scripts/tests/test_quarto_bootstrap.sh          # extended: C5-C11, C15-C18, C19-C22 asserts
bash scripts/tests/test_quarto_asset_deployment.sh   # extended: C1-C4 (extend clause-5 token loop :276 for key-page.js)
bash scripts/tests/test_quarto_render.sh             # verify path
```
New fixtures: `quarto-fixture/key-only.qmd` (C14), `quarto-fixture/key-page-meta.qmd` (C20), `quarto-fixture/feedback-optout.qmd` (C16), `quarto-fixture/feedback-optout-string.qmd` (C17). Runtime companion: extend `rodney-probes/auto-bootstrap.js` — assert `window.__btConfig.keyPageUrl` set, mountKeyPage no-throw on missing div, single feedback mount after start resolves. Review `scripts/tests/test_quarto_feedback.py:187` __btConfig allow-list for keyPageUrl. Latex leak check via C23 assert in render script.

**Negative:** (union, deduped) asset deployed but never imported; mountAllFeedback called before start() resolves; double-mount (two mountAllFeedback call sites); `bt-feedback: false` ignored (mountAllFeedback still emitted); `bt-auto-bootstrap: false` regression (bootstrap injected anyway); `.blendtutor-key` div renders but key-page.js not deployed; key-only page silently broken (guards not broadened → no bootstrap, key page dead); has_key set after early-return or never reset in Pandoc() (state leak across docs); bare `window.__btConfig = {...}` clobbers maxFeedbackPerSession; keyPageUrl missing on bt-auto-bootstrap:false pages (AC-4 link dead); keyPageUrl emitted only inside module bootstrap (unreachable on opt-out pages); custom bt-key-page value ignored; bootstrap-internal emission instead of head script; string "false" treated as truthy; latex output polluted with JS assets; registry re-built twice (start and mountAllFeedback get different registries).

**Verification:** code (shell render-and-assert scripts) + rodney companion probe for runtime mount/config behavior. No visual AC — no ui: block required.

**Fixture status:** Extended: `scripts/tests/test_quarto_bootstrap.sh`, `scripts/tests/test_quarto_asset_deployment.sh` (clause-5 token loop :276), `rodney-probes/auto-bootstrap.js`, `scripts/tests/test_quarto_render.sh` verify, `scripts/tests/test_quarto_feedback.py:187` allow-list review, `test_quarto_install_render.sh` OR `test_quarto_distribution.sh`. NEW: `quarto-fixture/key-only.qmd`, `quarto-fixture/key-page-meta.qmd`, `quarto-fixture/feedback-optout.qmd`, `quarto-fixture/feedback-optout-string.qmd`.

**Rubric anchor:** §2 (pure/effectful: Lua build-time filter vs browser runtime; pure helpers stay in key-page.js), §3 (boundary cut: filter emits config, runtime consumes — key-page.js does NOT read keyPageUrl).

**Design Intent:**
- **Types/interfaces (§1):** `window.__btConfig.keyPageUrl: string` merged non-destructively; YAML meta contract `bt-key-page`, `bt-feedback: bool|"false"`, existing `bt-auto-bootstrap`.
- **Pure/effectful (§2):** Lua filter = effectful build-time emission only; runtime mount logic lives in ES modules; pure helpers (buildValidationUrl etc.) stay in key-page.js per AC-2.
- **Boundary cuts (§3):** filter owns config emission + asset deployment; exercise-feedback.js owns feedback runtime; key-page.js owns key UI and ignores keyPageUrl (single consumer = AC-4).
- **Module responsibility (§4):** blendtutor.lua header documents: div handling, dual-asset deployment, bootstrap + head-script injection, opt-out semantics; NOT responsible for key validation logic.
- **Function discipline (§5):** registry hoisted to one shared const; guards broadened in place at :681/:695, no duplicated condition blocks; head-script emission one small helper.

**Technical Context:** Files: `blendtutor.lua` (bootstrap :587-614 currently `start(buildRegistry(scanExercises()), {...}).catch(...)` — no `.then`; hoist registry to shared const, add `.then(() => { mountAllFeedback(registry); mountKeyPage(document.querySelector(".blendtutor-key")); })`; broaden guards :681 + :695 to `has_blendtutor or has_key`; add has_key flag lifecycle Div()/Pandoc(); add include_text head script for keyPageUrl), `exercise-feedback.js` (ES module, `export mountAllFeedback` :673 — unchanged except AC-1 deltas), NEW `key-page.js` (AC-2). Imports via `libs_url()` `./`-prefixed. **Sync gap:** `scripts/sync-quarto-assets.sh` ASSET_FILES (:30-34) covers only codemirror/styles/coi from crates/core — NOT blendtutor.lua, exercise-feedback.js, key-page.js. `demo-book/_extensions/mcmullarkey/blendtutor/blendtutor.lua` is manual copy. AC-3 done-condition = manual `cp` of blendtutor.lua + exercise-feedback.js + key-page.js to demo-book extension dir + `cmp` clean. ⚠️ `test_sync_assets.sh` assertion 7 destructive (git checkout) — do NOT run with uncommitted assets. COI orthogonal — do NOT set `coi: true` on api-key.qmd; bootstrap changes COI-independent.

**Dependencies:** depends-on: 2 | blocks: 4, 7, 8 | conflict set: blendtutor.lua, test_quarto_bootstrap.sh, test_quarto_asset_deployment.sh (+ rodney-probes/auto-bootstrap.js, test_quarto_feedback.py:187 review) | notes: keyPageUrl emission owned by AC-3 → AC-4's blendtutor.lua touch (decomposition line 70) may be eliminated entirely — flag for AC-4 resolver; AC-7's api-key.qmd render assert depends on AC-3's manual demo-book sync done-condition.

**Clarifications resolved:**
- Key-page mount trigger: filter auto-mounts via bootstrap `mountKeyPage(document.querySelector(".blendtutor-key"))` unconditionally; AC-2 P12 null no-op removes guard need. No hand-wiring in api-key.qmd.
- keyPageUrl ownership: AC-3 owns C19–C22; bootstrap/emission is AC-3's surface; AC-4 depends on it.
- Emission location: SEPARATE include_text classic script in `<head>` on all has_blendtutor-or-has_key pages regardless of opt-outs — opt-out pages still need keyPageUrl for AC-4's no-key link; module bootstrap would be unreachable there.
- key-page.js + keyPageUrl: key-page.js does NOT read it; single consumer = AC-4 (AC-2.md authoritative).
- Demo-book sync: manual cp + cmp done-condition in AC-3; AC-7 render assert depends on it.
- COI: none on key page; orthogonal.

**needs-clarification:** NONE

### Progress
- [x] AC-3 spec resolved — 2026-08-06
- [ ] implementation — pending B3

### Decision Log
- 2026-08-06 — keyPageUrl emitted via separate head include_text (not bootstrap-internal); __btConfig merge pattern (no clobber); has_key guards broadened to has_blendtutor or has_key (key-only page); demo-book sync = manual cp + cmp done-condition (sync script gap discovered).

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run the three shell test scripts after any interrupted edit.
- Rollback: git revert the PR; re-sync demo-book extension dir manually.

---

# AC-4

---
ac: 4
depends_on: 1, 3
risk: medium
status: spec
---

**Predicate:** Six arms, all MUST hold:
1. **Link renders, inline form gone** — `renderKeyPrompt` (exercise-feedback.js 413–470) emits container `[data-byok="no-key"]` containing exactly one `<a>` with `textContent === "Enter your API key first"` and `href` resolved from `keyPageUrl()`; NO `input[name="provider-key"]`, NO Save/submit button, NO `[data-byok="provider"]` select remain in the rendered output.
2. **Lazy read at render time** — `href` is read inside the render function body, NOT cached at module init. Test: after module load, `eval`-set `window.__btConfig.keyPageUrl = "/custom/keys.html"`, then trigger no-key render → anchor `getAttribute("href") === "/custom/keys.html"`. Eager module-init cache FAILS this arm.
3. **Default fallback** — with `window.__btConfig` undefined OR `keyPageUrl` absent/empty → `href === "api-key.html"` (never `""`, `"undefined"`, or page's own URL).
4. **Existing key suppresses no-key state** — with valid key in localStorage (AC-1 `storeKey`), feedback click proceeds past no-key branch (no `[data-byok="no-key"]` rendered).
5. **DOM-built, `_blank`, no key echo** — anchor built via `createElement` + `.href`/`.textContent` (no `innerHTML` with URL interpolation); `target === "_blank"` + `rel` includes `noopener`; `href` has no `http(s)` scheme with key material, no key value anywhere in rendered DOM.
6. **Scheme rejection (defense-in-depth)** — `keyPageUrl()` rejects `javascript:`/`data:` schemes (case-insensitive, whitespace-trimmed), falling back to `"api-key.html"`.
7. **(folded from A)** `keyPageUrl()` is a pure exported function — importable in Node test: `keyPageUrl()` with stubbed/absent `window` returns `"api-key.html"`; with `window.__btConfig.keyPageUrl` set returns it verbatim (post scheme-check). Export required.

**Probe:**
```bash
# Pure Node: keyPageUrl() unit (NEW asset test file, e.g. key-page-url.test.js, per repo convention)
uv run node --test assets/key-page-url.test.js
# Source-pattern companion: no sessionStorage, no innerHTML-with-href, lazy read inside render body, export present
uv run pytest tests/test_quarto_feedback.py -k "no_key_link or key_page_url"
# Rodney E2E against quarto-fixture/_output/feedback.html (bt-auto-bootstrap:false, no bt-key-page meta → natural default path):
#   arm 1: clear localStorage → click .bt-feedback-btn → assert [data-byok="no-key"] a[href="api-key.html"], text exact, no key input/Save/provider select
#   arm 2: eval window.__btConfig.keyPageUrl="/custom/keys.html" post-load → re-render → assert href updated
#   arm 3: eval delete window.__btConfig → re-render → assert href "api-key.html"
#   arm 4: localStorage.setItem via AC-1 storeKey seam → click → assert NO [data-byok="no-key"]
#   arm 5/6: assert target/rel, no key echo; eval keyPageUrl="javascript:alert(1)" → href "api-key.html"
uvx rodney open <fixture-url> && uvx rodney click ".bt-feedback-btn" && uvx rodney assert "..."   # per arm
```

**Negative:** eager module-init read (passes naive test — AC-3 head script precedes deferred module — but freezes value, breaking dynamic config + eval-injection seam, violating §2.1); `javascript:` href emitted unfiltered; hardcoded `href="api-key.html"` literal with no `__btConfig` read; link rendered when key present; old inline form still in DOM alongside link; missing `_blank` → same-tab navigation loses in-progress CM6 code.

**Verification:** code + rodney · pytest (Node pure fn + source-pattern) + rodney click/eval probes

**Fixture status:** `quarto-fixture/feedback.qmd` (existing, unchanged); `scripts/tests/test_quarto_feedback.py` EDIT (additive no-key-link asserts); `rodney-probes/feedback-probe.js` EDIT (clauses 1/3/4/8 — inline-form asserts at 328/338/341/377/400/403/483/486 → no-key-link asserts + localStorage injection seam); NEW Node unit test for `keyPageUrl()`.

**Rubric anchor:** §2.1 (no module-level effectful — lazy read pinned), §2 (pure `keyPageUrl()` core vs effectful render shell), §5 (one-thing render fn, testable without patches)

**Design Intent:**
- **Types/interfaces (§1):** `keyPageUrl(): string` — total function, never throws, never returns empty/undefined; scheme-validated output.
- **Pure/effectful (§2):** `keyPageUrl()` pure-ish (reads `window` at CALL time, no module-level capture, no mutation); `renderKeyPrompt` = thin effectful shell calling it per render.
- **Boundary cuts (§3):** emission boundary stays in blendtutor.lua/AC-3; consumption boundary in exercise-feedback.js; probe-fixture boundary in rodney-probes (AC-4 owns inline-form clause root-cause fix).
- **Module responsibility (§4):** exercise-feedback.js owns no-key UX rendering; does NOT own keyPageUrl emission, storage internals (AC-1), or probe extension for new storage clauses (AC-8).
- **Function discipline (§5):** `keyPageUrl()` = read + scheme-check + fallback, one thing, Node-testable without browser patches; render fn reads config at call time, not import time.

**Technical Context:** Files: `_extensions/blendtutor/assets/exercise-feedback.js` (`renderKeyPrompt` 413–470 — replace body: remove key-input/Save retained by AC-6, emit `[data-byok="no-key"]` container + single DOM-built anchor; export `keyPageUrl()`), `scripts/tests/test_quarto_feedback.py`, `rodney-probes/feedback-probe.js`, NEW `key-page-url` unit test. Lazy-read pin: `const href = keyPageUrl()` INSIDE render body. Link semantics: `target="_blank" rel="noopener"`, copy exactly "Enter your API key first" (AC literal). Marker: `[data-byok="no-key"]`. NOT touched: `blendtutor.lua` (AC-3 owns emission). Demo-book mirror: sync exercise-feedback.js change (same sync gap as AC-3 — manual copy + cmp as done-condition).

**Dependencies:** depends-on: 1, 3 | blocks: 5, 8 | conflict set: `_extensions/blendtutor/assets/exercise-feedback.js`, `scripts/tests/test_quarto_feedback.py`, `rodney-probes/feedback-probe.js` (NOT `blendtutor.lua`) | notes: probe ownership split — AC-4 updates feedback-probe.js clauses 1/3/4/8 (root cause: inline form removed here; broken probe would fail rodney CI at this PR and arm 4 needs working path); AC-8 extends with no-key-flow + localStorage clauses (mirrors AC-1 delegating its sessionStorage probe break to AC-8).

**Clarifications resolved:**
- **Lazy read: PINNED** (arm 2) — eager read passes obvious test (AC-3 head script runs before deferred module) but freezes value; breaks dynamic config, eval-injection seam, §2.1.
- **Code-loss on navigation: `target="_blank"` + `rel="noopener"`** — learner's in-progress CM6 code is irreplaceable user work; nav-convention deviation scoped to this one fallback link; key-page-first intended flow preserved since new tab doesn't disrupt exercise tab. Arm 5 asserts `target === "_blank"`.
- **javascript: defense: KEEP arm 6** — 2 lines; defends future user-set `bt-key-page` YAML meta / XSS writing `__btConfig`; folded into `keyPageUrl()` purity contract.
- **Link copy: PINNED** "Enter your API key first" (AC literal).
- **feedback-probe.js: ADDED to conflict set; AC-4 owns inline-form clause updates** (root cause); AC-8 extends.
- **blendtutor.lua: CONFIRMED REMOVED** from conflict set.

**needs-clarification:** NONE

### Progress
- [x] AC-4 spec resolved — 2026-08-06
- [ ] implementation — pending B4

### Decision Log
- 2026-08-06 — no-key link opens in new tab (target=_blank + noopener) to preserve in-progress code; keyPageUrl() lazy-read at render time (not module init); scheme rejection defense-in-depth; AC-4 owns feedback-probe.js inline-form clause fix; blendtutor.lua removed from conflict set (AC-3 owns emission).

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run `uv run pytest scripts/tests/test_quarto_feedback.py -k "no_key_link or key_page_url"` after interrupted edit.
- Rollback: git revert the PR; re-sync demo-book mirror.

---

# AC-5

---
ac: 5
depends_on: 4
risk: medium
status: spec
---

**Predicate:**
1. **No auto-fire:** Clicking "Check" or "Run" triggers ZERO fetches to `*/chat/completions`; `exercise-runtime.js` contains ZERO references to exercise-feedback (regression guard).
2. **Button sole trigger, single click:** `.bt-feedback-btn[data-byok="submit"]` exists with EXACT `textContent === "Get feedback"`, visible, enabled; ONE click after Check completes → EXACTLY ONE `fetch` POST to `${providerBaseUrl("fireworks")}/chat/completions`.
3. **Model pin:** Request body `model === "accounts/fireworks/models/deepseek-v4-flash-0731"`; static assert `FIREWORKS_MODEL === "accounts/fireworks/models/deepseek-v4-flash-0731"` (both uses, :65,:92).
4. **Picker collapsed:** `[data-byok="model-picker"]` is NEVER rendered; no second click phase between key-check and rate-limit (renderModelPicker/modelPickerPresent/selectedModel removed from handleSubmitForExercise, :587-595).
5. **Check output in prompt:** Prompt body contains `<<<CAPTURED_OUTPUT>>>` label + the text of this exercise's `.bt-output`, exactly once, fenced.
6. **Lesson context in prompt:** Prompt contains Task line, `<<<CHECK_RESULTS>>>` block, and student code fences.
7. **No cross-exercise bleed:** Exercise A's prompt contains A's `.bt-output` text and NOT B's (two exercises on page, per-exercise scoping via `entry.element.querySelector(".bt-output")`, :399-408).
8. **textContent-only verdict (XSS):** Verdict payload containing `<img src=x onerror=window.__xss=1>` renders as literal text; `window.__xss === undefined`; static assert: zero `innerHTML` in exercise-feedback.js (renderVerdict :474-486).
9. **Rate-limit refusal:** With counter at cap, clicking "Get feedback" renders limit-reached message, ZERO fetches; `rateLimitReached()` evaluated BEFORE `getFeedback` (:374-378 ordering).
10. **No-key refusal:** With `readKey()` null, clicking renders AC-4 no-key state (`[data-byok="no-key"]` link to key page), ZERO fetches, old key form NOT shown.
11. **Error path:** Failed fetch (spy rejects) → `[data-byok="error"]` rendered via textContent; session counter still incremented.
12. **Concurrent guard:** Two rapid clicks → exactly ONE fetch (`_feedbackRunning` guard).
13. **Empty-output tolerated:** "Get feedback" stays enabled before Check; empty `.bt-output` yields empty `<<<CAPTURED_OUTPUT>>>` section — NOT an error, fetch still fires (user decision: explicit click, no gate).

**Probe:**
```
uv run pytest scripts/tests/test_quarto_feedback.py -k "ac5 or feedback"
```
- New pytest asserts: (a) static: `FIREWORKS_MODEL` string grep in exercise-feedback.js; no `innerHTML`; no `renderModelPicker`/`modelPickerPresent`/`selectedModel` symbols; `exercise-runtime.js` zero feedback references; (b) Node: `buildPrompt` emits `<<<CAPTURED_OUTPUT>>>` + `<<<CHECK_RESULTS>>>` + task + code fences given fixture args.
- Rodney (fetch-spy — NO stub server): override `window.fetch` with recording spy via rodney eval (session-scoped); pre-populate `.bt-output` via eval (skip webR boot); assert arms 1,2,4,5,7,8,9,10,11,12,13 by scripted clicks + spy inspection. Fetch-spy chosen over stub server: no new fixture file, no port management; AC-8 owns the production-grade stub for end-to-end; wiring provable purely at the `window.fetch` seam.

**Negative:**
- Existing pure-function test calls `buildPrompt` directly (:296-307) and passes while DOM→prompt wiring is broken — rodney wiring asserts (arms 2,5,7) MUST exist, pytest-only is insufficient.
- Auto-fire: any code path where Check/Run triggers a feedback fetch.
- Button mislabeled, disabled-gated, or duplicated (two buttons → two fetches).
- Verdict via innerHTML → onerror XSS executes.
- Prompt omits captured output or leaks other exercise's output.
- No-key / rate-limited click still fires fetch.
- Picker survives as hidden-but-rendered DOM (`display:none` still counts as rendered — assert absence, not visibility).

**Verification:** code + rodney (fetch-spy)

**Fixture status:** `quarto-fixture/feedback.qmd` (existing) — MUST add `window.__btConfig = { maxFeedbackPerSession: 3 }` (absent → `rateLimitReached()` returns `0>=0===true`, feedback silently disabled :374-378). No NEW files. Test migration: AC-1 owns sessionStorage→localStorage mock swap in `test_quarto_feedback.py` (:273-280), `test_quarto_ux.py` (:699-705), `test_quarto_distribution.sh` (:241-244); AC-5 adds wiring + picker-collapse asserts only.

**Rubric anchor:** §2 (pure buildPrompt vs effectful fetch/DOM), §5 (handleSubmitForExercise single-path discipline)

**Design Intent:**
- **Types/interfaces (§1):** Prompt shape is a typed contract — buildPrompt args (code, task, output, checks) map 1:1 to labelled sections; pinned model is a module constant, not user input.
- **Pure/effectful (§2):** buildPrompt pure (:111-129); all effects (fetch, DOM render, counter) in the handleSubmitForExercise shell; verdict render is textContent-only.
- **Boundary cuts (§3):** Feedback owns LLM call + verdict; runtime owns Check/Run — zero cross-references enforced as regression guard.
- **Module responsibility (§4):** exercise-feedback.js: prompt assembly, rate-limit, key gate, verdict. NOT: triggering (user click only), model choice (pinned), execution (runtime).
- **Function discipline (§5):** handleSubmitForExercise collapses to one path: key check → rate-limit check → fetch; no picker phase, no branch per model.

**Technical Context:** Files: `_extensions/blendtutor/assets/exercise-feedback.js` (deltas: remove renderModelPicker/modelPickerPresent/selectedModel from handleSubmitForExercise :587-595; pin FIREWORKS_MODEL at :65,:92), `exercise-runtime.js` (verify-only, no edits expected), `quarto-fixture/feedback.qmd` (add maxFeedbackPerSession), `scripts/tests/test_quarto_feedback.py` (new asserts). Verified-existing wiring: buildPrompt :111-129, currentSubmissionForExercise :399-408, mountFeedback :655-661, renderVerdict :474-486, byokFireworks/providerBaseUrl :342-360. Demo-book mirror: sync exercise-feedback.js (same manual cp + cmp done-condition).

**Dependencies:** depends-on: 4 | blocks: 8, 9 | conflict set: `_extensions/blendtutor/assets/exercise-feedback.js`, `exercise-runtime.js` (verify-only), `scripts/tests/test_quarto_feedback.py` | notes: AC-5 owns picker collapse + `-0731` model pin (2 real deltas atop regression guard); maxFeedbackPerSession is fixture-only — blendtutor.lua default emission is out-of-scope, defer to AC-9 docs or follow-up.

**Clarifications resolved:**
- Picker removal ownership → AC-5 owns (no other AC covers it; single-click-fetch asserted here; user approved pinning).
- Model string → AC-5 pins FIREWORKS_MODEL to `accounts/fireworks/models/deepseek-v4-flash-0731`.
- Empty .bt-output before Check → tolerated, no disabled gate (matches existing behavior; user specified no auto-fire, not a gate).
- maxFeedbackPerSession → fixture sets it; lua default flagged out-of-scope for AC-5.
- Stub server (A) dropped → rodney fetch-spy (B) sufficient; AC-8 owns production stub.
- Test migration → AC-1 owns storage mock swap; AC-5 adds wiring/picker asserts.

**needs-clarification:** NONE

### Progress
- [x] AC-5 spec resolved — 2026-08-06
- [ ] implementation — pending B5

### Decision Log
- 2026-08-06 — AC-5 owns picker collapse + model pin (-0731); empty-output tolerated (no gate); fetch-spy over stub server; fixture adds maxFeedbackPerSession.

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run `uv run pytest scripts/tests/test_quarto_feedback.py -k "ac5 or feedback"` after interrupted edit.
- Rollback: git revert the PR; re-sync demo-book mirror.

---

# AC-6

---
ac: 6
depends_on: 1
risk: medium
status: spec
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
- [ ] implementation — pending B2

### Decision Log
- 2026-08-06 — readProvider is a no-op change (already default-when-absent); Fireworks-only from select removal; stale byok_provider state zero-code (AC-1 clean break); both disclosure strings updated.

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run `uv run python scripts/tests/test_quarto_feedback.py` after interrupted edit.
- Rollback: git revert the PR; re-sync demo-book mirror via `scripts/sync-quarto-assets.sh`.

---

# AC-7

---
ac: 7
depends_on: 2, 3
risk: low
status: spec
---

**Predicate:**
- **P1 (file + div):** `demo-book/api-key.qmd` (NEW) exists with exactly the fenced div `::: {.blendtutor-key}` — class string `blendtutor-key`, no typo (wrong class silently no-ops AC-2's `mountKeyPage`).
- **P2 (registration + order):** `demo-book/_quarto.yml` `book.chapters` lists `- api-key.qmd` at a line number LESS than `- r-exercises.qmd`'s (first content chapter; order = nav order).
- **P3 (meta on BOTH exercise chapters — plural load-bearing):** `demo-book/r-exercises.qmd` AND `demo-book/python-exercises.qmd` front matter each carry scalar `bt-key-page: api-key.html`.
- **P4 (index BYOK rewrite):** `demo-book/index.qmd` BYOK section (lines 18-26) contains literal `Fireworks` AND a link target `api-key.html` AND ZERO `ANTHROPIC_API_KEY`.
- **P5 (render-time, gated on AC-3 done-condition):** `quarto render demo-book --to html` exits 0 AND `_output/api-key.html` exists AND contains `class="blendtutor-key"` AND `_output/r-exercises.html`, `_output/python-exercises.html`, `_output/api-key.html` each contain `__btConfig`. Catches stale-vendored-lua (demo-book/_extensions/mcmullarkey/blendtutor/blendtutor.lua lacking AC-3's key-page reader).

**Probe:** extend `scripts/tests/test_quarto_distribution.sh` (NOT a new script — avoids a second full book render):
- Structural arms (no quarto needed):
```
test -f demo-book/api-key.qmd \
  && grep -qF '::: {.blendtutor-key}' demo-book/api-key.qmd \
  && (line of - api-key.qmd in _quarto.yml < line of - r-exercises.qmd) \
  && grep -qF 'bt-key-page: api-key.html' demo-book/r-exercises.qmd \
  && grep -qF 'bt-key-page: api-key.html' demo-book/python-exercises.qmd \
  && (index.qmd BYOK section contains 'Fireworks') \
  && (index.qmd BYOK section contains 'api-key.html') \
  && ! grep -qF 'ANTHROPIC_API_KEY' demo-book/index.qmd
```
- Render arm, appended INSIDE clause 7's post-render block (reuses `$RENDER_HTML_DIR`, runs only when quarto present; SKIPs on quarto-missing like clause 7):
```
test -f "$RENDER_HTML_DIR/api-key.html" \
  && grep -qF 'class="blendtutor-key"' "$RENDER_HTML_DIR/api-key.html" \
  && for page in api-key r-exercises python-exercises; do grep -qF '__btConfig' "$RENDER_HTML_DIR/$page.html"; done
```
Run: `bash scripts/tests/test_quarto_distribution.sh`

**Negative:**
- **Stale-vendored-lua (primary):** source P1-P4 pass but vendored `demo-book/_extensions/.../blendtutor.lua` lacks AC-3's reader → rendered pages lack `__btConfig` → P5 fails. This AC-7-owned test fails without AC-3's manual `cp` + `cmp` done-condition.
- Chapter file exists NOT listed in `_quarto.yml` chapters → P2 fails (page not rendered).
- `bt-key-page` meta on only ONE exercise chapter → P3 fails (other chapter silently falls back to default; works by accident today, breaks if default ever changes).
- Partial BYOK edit: Fireworks mentioned but `export ANTHROPIC_API_KEY` block survives → P4 fails.
- Wrong div class (e.g. `.blendtutor-keys`, `.blendtutor-key-page`) → mount no-ops → P1 fails.

**Verification:** code (structural grep/awk + render arm — deterministic; no browser; mount UI verification = AC-8 rodney)

**Fixture status:** NEW `demo-book/api-key.qmd`; edit-in-place `demo-book/_quarto.yml:9-12` (insert api-key.qmd after index.qmd), `demo-book/index.qmd:18-26` (BYOK rewrite), `demo-book/r-exercises.qmd:1-4` (+bt-key-page), `demo-book/python-exercises.qmd:1-3` (+bt-key-page); extend `scripts/tests/test_quarto_distribution.sh` (DEMO_QMD_FILES glob :344-349 — verified NO count assert at :413, so a 4th .qmd breaks nothing; page loop :490 stays r/python only — api-key needs no site_libs runtime asserts; render invocation :431 + RENDER_HTML_DIR :447 reused).

**Rubric anchor:** §3 (cut at the joints — key page first-class navigable chapter, registered before exercises; content/nav cut vs filter behavior in AC-3), §4 (index.qmd BYOK section names what key + where to enter — what/where responsibility).

**Design Intent:**
- **Types/interfaces (§1):** `bt-key-page` YAML scalar = typed interface between chapter and runtime — value `api-key.html` matches AC-3's default so behavior pinned AND explicit.
- **Pure/effectful (§2):** AC-7 is declarative content/config; all behavior lives in AC-2/AC-3 code; AC-7 observes via the render arm.
- **Boundary cuts (§3):** demo book = canonical multi-chapter example; key page lives exactly once, shared across chapters via localStorage — the cut is content/nav (this AC) vs filter behavior (AC-3). P5's render arm guards the vendored-copy seam between them.
- **Module responsibility (§4):** `_quarto.yml` chapters list = single source of truth for nav order; each exercise chapter declares its key-page link via own front matter (no central registry drift).
- **Function discipline (§5):** test extension adds one structural clause + one render-arm block inside clause 7 — reuses existing render, helpers, ok/ko reporting; no duplicated render invocation.

**Technical Context:** book project (`type: book`, output-dir `_output`); book output FLATTENED (all chapters at `_output/*.html` same level — `api-key.html` is the correct relative link target from every chapter, verified). By-name filter `mcmullarkey/blendtutor` → vendored `demo-book/_extensions/mcmullarkey/blendtutor/`; `scripts/sync-quarto-assets.sh` does NOT sync blendtutor.lua or the demo-book vendored copy — vendored currency is AC-3's manual `cp` + `cmp` done-condition; P5 is the tripwire. Filter meta pattern `doc.meta["<key>"]` (blendtutor.lua:628-645 coi + bt-auto-bootstrap readers); scalar `bt-key-page: api-key.html` matches the `coi: true` shape. `include_text("in-header", ...)` (blendtutor.lua:699-700) is AC-3's emission point for `__btConfig`. No-exercise-page edge: api-key.qmd has no `::: {.blendtutor}` → `has_blendtutor` false → AC-3 C14's broadened `has_blendtutor or has_key` guard deploys assets + mounts — NO sentinel needed in api-key.qmd. Current index.qmd BYOK text is `export ANTHROPIC_API_KEY=sk-ant-...` (index.qmd:22-24) — replaced with Fireworks-only: key entered once on key page (link api-key.html), stored in browser localStorage, sent only in Authorization Bearer to api.fireworks.ai; book must be served over HTTP (localStorage + ES modules) — note `python -m http.server 8000` like test_demo_docs.sh. Render/deploy: `ci.yml:201`, `docs.yml:93`, `check-docs.sh:76` (quarto render demo-book) → `check-docs.sh:80` dot-copy to `/demo-book/` Pages artifact.

**Dependencies:** depends-on: 2 (mountKeyPage + `.blendtutor-key` API), 3 (key-page.js deployment + `__btConfig` emission + vendored-sync done-condition) | blocks: 8 (rodney probe targets key page mount) | conflict set: `demo-book/api-key.qmd` (NEW), `demo-book/_quarto.yml`, `demo-book/index.qmd`, `demo-book/r-exercises.qmd`, `demo-book/python-exercises.qmd`, `scripts/tests/test_quarto_distribution.sh` | notes: SEQUENCING HAZARD — AC-7 requires AC-3 merged AND its demo-book vendored-sync done-condition executed; if AC-7 lands first, the render arm must SKIP (not fail) until AC-3 merges, same pattern as the quarto-missing skip.

**Clarifications resolved:**
- Vendored-sync ownership: AC-3 owns `cp` + `cmp` done-condition (resolved there). AC-7 does NOT re-own sync; P5 render arm IS the AC-7-owned test that fails without it — kept, gated.
- No-exercise-page: pure `::: {.blendtutor-key}` div suffices — AC-3 C14's broadened has_key guard handles asset deploy + mount. No sentinel div, no dummy exercise.
- `__btConfig` name: `window.__btConfig` per feature context, emitted by AC-3; P5 greps `__btConfig` substring — final-name drift fails P5 loudly (desired).
- Meta format: scalar `bt-key-page: api-key.html` — matches `coi: true` reader pattern; value identical to AC-3 default.
- Test placement: extend `test_quarto_distribution.sh` clause 7 (structural clause + render arm), NOT a new `test_demo_book_key_page.sh` — clause 7 already pays the `quarto render demo-book` cost once; a second full book render doubles local + CI render time for zero new signal.

**needs-clarification:** NONE

### Progress
- [x] AC-7 spec resolved — 2026-08-06
- [ ] implementation — pending B4

### Decision Log
- 2026-08-06 — test extended into test_quarto_distribution.sh clause 7 (no second render); P5 render arm gates on AC-3's vendored-sync done-condition; api-key.qmd needs no exercise sentinel.

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run `bash scripts/tests/test_quarto_distribution.sh` after interrupted edit.
- Rollback: git revert the PR; re-sync demo-book extension dir manually if vendored drift.

---

# AC-8

---
ac: 8
depends_on: 4, 5, 6, 7
risk: medium
status: spec
---

**Predicate:** Rodney coverage passes CI-gated end-to-end with no sneaky-pass surfaces. All of:
- **P1 exit-code gate:** `rodney-probes/key-page-probe.js` AND `rodney-probes/feedback-probe.js` call `process.exit(1)` (non-zero) when writeReport verdict is PROBES_FAIL. Fixes feedback-probe.js:537 defect (currently exits 0 on failure; pages-live.js:701 is the reference pattern).
- **P2 CI-wired, PR-gating:** `.github/workflows/ci.yml` contains a NEW job (e.g. `rodney-probes`) triggered on `pull_request` that runs BOTH probes; probe steps have NO `continue-on-error`, NO `|| true`, NO `if: always()`. Job includes setup-uv, setup-node, Chrome, and `ROD_CHROME_BIN=scripts/rodney-chrome.sh`.
- **P3 real rendered pages only:** neither probe calls `generateProbeHtml` or substitutes synthetic DOM (feedback-probe.js:188-216 sneaky-pass removed); all navigation targets files under the served `demo-book/_output/` root.
- **P4 storage swap complete:** `grep -c 'sessionStorage' rodney-probes/feedback-probe.js` == 0 (ALL THREE refs :254, :259, :381 converted to localStorage; :259 `freshFixture()` included).
- **P5 cross-page persistence:** key saved via UI on `api-key.html` (page 1) → real navigation (`location.href`, blank-then-href per :235-241) to `r-exercises.html` (page 2, same origin) → localStorage key present AND feedback UI proceeds past no-key state. NOT eval pre-seed, NOT new tab.
- **P6 save flow UI+effect:** input `type=password` + `autocomplete=off`; status line visible per `getComputedStyle` (not display:none/off-screen); localStorage slot populated; input value cleared after save; fetch spy observed GET `/models` against stub.
- **P7 clear flow counter reset:** after clear, key slot AND `bt_feedback_count` both null in localStorage.
- **P8 invalid-key 401:** stub `/_config/auth` toggle forces 401 on `/models`; key page shows invalid-key error AND key NOT stored (matches AC-2 classifyValidation 401 → invalid-key).
- **P9 network error optimistic save:** stub unreachable → key still stored (optimistic save per AC-2) AND network-error status shown.
- **P10 verdict end-to-end through real stub:** with localStorage key present, clicking `[data-byok="submit"]` drives request through localhost stub `/chat/completions`; verdict renders into `[data-byok="verdict"]` via textContent only — XSS payload in stub response renders as literal text (no element injection). No fetch-spy substitution for this clause.
- **P11 no-key link end-to-end:** with empty localStorage, exercise page renders `[data-byok="no-key"]` link (target=_blank rel=noopener, href = keyPageUrl) AND ZERO `/chat/completions` fetches observed.
- **P12 EVIDENCE_DIR parameterized:** both probes read `EVIDENCE_DIR` env with default `docs/evidence/<issue>`; no hardcoded `docs/evidence/112` (feedback-probe.js:25 defect fixed).
- **P13 Chrome wrapper:** `scripts/rodney-chrome.sh` used via `ROD_CHROME_BIN` for the cross-page COI leg; strips poison flags (`--single-process`, `--disable-site-isolation-trials`, `--disable-features=*`).

**Probe:**
```bash
# CI wiring greps
grep -q 'rodney-probes' .github/workflows/ci.yml && \
! grep -A20 'rodney-probes' .github/workflows/ci.yml | grep -qE 'continue-on-error|\|\| true|if: always\(\)' && \
grep -q 'pull_request' .github/workflows/ci.yml && \
grep -q 'ROD_CHROME_BIN' .github/workflows/ci.yml
# source scans
test "$(grep -c 'sessionStorage' rodney-probes/feedback-probe.js)" = "0" && \
! grep -q 'generateProbeHtml' rodney-probes/key-page-probe.js rodney-probes/feedback-probe.js && \
! grep -q 'docs/evidence/112' rodney-probes/feedback-probe.js
# render + run (mirrors ci.yml:201 render step)
quarto render demo-book && \
EVIDENCE_DIR=docs/evidence/<issue> uv run node rodney-probes/key-page-probe.js && \
EVIDENCE_DIR=docs/evidence/<issue> uv run node rodney-probes/feedback-probe.js
# both exit 0; both write probe-report.json verdict PROBES_PASS covering P5-P11
```

**Negative:**
- Probe asserts selector presence only (element exists but display:none/off-screen) → must use getComputedStyle for visibility clauses (P6, P11).
- Probe passes while feedback-probe.js still reads sessionStorage (:254/:259/:381 unconverted) → P4 grep gate.
- Happy-path only: no invalid-key 401, no network-error optimistic save → P8/P9 mandatory.
- Same-page localStorage read without navigation hop, or eval pre-seed / new-tab fake cross-page → P5 requires real `location.href` navigation same-origin.
- Exit-0-on-failure (report says PROBES_FAIL, CI green) → P1 non-zero exit + P2 no-swallow wiring.
- generateProbeHtml DOM substitution passes against fake page → P3 real rendered `_output/` only.
- Verdict clause satisfied by fetch-spy without real stub round-trip → P10 requires stub `/chat/completions` + XSS literal check.
- CI job present but decorative (continue-on-error/|| true/if: always(), or only in deploy-gated docs.yml) → P2 greps.
- Hardcoded evidence path → P12.
- Chrome missing/poisoned flags in CI runner → P13 wrapper + builder verification instruction.
- Stub 401 implemented as `?key=invalid` hack diverging from one-server config seam → P8 requires `/_config/auth` toggle (consistent with `/_config/delay`).

**Verification:** rodney (P5-P11, P13 via probe execution) + code (P1, P3, P4, P12 source scans; P2 CI-wiring greps)

**Fixture status:** `rodney-probes/key-page-probe.js` NEW · `rodney-probes/feedback-probe.js` EDIT (:25, :254, :259, :381, :188-216, :537) · `scripts/rodney-chrome.sh` NEW · `.github/workflows/ci.yml` EDIT (new PR-gating job) · stub server EDIT (add `/_config/auth` toggle) · serves `demo-book/_output/` (AC-7 render arm output — not new fixture)

**Rubric anchor:** §3 (boundary cuts — probe harness vs production code; real-stub boundary vs fetch-spy fake), §2 (pure/effectful — probe report/writeReport separated from browser-driving shell)

**Design Intent:**
- **Types/interfaces (§1):** probe-report.json schema {verdict: PROBES_PASS|PROBES_FAIL, clauses: [...]} is the contract consumed by CI; EVIDENCE_DIR + ROD_CHROME_BIN env interface.
- **Pure/effectful (§2):** clause assertions pure predicates over observed state; servers/navigation/writeReport effectful shell at edges (:38-97, :162-169, :534-568).
- **Boundary cuts (§3):** probes exercise production code only through real HTTP + real rendered pages; stub is the single fake boundary (`/_config/delay`, `/_config/auth`); no in-process substitution.
- **Module responsibility (§4):** key-page-probe.js owns key-page clauses (P6-P9); feedback-probe.js owns feedback clauses (P5, P10, P11); ci.yml owns gating; neither probe duplicates the other's clause set.
- **Function discipline (§5):** one clause = one assertion function, independently reported in probe-report.json; writeReport + exit-code gate single responsibility.

**Technical Context:** Harness mirrors feedback-probe.js verified architecture: COI static server :8080 + stub :8081 (:38-97), rodney wrapper via execFileSync uvx (:162-169), navigateToFixture blank-then-location.href (:235-241 — required for P5 cross-page hop), installSpies (:218-233), writeReport (:534-568). Server root = `demo-book/_output/` (flat per AC-7; provides both api-key.html + r-exercises.html for P5). rodney 0.4.0 has no addInitScript — hence P5 forbids eval pre-seed. Stub extended with `/_config/auth` toggle for 401 (P8) alongside existing `/_config/delay`. CI: ci.yml quarto-render job (:53) lacks Chrome/uv and ci.yml:201 already renders demo-book — new `rodney-probes` job in ci.yml reuses render pattern, adds setup-uv, setup-node, Chrome, ROD_CHROME_BIN=scripts/rodney-chrome.sh. docs.yml rejected as target (deploy-gated, :172 runs pages-live.js only; not a PR gate). Builder instruction (not spec clause): verify Chrome + rodney launch in the GitHub runner per github-pages-deploy retro tool-behavior research requirement — rodney-chrome.sh must strip `--single-process` and related poison flags. Exit-code pattern copy pages-live.js:701.

**Dependencies:** depends-on: 4 (no-key link), 5 (submit/verdict/pinned model), 6 (provider select removed — probe asserts no provider UI), 7 (rendered `_output/` api-key.html — fixture source) | blocks: none | conflict set: `rodney-probes/key-page-probe.js` (NEW), `rodney-probes/feedback-probe.js` (EDIT), `.github/workflows/ci.yml` (EDIT — decomposition missed it), `scripts/rodney-chrome.sh` (NEW), stub server (EDIT) | notes: lands LAST in feature; transitively depends on AC-3 vendored sync done-condition (book must render correctly); builder must verify Chrome/rodney in CI runner before declaring done.

**Clarifications resolved:**
- Workflow target: NEW PR-gating `rodney-probes` job in ci.yml. Rationale: docs.yml is deploy-gated (not a PR gate); existing ci.yml jobs lack Chrome — new job isolates browser deps without destabilizing Python-test job.
- :259 scope: AC-8 owns ALL THREE sessionStorage refs (:254, :259, :381), enforced as `grep -c == 0`. Rationale: count-based gate can't miss a ref the way enumeration did in AC-1.md.
- Stub 401 mechanism: `/_config/auth` toggle on existing stub. Rationale: one stub server, consistent with `/_config/delay` seam.
- Fixture source: serve `demo-book/_output/` for all clauses. Rationale: P5 cross-page hop needs two real same-origin pages (api-key.html + r-exercises.html) which only exist in the book render. Adds AC-7 + AC-3 transitive dependency — confirmed.
- Invalid-key semantics: stub 401 → invalid-key error shown + key NOT stored. Matches AC-2 classifyValidation contract.
- P12 EVIDENCE_DIR: env-parameterized, default `docs/evidence/<issue>`, both probes.

**needs-clarification:** NONE

### Progress
- [x] AC-8 spec resolved — 2026-08-06
- [ ] implementation — pending B6

### Decision Log
- 2026-08-06 — NEW PR-gating rodney-probes job in ci.yml (docs.yml rejected — deploy-gated); all 3 sessionStorage refs in feedback-probe.js; /_config/auth stub toggle; demo-book/_output served root; EVIDENCE_DIR env-parameterized; exit-code gate + CI-wiring are the non-negotiable core.

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run the probe probe command chain after interrupted edit.
- Rollback: git revert the PR.

---

# AC-9

---
ac: 9
depends_on: 7
risk: low
status: spec
---

**Predicate:** All clauses hold:
1. README.md `## BYOK` section (scoped via awk section extraction) contains `Fireworks` AND literal `accounts/fireworks/models/deepseek-v4-flash-0731` AND `localStorage`; and does NOT contain `sessionStorage` NOR `ANTHROPIC_API_KEY` within the BYOK section.
2. Whole-README: literal `Anthropic-only` absent (kills stale :44 browser claim). `ANTHROPIC_API_KEY` env var doc at :43 (CLI section, supported via rig/ADR-0006) is KEPT — only the :44 browser claim is fixed.
3. BYOK section states feedback is auto-mounted; BYOK section does NOT contain `manual opt-in` or `not auto-mounted` (reversal of filter-runtime-bootstrap decision, per AC-3).
4. BYOK section contains a sentence stating `file://` breaks localStorage sharing across pages AND ES modules — book must be served over HTTP.
5. BYOK section contains `connect-src` AND `api.fireworks.ai` with RECOMMENDATION framing (`recommend|consider|should` present); section does NOT frame CSP as `enforced|applied|configured` (no CSP-header mechanism exists on GitHub Pages; coi-serviceworker covers COOP/COEP only).
6. Security wording: README states key stored in browser localStorage (readable by any JS on the origin; XSS can steal it — do not reuse critical keys) and sent only in the `Authorization` header to `api.fireworks.ai`; README contains NO overclaim literals `fully secure|vault|completely safe|cannot be stolen`.
7. `docs/adr/0016-*.md` exists and references `api-key`, Fireworks CORS, ADR-0009, and ADR-0014.
8. ADR-0009 status line (:3) updated to note partial supersession by ADR-0016 (Fireworks-only UX; :79-87 multi-provider chooser + :99-102 sessionStorage slots stale); ADR-0014 status line (:3) updated to `Accepted; superseded by ADR-0016` (title "shared sessionStorage" + :29-33/:46 stale).
9. `scripts/tests/test_quarto_distribution.sh` exits 0 — Clause 10 (:238-260) rewritten in lockstep in the SAME PR: :251 assertion flipped from `not auto-mounted|manual` to an auto-mounted assertion; :256 `ANTHROPIC_API_KEY` assertion kept green via surviving CLI doc at :43 (or rescoped to BYOK section only).
10. `scripts/tests/test_demo_docs.sh` exits 0 — c9 line-pin (288-342) not shifted by README edits (adjust pins in lockstep if needed).
11. `scripts/check-docs.sh` exits 0.
12. `.opencode/plans/filter-runtime-bootstrap.md:78` annotated in-place SUPERSEDED (or ADR-0016 explicitly records the auto-mount reversal — annotate-in-place preferred).
13. ADR-0016 references out-of-conflict-set stale surfaces as corrected-on-next-regen: `docs/okf/interfaces/js-runtime-seam.md:57` (GENERATED — reference-only, do NOT hand-edit; regenerate via okf-bundle skill) and `docs/agent-notes/feedback.md:53` (hand-curated — EDITED directly in this PR, included in conflict set).

**Probe:**
```bash
awk '/^## BYOK/,/^## /' README.md | grep -q 'Fireworks' \
  && awk '/^## BYOK/,/^## /' README.md | grep -q 'accounts/fireworks/models/deepseek-v4-flash-0731' \
  && awk '/^## BYOK/,/^## /' README.md | grep -q 'localStorage' \
  && ! awk '/^## BYOK/,/^## /' README.md | grep -q 'sessionStorage' \
  && ! awk '/^## BYOK/,/^## /' README.md | grep -q 'ANTHROPIC_API_KEY' \
  && ! grep -q 'Anthropic-only' README.md \
  && awk '/^## BYOK/,/^## /' README.md | grep -qi 'auto-mounted' \
  && ! awk '/^## BYOK/,/^## /' README.md | grep -qiE 'manual opt-in|not auto-mounted' \
  && awk '/^## BYOK/,/^## /' README.md | grep -q 'file://' \
  && awk '/^## BYOK/,/^## /' README.md | grep -q 'connect-src' \
  && awk '/^## BYOK/,/^## /' README.md | grep -qiE 'recommend|consider|should' \
  && ! awk '/^## BYOK/,/^## /' README.md | grep -qiE 'CSP.*(enforced|applied|configured)' \
  && awk '/^## BYOK/,/^## /' README.md | grep -q 'Authorization' \
  && ! grep -qiE 'fully secure|vault|completely safe|cannot be stolen' README.md \
  && ls docs/adr/0016-*.md \
  && grep -q 'api-key' docs/adr/0016-*.md \
  && grep -qi 'CORS' docs/adr/0016-*.md \
  && grep -q 'ADR-0009' docs/adr/0016-*.md \
  && grep -q 'ADR-0014' docs/adr/0016-*.md \
  && grep -q 'superseded' docs/adr/0009-*.md \
  && grep -q 'superseded' docs/adr/0014-*.md \
  && bash scripts/tests/test_quarto_distribution.sh \
  && bash scripts/tests/test_demo_docs.sh \
  && bash scripts/check-docs.sh \
  && grep -qi 'SUPERSEDED' .opencode/plans/filter-runtime-bootstrap.md
```

**Negative:**
1. **Test-lockstep sneaky-pass:** builder keeps `manual`/`ANTHROPIC_API_KEY` claims in BYOK section to keep :251/:256 green without rewriting Clause 10 → README internally contradicts AC-3/AC-5. Caught by clauses 1+3 (BYOK-scoped negatives) — test edits and doc edits land in the SAME PR.
2. README updated but no ADR-0016 → decision unrecorded; clause 7 fails.
3. CSP overclaim: README implies CSP enforced/applied by the book → false security claim on GitHub Pages; clause 5 negative fails.
4. Security overclaim: README claims key "fully secure"/"vault"/"completely safe" → dishonest framing of XSS-inherent client-side key risk; clause 6 negative fails.
5. Stale claims survive in ADR-0009/ADR-0014 without supersession status → next reader trusts sessionStorage/multi-provider docs; clause 8 fails.
6. README edit shifts demo-doc line pins → test_demo_docs.sh c9 pin (288-342) red; clause 10 fails. Fix by adjusting pins in lockstep, not reverting docs.
7. Generated okf doc hand-edited → regen overwrites silently; clause 13 enforces reference-only + okf-bundle regen path.
8. Pre-existing ADR-0016 collision: docs/adr/0018:94 references a possibly-retracted 0016 — builder MUST `ls docs/adr/0016-*` before creating; if a pre-existing 0016 exists, renumber to next free slot and update all references.

**Verification:** code (grep/awk-based shell tests + existing test scripts)

**Fixture status:** EDIT README.md | NEW docs/adr/0016-*.md (verify no pre-existing 0016 — 0018:94 anomaly) | EDIT docs/adr/0009-*.md (status field) | EDIT docs/adr/0014-*.md (status field) | EDIT docs/agent-notes/feedback.md:53 | EDIT scripts/tests/test_quarto_distribution.sh Clause 10 (:238-260) | EDIT scripts/tests/test_demo_docs.sh (c9 pin only if shifted) | ANNOTATE .opencode/plans/filter-runtime-bootstrap.md:78

**Rubric anchor:** §4 (module responsibility — docs name what/where/what-NOT: ADR-0016 records the reversal and supersession chain; README BYOK section owns the user-facing contract)

**Design Intent:**
- **Types/interfaces (§1):** N/A — documentation slice. The "contract" is prose invariants enforced by grep assertions (auto-mounted, localStorage, Fireworks-only literals).
- **Pure/effectful (§2):** N/A — no runtime code. Test-script edits are the only executable surface; assertions are pure text checks.
- **Boundary cuts (§3):** Docs cut at ownership boundary: README = user-facing BYOK contract; ADRs = decision history with supersession chain (0009 → 0016, 0014 → 0016); generated okf docs excluded from hand-edit (regen boundary respected); hand-curated agent-notes edited directly.
- **Module responsibility (§4):** ADR-0016 header names what (BYOK storage/provider/auto-mount decisions), where (supersedes ADR-0014 storage claims, partially supersedes ADR-0009), what NOT (does not re-document CLI ANTHROPIC_API_KEY — that stays in ADR-0006/rig).
- **Function discipline (§5):** Each grep/awk clause asserts one claim; Clause 10 rewrite in test_quarto_distribution.sh keeps one assertion per line, scoped to BYOK section where possible.

**Technical Context:**
- README stale claims (verbatim locations): :43-44 (CLI env var KEEP :43, fix :44 browser Anthropic-only claim), :267-268, :270-271, :273-284, :286 — rewrite BYOK section Fireworks-only.
- ADR chain: numbering 0001-0015, 0017, 0018 present — 0016 free BUT 0018:94 references possibly-retracted ADR-0016 → builder verifies `ls docs/adr/0016-*` empty before creating. ADR-0009 (:3 Extended; :79-87 multi-provider chooser; :99-102 sessionStorage slots stale). ADR-0014 (title "shared sessionStorage"; :3 Accepted → `Accepted; superseded by ADR-0016`; :29-33/:46 stale).
- Test-lockstep (mandatory, same PR): test_quarto_distribution.sh Clause 10 :238-260 — :251 currently greps `not auto-mounted|manual` (FAILS once auto-mount documented → flip to auto-mounted assertion); :256 greps `ANTHROPIC_API_KEY` (stays green via CLI :43 doc — or rescope assertion to BYOK section). test_demo_docs.sh c9 line-pin 288-342 — adjust only if README edit shifts lines.
- Out-of-conflict surfaces: docs/okf/interfaces/js-runtime-seam.md:57 GENERATED (okf-bundle frontmatter) → reference-only in ADR-0016, regenerate via okf-bundle skill (respects generated boundary; avoids silent regen-overwrite); docs/agent-notes/feedback.md:53 hand-curated → EDIT directly, in conflict set; .opencode/plans/filter-runtime-bootstrap.md:78 historical plan → annotate SUPERSEDED in place.
- Research-verified facts to document: Fireworks CORS open from GitHub Pages origin; localStorage shared same-origin across pages; file:// breaks localStorage sharing + ES modules (serve over HTTP); CSP connect-src = recommendation for self-hosted deployments only (Pages can't set headers; coi-serviceworker = COOP/COEP only); XSS→key-theft inherent to client-side keys (mitigations already shipped: no third-party scripts, textContent-only rendering).

**Dependencies:** depends-on: 7 (docs surface settled by demo-book rewrite) | blocks: none | conflict set: README.md, docs/adr/0016-*.md (NEW), docs/adr/0009-*.md, docs/adr/0014-*.md, docs/agent-notes/feedback.md, scripts/tests/test_quarto_distribution.sh, scripts/tests/test_demo_docs.sh, .opencode/plans/filter-runtime-bootstrap.md | notes: test-lockstep mandatory (Clause 10 + c9 pins in SAME PR); okf js-runtime-seam.md referenced-not-edited; verify no pre-existing ADR-0016

**Clarifications resolved:**
- NC-1 test_quarto_distribution.sh in conflict set → YES. Stale test asserting manual against auto-mount README = discipline failure; lockstep in same PR.
- NC-2 ADR strategy → NEW ADR-0016 + superseded-by status fields on ADR-0009 (:3 → "Extended; partially superseded by ADR-0016") and ADR-0014 (:3 → "Accepted; superseded by ADR-0016"). Amending ADR-0014 can't fix its stale title; new ADR preserves decision history. Builder verifies 0016 slot empty (0018:94 anomaly).
- NC-3 out-of-conflict surfaces → split handling: agent-notes/feedback.md:53 EDITED (hand-curated, in conflict set); okf js-runtime-seam.md:57 REFERENCE-ONLY in ADR-0016 + regen via okf-bundle (respects generated boundary); filter-runtime-bootstrap.md:78 annotated SUPERSEDED in place.
- NC-4 CSP framing → RECOMMENDATION ("we recommend adding CSP connect-src https://api.fireworks.ai for self-hosted deployments"); negative on enforced/applied/configured literals.
- NC-5 CLI section → KEEP :43 ANTHROPIC_API_KEY (rig/ADR-0006 supports it); fix ONLY :44 browser Anthropic-only claim.
- Security framing → honest: localStorage readable by any JS on origin, XSS theft risk explicit, Authorization-header-only transit, no overclaim literals.

**needs-clarification:** NONE

### Progress
- [x] AC-9 spec resolved — 2026-08-06
- [ ] implementation — pending B7

### Decision Log
- 2026-08-06 — NEW ADR-0016 supersedes ADR-0014/partially ADR-0009 (status-field annotations, bodies untouched); test_quarto_distribution.sh Clause 10 lockstep in same PR; CSP framed as recommendation not enforcement; CLI ANTHROPIC_API_KEY doc kept (:43); security wording honest (no overclaims); okf referenced-not-edited, agent-notes edited.

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run the probe bash chain after interrupted edit.
- Rollback: git revert the PR.

---

# Cross-AC Findings

Key discoveries surfaced across the AC specs:

- **sync-quarto-assets.sh gap:** `scripts/sync-quarto-assets.sh` ASSET_FILES (:30-34) covers only codemirror/styles/coi from crates/core — NOT blendtutor.lua, exercise-feedback.js, key-page.js. The demo-book vendored copy (`demo-book/_extensions/mcmullarkey/blendtutor/`) is a manual copy. Every AC touching `_extensions/` assets folds a manual `cp` + `cmp` done-condition into its own PR (AC-1, AC-3, AC-4, AC-5, AC-6). AC-7's P5 render arm is the tripwire that fails if the vendored lua is stale.
- **AC-4 blendtutor.lua touch elimination:** decomposition line 70 listed blendtutor.lua in AC-4's conflict set for `window.__btConfig.keyPageUrl` emission. AC-3 resolver moved emission ownership entirely to AC-3 (C19-C22, separate head include_text). AC-4's conflict set confirmed blendtutor.lua REMOVED — AC-4 owns only exercise-feedback.js, test_quarto_feedback.py, feedback-probe.js.
- **AC-5 picker-collapse ownership:** no other AC covered removal of the model picker (`renderModelPicker`/`modelPickerPresent`/`selectedModel`). AC-5 owns it (2 real deltas: picker collapse + `-0731` model pin) atop the regression guard. Picker must be ABSENT, not hidden (`display:none` still counts as rendered).
- **feedback-probe.js decorative-probe defect:** feedback-probe.js:537 exits 0 even when writeReport verdict is PROBES_FAIL — a decorative probe that can't fail CI. AC-8 P1 fixes it (non-zero exit on PROBES_FAIL, pattern from pages-live.js:701) and P2 wires it PR-gating in a new ci.yml `rodney-probes` job with no `continue-on-error`/`|| true`/`if: always()`.
- **ADR-0016 gap:** docs/adr numbering 0001-0015, 0017, 0018 present — 0016 free BUT 0018:94 references a possibly-retracted ADR-0016. AC-9 requires builder to `ls docs/adr/0016-*` before creating; renumber to next free slot if a pre-existing 0016 exists.
- **__btConfig merge pattern:** `window.__btConfig = window.__btConfig || {}` then property assignment — NEVER bare `= {...}`. config.js (crates/core/src/site/mod.rs:321) sets `maxFeedbackPerSession` on the same object; a clobber breaks rate limiting at exercise-feedback.js:376. AC-3 C22 enforces it.
- **has_key guard broadening:** blendtutor.lua guards at :681 (`build_html_dependency()`) and :695 (bootstrap injection) must both broaden to `has_blendtutor or has_key` so a key-only page (only `.blendtutor-key`, zero exercises) deploys both assets AND injects bootstrap AND calls mountKeyPage. AC-3 C14.
- **target=_blank decision:** AC-4's no-key link opens in a new tab (`target="_blank"` + `rel="noopener"`) to preserve the learner's in-progress CM6 code — same-tab navigation would lose irreplaceable user work. Nav-convention deviation scoped to this one fallback link.
- **maxFeedbackPerSession fixture note:** `quarto-fixture/feedback.qmd` MUST add `window.__btConfig = { maxFeedbackPerSession: 3 }` — absent, `rateLimitReached()` returns `0>=0===true`, silently disabling feedback (:374-378). AC-5 fixture-only; blendtutor.lua default emission out-of-scope (defer to AC-9 docs or follow-up).