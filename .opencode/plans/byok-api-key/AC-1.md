---
ac: 1
depends_on: none
risk: medium
status: complete
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
- [x] implementation — red test (separate-Map mock + P1-P7 asserts) 2026-08-06 72615e6
- [x] implementation — module migration + clearKey + mirror sync 2026-08-06 6b9e041
- [x] E2E evidence at docs/evidence/162/ — python3 tests 35/35, cmp OK, cargo cli + build.rs green

### Decision Log
- 2026-08-06 — storage backend localStorage + zero-sessionStorage invariant: adopted B's P1-P6 spine; inverted B on byok_provider (migrate, no exemptions); clearKey resets rate-limit counter; maxFeedbackPerSession documented not renamed; readKey try/catch guard.
- 2026-08-06 — counter fns keep the BARE `localStorage` access pattern (mirroring the old bare `sessionStorage`), key fns keep `window.localStorage` — minimal diff; the separate-Map mock covers both access paths.
- 2026-08-06 — renderLimitReached copy updated ("Reload the page to reset" became false under persistent localStorage; now "Clear your saved API key to reset") — P6 honesty, not scope creep.
- 2026-08-06 — docs/agent-notes/feedback.md :52-53 and docs/okf/interfaces/js-runtime-seam.md :57 NOT rewritten: both document crates/core/assets/shared/feedback.js (the Rust-built system, still sessionStorage, pinned by crates/cli/tests/build.rs). The spec's listed :61/:144 HTML lines are Quarto build artifacts of those same crates-system notes — updating would falsify records of the untouched system.

### Surprises & Discoveries
- AC-1 (byok-api-key): The two docs the spec listed for "storage-claim updates" (docs/agent-notes/feedback.html :61, docs/okf/interfaces/js-runtime-seam.html :144) both document crates/core/assets/shared/feedback.js — the separate Rust-built system this issue must NOT touch — not the extension asset. Their sessionStorage claims remain TRUE for that system, so no doc update was needed; updating would have falsified the record. The OKF/agent-note bundles don't cover the extension fork at all.
- AC-1 (byok-api-key): crates/cli/tests/build.rs asserts the crates feedback.js storage contract verbatim ("key is read from the tab-scoped sessionStorage `anthropic_api_key` slot", line 517) — a live guard proving the two feedback systems are genuinely separate; cargo test -p blendtutor-cli passed 22/22 in tests/build.rs untouched.
- AC-1 (byok-api-key): `scripts/sync-quarto-assets.sh` covers codemirror.js/styles.css/coi-serviceworker.js only — exercise-feedback.js is a manual-copy mirror, exactly as the spec warned; hand `cp` + `cmp` verified.
- (2026-08-06) AC-1 spec's "Files touched" listed `docs/agent-notes/feedback.html :61` + `docs/okf/interfaces/js-runtime-seam.html :144` for storage-claim updates — SPEC ERROR, not builder deviation. Both docs describe the CRATES system (crates/core/assets/shared/feedback.js, pinned by build.rs:517 with sessionStorage assertions; section dated 2026-06-08 #18 predating the extension #112; okf frontmatter resource: crates/core/assets/shared/lesson-runner-core.js) — their sessionStorage claims remain TRUE and untouched. Lesson: verify a doc's subject (frontmatter resource:/section date/citations) before listing it as an edit target.
- (2026-08-06) storeKey signature KEPT (key, providerId) — spec P1 prose was sloppy; AC-2 amended accordingly.
- (2026-08-06) FLAG FOR AC-9: AC-9's spec says "EDIT docs/agent-notes/feedback.md:53" — that line documents the CRATES system (still sessionStorage, still true). AC-9 must NOT rewrite it. AC-9's correct doc scope: README BYOK section + ADR-0014 status field + okf regen (ADD extension localStorage coverage, do NOT overwrite crates invariants).

### Idempotence & Recovery
- Safe retry: re-run `python3 scripts/tests/test_quarto_feedback.py` after any interrupted edit.
- Rollback: git revert the PR; mirror re-sync via `scripts/sync-quarto-assets.sh`.
