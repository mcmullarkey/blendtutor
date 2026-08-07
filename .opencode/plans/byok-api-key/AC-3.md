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
- [x] Implementation committed — 2026-08-06 (PR #176)
  - Red: fixtures + extended test scripts → confirmed 10 FAILs on unimplemented clauses
  - Green: blendtutor.lua (has_key, dual-asset deploy, bootstrap .then, bt-feedback, __btConfig, broadened guards) → 71/71 bootstrap + 68/68 asset
  - Demo-book vendored sync: blendtutor.lua + key-page.js + exercise-feedback.js, cmp byte-identical

### Decision Log
- 2026-08-06 — keyPageUrl emitted via separate head include_text (not bootstrap-internal); __btConfig merge pattern (no clobber); has_key guards broadened to has_blendtutor or has_key (key-only page); demo-book sync = manual cp + cmp done-condition (sync script gap discovered).
- 2026-08-06 — `bt-feedback` / `bt-auto-bootstrap` string "false" handling requires meta_string() normalization: pandoc 3 (quarto 1.10.18) wraps quoted YAML strings in a structured Inlines table, NOT a plain Lua string — the pre-existing `type(x)=="string"` branch silently never matched. Same normalization applied to bt-key-page value read (else custom keyPageUrl silently ignored).
- 2026-08-06 — test-script SIGPIPE bug fixed: `printf '%s' "$content" | grep -qF` races (grep -q exits on early match → printf broken pipe → pipefail → has_token falsely fails). Replaced with here-string `grep -qF <<< "$content"` in BOTH extended scripts. Pre-existing latent bug; larger blendtutor.lua made it flaky.

### Surprises & Discoveries
- pandoc 3 meta strings are structured (Inlines list with Str "false"), not plain Lua strings — the spec's "string false parity" only works via a normalizer helper; verified empirically with a probe filter.
- `set -uo pipefail` + command-substitution greps in test scripts are lethal during the red phase: a legitimately-absent token (grep exit 1) aborts the whole script before later clauses run. All new greps need `|| true`.
- Running the two extended shell scripts CONCURRENTLY corrupts renders (both render the same quarto-fixture pages; one's rm -f/delete races the other's read). Must run sequentially.
- The `resources = {` static-pin check matches via `local resources = {` (the table initialization), not the add_html_dependency table (which uses `resources = resources`) — passed by coincidence of naming; harmless but noted.

### Idempotence & Recovery
- Safe retry: re-run the three shell test scripts after any interrupted edit.
- Rollback: git revert the PR; re-sync demo-book extension dir manually.