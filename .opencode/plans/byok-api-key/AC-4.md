---
ac: 4
depends_on: 1, 3
risk: medium
status: complete
---

**Predicate:** Six arms, all MUST hold:
1. **Link renders, inline form gone** — `renderKeyPrompt` emits container `[data-byok="no-key"]` containing exactly one `<a>` with `textContent === "Enter your API key first"` and `href` resolved from `keyPageUrl()`; NO `input[name="provider-key"]`, NO Save/submit button, NO `[data-byok="provider"]` select remain.
2. **Lazy read at render time** — `const href = keyPageUrl()` INSIDE render body; eval-set config after module load → href updated (eager module-init cache FAILS).
3. **Default fallback** — `window.__btConfig` undefined OR `keyPageUrl` absent/empty → `href === "api-key.html"`.
4. **Existing key suppresses no-key state** — `if (!apiKey) { renderKeyPrompt(container); return; }` guard stays FIRST in handleSubmitForExercise.
5. **DOM-built, `_blank`, no key echo** — `createElement` + `.href`/`.textContent` (no `innerHTML`); `target === "_blank"` + `rel` includes `noopener`; no key value in rendered DOM.
6. **Scheme rejection** — `keyPageUrl()` rejects `javascript:`/`data:` (case-insensitive, whitespace-trimmed) → fallback `"api-key.html"`.
7. **Pure exported `keyPageUrl()`** — Node-importable; absent window → default; set → verbatim post scheme-check.

**Probe:**
```
python3 scripts/tests/test_quarto_feedback.py          # source-pattern + embedded Node render/keyPageUrl tests
uv run node --test scripts/tests/key-page-url.test.js  # standalone pure keyPageUrl() unit test
```
Rodney: AC-4 owns feedback-probe.js inline-form clause fix (clauses 1/3/4/8 → localStorage.setItem injection seam). E2E no-key-flow clauses are AC-8's.

**Negative:** eager module-init read; `javascript:` href emitted unfiltered; hardcoded href with no `__btConfig` read; link rendered when key present; old inline form in DOM alongside link; missing `_blank` → same-tab nav loses in-progress CM6 code.

**Verification:** code (python source-pattern + Node pure fn + Node render) + rodney (AC-4 probe fix; AC-8 owns new no-key-flow clauses).

**Fixture status:** `scripts/tests/test_quarto_feedback.py` EDIT (replace AC-6 C1/C5/C6 Node blocks with AC-4 no-key-link asserts + keyPageUrl pure tests + source check); NEW `scripts/tests/key-page-url.test.js`; `rodney-probes/feedback-probe.js` EDIT (clauses 1/3/4/8 — inline-form asserts → localStorage injection seam + no-key-link asserts).

**Rubric anchor:** §2.1 (no module-level effectful — lazy read pinned), §2 (pure `keyPageUrl()` core vs effectful render shell), §5 (one-thing render fn).

### Progress
- [x] Red: tests extended (test_quarto_feedback.py source + Node, key-page-url.test.js, feedback-probe.js) → 5 FAILs against inline form (keyPageUrl not a function, missing export, missing lazy read, Save button still present) — 2026-08-07
- [x] Green: renderKeyPrompt → [data-byok="no-key"] single DOM-built anchor + keyPageUrl() exported pure fn — 49/49 python + 8/8 node --test — 2026-08-07
- [x] Negative control: eager module-init cache injected → arm-2 lazy-read test FAILS (2 fails); reverted, 49/49 again
- [x] Demo-book mirror sync (exercise-feedback.js + styles.css) + cmp byte-identical
- [ ] Evidence at docs/evidence/165/
- [ ] PR + push

### Decision Log
- 2026-08-07 — renderKeyPrompt KEEPS its name (spec: rename optional; AC-6 tests + guard reference it) and stays exported; body replaced with no-key link.
- 2026-08-07 — PROVIDER_DISCLOSURES const KEPT as documentation-only (C5 source check in test_quarto_feedback.py requires both localStorage disclosure strings; the no-key state no longer renders them — AC-4 spec defines container + single anchor only).
- 2026-08-07 — styles.css touched (beyond spec conflict set): [data-byok="no-key"] added to the card-container group + dead inline-form rules removed (key-prompt, provider select, input[type=password], button[type=submit], #byok-disclosure). Dead CSS was direct fallout of removing the inline form; crates/core origin styles.css untouched.
- 2026-08-07 — keyPageUrl() inlines the "api-key.html" fallback literal (no DEFAULT const) so the spec's region source-check (keyPageUrl export → renderKeyPrompt body contains api-key.html) passes.
- 2026-08-07 — standalone keyPageUrl unit test at scripts/tests/key-page-url.test.js (repo has no top-level assets/; spec probe path generic). CJS + dynamic import matches rodney-probes/*.test.js convention.

### Surprises & Discoveries
- The AC-6 C6 behavioral test (submit stores fireworks key) died with the inline form: there is no submit path anymore. It was replaced by no-form/no-submit render assertions — the "store on submit" behavior moved to key-page.js (AC-2) and is pinned there.
- styles.css had a whole dead section for the removed inline form (key-prompt/provider/password/submit/disclosure) — no test pinned it (test_quarto_ux.py only checks cursor/disabled/data-status/hints/solution), so cleanup was safe; demo-book mirror needed a manual cp + cmp (same sync gap as AC-3 — sync-quarto-assets.sh doesn't cover extension assets).

### Idempotence & Recovery
- Safe retry: re-run `python3 scripts/tests/test_quarto_feedback.py` after any edit.
- Rollback: git revert the PR; re-sync demo-book extension dir manually.
