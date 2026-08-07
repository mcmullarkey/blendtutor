---
ac: 4
depends_on: 1, 3
risk: medium
status: in-progress
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
- [ ] Red: tests extended (test_quarto_feedback.py source + Node, key-page-url.test.js, feedback-probe.js) → confirm FAIL against current inline form
- [ ] Green: renderKeyPrompt → no-key link + keyPageUrl() export
- [ ] Demo-book mirror sync + cmp byte-identical
- [ ] Evidence at docs/evidence/165/
- [ ] PR + push

### Decision Log
- (pending)

### Surprises & Discoveries
- (placeholder — remove once real entries exist)

### Idempotence & Recovery
- Safe retry: re-run `python3 scripts/tests/test_quarto_feedback.py` after any edit.
- Rollback: git revert the PR; re-sync demo-book extension dir manually.
