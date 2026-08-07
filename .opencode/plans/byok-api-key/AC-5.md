---
ac: 5
depends_on: 4
risk: medium
status: complete
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
- [x] implementation — 2026-08-07 (builder B5): RED 20 fails → GREEN 62/62
- [ ] PR review — pending

### Decision Log
- 2026-08-06 — AC-5 owns picker collapse + model pin (-0731); empty-output tolerated (no gate); fetch-spy over stub server; fixture adds maxFeedbackPerSession.
- 2026-08-07 — NO new ADR: picker collapse + model pin is a behavior delta inside the existing module, not a new interface/boundary; ADR-0009 stays accurate for the unchanged crates feedback.js seam. Documented here instead.
- 2026-08-07 — listModels removed with the picker (private effectful fn, zero callers after collapse; parseModels/modelRoster kept — exported pure layer, Node-tested).
- 2026-08-07 — fixture uses C22 MERGE pattern (`window.__btConfig = window.__btConfig || {};` + property set), NOT a bare clobber — lua head script sets keyPageUrl on the same object (blendtutor.lua:691-695).
- 2026-08-07 — model = `PROVIDERS[providerId].fallbackModel` in the collapsed flow (fireworks → pinned -0731; anthropic → claude-opus-4-8).
- 2026-08-07 — test_quarto_feedback.py check_mount_per_exercise updated: literal `data-byok` tokens were only in the removed picker strings; check now follows the actual mechanism (`dataset.byok`).

### Surprises & Discoveries
- AC-5 (byok-api-key): test_quarto_ux.py's 60s subprocess timeout is a pre-existing local-machine flake — quarto render of ux.qmd takes ~56-59s on this machine on BOTH main and this branch (stash-verified), independent of this slice. CI historically passes; not a regression.
- AC-5 (byok-api-key): the AC-4 `check_no_key_link` and AC-7 `check_mount_per_exercise` source checks depended on the picker's `data-byok="model-picker"`/`data-byok="model"` literals for "markers present" — removing the picker broke check_mount_per_exercise until it was repointed at the real mechanism (dataset.byok). Picker removal silently removed the only hyphenated data-byok literals in the file.
- AC-5 (byok-api-key): the spec's pytest probe form `pytest -k "ac5 or feedback"` collects 0 items — the file is a plain script (no test_* functions); the real CI gate is ci.yml:82 `python3 scripts/tests/test_quarto_feedback.py`. Ran the direct form; noted for spec-author awareness.

### Idempotence & Recovery
- Safe retry: re-run `uv run pytest scripts/tests/test_quarto_feedback.py -k "ac5 or feedback"` after interrupted edit.
- Rollback: git revert the PR; re-sync demo-book mirror.