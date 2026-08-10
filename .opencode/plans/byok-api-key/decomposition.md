# Decomposition: byok-api-key

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

### Existing state (prior art — do NOT rebuild)

`_extensions/blendtutor/assets/exercise-feedback.js` already implements most of
this: `byokFireworks` backend (Bearer, `/chat/completions`, tool-call verdict),
`listModels` (cheap validation call), prompt constants byte-identical to Rust
core, per-exercise containers, rate limiting, `?provider=` localhost stub
override for rodney, textContent-only verdict rendering. Current gaps:
(1) key storage is `sessionStorage` (tab-scoped) not `localStorage`;
(2) no dedicated key page — key prompt is inline per-exercise;
(3) feedback is manual opt-in (`mountAllFeedback` hand-wired in
`quarto-fixture/feedback.qmd`); the filter's `add_html_dependency` does NOT
deploy `exercise-feedback.js`; a prior plan decision
(`.opencode/plans/filter-runtime-bootstrap.md`) explicitly kept auto-mount OUT
— this feature reverses that decision;
(4) provider chooser offers Anthropic (out of scope: Fireworks only);
(5) no-key state renders the inline key prompt, not a link to a key page;
(6) demo book (`demo-book/`: index/r-exercises/python-exercises, `type: book`,
chapters in `demo-book/_quarto.yml`) has no key page.

## AC Table

| AC | Description | Dependencies | Conflict Set | Risk | Medium |
|----|-------------|--------------|--------------|------|--------|
| 1  | Switch Fireworks key persistence from sessionStorage to localStorage in `exercise-feedback.js` (readKey/storeKey for `fireworks_api_key`, add `clearKey()`), and migrate the `bt_feedback_count` rate-limit counter consistently; update `scripts/tests/test_quarto_feedback.py` Node/static tests that pin sessionStorage. | none | `_extensions/blendtutor/assets/exercise-feedback.js`, `scripts/tests/test_quarto_feedback.py` | medium | code |
| 2  | Add `assets/key-page.js`: mountable key-management UI module (password input, Save, Clear, status line, validation via `GET {baseUrl}/models` against host-gated `providerBaseUrl("fireworks")`, friendly invalid-key/network errors, textContent-only rendering, key never logged or echoed into DOM). Export pure helpers for Node tests. | AC-1 | `_extensions/blendtutor/assets/key-page.js` (NEW), `scripts/tests/test_quarto_key_page.py` (NEW) | medium | code |
| 3  | Extend `blendtutor.lua` with a `::: {.blendtutor-key}` div → key-page mount point, deploy `exercise-feedback.js` + `key-page.js` via `add_html_dependency`, and auto-mount feedback in the injected bootstrap (`mountAllFeedback` after `start()`, opt-out via existing `bt-auto-bootstrap: false` or new `bt-feedback: false` YAML key). | AC-2 | `_extensions/blendtutor/blendtutor.lua`, `scripts/tests/test_quarto_bootstrap.sh`, `scripts/tests/test_quarto_asset_deployment.sh` | high | code (render asserts) |
| 4  | Replace the no-key inline prompt in `handleSubmitForExercise` with a "Enter your API key first" state linking to the key page; link target read from `window.__btConfig.keyPageUrl` (set by the AC-3 bootstrap from a `bt-key-page` YAML meta value, default `api-key.html`). | AC-1, AC-3 | `_extensions/blendtutor/assets/exercise-feedback.js`, `scripts/tests/test_quarto_feedback.py`, `_extensions/blendtutor/blendtutor.lua` (small) | medium | rodney |
| 5  | Wire LLM feedback into the Run flow: after a Run completes, the exercise's feedback button/container reflects check output in the prompt (output already captured via `.bt-output`); confirm/implement whether Run auto-triggers feedback or feedback stays a separate auto-mounted button — see Open Questions Q1. | AC-4 | `_extensions/blendtutor/assets/exercise-feedback.js`, `_extensions/blendtutor/assets/exercise-runtime.js` (small), `scripts/tests/test_quarto_feedback.py` | medium | rodney |
| 6  | Scope the learner-facing UX to Fireworks only: remove the provider `<select>` from the (remaining) key prompt/picker path, hard-default `readProvider()` to `fireworks`, keep the Anthropic backend code path + `?provider=` localhost stub override intact for tests; update disclosure copy to localStorage wording. | AC-1 | `_extensions/blendtutor/assets/exercise-feedback.js`, `scripts/tests/test_quarto_feedback.py` | low | code |
| 7  | Add the demo-book key page: new `demo-book/api-key.qmd` chapter using the `.blendtutor-key` div + `bt-key-page` meta on exercise chapters, register it in `demo-book/_quarto.yml` chapters (first, before r-exercises), update `demo-book/index.qmd` BYOK section (Fireworks, link to key page, drop `ANTHROPIC_API_KEY` env advice). | AC-2, AC-3 | `demo-book/api-key.qmd` (NEW), `demo-book/_quarto.yml`, `demo-book/index.qmd` | low | code (render asserts) |
| 8  | Add rodney coverage: new `rodney-probes/key-page-probe.js` (save/clear/validate against `?provider=` localhost stub, invalid-key error) + extend `rodney-probes/feedback-probe.js` (cross-page localStorage persistence, no-key link state, verdict end-to-end through stub). | AC-4, AC-5, AC-6, AC-7 | `rodney-probes/key-page-probe.js` (NEW), `rodney-probes/feedback-probe.js` | medium | rodney |
| 9  | Update docs: README BYOK section (Fireworks-only, localStorage, key page, file:// limitation, CSP/`connect-src https://api.fireworks.ai` note), security wording (key only in Authorization header to api.fireworks.ai), and the prior "feedback stays manual opt-in" decision records. | AC-7 | `README.md`, `docs/adr/` (new or amended ADR) | low | manual |

## Dependency DAG

```
AC-1 ──→ AC-2 ──→ AC-3 ──→ AC-4 ──→ AC-5 ──→ AC-8
AC-1 ──→ AC-6 ────────────────────────────→ AC-8
AC-2 ──→ AC-7 ───────────────────────────→ AC-8
AC-3 ──→ AC-7
AC-7 ──→ AC-9
```

## Hot Conflict Files

- `_extensions/blendtutor/assets/exercise-feedback.js`: touched by AC-1, AC-4,
  AC-5, AC-6 — strictly serialize (AC-1 → AC-6 → AC-4 → AC-5 order
  recommended; never two in one batch).
- `scripts/tests/test_quarto_feedback.py`: touched by AC-1, AC-4, AC-5, AC-6 —
  serialized with the same chain (same PRs).
- `_extensions/blendtutor/blendtutor.lua`: touched by AC-3 (major) and AC-4
  (small, `window.__btConfig.keyPageUrl` emission) — serialize AC-3 → AC-4.
- `rodney-probes/feedback-probe.js`: touched by AC-8 only (AC-5 may add a
  fixture expectation in `quarto-fixture/feedback.qmd` — check at spec time).
- `demo-book/_quarto.yml`: AC-7 only.
- `demo-book/_extensions/mcmullarkey/blendtutor/` is a synced copy
  (`scripts/sync-quarto-assets.sh`, mode=copy) — never edit directly; any AC
  touching `_extensions/` assets must re-run the sync script (fold into each
  such AC's done-condition, not a separate AC).

## Suggested Batch Schedule

- Batch 1 (sequential): AC-1 (storage foundation — everything reads keys through it)
- Batch 2 (parallel): AC-2 (new file), AC-6 (exercise-feedback.js only)
- Batch 3 (sequential): AC-3 (needs AC-2's key-page.js to deploy + mount)
- Batch 4 (parallel): AC-4 (feedback no-key state), AC-7 (demo-book page — disjoint files)
- Batch 5 (sequential): AC-5 (Run-flow wiring; hot file after AC-4)
- Batch 6 (sequential): AC-8 (rodney probes over the finished runtime)
- Batch 7 (sequential): AC-9 (docs last, states final behavior)

## Open Questions

- [needs-clarification] **Q1 — Run-button semantics.** Feature text says
  "clicks the Run button and gets active LLM feedback". Current design: Run =
  deterministic checks (free); feedback = separate "Get feedback" button
  (costs the learner API tokens). Auto-firing an LLM call on EVERY Run has
  real cost implications. Options: (a) Run auto-triggers feedback after checks
  complete; (b) Run stays checks-only, feedback is an auto-mounted separate
  button (minimal change, AC-5 becomes wiring-only); (c) Run triggers feedback
  only on check failure. Recommend (b) or (c); AC-5's scope depends on the
  answer.
- [needs-clarification] **Q2 — Anthropic code path.** Remove the Anthropic
  backend + key slot from `exercise-feedback.js` entirely, or keep the code
  and hide it from learner UX (AC-6 assumes keep-code/hide-UI)? Full removal
  shrinks the attack surface but deletes a tested path shared conceptually with
  `crates/core/assets/shared/feedback.js`.
- [needs-clarification] **Q3 — Key-page URL mechanism.** Lesson pages need the
  key page's URL for the no-key link. Proposed: `bt-key-page: <url>` YAML meta
  → bootstrap sets `window.__btConfig.keyPageUrl`, defaulting to
  `api-key.html`. Acceptable, or prefer a fixed convention / `?keypage=`
  override?
- [needs-clarification] **Q4 — Model picker on key page / lesson pages.**
  Existing flow shows a live model picker (fetched via `/models`) after key
  entry. Keep the picker (with the roster fallback), or pin the default model
  (`accounts/fireworks/models/deepseek-v4-flash`) for a simpler UX?
- [needs-clarification] **Q5 — Fireworks browser CORS, live-verified?** All
  existing rodney coverage routes through the `?provider=` localhost stub. That
  `https://api.fireworks.ai/inference/v1` accepts CORS requests from an
  arbitrary GitHub Pages origin appears unverified in-repo — needs a one-time
  live manual check (part of AC-9 or a pre-flight task) before relying on it in
  the deployed demo book.
- [needs-clarification] **Q6 — sessionStorage→localStorage migration.** A
  learner with a key already in sessionStorage (old build) — silent one-time
  migration on read, or clean break (key page requires re-entry)? Clean break
  recommended (demo-stage project, avoids dual-source reads).

## Notes for spec phase

- Verification mediums: `code` = Node-importable pure tests +
  `scripts/tests/test_quarto_*.py|sh` render/DOM asserts (server-rendered HTML
  greppable); `rodney` = client-side JS state requiring the browser probe
  harness (`rodney-probes/`, `.rodney/`, `scripts/rodney-chrome.sh`); `manual`
  = human judgment.
- The `?provider=` localhost stub override in `providerBaseUrl` is the
  sanctioned test seam for AC-8 — do NOT remove it when scoping to
  Fireworks-only (AC-6).
- COI book-mode limitation (README/ADR-0015) is orthogonal but adjacent: the
  key page needs no COI; do not set `coi: true` on `api-key.qmd`.
- `file://` limitation (ES modules blocked) applies to the key page too —
  document in AC-9 alongside the existing README note.
