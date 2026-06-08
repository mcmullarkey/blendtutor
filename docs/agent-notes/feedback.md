---
topic: feedback
created: 2026-06-08
slices: [18]
---

In-browser LLM feedback in a built site (`assets/shared/feedback.js`). A learner
brings their own key and gets written feedback on a submission. See [[site-build]]
for how the asset is assembled and [[llm]] for the author-side Rust prompt/Verdict
contract this mirrors.

- 2026-06-08 (#18): **The `FeedbackBackend` seam (the noteworthy decision — no
  full ADR per the issue, a consequence of ADR-0004/0005/0006/0008).** Feedback is
  one JS contract: `getFeedback(prompt) -> Promise<Verdict>`, where `Verdict` is
  `{ correct: boolean, message: string }` — a faithful JS projection of the Rust
  `core::llm::Verdict` sum (`Correct{message}/Incorrect{message}`), so author-side
  evals and learner-side feedback agree on shape (§1.2/§3.2). v1 ships exactly one
  impl, `byok-anthropic`. **This is the seam WebLLM (Slice 21) plugs into** with no
  change to lesson rendering or execution (§3.3/§3.4) — a second backend is a second
  object with the same one method, selected where `byokAnthropic(...)` is
  constructed. Key handling (get/set, header injection) is isolated inside the BYOK
  backend; the render/exec layers never see a key (§4.1/§4.2).
- 2026-06-08 (#18): **feedback.js is a *shared* asset, not per-target.** The plan
  said `assets/*/feedback.js`, but that predates #17's `assets/shared/` refactor.
  The Anthropic BYOK call is identical whether the lesson runs in webR or Pyodide,
  so it is assembled once in `core::site::assemble` (a `FEEDBACK_JS` `include_str!`)
  and loaded by both shells — byte-identical, asserted by both the unit test
  (`plan_site_assembles_the_shared_byok_feedback_backend`) and the cli twin
  (`build_pyodide_ships_the_same_shared_feedback_seam`). A per-target fork would be
  the §4.2 smell the shared-core/shim pattern already rejected.
- 2026-06-08 (#18): **The JS prompt mirrors the Slice-10 Rust delimiters as the
  single source of structure.** `feedback.js`'s `OPEN_CODE`/`CLOSE_CODE`/
  `OUTPUT_LABEL`/`CHECKS_LABEL`/`NEUTRALIZED` are byte-identical to the
  `core::llm::prompt` consts, and the cli integration test pins the JS against the
  *exported* Rust constants (`feedback.contains(OPEN_CODE)` etc.) — not a hand-copied
  twin — so the learner-side prompt can't drift from the author-side one (§1.5). The
  student submission is untrusted input bound for an LLM, so `neutralize` rewrites
  any forged delimiter before it reaches the prompt (same injection defense as the
  Rust `build_prompt`). The browser grades holistically, so the `CHECK_RESULTS`
  section lists the lesson's check strings (neutralized), not per-check pass/fail —
  no fabricated verdicts.
- 2026-06-08 (#18): **Anthropic direct-browser call shape.** Raw `fetch` (no SDK in
  a static site) to `${provider}/v1/messages`; headers `x-api-key`,
  `anthropic-version: 2023-06-01`, and the **required** `anthropic-dangerous-direct-
  browser-access: true` (the CORS opt-in). Forces the `respond_with_feedback` tool
  via `tool_choice`; the tool input `{is_correct, feedback_message}` maps to a
  Verdict. Model is `claude-opus-4-8` (fixed, per the claude-api skill — not learner-
  configurable in v1). `?provider=<url>` is a test seam for the base URL; it only
  ever changes the *configured* host, never adds a second sink. Key lives in
  `sessionStorage` under `anthropic_api_key` (tab-scoped, gone on tab close).
- 2026-06-08 (#18): **rodney realization for AC2 (in-browser fetch spy, not a live
  9099 server).** The spec's `window.__stub.requests` is populated by installing a
  `fetch` spy via `rodney js` *before* clicking submit; the spy records
  `{url, headers, body}` and returns a canned Anthropic tool-call response. This
  proves exactly-one-request, the auth + direct-browser headers, provider-only
  origin (`http://localhost:9099/v1/messages`), the delimited student code in the
  body, and the rendered verdict — all without a network call or a real key. Two
  timing gotchas: (a) the **no-key path renders synchronously** within `click()`
  (no `await` before `renderKeyPrompt`), so AC1 can assert the key prompt in the
  same `rodney js` call; the **verdict path is async**, so read it in a *separate*
  `rodney js` call (the inter-call round-trip flushes the awaited fetch+render).
  (b) rodney's `js` evaluates an **expression**, not a statement list — wrap any
  multi-statement probe in an IIFE (`(() => { …; return …; })()`) or it
  `SyntaxError`s on the first `;`. As in #16/#17, neuter the served
  `out/coi-serviceworker.js` (no-op) so the reload-to-isolate loop doesn't reset
  page state mid-probe; the shipped artifact keeps the real shim.
- 2026-06-08 (#18): **No `sk-ant` literal anywhere in `feedback.js`.** The key-entry
  `<input>` placeholder is plain text ("Paste your Anthropic API key"), deliberately
  *not* an `sk-ant-…` example — an example string would trip the AC1/AC2 negative
  (`rg -l 'sk-ant' crates/core/assets out`) that guards against a key baked into a
  shipped file.
