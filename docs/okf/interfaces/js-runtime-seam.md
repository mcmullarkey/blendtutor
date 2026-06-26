---
type: Interface
title: JS runtime seam (browser-side contract)
description: window.__bt / start(runtime) / FeedbackBackend — the browser-side contract spanning assets/shared/*.js.
resource: crates/core/assets/shared/lesson-runner-core.js
tags: [javascript, browser, contract, byok, webr, pyodide]
timestamp: 2026-06-25
pure: false
---

# Responsibility

The browser-side runtime contract for built static sites. Three pieces: (1) `lesson-runner-core.js` loads lessons, renders UI, wires controls; (2) each target's `lesson-runner.js` adapts it to webR or Pyodide; (3) `feedback.js` does BYOK LLM feedback. This is the client-side mirror of the Rust [site](/modules/site.md) assembly.

# Interface

**`window.__bt` test handle** (`lesson-runner-core.js`):
- `lessons` — array of lesson metadata
- `current` — currently selected lesson
- `selectLesson(index)` — switch lesson
- `runSubmission(code) -> "pass" | "fail"` — run checks
- `getSubmission() -> string` — read the current editor doc (CM6 or fallback textarea)
- `editorView` — the CodeMirror 6 `EditorView` singleton (null under graceful degradation)
- `ready` — boot promise

**`start(runtime)`** (`lesson-runner-core.js`) — exported, called by each target's `lesson-runner.js` with:
```js
{ name, language, boot(), run(code, checks) -> { output, ok } }
```
`language` is `"r" | "python"` — selects the CodeMirror 6 language extension loaded
into the editor (closed set, looked up in a `LANG_EXT` map, NOT a string match on
`name`). The editor is a singleton created once in `start()` and mounted in
`#submission` (a `<div>`, not a textarea); `renderLesson` dispatches a doc replace
(never destroys/recreates), so there is no editor leak across lesson switches.

**`FeedbackBackend`** (`feedback.js:284-303, 362-380`):
```js
{ name, getFeedback(prompt, model) -> Promise<{ correct, message }> }
```
Implementations: `byokAnthropic`, `byokFireworks`.

**Prompt constants** — byte-identical to Rust [llm-prompt](/modules/llm-prompt.md): `OPEN_CODE`, `CLOSE_CODE`, `OUTPUT_LABEL`, `CHECKS_LABEL`, `NEUTRALIZED`.

# Dependencies

- Assembled into the site by [site-build-pipeline](/interfaces/site-build-pipeline.md) via `include_str!`.
- Consumed by rodney browser probes (test hooks: `data-test="lesson-select|submission|run|lesson-status|output|feedback"`, `data-status="idle|running|pass|fail"`, `data-byok="key-prompt|provider|model-picker|model|verdict|pending|error"`, `#byok-disclosure`).
- BYOK flow calls Anthropic/Fireworks directly from the browser (no Rust intermediary).

# Invariants

- Lesson `<option>`s built via DOM `Option` API, never parsed as HTML — untrusted titles can't inject HTML.
- The submission mount is a `<div id="submission">` hosting a CodeMirror 6 editor (not a textarea). `feedback.js` reads the doc via `window.__bt.getSubmission()`, never `.value` on the element (the div has none).
- The `EditorView` is a singleton: `renderLesson` dispatches a full-doc replace, never destroys/recreates the editor — no editor leak across lesson switches.
- `code_template` is untrusted lesson content; it is set via `EditorView.dispatch` (or `textarea.value` under degradation), never via `innerHTML`.
- Graceful degradation: if `new EditorView(...)` throws, a fallback `<textarea>` is mounted in `#submission`; `getSubmission()` and `setEditorContent()` read/write it transparently.
- API keys stored in `sessionStorage` (tab-scoped): `fireworks_api_key` / `anthropic_api_key` / `byok_provider`.
- `providerBaseUrl` host-gated: only `localhost`/`127.0.0.1` override, no embedded credentials.
- `handleSubmit` four-phase: no key → key prompt; key + no picker → model picker; picker shown → send through `PROVIDERS[id].factory`.
- Two backend shapes: Anthropic (`x-api-key`, `anthropic-dangerous-direct-browser-access`, `/v1/messages`, tool `input_schema`) vs Fireworks (Bearer, `/chat/completions`, OpenAI tool `parameters`, `JSON.parse(function.arguments)`).

# Pure/Effectful

- Pure (JS): `neutralize`, `buildPrompt`, `parseModels`, `modelRoster`, `feedbackRequest`, `fireworksRequest`, `toVerdict`, `fireworksToVerdict`.
- Effectful (JS): `listModels`, `byok*.getFeedback`, `handleSubmit`, `renderModelPicker`, webR/Pyodide boot.

# Citations

- `crates/core/assets/shared/lesson-runner-core.js`
- `crates/core/assets/shared/feedback.js`
- `crates/core/assets/shared/coi-serviceworker.js` (vendored, COOP/COEP shim for SharedArrayBuffer)
- ADR-0008 (static site), ADR-0009 (dynamic model discovery, browser BYOK)
