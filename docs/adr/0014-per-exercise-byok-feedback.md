# ADR-0014: Per-exercise BYOK feedback with shared sessionStorage

- Status: Accepted
- Date: 2026-07-23

## Context

The Quarto extension (AC-7) needs in-browser LLM feedback. The existing
`feedback.js` (crates/core/assets/shared/feedback.js) was built for the
single-exercise static site: it reads from a singleton `#feedback` container,
`window.__bt.getSubmission()`, and wires a single submit button at module
load. AC-4 killed the `window.__bt` singleton in favor of a per-exercise
registry (`window.__btExercises`), so the feedback module must be forked to
mount per-exercise while preserving the proven pure layer (prompt structure,
provider map, key handling, model discovery — all pinned by ADR-0006/0009).

The key question: how to share the BYOK key across exercises (entered once,
reused) while scoping the feedback UI per-exercise (no cross-exercise bleed),
without re-introducing a singleton or module-level effectful code.

## Options

1. **Singleton feedback container + per-exercise submit buttons.** Keep one
   `#feedback` container, route submits to it by exercise id. Simplest port,
   but two exercises overwrite each other's verdict/pending/error UI — the
   container is shared mutable state. Re-introduces the exact singleton the
   AC-4 fork killed, just at a different layer.

2. **Per-exercise feedback container + shared sessionStorage key.** Each
   `div.bt-exercise` gets its own feedback button + container (created by
   `mountFeedback(entry)`). The key lives in provider-scoped sessionStorage
   slots (`fireworks_api_key`, `anthropic_api_key`) — NOT exercise-scoped — so
   the key entered for exercise 1 is read by exercise 2 without re-prompting.
   The pure layer is ported unchanged and exported so it is testable in
   Node.js without a browser. No module-level effectful code: `mountAllFeedback`
   is called by the page after the runtime boots.

3. **Per-exercise key (re-prompt per exercise).** Each exercise stores its key
   in an exercise-scoped slot. Maximally isolated, but the learner re-enters
   the key for every exercise — a hostile UX that the spec's "key entered once"
   clause explicitly rejects.

## Decision

Option 2. Per-exercise containers kill cross-exercise bleed at the UI layer;
shared provider-scoped sessionStorage slots satisfy "key entered once, reused."
The pure layer (neutralize, buildPrompt, parseModels, modelRoster,
feedbackRequest, toVerdict, fireworksRequest, fireworksToVerdict,
providerBaseUrl) is ported byte-identical from feedback.js and exported — no
behavioral drift from the pinned prompt structure (ADR-0006). The effectful
shell is rewritten: `currentSubmissionForExercise(entry)` reads from
`entry.getSubmission()` (the AC-4 registry), `handleSubmitForExercise(entry)`
renders into `entry.feedbackContainer` (per-exercise), and
`mountAllFeedback(registry)` is the single entry point the page calls after
`start(registry, adapter)`.

- **No module-level effectful code (§2.1).** The old `applyEmbeddedKey()` bare
  call and submit-button wiring moved into `mountAllFeedback`. Pure functions
  are importable in Node.js without side effects — testable without a browser.
- **Per-exercise concurrent guard (§5.3).** `entry._feedbackRunning` prevents
  overlapping feedback requests on the SAME exercise. Different exercises have
  independent guards and can fetch concurrently.
- **`?provider=` override preserved.** `providerBaseUrl` honors a localhost-only
  override (test seam) and rejects non-local/credentialed overrides (key-exfil
  defense), unchanged from feedback.js.

## Consequences

- `exercise-feedback.js` owns the feedback lifecycle (§4.1): prompt build, key
  handling, provider routing, verdict render. It does NOT execute code or
  render the editor — those stay in exercise-runtime.js (§3.4).
- The pure layer is shared verbatim with feedback.js. If the prompt structure
  changes (new delimiter, new neutralization), both files must update — the
  integration test pins the constants, but the duplication is a maintenance
  cost. A future refactor could extract the pure layer into a shared module
  imported by both, but that is out of scope for AC-7 (the extension assets are
  vendored, not shared at runtime — ADR-0010).
- `mountAllFeedback` must be called AFTER `start(registry, adapter)` resolves,
  because it reads `entry.element` (populated by mountEditor) and
  `entry.getSubmission` (wired by wireExercise). Calling it before boot would
  mount feedback UI on exercises whose editors aren't ready.
- The `feedback.qmd` fixture demonstrates the wiring: `start(registry,
  adapter).then(() => mountAllFeedback(registry))`.
