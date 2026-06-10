# ADR-0009: Dynamic model discovery for browser BYOK feedback

- Status: Accepted
- Date: 2026-06-09

## Context

The built static site's in-browser feedback (`feedback.js`, the `byok-anthropic`
backend — Slice 18, a consequence of ADR-0006) hardcodes the Anthropic model
`claude-opus-4-8`: the request builder closes over a module constant and there is
no picker. A learner bringing their own key cannot choose a model, and the list
they *could* use is invisible. Issue #46 (Slice 1 of the multi-provider plan)
wants the learner to pick from the Anthropic models their key can actually reach,
fetched live, with their choice driving the request — degrading gracefully when
the list can't be fetched. This must preserve the existing security posture: the
key is sent only to a host that passes the `providerBaseUrl()` localhost-only,
credential-rejecting gate (ADR-0006's browser consequence), and no model list or
key is ever baked into a shipped file.

## Options

1. **Static roster baked at build time.** Ship a hardcoded list of known Anthropic
   model ids in `feedback.js`. Offline, one fewer request — but it drifts the
   moment Anthropic adds or retires a model, cannot reflect what *this* key is
   entitled to, and re-introduces exactly the magic-list smell the single
   hardcoded model already is.
2. **Live `/v1/models` query with the learner's key; pure parse + pure fallback.**
   Query `/v1/models` (same `x-api-key` + `anthropic-dangerous-direct-browser-access`
   headers and the same host-gated base URL as messages), parse with a pure
   `parseModels(json) -> string[]`, render a `<select>`, default to the named
   fallback model. On any failure (non-OK, network, malformed, empty) fall back to
   a roster containing that default so the picker is never a zero-option dead end
   and submit still works. The model becomes an explicit argument of the request
   builder, not a captured constant.

## Decision

Option 2. Introduce a model-source seam: a pure `parseModels(json)` extracting ids,
an effectful `listModels({baseUrl, apiKey})` that fetches `/v1/models` through the
shared `providerBaseUrl()` gate and returns `[]` on any failure, and a pure
`modelRoster(models)` applying the named-fallback policy
(`models.length ? models : [MODEL]`). `renderModelPicker` renders the roster into a
`<select data-byok="model">` scoped to `#feedback`, defaulting to the fallback
`MODEL` when present. `feedbackRequest` and `byokAnthropic.getFeedback` take the
chosen model as an explicit argument; the constant `MODEL` is demoted from "the
model" to "the fallback/default". Submit becomes two-phase: key present and no
picker yet → the first submit renders the picker (one `/v1/models` call); picker
present → submit reads `selectedModel(container)` and sends with that id.

Relates to ADR-0006: that ADR owns the CLI `run`/`eval` provider boundary via rig;
this one owns the *browser* BYOK model-discovery seam. They share the contract
shape (`respond_with_feedback` → Verdict) but not the transport — the browser path
has no rig and no server, and the host-gate is its key-exfil defense.

## Consequences

- The list reflects the learner's actual key entitlements and never drifts with
  Anthropic's catalog; no roster is maintained in source.
- `parseModels`/`modelRoster` are pure and behavior-tested (§2.2 — they take values,
  not connections); `listModels` is the thin effectful shell that owns the I/O
  (§2.1). The model is a named, threaded value, not a magic constant the request
  builder closes over.
- The models query inherits the localhost-only, credential-rejecting host-gate for
  free (§3.4): a non-local `?provider=` override is ignored for `/v1/models` exactly
  as for `/v1/messages`, so the picker opens no new key-exfil vector.
- A models outage is a non-event — the picker falls to the named default and stays
  submittable (§3.5: the seam covers both the resolved-list and query-failure
  exits). No dead end.
- Two-phase submit (pick, then submit) is a small UX cost: the first submit surfaces
  the picker rather than feedback. Acceptable for the slice; a future slice may
  render the picker eagerly on key entry.
- Sets up the provider-agnostic picker seam Slices 2–3 (Fireworks provider +
  serverless filtering) plug into.
