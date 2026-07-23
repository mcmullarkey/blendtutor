# ADR-0015: Opt-in cross-origin isolation via coi attribute and YAML metadata

- Status: Accepted
- Date: 2026-07-23

## Context

webR and Pyodide need `SharedArrayBuffer` for multi-threaded WASM, which requires
cross-origin isolation (COOP/COEP headers). GitHub Pages cannot set response headers,
so the vendored `coi-serviceworker.js` (ADR-0008) re-serves pages with those headers
via a service worker.

The filter must decide *whether* to activate COI on a given page. Not every page
needs it — only pages with webR/Pyodide exercises do. Always injecting the service
worker would register it on every page, causing unnecessary reloads and service
worker overhead on pages that never run WASM.

## Options

1. **Always inject coi-serviceworker.js on every HTML page.** Simplest. But
   registers a service worker on pages that never need `SharedArrayBuffer`,
   causing a reload flash and persistent service worker overhead. Violates §5
   (one activation path per page — here it's every page, not opt-in).

2. **Opt-in via div attribute `coi="true"` only.** User adds `coi="true"` to any
   div on the page. Filter detects it and injects the script tag. Clean
   per-page opt-in. But requires a visible div in the document body — cannot
   activate COI on a page with no content divs.

3. **Opt-in via BOTH div attribute `coi="true"` AND document-level YAML
   `coi: true`.** Div attribute for inline opt-in (co-located with exercise
   content). YAML metadata for document-level opt-in (no div needed — works on
   any page, including empty pages). Filter checks both: `has_coi` flag set in
   `Div()` for div attribute, checked again in `Pandoc()` for YAML metadata.

## Decision

Option 3 — opt-in via both div attribute and YAML metadata.

- **Div attribute `coi="true"`** (string enum, §1): detected on ANY div, not just
  `.blendtutor` divs. COI is a page-level concern, not an exercise-level concern
  (§3 — COI activation separate from exercise runtime). Only the exact string
  `"true"` activates; `"false"`, `"yes"`, `""` are rejected.

- **YAML metadata `coi: true`** (boolean, §1): checked in `Pandoc()` via
  `doc.meta["coi"]`. Activates COI for the entire document without requiring a
  div in the body.

- **Dedup guard** (§5): `hasCoiDone` flag prevents duplicate script tag injection
  when multiple `coi="true"` divs exist on the same page. One activation path
  per page.

- **Per-page isolation** (§3): `has_coi` and `hasCoiDone` are reset in `Pandoc()`
  so each document in a multi-page render (Quarto book) gets a fresh check.

- **Script tag path**: `_extensions/blendtutor/assets/coi-serviceworker.js` —
  vendored via `sync-quarto-assets.sh` (mode=copy, byte-identical to source).

## Consequences

- Users must explicitly opt in — no surprise service worker registration.
- Two activation paths (div + YAML) — both set the same `has_coi` flag, so the
  injection logic is unified (one path in `Pandoc()`).
- The service worker scope is determined by the script's URL at runtime; this
  ADR addresses only the filter's injection decision, not the browser's scope
  resolution (runtime concern, tested via rodney in AC-5).
- `sync-quarto-assets.sh` gains a new `coi-serviceworker.js` entry (mode=copy).
