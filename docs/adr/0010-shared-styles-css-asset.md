# ADR-0010: Shared `styles.css` asset — design-token system and semantic layout shell

- Status: Accepted
- Date: 2026-06-25

## Context

Issue #56 (AC-1 of the site design v1 plan) introduces a principled design-token
system and restructures both target shells (webR and Pyodide) into semantic
regions (header/main/footer). Before this change, each shell carried its own
inline `<style>` block with hardcoded values — duplicated across two nearly-
identical files with no shared token vocabulary, and no single source of truth
for the cross-target visual vocabulary.

The site build pipeline (`crates/core/src/site/mod.rs`) already embeds shared
JS assets (`lesson-runner-core.js`, `coi-serviceworker.js`, `feedback.js`)
at compile time via `include_str!`. A fourth shared asset is the natural
extension: a `styles.css` that both target shells reference via
`<link rel="stylesheet" href="styles.css">`.

## Options

### Option 1: Shared `styles.css` asset in `include_str!` pipeline

Insert `styles.css` as a new file at `crates/core/assets/shared/styles.css`,
add a `STYLES_CSS` const in `mod.rs` alongside the existing shared JS consts,
and reference it from `assemble()` at position 6 (after `feedback.js`, before
per-lesson JSONs). Both shells replace their inline `<style>` with
`<link rel="stylesheet" href="styles.css">`.

**Pro:** Single source of truth for tokens and shell layout; both targets
consume the same bytes (byte-identical, test-asserted); changing a token value
touches one `:root` declaration and both targets pick it up; leverages the
existing `include_str!` mechanism without new infrastructure; no runtime asset
loading or CDN dependency.

**Con:** Adds a fourth shared file in the seam; the `<link>` pattern introduces
a second HTTP request at runtime (vs inline `<style>` which ships with the
HTML). However, CSS is cacheable by the browser and the additional request is
negligible for a learning-tool static site where the JS runtime (webR/Pyodide)
dominates load time.

### Option 2: Keep inline `<style>` duplicated per target

Each target shell continues carrying its own inline `<style>` block.
Tokens are duplicated across both shells, or a shared set of `--bt-`
declarations is copy-pasted.

**Pro:** No additional HTTP request. No new file in the pipeline.

**Con:** Two sources of truth that can drift; no single place to change a
token value; no way to add future CSS features (dark mode, print stylesheets,
responsive breakpoints) once without editing two shells; the `include_str!`
pipeline already supports shared assets, so the incremental cost of adding
one more is near-zero.

### Option 3: External CDN-hosted stylesheet

Host `styles.css` on a CDN and reference it with an absolute URL.

**Con:** Requires a separate deployment pipeline, introduces a network
dependency and a potential point of failure (CDN down = unstyled site),
contradicts ADR-0008's "no runtime asset loading" principle, and adds
complexity with no benefit for a static site that already embeds all its
other assets locally.

## Decision

**Option 1: Shared `styles.css` asset in `include_str!` pipeline.**

The existing pipeline already embeds three shared JS assets this way; adding a
fourth for CSS follows the same pattern (§4.2). The single-source-of-truth
argument dominates for a foundation that AC-2 (workspace styling) and AC-3
(BYOK feedback panel styling) will extend — those slices add their own tokens
and region styles on top of the foundation, and a shared CSS file lets them
import and extend without forking.

The additional HTTP request is acceptable: the stylesheet is small (~3 KB
gzipped), cacheable, and pales next to the JS runtime download.

## Consequences

- `styles.css` is embedded at compile time, not loaded at runtime — the
  `include_str!` const is the single source of truth.
- Both shells replace inline `<style>` with `<link>`; the `<style>`-absent
  invariant is test-asserted so a regression that reintroduces inline styles
  is caught.
- Changing a token value (`--bt-color-brand`, `--bt-space-page-width`) touches
  one `:root` declaration and both targets consume the new value without any
  Rust recompilation (CSS asset recompilation on change, yes; Rust code
  unchanged).
- Future dark mode handling: a `prefers-color-scheme: dark` media query in
  `styles.css` updates both targets — no shell edit needed.
- AC-2 and AC-3 own their region styles (workspace internals, feedback panel);
  they add their own CSS rules in the same file or their own asset files.
- The `assemble()` output order places `styles.css` at position 6 (after
  feedback.js, before lesson JSONs), preserving the "code precedes data"
  invariant.
