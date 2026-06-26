---
ac: 1
depends_on: none
risk: medium
status: complete
---

# AC-1: Design system foundation + layout shell

## Executable Spec

- **predicate:** A site built for webr + pyodide from matching fixtures passes ALL of:
  1. `styles.css` present in `SiteFiles.files()` for both targets AND byte-identical across targets
  2. Both shells contain `<link rel="stylesheet" href="styles.css">` AND `document.querySelectorAll('head style').length === 0` (zero inline `<style>`)
  3. `styles.css` declares `--bt-`-prefixed custom properties in `:root` AND ≥4 rules reference `var(--bt-` (proves tokens USED, not declared-dead)
  4. Both shells contain exactly one `<header class="site-header">`, one `<main class="workspace">`, one `<footer class="site-footer">`; `<header>` contains `<h1>blendtutor</h1>`; `<main>` contains all 6 `data-test` hooks (`lesson-select|submission|run|lesson-status|output|feedback`), `data-action="submit"` on `#submit`, `data-status="idle"` on `#lesson-status`, and all JS-required IDs (`boot-status`, `lesson-title`, `lesson-prompt`, `submission`, `lesson-select`, `output`, `run`, `feedback`, `submit`) with original attribute values
  5. In both shells, `<script src="coi-serviceworker.js">` appears before `<link rel="stylesheet" href="styles.css">`; head load order preserved: coi-serviceworker.js → [pyodide only] pyodide.js CDN → `<link>` → body-end modules (`lesson-runner.js` then `feedback.js`)
  6. The two shells differ ONLY by: `<title>`, `#boot-status` initial text, pyodide CDN `<script>`. Normalize (strip those 3) → remaining byte-identical
  7. All existing build tests (`crates/cli/tests/build.rs` × 8, `crates/core/src/site/mod.rs` × 16) remain green without weakening assertion logic (adding assertions expected; weakening = regression)
- **probe:**
  ```
  cargo test -p blendtutor-core -p blendtutor-cli

  # rodney browser probe on built site (both targets):
  getComputedStyle(body).getPropertyValue('--bt-font-family-ui').trim() !== '' &&
    document.querySelector('style') === null &&
    document.querySelector('link[rel="stylesheet"][href="styles.css"]') !== null &&
    getComputedStyle(document.querySelector('main.workspace')).display !== 'none' &&
    getComputedStyle(document.querySelector('header.site-header')).display !== 'none' &&
    getComputedStyle(document.querySelector('footer.site-footer')).display !== 'none'
  ```
- **negative:**
  - SP1 (highest leverage): Builder adds `styles.css` as `include_str!` + to `SiteFiles` (build tests pass: file-present, byte-identical) but `<link>` points to wrong path OR is absent (inline `<style>` still present). Site builds green, CI passes, runtime stylesheet 404s → no tokens visible. Blocked by predicate 2 (link present + style absent) AND rodney probe (`getPropertyValue` returns `''` on 404).
  - SP2: Builder keeps inline `<style>` AND links `styles.css` (DRY defeated, two sources of truth). Blocked by predicate 2 (`<style>` count === 0).
  - SP3: Builder restructures webr shell, forgets to apply same restructure to pyodide (or introduces a 4th diff). Blocked by predicate 6 (normalize-3-diffs → byte-identical).
  - SP4: Builder declares tokens in `:root` but hardcodes raw values in rules (tokens declared-dead). Blocked by predicate 3 (≥4 `var(--bt-` usages).
- **verification:** `code + visual` · Rust unit + integration tests (build-time, deterministic) + rodney JS probes (runtime deterministic: computed-style + selector-absence) + builder-vision-eval (subjective residual via LLM vision on screenshot)
- **fixture status:** NEW — assertions in `crates/core/src/site/mod.rs` (extend ~:535-699: styles.css presence + byte-identity + token-usage floor) + `crates/cli/tests/build.rs` (extend file-presence lists ~:79-85, ~:155-161; add link + style-absence + load-order + token + region + known-diff-normalization assertions)
- **rubric anchor:** §1.5, §2.1, §2.2, §2.3, §3.2, §3.4, §4.1, §4.2

## Design Intent

- **Types / interfaces (§1):** `SiteFiles` vector + named `TargetAssets` fields make illegal transpositions unrepresentable; shared `STYLES_CSS: &str` const = one typed source of truth (not positional). `<link>`/`<style>` mutually exclusive — predicate asserts `<style>` ABSENT not just `<link>` present (vacuous-green prevention). Colors always `var(--bt-color-*)`, never raw hex in rules.
- **Pure / effectful (§2):** `plan_site` + `assemble` stay pure; `write_site` sole effectful FS step. `styles.css` embedded at compile via `include_str!`. Rodney probe is the only runtime effectful step (reads computed styles).
- **Boundary cuts (§3):** CSS asset at `assets/shared/` seam (same dir as `lesson-runner-core.js`, `feedback.js`). Changing a token value changes both targets without touching Rust/JS. Rust only cares `<link>` references the file; CSS contents opaque to Rust. `BuildTarget` seam stays narrow — target code contributes shell + runner adapter only; shared presentation doesn't fork per runtime.
- **Document module responsibility (§4):** `core::site` owns assembly + asset contract. `assets/shared/styles.css` owns cross-target visual vocabulary. Header must declare: tokens defined here, where it fits (shared seam), what it does NOT do (no dark mode v1, no component styles — AC-2/AC-3 own their regions, no eval-results styling).
- **Function discipline (§5):** `assemble` gains exactly one `asset("styles.css", STYLES_CSS)` call at position 6; target `plan` functions unchanged. CSS organized by semantic region (header, main workspace, footer) matching the HTML restructuring. A future token-value change touches one `:root` declaration; a future header-spacing change touches one `header.site-header` rule block.

## Technical Context

**Files created:**
- `crates/core/assets/shared/styles.css` — NEW shared CSS asset (~80-120 lines: `:root` token block + semantic region style rules). Header comment declares: tokens defined, where it fits (shared seam loaded by both shells via `include_str!`), what it does NOT do (no dark mode v1, no component styles — AC-2/AC-3 own regions, no eval-results).

**Files modified:**
- `crates/core/assets/webr/index.html` — replace inline `<style>` (`:14-72`) with `<link rel="stylesheet" href="styles.css">`; restructure `<body>` (`:74-105`) into `<header class="site-header">` / `<main class="workspace">` / `<footer class="site-footer">`; preserve title (`:6`), boot-status (`:76`), COI script load order (`:13`), all IDs + data-test/data-status/data-action hooks.
- `crates/core/assets/pyodide/index.html` — same as webr plus preserve Pyodide CDN script (`:19`). Byte-identical to webr shell except the 3 known diffs.
- `crates/core/src/site/mod.rs` — add `const STYLES_CSS: &str = include_str!("../assets/shared/styles.css");` near `:257` (alongside `LESSON_RUNNER_CORE_JS` `:237`, `COI_SERVICEWORKER_JS` `:244`, `FEEDBACK_JS` `:253`); insert `asset("styles.css", STYLES_CSS)` into `assemble()` at `:280-287` at position 6 (after `feedback.js`, before per-lesson JSON loop); extend unit tests (`:535-699`) to assert `styles.css` presence, byte-identity across targets, token-usage floor (≥4 `var(--bt-`), `<style>` absence in shells, `<link>` presence, load order, known-diff normalization.
- `crates/cli/tests/build.rs` — extend file-presence lists at `:79-85` (webr) and `:155-161` (pyodide) to include `"styles.css"`; add assertions: `<link rel="stylesheet" href="styles.css">` present, `<style>` absent, `coi-serviceworker.js` precedes `styles.css` link, semantic regions present, known-diff normalization across targets.

**Files NOT touched:** `crates/core/src/site/webr.rs`, `crates/core/src/site/pyodide.rs` (existing `include_str!` for shell + runner unchanged — `styles.css` is a shared asset, embedded in `mod.rs`), `lesson-runner-core.js`, `feedback.js`, `coi-serviceworker.js` (byte-identity preserved).

**`assemble()` output order** (before → after):
```
Before:                           After:
1. index.html                     1. index.html
2. lesson-runner.js               2. lesson-runner.js
3. lesson-runner-core.js          3. lesson-runner-core.js
4. coi-serviceworker.js           4. coi-serviceworker.js
5. feedback.js                    5. feedback.js
6. lessons/{i}.json               6. styles.css          ← NEW (position 6)
7. lessons.json                   7. lessons/{i}.json    ← shifted
                                  8. lessons.json        ← shifted
```
`styles.css` inserted after all JS assets (shared scaffolding complete) and before lesson data JSONs — preserves "code precedes data" invariant.

**Head load order** (MUST preserve, both shells):
1. `<script src="coi-serviceworker.js">` (classic, first — COOP/COEP shim)
2. [pyodide only] `<script src="https://cdn.jsdelivr.net/pyodide/v0.27.2/full/pyodide.js">`
3. `<link rel="stylesheet" href="styles.css">` ← was `<style>`, now a link (after shim, before module scripts)
4. [end of body] `<script type="module" src="lesson-runner.js">`
5. [end of body] `<script type="module" src="feedback.js">`

## Token System (final)

**Naming convention:** `--bt-<category>-<role>[--variant]` — `--bt-` prefix for namespace isolation (no collision with browser defaults or future libraries), semantic role naming (`--bt-color-status-pass` not `--bt-color-green`), so dark-mode swap replaces values under same token names.

**Minimum principled token set (~18, all-used), enforced by predicate 3 (≥4 `var(--bt-` usages) + no-dead-tokens discipline:**

| Token | Category | Value (light theme) | Rationale |
|-------|----------|---------------------|-----------|
| `--bt-font-family-ui` | Typography | `system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif` | System font stack, zero deps |
| `--bt-font-family-code` | Typography | `ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace` | Code in textarea/output |
| `--bt-font-size-base` | Typography | `1rem` | Browser default, accessible |
| `--bt-line-height` | Typography | `1.5` | Readable prose spacing |
| `--bt-color-surface` | Color | `#ffffff` | Page background |
| `--bt-color-surface-code` | Color | `#f5f5f5` | Output `<pre>` background |
| `--bt-color-text-primary` | Color | `#1a1a1a` | Body text |
| `--bt-color-text-secondary` | Color | `#555555` | Idle status / muted text |
| `--bt-color-status-pass` | Color | `#0a7d28` | Pass verdict (green) |
| `--bt-color-status-fail` | Color | `#c0202a` | Fail verdict (red) |
| `--bt-color-status-running` | Color | `#b06a00` | Running/busy (amber) |
| `--bt-color-brand` | Color | `#1a1a1a` | Brand/h1 color |
| `--bt-space-page-width` | Spacing | `50rem` | Max content width |
| `--bt-radius-sm` | Radii | `0.25rem` | Small rounding (status badge, output) |
| `--bt-radius-md` | Radii | `0.5rem` | Medium rounding (future: buttons, cards) |

(Builder may add `--bt-space-*` scale tokens, `--bt-color-border`, `--bt-color-accent`, `--bt-shadow-*` ONLY if a v1 shell rule actually consumes them. No dead tokens. AC-2/AC-3 may ADD their own tokens on top but must not duplicate/override foundation semantics.)

**Anti-gold-plating:** no token declared in `:root` that's never used; no token identical to a browser default; no component-specific tokens (AC-2/AC-3 own theirs). Contrast targets: WCAG 4.5:1 normal text, 3:1 large text/borders.

**Token contract for AC-2/AC-3 consumers:**
- AC-2 (workspace styling) consumes: `--bt-font-family-code`, `--bt-color-surface-code`, `--bt-color-status-*`, `--bt-radius-*`, `--bt-space-*`
- AC-3 (BYOK panel styling) consumes: `--bt-color-text-*`, `--bt-radius-*`, `--bt-font-family-ui`, `--bt-space-*`

## Layout Proposal (final HTML structure, both shells)

```html
<header class="site-header">
  <h1>blendtutor</h1>
  <p id="boot-status">Booting {webR|Pyodide}…</p>
</header>

<main class="workspace">
  <div class="lesson-picker">
    <label for="lesson-select">Lesson:</label>
    <select id="lesson-select" data-test="lesson-select"></select>
  </div>

  <h2 id="lesson-title"></h2>
  <p id="lesson-prompt"></p>

  <textarea id="submission" data-test="submission" spellcheck="false"></textarea>

  <div class="controls">
    <button id="run" type="button" data-test="run">Run checks</button>
    <button id="submit" type="button" data-action="submit">Submit for feedback</button>
    <span id="lesson-status" data-test="lesson-status" data-status="idle">idle</span>
  </div>

  <pre id="output" data-test="output"></pre>

  <section id="feedback" data-test="feedback"></section>
</main>

<footer class="site-footer"></footer>

<script type="module" src="lesson-runner.js"></script>
<script type="module" src="feedback.js"></script>
```

Footer content = builder judgment (predicate asserts `<footer class="site-footer">` exists only; suggest minimal `<small>` attribution or empty). `#boot-status` stays `<p>` (block-level status line, matches current shell). No `<nav>`/`<aside>`/`<article>` (no multi-page nav, no sidebar, single linear flow in v1).

## UI Block

- **selectors:** `header.site-header` (contains `h1` text "blendtutor"), `main.workspace` (contains all 6 `[data-test]` hooks + `#submit[data-action="submit"]` + `#lesson-status[data-status="idle"]`), `footer.site-footer` (exists), `link[rel="stylesheet"][href="styles.css"]` (present in `<head>`), `style` (MUST NOT exist in document)
- **layout_assertions:**
  - `getComputedStyle(document.body).getPropertyValue('--bt-font-family-ui').trim() !== ''` — proves stylesheet loaded + custom properties resolve (returns `''` if 404)
  - `getComputedStyle(document.querySelector('main.workspace')).display !== 'none'` — workspace visible (catches `display:none` sneaky-pass)
  - `getComputedStyle(document.querySelector('header.site-header')).display !== 'none'` — header visible
  - `getComputedStyle(document.querySelector('footer.site-footer')).display !== 'none'` — footer visible
  - `document.querySelector('header.site-header').getBoundingClientRect().top < document.querySelector('main.workspace').getBoundingClientRect().top` — header precedes main in layout flow
  - `document.querySelector('footer.site-footer').getBoundingClientRect().top > document.querySelector('main.workspace').getBoundingClientRect().bottom - 1` — footer below main
- **viewports:** `1024x768` (desktop — `--bt-space-page-width: 50rem` centers content), `375x812` (mobile — content must not overflow, padding ensures breathing room)
- **deterministic_check (rodney):** `getComputedStyle(document.body).getPropertyValue('--bt-font-family-ui').trim() !== '' && document.querySelector('style') === null && document.querySelector('link[rel="stylesheet"][href="styles.css"]') !== null`
- **subjective_residual:** visual harmony (color balance, spacing rhythm, typographic hierarchy), "polished" feel (intentional/professional vs mechanically correct), readability of prose at chosen font size + line height on 50rem column — evaluated by builder-vision-eval on screenshot, NOT deterministic rodney assertions

## Dependencies

- **Depends on:** none (foundation AC)
- **Blocks:** AC-2 (lesson workspace styling), AC-3 (BYOK feedback panel styling)
- **Conflict set:** `crates/core/assets/webr/index.html`, `crates/core/assets/pyodide/index.html`, `crates/core/assets/shared/styles.css`, `crates/core/src/site/mod.rs`, `crates/cli/tests/build.rs` — AC-2 and AC-3 will edit the same shells + styles.css + tests

## Progress
- [x] spec complete — 2026-06-25
- [x] red tests written — 2026-06-25
- [x] styles.css created — 2026-06-25
- [x] ADR-0010 written — 2026-06-25
- [x] webr shell restructured — 2026-06-25
- [x] pyodide shell restructured — 2026-06-25
- [x] `STYLES_CSS` const + `assemble` entry — 2026-06-25
- [x] unit tests extended — 2026-06-25
- [x] integration tests extended — 2026-06-25
- [x] all tests pass (108 core + 10 CLI) — 2026-06-25
- [x] E2E evidence captured (both targets built, verified, served) — 2026-06-25

## Decision Log
- 2026-06-25 — Token naming: `--bt-` prefixed (B's proposal) over `--color-*` (A's): namespace isolation for shared cross-target asset, forward dark-mode swap, no collision with browser/third-party custom props.
- 2026-06-25 — Token count: ~18 all-used + ≥4 `var(--bt-` floor + no-dead-tokens discipline (merge of A's breadth + B's anti-gold-plating). Declare only what v1 shell consumes; AC-2/AC-3 add their own.
- 2026-06-25 — Region classes: A's classes (`.site-header`/`.workspace`/`.site-footer`) ON B's semantic elements (`<header>`/`<main>`/`<footer>`): §3 forward-safe scoping — bare `header{}` collides with future component `<header>`s; class-scoped foundation lets AC-2/AC-3 own their regions.
- 2026-06-25 — Footer content: builder judgment (predicate asserts `<footer class="site-footer">` exists only).
- 2026-06-25 — Probe strength: adopt B's rodney `getPropertyValue('--bt-font-family-ui')` probe (catches 404'd stylesheet A's probe misses) + ADD B's predicate 6 known-diff normalization (no existing test guards the 3-diff invariant — real sneaky-pass).
- 2026-06-25 — `assemble` order: position 6 (after feedback.js, before lesson JSONs) — preserves "code precedes data" invariant. Both proposers agreed.
- 2026-06-25 — `#boot-status`: `<p>` (B's proposal) over `<span>` (A's): block-level status line, matches current shell, no `display:block` hack.

## Surprises & Discoveries
- Known-diff normalization required extra attention: the pyodide shell has both a comment block *and* the CDN `<script>` tag as part of the 3rd diff. The normalize function had to strip the comment + script together (including preceding whitespace) to achieve byte-identity. An off-by-one in the strip logic produced 8-space indentation instead of 4 on the `<link>` tag — caught by the assertion, fixed by backing up to the preceding newline.
- The `<link>` tag can be self-closing (`<link ... />`) or HTML5 (`<link ... >`). The HTML spec says both are valid in HTML5. The initial test only checked for `<link ... >` but the file used `/>`. Fixed by accepting both forms in both unit and integration tests.
- The COOP/COEP comment had slightly different text between the two shells ("webR's SharedArrayBuffer" vs "a runtime that wants SharedArrayBuffer"). Made them identical for byte-identity. This is a minor improvement — the comment now reads generically for both runtimes.

## Idempotence & Recovery
- Safe retry: re-run `cargo test -p blendtutor-core -p blendtutor-cli` after any change; rebuild site via `cargo run -p blendtutor -- build --target {webr|pyodide} <course> -o /tmp/bt-site` and re-run rodney probe.
- Rollback: revert changes to both `index.html` shells + `styles.css` (delete file) + `mod.rs` (`STYLES_CSS` const + `assemble` entry + test assertions) + `build.rs` assertions. The shared JS assets are untouched so runtime contract is preserved on rollback.
