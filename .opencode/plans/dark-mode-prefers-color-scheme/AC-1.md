---
ac: 1
depends_on: none
risk: low
status: complete
---

## AC-1: Dark-mode token overrides in shared styles.css

Add `@media (prefers-color-scheme: dark) { :root { ... } }` block overriding all `--bt-color-*` tokens with dark-mode-appropriate values meeting WCAG AA contrast (4.5:1 text, 3:1 UI/large text). Both webR + Pyodide generated sites inherit via the shared file.

### Executable Spec
- **predicate:** 10 clauses, all ANDed:
  1. `@media (prefers-color-scheme: dark)` block appears AFTER the closing `}` of the light `:root` block (positional check)
  2. Media block contains a `:root { ... }` sub-block, extracted via brace-counting (not substring)
  3. All 13 `--bt-color-*` tokens declared in the light `:root` (surface, surface-code, text-primary, text-secondary, status-pass, status-fail, status-running, status-idle, brand, brand-hover, border, success-bg, danger-bg) are also declared in the dark `:root` (exact property-name match)
  4. Each dark value DIFFERS from its light counterpart (case-insensitive hex comparison)
  5. Directional sanity: surface/bg tokens darken (RGB sum strictly lower in dark), text tokens lighten (RGB sum strictly higher in dark)
  6. Non-color token exclusion: dark `:root` contains NO `--bt-font-*`, `--bt-space-*`, `--bt-shadow-*`, `--bt-radius-*` declarations
  7. Hardcoded-hex scan: zero hex literals (`#rgb` / `#rrggbb` / `#rrggbbaa`) outside both `:root` blocks across the entire file
  8. Sampled semantic pairs compute WCAG contrast ratios: text-primary on surface >= 4.5, text-secondary on surface >= 4.5, text-primary on surface-code >= 4.5, text-primary on success-bg >= 4.5, text-primary on danger-bg >= 4.5, status-pass/fail/running/idle on surface >= 3.0, brand on surface >= 3.0
  9. Cross-target byte-identity: `styles.css` is byte-identical in webR and Pyodide builds
  10. Both `index.html` shells reference `styles.css` via `<link rel="stylesheet" href="styles.css">`
- **probe:** `cargo test --test build -- build_dark_mode_token_overrides`
- **negative:** missing media block entirely; empty media block (no `:root` sub-block); missing override for any of the 13 tokens; wrong media feature (not `prefers-color-scheme: dark`); wrong inner selector (`html`/`body` instead of `:root`); media block positioned before light `:root`; dark values identical to light values; hardcoded hex bypass (hex literal outside `:root` blocks); inverted luminance direction (surfaces lighten / text darkens); contrast < 4.5 for normal-text pairs or < 3.0 for UI/large-text pairs; styles.css diverges across targets; shell missing `<link>` to styles.css.
- **verification:** `code` (Rust integration test in `crates/cli/tests/build.rs` — WCAG contrast computed from parsed hex via relative-luminance formula, deterministic, no browser dependency)
- **fixture status:** NEW — `crates/cli/tests/build.rs` (new test `build_dark_mode_token_overrides`, reusing `css_decl_block`:997, `emitted_files`:430, `build`:48, `hex_pat`:173)
- **rubric anchor:** §3.2, §1.2, §1.5

### Design Intent
- **Types / interfaces (§1):** No Rust type changes. Theme state encoded by CSS custom properties + `@media` query — dark/light is CSS media-query state, not a Rust `bool` flag. 13 `--bt-color-*` tokens form a closed set; test enforces the contract (all overridden, no non-color leakage).
- **Pure / effectful (§2):** Pure CSS content. No JS, no DOM mutation, no server logic. `feedback.js` byte-unchanged.
- **Boundary cuts (§3):** One shared stylesheet updates both targets via a single `@media` block. No per-target fork.
- **Module responsibility (§4):** Update 3 stale "No dark mode v1 (future AC)" comments (styles.css lines 15, 128, 286) to document the dark-mode contract. Header block (lines 14-16) must reflect the new capability.
- **Function discipline (§5):** Single `@media` block, no per-target fork, no inline `<style>` in shells.

### Technical Context
- **Files likely touched:** `crates/core/assets/shared/styles.css` (add `@media` block after light `:root` closing `}` at line 68; update comments at lines 15, 128, 286), `crates/cli/tests/build.rs` (new test `build_dark_mode_token_overrides`).
- **Architecture notes:**
  - Both `index.html` shells already link `styles.css` — no HTML changes.
  - `eval-results.html` does NOT link `styles.css` — out of scope.
  - `feedback.js` references no colors directly — no changes needed.
  - Existing byte-identity tests keep file identical across targets — new test re-asserts as regression guard.
  - Reuse `css_decl_block` (build.rs:997-1023) for brace-counted block extraction, `emitted_files` (build.rs:430-445) for cross-target file scan, `hex_pat` (build.rs:173-174) for hardcoded-hex scan, `build` (build.rs:48-59) for binary execution.
  - Contrast computation: implement WCAG 2.1 §1.4.3 relative-luminance + contrast-ratio formula in the test. Formula: `L = 0.2126*R + 0.7152*G + 0.0722*B` with channel linearization; `contrast = (L_lighter + 0.05) / (L_darker + 0.05)`.
  - Test must build BOTH targets (`build("webr", ...)` + `build("pyodide", ...)`) for byte-identity check.

### Dependencies
- **Depends on:** none
- **Blocks:** none
- **Conflict set:** `crates/core/assets/shared/styles.css`, `crates/cli/tests/build.rs`
- **Risk level:** low

### Pattern Detectors
- **Rodney-feasibility:** AMBER — `--emulate-prefers-color-scheme=dark` flag unverified. Not blocking (verification is `code`), but if builder adds a rodney secondary check, flag must be validated first.
- **Bidirectional-contract:** CLEAR — single shared file propagates to both targets.
- **Route-existence:** CLEAR — no new routes.
- **Verification file-path:** CROSS-FILE — new test in `build.rs` exercises `styles.css` asset.
- **Refusal-arm:** CLEAR — no AI-refusal-sensitive content.
- **Producer-shape:** PROPAGATION SURFACE ENUMERATED — `styles.css` (producer, embedded via `include_str!` in mod.rs), both `index.html` shells (consumers), `eval-results.html` (NOT consumer), `feedback.js` (NOT consumer, no color refs).
- **UI sneaky-pass:** FLAG — if rodney secondary is added, `getComputedStyle` catches cascade-override sneaky-passes. Adequate for dark-mode cascade verification. Not blocking for `code` verification.

### Progress
- [x] 2026-06-25 10:30 — Red: write integration test `build_dark_mode_token_overrides` — confirmed fails (no `@media` block)
- [x] 2026-06-25 10:45 — Green: add `@media (prefers-color-scheme: dark)` block + update 3 comments — all 10 clauses pass
- [x] 2026-06-25 10:50 — Refactor: applied `cargo fmt`, added `strip_css_comments` helper to handle CSS-comment + declaration in same `;`-delimited chunk
- [x] 2026-06-25 10:55 — E2E evidence committed to `docs/evidence/62/`
- [x] 2026-06-25 11:00 — Fix(review): idle pill text contrast (color rule-level fix) + border color value (#444444→#808080) + 5 new contrast pairs in clause 8

### Decision Log
- 2026-06-25 — rodney/visual dropped from verification: contrast compliance is computable in code (WCAG formula). Aesthetics evaluation is a separate concern if desired.
- 2026-06-25 — test location: `crates/cli/tests/build.rs` (integration) over `crates/core/src/site/mod.rs` (unit): reuses existing `css_decl_block`, `emitted_files`, `hex_pat` helpers; builds both targets for byte-identity check.
- 2026-06-25 — 10-clause predicate (adversarial structure from Proposer B): catches sneaky-passes (empty block, identical values, positional, hardcoded hex, non-color leakage) that minimal predicate misses.
- 2026-06-25 — `@media` clause-1 search scoped to `after_root` slice (after light `:root`'s closing `}`) instead of the full CSS, to avoid matching the `@media` reference in the file header comment. Same approach for `css_decl_block` calls that need the actual rule block. For clause-7 hex-position check, `drb.as_ptr() - css.as_ptr()` pointer arithmetic still works correctly since `after_root` is a sub-slice of `css`.

### Surprises & Discoveries
- `css.find("@media (prefers-color-scheme: dark)")` matched the first occurrence in the header comment (`* - Overrides all --bt-color-* tokens inside @media (prefers-color-scheme: dark)`), not the actual `@media` rule block. Fixed by searching from the `after_root` slice (after light `:root` closing `}`), which avoids all pre-root comments. The `css_decl_block` helper uses the same `css.find` internally — passing the `after_root` slice scoped the search correctly.
- `parse_css_declarations` initially filtered by `trimmed.starts_with("/*")`, but a `;`-delimited chunk can contain both a comment and a declaration with no `;` between them (e.g., `  /* Colors — light theme */\n  --bt-color-surface: #ffffff`). Added `strip_css_comments` helper that removes `/* ... */` pairs before parsing declarations. Without this, the test found only 11 of 13 `--bt-color-*` tokens in the light `:root`.
- PR review finding (review cycle 1): clause 8's 10 sampled pairs missed two real WCAG AA regressions in dark mode — `text-secondary (#a0a0a0) on status-idle (#888888)` measured 1.36:1 (idle pill, default state on every lesson load), and `border (#444444)` against all 4 dark surfaces (surface, surface-code, success-bg, danger-bg) measured 1.27-2.08, all below the 3:1 UI floor. The idle pill fix required a rule-level change (color from text-secondary to surface) rather than a token-level change, because darkening `--bt-color-status-idle` to make text-secondary pass 4.5 would break the existing `status-idle on surface` 3.0 pair — the two constraints are contradictory due to mid-luminance text-secondary. The border fix was a straightforward token value change (#444444→#808080), the minimum that passes all 4 surface pairs. Both fixes are pinned by 5 new test pairs added to clause 8.

### Idempotence & Recovery
- Safe retry: re-run `cargo test --test build -- build_dark_mode_token_overrides` — test is deterministic, no side effects.
- Rollback: revert `styles.css` changes (remove `@media` block, restore 3 comments) + remove test from `build.rs`.
