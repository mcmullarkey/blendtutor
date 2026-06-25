---
ac: 3
depends_on: AC-1, AC-2
risk: low
status: complete
---

# AC-3: BYOK feedback panel styling

## Executable Spec

- **predicate:**
  (1) `crates/core/assets/shared/styles.css` contains CSS rules targeting EVERY required BYOK hook: `[data-byok="key-prompt"]`, `[data-byok="provider"]`, `[data-byok="model-picker"]`, `[data-byok="model"]`, `[data-byok="models-loading"]`, `[data-byok="pending"]`, `[data-byok="verdict"]`, `[data-byok="verdict"][data-correct="true"]`, `[data-byok="verdict"][data-correct="false"]`, `[data-byok="error"]`, `#byok-disclosure`.
  (2) Each BYOK selector block references ≥1 `var(--bt-*)` token for colors/backgrounds/borders/shadows/radii/spacing; no hardcoded color hex/rgb in the BYOK block duplicating a declared token's semantic role.
  (3) ≥1 new AC-3-declared token in `:root` AND referenced in the BYOK block (AC-3 contributes, not pure consumer).
  (4) `[data-byok="verdict"][data-correct="true"]` and `[data-byok="verdict"][data-correct="false"]` produce OBSERVABLY DIFFERENT computed styles — different `backgroundColor`, `borderColor`, or `color` (NOT merely `fontWeight`).
  (5) `#byok-disclosure` computed `color` OR `font-size` measurably distinct from `#feedback` body/button text.
  (6) `feedback.js` byte-identical to pre-AC-3 — zero `className` assignments, zero `style.` property sets, zero `classList` calls in the rendering path (lines 410-498: renderKeyPrompt/renderVerdict/renderPending/renderError/renderModelPicker).
  (7) Rodney (key-prompt phase, no credentials): clicking `[data-action="submit"]` renders `form[data-byok="key-prompt"]` in `#feedback` with `display !== 'none'`, `backgroundColor !== 'rgba(0,0,0,0)'`, `parseFloat(paddingTop) > 0`, `parseFloat(borderRadius) > 0`, AND card visually elevated relative to page (`backgroundColor !== pageBackgroundColor` OR `boxShadow !== 'none'` OR `borderWidth !== '0px'`).
- **probe:**
  ```
  # 1. Build-test (code): build webr → read styles.css → assert all 11 selectors present;
  #    each block contains 'var(--bt-'; verdict true/false selectors have DIFFERENT
  #    property-value pairs for backgroundColor/borderColor/color (not just fontWeight);
  #    #byok-disclosure block has 'color' or 'font-size'; ≥1 new AC-3 token in :root +
  #    referenced in BYOK block; no hardcoded color hex matching token semantic role.
  # 2. Byte-identity (code): compare feedback.js output vs pre-AC-3 source → byte-identical;
  #    assert no 'className'/'style.'/'classList' in lines 410-498.
  # 3. Rodney — key-prompt phase (no creds):
  rodney open <built-site>
  rodney eval "document.querySelector('[data-action=submit]').click()"
  rodney assert '#feedback form[data-byok="key-prompt"]' present
  rodney assert getComputedStyle(form).display !== 'none'
  rodney assert getComputedStyle(form).backgroundColor !== 'rgba(0, 0, 0, 0)'
  rodney assert parseFloat(getComputedStyle(form).paddingTop) > 0
  rodney assert parseFloat(getComputedStyle(form).borderRadius) > 0
  rodney assert (getComputedStyle(form).backgroundColor !== getComputedStyle(document.body).backgroundColor)
           || (getComputedStyle(form).boxShadow !== 'none')
           || (parseFloat(getComputedStyle(form).borderTopWidth) > 0)
  rodney assert getComputedStyle(#byok-disclosure).color !== getComputedStyle('#feedback form button').color
  # 4. Rodney — injected verdict DOM (no creds, re-query after each injection — replaceChildren stales handles):
  rodney eval "document.querySelector('#feedback').innerHTML = '<div data-byok=\"verdict\" data-correct=\"true\"><strong>Correct</strong><p>Nice work!</p></div>'"
  rodney assert '#feedback div[data-byok="verdict"][data-correct="true"]' present
  rodney assert getComputedStyle(trueVerdict).display !== 'none'
  rodney assert parseFloat(getComputedStyle(trueVerdict).borderRadius) > 0
  rodney assert getComputedStyle(trueVerdict).backgroundColor !== 'rgba(0, 0, 0, 0)'
  rodney eval "document.querySelector('#feedback').innerHTML = '<div data-byok=\"verdict\" data-correct=\"false\"><strong>Not yet</strong><p>Try again.</p></div>'"
  rodney assert getComputedStyle(falseVerdict).backgroundColor !== getComputedStyle(trueVerdict).backgroundColor
  # 5. Builder-vision-eval (subjective residual): screenshot after styling applied;
  #    evaluate verdict visual harmony, pending/error/loading feel, card polish,
  #    cross-browser consistency, integration with AC-2 workspace.
  ```
- **negative:**
  - Builder writes `[data-byok="verdict"]` rule styling both correct AND incorrect identically (missing `[data-correct="true"]`/`[data-correct="false"]` qualifiers, or both mapping to same declaration block) — learner cannot visually distinguish "Correct" from "Not yet." [highest leverage — B]
  - OR hardcodes hex/rgb instead of `var(--bt-*)` in BYOK block. [A]
  - OR edits feedback.js to inject classes/inline styles (breaks byte-identity invariant). [A]
  - OR declares new tokens but never uses them (token bloat, violates "all added tokens USED"). [A]
  - OR removes/renames `#byok-disclosure` or any `data-byok` hook (breaks rodney hooks + existing build-test tripwire). [A]
  - OR styles `#byok-disclosure` identically to body text (no color/font-size distinction — privacy note reads as body copy). [B]
- **verification:** code · rodney · visual — code: Rust build test (styles.css selector + token-usage + verdict-differentiation scan) + feedback.js byte-identity; rodney: key-prompt phase (no creds) + injected verdict DOM state-diffing (no creds); visual: builder-vision-eval for subjective residual on verdict/pending/error rendered by real feedback.js.
- **fixture status:** NEW — `crates/cli/tests/build.rs` (add ~60-line test `build_webr_styles_byok_feedback_panel`), `crates/core/assets/shared/styles.css` (NEW file — AC-1 creates with token foundation + `<link>` + `include_str!` + `assemble` entry; AC-3 appends BYOK rules + new token declarations).
- **rubric anchor:** §1.2, §1.5, §3.2, §4.1, §5

## Design Intent

- **Types / interfaces (§1):** Two verdict states encoded as distinct CSS selectors (`[data-byok="verdict"][data-correct="true"]` vs `[data-byok="verdict"][data-correct="false"]`) with different declarations. A single `[data-byok="verdict"]` block styling both identically = illegal state this AC prevents. `data-correct` attribute on DOM = type; CSS must discriminate on it.
- **Pure / effectful (§2):** `styles.css` pure — static text, no JS, no runtime computation. Effectful DOM manipulation in feedback.js (unchanged); pure visual presentation in styles.css. CSS refactor never risks breaking BYOK flow logic.
- **Boundary cuts (§3):** Seam between feedback.js (DOM construction) and styles.css (visual presentation) = the set of `data-byok` attribute values + `#byok-disclosure`. feedback.js sets attributes; styles.css selects on them. Neither module names the other's internals — no class names, no element-type assumptions beyond what `data-byok` implies. JS change (new phase/attribute) doesn't force CSS change; CSS refactor (new tokens/layout) doesn't touch JS.
- **Document module responsibility (§4):** `styles.css` — shared asset — owns ONLY visual presentation of the lesson site. Does NOT construct DOM, handle events, make network calls. AC-3 purpose: style BYOK feedback panel via attribute selectors only.
- **Function discipline (§5):** Each CSS rule block targets exactly one `data-byok` state. No mega-selector styling 4 states in one block — changing one state's style forces review of all others.

## Technical Context

- **Files touched (AC-3 writes/edits ONLY):** `crates/core/assets/shared/styles.css` (NEW: all BYOK panel styles + 2 new token declarations, appended to AC-1/AC-2 foundation).
- **Files verified (NOT touched — hard invariant):** `feedback.js` (byte-identical), `lesson-runner-core.js` (byte-identical), both `lesson-runner.js` (byte-identical), both `index.html` (AC-1 added `<link>`; AC-3 verifies exists), `crates/core/src/site/mod.rs` (AC-1 added `include_str!` + `assemble` entry; AC-3 verifies).
- **Files touched (test):** `crates/cli/tests/build.rs` (add `build_webr_styles_byok_feedback_panel` ~60-line test).
- **AC-3 consumes foundation tokens (AC-1):** `--bt-color-surface`, `--bt-color-text-primary`, `--bt-color-text-secondary`, `--bt-color-status-pass`, `--bt-color-status-fail`, `--bt-radius-md`, `--bt-font-family-ui`.
- **AC-3 consumes foundation tokens (AC-2):** `--bt-color-border`, `--bt-shadow-sm`, `--bt-space-sm`, `--bt-space-xs`.
- **AC-3 ADDS (both USED, no duplication):** `--bt-color-success-bg` (correct verdict tint bg — semantic role distinct from `--bt-color-status-pass` accent), `--bt-color-danger-bg` (incorrect verdict tint bg — semantic role distinct from `--bt-color-status-fail` accent).
- **AC-3 does NOT add (resolved — avoid token proliferation):** `--bt-color-surface-elevated` (card uses `--bt-color-surface` + `--bt-color-border` + `--bt-shadow-sm` for elevation — border+shadow on existing surface achieves "polished card" without a third surface token), `--bt-shadow-sm` (AC-2 adds — consume), `--bt-space-xs` (AC-2 adds — consume).

## Dependencies

- **Depends on:** AC-1 (styles.css must exist with token foundation + `<link>` + `include_str!` + `assemble` entry; AC-3 consumes `var(--bt-color-surface)`, `var(--bt-color-text-*)`, `var(--bt-radius-md)`, `var(--bt-font-family-ui)` — if renamed/removed, AC-3's build-test `var(--bt-` scan goes red). AC-2 (workspace styling establishes visual context feedback panel sits within; AC-3 consumes `var(--bt-color-border)`, `var(--bt-shadow-sm)`, `var(--bt-space-sm)`, `var(--bt-space-xs)` — token contract coupled).
- **Blocks:** None. AC-3 is a terminal styling AC; no downstream AC competes for styles.css real estate.
- **Conflict set:** None. AC-1/AC-2/AC-3 styles coexist in one styles.css file (AC-1 foundation, AC-2 workspace + spacing scale, AC-3 BYOK panel appended).
- **Risk level:** low — pure CSS append, no JS touched, no logic change. Build-test tripwire: existing `build_webr_ships_the_byok_anthropic_feedback_seam` test scans feedback.js for `data-byok` tokens — if someone removes a `data-byok` attribute from feedback.js, both that test AND AC-3's CSS selectors break. Contracts coupled by design.

## Feedback Panel Styling Proposal (final per-state)

1. **Card container** — each `#feedback` child (key-prompt form, model-picker div, verdict div, pending p, error p, models-loading p): `background: var(--bt-color-surface)`, `border: 1px solid var(--bt-color-border)`, `border-radius: var(--bt-radius-md)`, `box-shadow: var(--bt-shadow-sm)`, `padding: var(--bt-space-sm)`, `font-family: var(--bt-font-family-ui)`. (Elevation via border + shadow on existing surface token — no `--bt-color-surface-elevated` needed.)
2. **Verdict correct** (`[data-byok="verdict"][data-correct="true"]`): `background: var(--bt-color-success-bg)` green-tinted (overrides card surface), `color: var(--bt-color-text-primary)`, `strong` "Correct" bold + `color: var(--bt-color-status-pass)`, `border-left: 4px solid var(--bt-color-status-pass)`.
3. **Verdict incorrect** (`[data-byok="verdict"][data-correct="false"]`): `background: var(--bt-color-danger-bg)` amber/red-tinted (overrides card surface), `color: var(--bt-color-text-primary)`, `strong` "Not yet" distinct, `border-left: 4px solid var(--bt-color-status-fail)`.
4. **Disclosure** (`#byok-disclosure`): `color: var(--bt-color-text-secondary)` muted, `font-size: 0.875em` (or `var(--bt-font-size-sm)` if AC-2 adds), `font-style: italic`, `margin-bottom: var(--bt-space-sm)`.
5. **Form elements** (select, input, button): inherit `font-family: var(--bt-font-family-ui)` + font-size from card. Consistent with AC-2 workspace input styling (same `--bt-radius-*`, `--bt-space-*`). Button uses `var(--bt-color-brand)` or `var(--bt-color-text-primary)`.
6. **Pending/Loading/Error** — transient text in card. Pending/loading: `color: var(--bt-color-text-secondary)`, CSS animation (pulsing opacity — no JS). Error: `color: var(--bt-color-status-fail)`, distinct from verdict (no `--bt-color-danger-bg` bg — error is text-only emphasis, verdict is bg-tinted card).

## Token System (final)

- **AC-3 ADDS (both USED, no duplication):** `--bt-color-success-bg`, `--bt-color-danger-bg`.
- **AC-3 CONSUMES from AC-1:** `--bt-color-surface`, `--bt-color-text-primary`, `--bt-color-text-secondary`, `--bt-color-status-pass`, `--bt-color-status-fail`, `--bt-radius-md`, `--bt-font-family-ui`.
- **AC-3 CONSUMES from AC-2:** `--bt-color-border`, `--bt-shadow-sm`, `--bt-space-sm`, `--bt-space-xs`.
- **AC-3 does NOT add (resolved):** `--bt-color-surface-elevated` (use `--bt-color-surface` + `--bt-color-border` + `--bt-shadow-sm`), `--bt-shadow-sm` (AC-2 adds), `--bt-space-xs` (AC-2 adds).

## UI Block

- **selectors:** `#feedback[data-test="feedback"]`, `#feedback form[data-byok="key-prompt"]`, `#byok-disclosure`, `#feedback select[data-byok="provider"]`, `#feedback input[type="password"]`, `#feedback button[type="submit"]`, `#feedback div[data-byok="model-picker"]`, `#feedback select[data-byok="model"]`, `#feedback p[data-byok="models-loading"]`, `#feedback p[data-byok="pending"]`, `#feedback div[data-byok="verdict"][data-correct="true"]`, `#feedback div[data-byok="verdict"][data-correct="false"]`, `#feedback p[data-byok="error"]`.
- **layout_assertions:**
  - card visible bg (non-transparent `backgroundColor !== 'rgba(0,0,0,0)'`)
  - card visually elevated relative to page (`backgroundColor !== pageBg` OR `boxShadow !== 'none'` OR `borderWidth > 0`)
  - card not hidden (`display !== 'none'`)
  - card non-zero padding (`paddingTop > 0`)
  - card non-zero radius (`borderRadius > 0`)
  - disclosure distinct from body text (`color !== button.color` OR `font-size` smaller)
  - verdict correct visible bg (non-transparent)
  - verdict incorrect visible bg (non-transparent)
  - verdict states visually differ (`backgroundColor` true ≠ false — not merely `fontWeight`)
  - verdict label (`strong`) present (injected DOM includes `<strong>` to exercise CSS-on-strong rules)
- **viewports:** 375x812 (mobile — card no overflow), 800x (desktop — card no unreasonable stretch).
- **deterministic_check (rodney):**
  - Phase 1 (key-prompt, no creds): click `[data-action="submit"]` → assert `form[data-byok="key-prompt"]` present + `display !== 'none'` + `backgroundColor !== 'rgba(0,0,0,0)'` + `paddingTop > 0` + `borderRadius > 0` + elevation (`backgroundColor !== pageBg` OR `boxShadow !== 'none'` OR `borderWidth > 0`) + `#byok-disclosure` present + `getComputedStyle(#byok-disclosure).color !== getComputedStyle('#feedback form button').color`.
  - Phase 2 (inject verdict true, no creds): `#feedback.innerHTML = '<div data-byok="verdict" data-correct="true"><strong>Correct</strong><p>Nice work!</p></div>'` → re-query → assert present + `display !== 'none'` + `borderRadius > 0` + `backgroundColor !== 'rgba(0,0,0,0)'` + `strong` present.
  - Phase 3 (inject verdict false, no creds): `#feedback.innerHTML = '<div data-byok="verdict" data-correct="false"><strong>Not yet</strong><p>Try again.</p></div>'` → re-query → assert `getComputedStyle([data-correct="false"]).backgroundColor !== getComputedStyle([data-correct="true"]).backgroundColor`.
  - (Re-query after each injection — `replaceChildren` stales cached handles. Injected DOM matches feedback.js:472-484 contract shape so CSS-on-strong rules are exercised; assertions are on computed styles, NOT label text content — label text is feedback.js's contract, covered by existing build tests.)
- **subjective_residual:** card polish (shadow softness, border subtlety, padding balance); verdict correct green tint reads positive not harsh neon; verdict incorrect amber/red reads encouraging not alarming; disclosure feels like privacy assurance not legal disclaimer; feedback panel looks intentional + integrated with AC-2 workspace not bolted-on; pending/loading feel responsive (CSS transitions on opacity/height OK, no JS).

## Progress
- [x] spec complete — 2026-06-25
- [x] test(red): BYOK feedback panel test written, confirmed fails — 2026-06-25
- [x] feat: BYOK/feedback CSS appended to styles.css — 2026-06-25
- [x] All 155 tests pass (109 core + 11 build + 35 other) — 2026-06-25

## Decision Log
- 2026-06-25 — Verdict verification: adopted B's DOM-injection (inject verdict HTML matching feedback.js:472-484 contract, check computed styles) + builder-vision-eval for subjective. DOM-injection tests CSS in isolation against contracted DOM shape. Combined with existing build tests (feedback.js produces that DOM) + AC-3 build test (styles.css has selectors), composition sound. No credentials needed.
- 2026-06-25 — Card elevation: merged — bg-different-from-page OR shadow OR border. "Polished card" = visually distinct from page. Distinct bg one technique; shadow/border another. Mandating shadow specifically over-constrains. Merged guard catches all sneaky-passes where card invisible/flat.
- 2026-06-25 — Disclosure distinction: adopted B's guard (computed color OR font-size distinct from body/button text). "Disclosure = privacy note" is AC language — must read differently from body. Load-bearing.
- 2026-06-25 — `display: none` sneaky-pass: adopted B's guard. Cheap, catches real failure (element exists but hidden).
- 2026-06-25 — Verdict label textContent: dropped from AC-3 predicate (feedback.js contract). `label.textContent = "Correct"/"Not yet"` is feedback.js:478 — AC-3 doesn't touch feedback.js. Existing build tests cover rendering path. Probe injects `<strong>Correct</strong>` to exercise CSS-on-strong rules but asserts computed styles, NOT text.
- 2026-06-25 — Token sets: AC-3 ADDS 2 (success-bg, danger-bg); CONSUMES shadow-sm, space-xs/sm, border from AC-2. Avoid duplication.
- 2026-06-25 — `--bt-color-surface-elevated`: dropped — use surface + border + shadow. Token-lean principle (AC-1 "~18 all-used tokens" values leanness). Border + shadow on existing `--bt-color-surface` achieves "polished card." Third surface token avoidable.
- 2026-06-25 — `color-mix` vs explicit tokens: kept explicit `--bt-color-success-bg`/`--bt-color-danger-bg`. Both proposers converged on explicit tokens. Semantic role (tint bg) distinct from status-pass/fail (accent). Wider browser support.

## Surprises & Discoveries
- 2026-06-25 — The `css_decl_block` helper function in the test needed to use brace-counting (not regex) for robust CSS block extraction. A simple `find('{')` then balanced `{}` scan works across all selectors regardless of nesting. The `regex_lite` crate doesn't support backreferences (not needed here), but its `(?s)` flag was not needed since brace-counting is simpler and more correct.
- 2026-06-25 — The `--bt-color-success-bg`/`--bt-color-danger-bg` tokens use `#e8f5e9` (light green, Material Green 50) and `#fff3e0` (light amber, Material Orange 50). These are the ONLY hex values added (in `:root` token declarations). All BYOK rule blocks use `var(--bt-*)` exclusively — zero hex in the rules section. The hex-regex test scans only the BYOK section (after the marker), so the `:root` hex values are correctly excluded.
- 2026-06-25 — Card container uses a grouped selector for all 6 `#feedback` child phases (key-prompt, model-picker, verdict, pending, error, models-loading). The verdict-specific overrides (`[data-correct="true"]`/`[data-correct="false"]`) follow the grouped rule with higher specificity, correctly overriding `background` while inheriting border/shadow/radius/padding.
- 2026-06-25 — `#feedback input[type="password"]` selector matches the API key input. The `[data-byok="provider"]` and `[data-byok="model"]` selectors match the `<select>` elements. These form selectors are NOT among the 11 enumerated selectors in the predicate — they're additional stylings for consistency with AC-2 workspace. The test only checks the 11 enumerated selectors, so adding extra selectors is safe.

## Idempotence & Recovery
- Safe retry: re-run `cargo test -p blendtutor-core -p blendtutor-cli` after any change; rebuild site via `cargo run -p blendtutor -- build --target {webr|pyodide} <course> -o /tmp/bt-site` and re-run rodney probe (key-prompt phase + injected verdict DOM).
- Rollback: revert `styles.css` BYOK section (the `[data-byok]` rules + AC-3's 2 added `:root` tokens `--bt-color-success-bg`/`--bt-color-danger-bg`). AC-1 foundation + AC-2 workspace rules remain. feedback.js untouched so BYOK flow logic preserved on rollback.
