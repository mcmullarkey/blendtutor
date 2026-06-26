---
ac: 2
depends_on: AC-1
risk: medium
status: complete
---

# AC-2: Lesson workspace styling

## Executable Spec

- **predicate:** A site built for webr + pyodide passes ALL of (1–12):
  1. **Workspace selectors present:** `styles.css` contains rules targeting `.lesson-picker`, `#lesson-title`, `#lesson-prompt`, `#submission`, `.controls`, `#run`, `#submit`, `#lesson-status`, `#output`.
  2. **Token-usage floor (workspace-only):** ≥6 `var(--bt-` references within rules targeting workspace selectors. A `var(--bt-` inside `:root` or a region NOT matching a workspace selector does NOT count. Catches tokens-declared-in-root-but-dead-in-workspace.
  3. **No hardcoded hex in workspace rules:** No `#<hex>` color literals appear within workspace-targeting rule blocks. All colors via `var(--bt-*)`.
  4. **Badge/pill contract:** `styles.css` contains exactly four `#lesson-status[data-status="…"]` rules (idle, running, pass, fail) — no `.status-idle`/`.status-running`/`.status-pass`/`.status-fail` class selectors. For each state via rodney: `getComputedStyle(#lesson-status).backgroundColor` ≠ `rgba(0, 0, 0, 0)` (non-transparent), `paddingLeft + paddingRight > 0`, `borderRadius ≥ 999px` (pill). Bare colored text fails; barely-rounded square fails.
  5. **State colors distinct:** Setting `#lesson-status` `data-status` to each of idle/running/pass/fail yields four distinct `getComputedStyle.backgroundColor` values. Attribute selector alive (idle ≠ pass).
  6. **Button hierarchy:** `getComputedStyle(#run).backgroundColor` ≠ `getComputedStyle(#submit).backgroundColor`. Run = filled `--bt-color-brand` bg (non-transparent); Submit = transparent or subdued bg. Two identical buttons fail.
  7. **Code readability — textarea:** `getComputedStyle(#submission).fontFamily` matches `/mono/i`.
  8. **Code readability — output:** `getComputedStyle(#output).whiteSpace` is `pre` or `pre-wrap`.
  9. **Output min-height:** `getComputedStyle(#output).minHeight` ≠ `0px` AND ≠ `auto`. Empty output panel has visible height.
  10. **Controls row layout:** `getComputedStyle(.controls).display` is `flex` AND `flexDirection` is `row`.
  11. **Lesson picker grouped:** `getComputedStyle(.lesson-picker).display` is `flex` or `inline-flex` OR `gap` > 0. Bare inline label+select fails.
  12. **Hooks + IDs + byte-identity + no-dead-tokens + build green:** All 6 `data-test` hooks intact, `data-status` on `#lesson-status`, `data-action="submit"` on `#submit`, all workspace element IDs intact. `styles.css` byte-identical across webr+pyodide (extend `plan_site_carries_the_same_shared_core_and_shim_across_both_targets`). Every token AC-2 adds to `:root` appears in ≥1 `var(--bt-` outside `:root`. Build tests green without weakening.
- **probe (rodney IIFE — computed-style surface):**
  ```
  (() => {
    const s = document.getElementById('lesson-status');
    const states = ['idle','running','pass','fail'];
    const bgs = {};
    let badgeOk = true;
    for (const st of states) {
      s.dataset.status = st;
      const cs = getComputedStyle(s);
      bgs[st] = cs.backgroundColor;
      const padX = parseFloat(cs.paddingLeft) + parseFloat(cs.paddingRight);
      const radius = parseFloat(cs.borderRadius);
      if (bgs[st] === 'rgba(0, 0, 0, 0)' || padX <= 0 || radius < 999) badgeOk = false;
    }
    const distinct = new Set(Object.values(bgs)).size === 4;
    const alive = bgs.idle !== bgs.pass;
    s.dataset.status = 'idle';
    const runBg = getComputedStyle(document.getElementById('run')).backgroundColor;
    const submitBg = getComputedStyle(document.getElementById('submit')).backgroundColor;
    const buttonsDistinct = runBg !== submitBg && runBg !== 'rgba(0, 0, 0, 0)';
    const subMono = /mono/i.test(getComputedStyle(document.getElementById('submission')).fontFamily);
    const outCs = getComputedStyle(document.getElementById('output'));
    const outPre = ['pre','pre-wrap'].includes(outCs.whiteSpace);
    const outMinOk = outCs.minHeight !== '0px' && outCs.minHeight !== 'auto';
    const ctrl = getComputedStyle(document.querySelector('.controls'));
    const ctrlOk = ctrl.display === 'flex' && ctrl.flexDirection === 'row';
    const pk = getComputedStyle(document.querySelector('.lesson-picker'));
    const pickerOk = (pk.display === 'flex' || pk.display === 'inline-flex') || parseFloat(pk.gap) > 0;
    return badgeOk && distinct && alive && buttonsDistinct && subMono && outPre && outMinOk && ctrlOk && pickerOk;
  })()
  ```
- **probe (code surface — Rust unit test, CSS text scan):**
  ```
  // Extend crates/core/src/site/mod.rs tests:
  // - workspace_styles_use_tokens_and_status_renders_as_pill():
  //     assert styles.css contains selectors for .lesson-picker, #lesson-title,
  //       #lesson-prompt, #submission, .controls, #run, #submit, #lesson-status, #output
  //     assert count of "var(--bt-" within workspace-selector rule blocks >= 6
  //     assert no /#[0-9a-fA-F]{3,8}/ inside workspace rule blocks
  //     assert exactly 4 occurrences of /#lesson-status\[data-status="[^"]+"\]/
  //     assert zero occurrences of /\.status-(idle|running|pass|fail)/
  //     assert every AC-2-added :root token name appears in a var(--bt-) outside :root
  // - extend plan_site_carries_the_same_shared_core_and_shim_across_both_targets (:680):
  //     add "styles.css" to the shared-asset byte-identity loop
  ```
- **negative:** Builder adds workspace rules using hardcoded hex (`background: #0a7d28`) instead of `var(--bt-color-status-pass)` — tokens exist in `:root` but dead in workspace; build tests pass (file present, byte-identical, `<link>` loads, AC-1's ≥4 `var(--bt-` floor vacuously satisfied by header/footer rules); site LOOKS right. Blocked by: workspace-only `var(--bt-` count ≥6 AND no-hardcoded-hex AND new-token-consumption assertions. OR: `#lesson-status` is bare colored text with no bg/padding/pill-radius. OR: `#lesson-status` uses `.status-pass` class selectors instead of `[data-status="pass"]` attribute selectors. OR: `styles.css` diverges between targets (4th diff introduced). OR: run + submit buttons identical bg. OR: a rodney hook/ID renamed or removed.
- **verification:** code (Rust unit test — CSS text scan for selectors, token count, hex-free, data-status selector count, no-class-status, new-token consumption, cross-target byte-identity) + rodney (IIFE — computed-style badge/pill shape, state distinctness, button hierarchy, mono font, pre-wrap, min-height, flex-row, picker grouped) + visual (builder-vision-eval — subjective residual: polish, harmony, spacing rhythm)
- **fixture status:** Extend `crates/core/src/site/mod.rs:680` (`plan_site_carries_the_same_shared_core_and_shim_across_both_targets` — add `styles.css` to byte-identity loop). NEW test `workspace_styles_use_tokens_and_status_renders_as_pill` in `crates/core/src/site/mod.rs` (CSS text scan). Extend `crates/cli/tests/build.rs` (file-scan: workspace `var(--bt-` count, data-status selector count =4, no `.status-*` class selectors, no hardcoded hex in workspace rules). Existing fixtures `r_course()`/`python_course()` at `crates/core/src/site/mod.rs:462`/`:474` reused.
- **rubric anchor:** §1.2 (closed-set `data-status` as type), §1.5/§1.5.1 (visual states as attribute-selector tokens), §2.2 (pure CSS declarations, only `write_site` effectful), §3.2 (single `styles.css` style seam), §3.4 (workspace rules scoped, never bleed to header/footer), §4.2 (`styles.css` owns presentation, `site/mod.rs` owns assembly), §5.1 (each selector one element/state, predicate one invariant cluster)

## Design Intent

- **Types / interfaces (§1):** `data-status` = closed-set styling hook (4 values: idle/running/pass/fail), each via `#lesson-status[data-status="…"]` attribute selector — CSS attribute selector IS the type system. Badge/pill contract typed: (1) non-transparent bg per state, (2) non-zero horizontal padding, (3) `border-radius ≥ 999px` (pill), (4) four distinct state bg colors. No `.status-*` class strings (illegal state unrepresentable — class-based status selector cannot exist).
- **Pure / effectful (§2):** All AC-2 styling pure CSS declarations — zero JS. `data-status` set by untouched `lesson-runner-core.js`; CSS attribute selectors reactively style. `plan_site` returns snapshot-testable `SiteFiles`; only `write_site` effectful. Rodney probe is the only effectful verification step.
- **Boundary cuts (§3):** AC-2 owns `<main class="workspace">` and its descendants. AC-1 owns shell + foundation tokens. AC-3 owns `#feedback`. Workspace rules scoped to `.workspace`-child selectors — never bare element selectors that bleed into header/footer. `styles.css` single style seam, embedded once via `include_str!`, both targets consume same bytes.
- **Document module responsibility (§4):** `styles.css` owns cross-target visual vocabulary. AC-2's section visibly separated (comment `/* === workspace === */`), declares what it does NOT do: no dark mode v1, no `#feedback` panel, no header/footer overrides, no JS changes. `site/mod.rs` owns assembly + test invariants. `lesson-runner-core.js`/`feedback.js` untouched.
- **Function discipline (§5):** Each workspace element = self-contained rule block. `#lesson-status[data-status="…"]` rules adjacent. Button rules adjacent. Token declarations grouped by category in `:root`. Predicate checks one invariant cluster (token usage + badge/pill shape + layout correctness).

## Technical Context

- **Files likely touched:**
  - `crates/core/assets/shared/styles.css` — AC-2 appends workspace section after AC-1's header/footer foundation. Adds 9 new tokens to `:root` + ~40–60 lines of workspace component rules + 4 data-status badge rules. Primary touched file.
  - `crates/core/src/site/mod.rs` — extend `plan_site_carries_the_same_shared_core_and_shim_across_both_targets` (:680) to include `styles.css` in byte-identity loop; add NEW test `workspace_styles_use_tokens_and_status_renders_as_pill` (CSS text scan).
  - `crates/cli/tests/build.rs` — extend file-scan assertions: workspace `var(--bt-` count ≥6, data-status selector count =4, no `.status-*` class selectors, no hardcoded hex in workspace rules.
- **Files NOT touched:** both `index.html` shells (AC-1 structure unchanged — AC-2 only adds CSS matching existing HTML), `webr.rs`/`pyodide.rs`, `lesson-runner-core.js`, `feedback.js`, `coi-serviceworker.js`.
- **Architecture notes:** AC-1 creates `STYLES_CSS` embed alongside `LESSON_RUNNER_CORE_JS` etc. (`site/mod.rs:237-256` region). `assemble()` output order unchanged — `styles.css` at its AC-1 position. AC-2 must not introduce 4th diff between shells. Per-lesson JSON keyed by index (unchanged). No LLM calls, no code execution at build time (ADR-0008).

## Dependencies

- **Depends on:** AC-1 (creates `styles.css`, foundation tokens `--bt-font-family-code`, `--bt-color-surface-code`, `--bt-color-text-primary`, `--bt-color-text-secondary`, `--bt-color-status-pass/fail/running`, `--bt-color-brand`, `--bt-line-height`, `--bt-radius-sm/md`, `--bt-space-page-width`; restructures shells into `<header>/<main class="workspace">/<footer>`; establishes byte-identity test at `:680`).
- **Blocks:** AC-3 (`#feedback` panel styling — consumes same `styles.css` seam, must not collide with workspace rules).
- **Conflict set:** `crates/core/assets/shared/styles.css` (AC-1 writes foundation, AC-2 appends workspace, AC-3 appends feedback — sequential, not parallel), `crates/core/src/site/mod.rs` (AC-1 + AC-2 both extend tests), `crates/cli/tests/build.rs` (AC-1 + AC-2 both extend file-scan).
- **Risk level:** medium — CSS-only change but 9 new tokens + 4 state selectors + 12 assertions; risk of token-name collision with AC-3, risk of workspace rules bleeding to header/footer if selectors not scoped.

## Workspace styling proposal (final per-element table)

| Element | Treatment | Tokens consumed |
|---|---|---|
| `.lesson-picker` | `display: flex; align-items: center; gap: var(--bt-space-sm)`. Label secondary text; select surface + border + radius. | `--bt-color-text-secondary`, `--bt-color-surface`, `--bt-color-border`, `--bt-radius-md`, `--bt-space-sm` |
| `#lesson-title` | Prominent heading (font-size > base), primary text, `margin-bottom: var(--bt-space-sm)`. | `--bt-font-family-ui`, `--bt-color-text-primary`, `--bt-space-sm` |
| `#lesson-prompt` | `line-height: var(--bt-line-height)`, secondary text, `margin-bottom: var(--bt-space-md)`. | `--bt-color-text-secondary`, `--bt-line-height`, `--bt-space-md` |
| `#submission` | Editor surface: monospace, `--bt-color-surface-code` bg, `--bt-color-border`, padding, `--bt-radius-md`, `--bt-shadow-sm`, focus ring `--bt-color-brand`, min-height. | `--bt-font-family-code`, `--bt-color-surface-code`, `--bt-color-border`, `--bt-color-brand`, `--bt-radius-md`, `--bt-shadow-sm`, `--bt-space-sm` |
| `.controls` | `display: flex; align-items: center; gap: var(--bt-space-md)`, margin above/below. | `--bt-space-md` |
| `#run` | Primary: `background: var(--bt-color-brand)`, white/high-contrast text, `--bt-radius-sm`, padding, `:hover` → `var(--bt-color-brand-hover)`. | `--bt-color-brand`, `--bt-color-brand-hover`, `--bt-radius-sm`, `--bt-space-xs`, `--bt-space-sm` |
| `#submit` | Secondary: `background: transparent`, `border: 1px solid var(--bt-color-border)`, primary text color, `--bt-radius-sm`, padding, `:hover` bg tint. | `--bt-color-border`, `--bt-color-text-primary`, `--bt-radius-sm`, `--bt-space-xs`, `--bt-space-sm` |
| `#lesson-status` | Pill badge per `data-status`: bg + matching text + `padding: var(--bt-space-xs) var(--bt-space-sm)` + `border-radius: var(--bt-radius-pill)` + `font-weight: 600` + `text-transform: uppercase` + `--bt-shadow-sm`. idle=neutral, running=amber, pass=green, fail=red. | `--bt-color-status-idle/running/pass/fail`, `--bt-space-xs`, `--bt-space-sm`, `--bt-radius-pill`, `--bt-shadow-sm` |
| `#output` | Console/result: `--bt-color-surface-code` bg, `--bt-color-border`, padding, `--bt-radius-md`, `--bt-shadow-sm`, `white-space: pre-wrap`, min-height. | `--bt-font-family-code`, `--bt-color-surface-code`, `--bt-color-border`, `--bt-radius-md`, `--bt-shadow-sm`, `--bt-space-sm` |

## Token system (final added-token set — all consumed ≥1 outside `:root`)

| Token | Value | Consumed by |
|---|---|---|
| `--bt-color-status-idle` | `#e8e8e8` (neutral gray) | `#lesson-status[data-status="idle"]` bg |
| `--bt-color-border` | `#d0d0d0` (subtle) | `#submission` border, `#output` border, `#submit` border, `.lesson-picker` select border |
| `--bt-color-brand-hover` | darker `--bt-color-brand` (e.g. `#1558b0` if brand=`#1a6dd4`) | `#run:hover` bg |
| `--bt-space-xs` | `0.25rem` | Badge padding, button padding |
| `--bt-space-sm` | `0.5rem` | Textarea/output padding, title margin, picker gap, badge padding |
| `--bt-space-md` | `0.75rem` | Controls gap, prompt margin |
| `--bt-space-lg` | `1.5rem` | Section spacing between workspace sub-regions |
| `--bt-shadow-sm` | `0 1px 3px rgba(0,0,0,0.08)` | Textarea, output, status badge (raised feel) |
| `--bt-radius-pill` | `9999px` | `#lesson-status` badge/pill |

9 new tokens. AC-1 foundation tokens also consumed: `--bt-font-family-ui`, `--bt-font-family-code`, `--bt-color-surface`, `--bt-color-surface-code`, `--bt-color-text-primary`, `--bt-color-text-secondary`, `--bt-color-status-pass`, `--bt-color-status-fail`, `--bt-color-status-running`, `--bt-color-brand`, `--bt-line-height`, `--bt-radius-sm`, `--bt-radius-md`.

**Dropped from proposals (avoid proliferation):**
- `--bt-color-accent` / `--bt-color-accent-hover` (B) → collapsed into `--bt-color-brand` / `--bt-color-brand-hover`. Brand = primary action color for a single-action learning tool; no semantic distinction.
- `--bt-color-surface-raised` (B) → dropped. Output uses `--bt-color-surface-code` (semantically correct: code output) + `--bt-shadow-sm` for raised feel. No third surface token.
- `--bt-space-1..6` (A) → replaced by `--bt-space-xs/sm/md/lg` (B). Semantic naming is design-system convention (Tailwind/Material); self-documenting.

## UI Block

- **selectors:** `.workspace`, `.lesson-picker`, `#lesson-title`, `#lesson-prompt`, `#submission`, `.controls`, `#run`, `#submit`, `#lesson-status` (+ 4 `data-status` states: idle/running/pass/fail), `#output`. `.status-idle`/`.status-running`/`.status-pass`/`.status-fail` MUST NOT exist.
- **layout_assertions:**
  - `.controls` `display: flex` AND `flex-direction: row` (buttons horizontal, not stacked)
  - `.lesson-picker` `display: flex`/`inline-flex` OR `gap > 0` (label + select grouped)
  - `#lesson-status` height ≥ 18px, width ≥ 24px (badge visible, not collapsed)
  - `#lesson-status` `border-radius ≥ 999px` (pill, not square)
  - `#lesson-status` bg non-transparent in all 4 `data-status` states; 4 states yield 4 distinct bg colors
  - `#run` bg ≠ `#submit` bg (button hierarchy)
  - `#run` bg non-transparent (filled primary)
  - `#submission` `font-family` includes `mono`
  - `#output` `white-space` is `pre` or `pre-wrap`
  - `#output` `min-height` ≠ `0px` AND ≠ `auto`
- **viewports:** 1024×768 (desktop), 375×812 (mobile), 600×400 (small/narrow)
- **deterministic_check (rodney IIFE):** the merged IIFE in the `probe (rodney IIFE)` block above — checks badge bg (all 4 states non-transparent) + selector alive (idleColor ≠ passColor) + badge size (h≥18, w≥24) + badge radius ≥999 + buttons distinct + run bg non-transparent + mono font + pre-wrap + min-height + flex-row controls + picker grouped. Returns boolean.
- **subjective_residual (builder-vision-eval):** visual harmony, spacing rhythm, button hierarchy legibility, typographic polish, "polished modern learning-tool" feel, shadow depth, pill-badge modernity. Evaluated via LLM vision on screenshot at 1024×768 + 375×812.

## Progress
- [x] spec complete — 2026-06-25
- [x] red integration test — 2026-06-25 (workspace_styles_use_tokens_and_status_renders_as_pill)
- [x] green CSS implementation — 2026-06-25 (4 new tokens + workspace rules + badge rules)
- [x] build.rs extended with file-scan assertions — 2026-06-25
- [x] all tests pass (173 tests, 0 failures) — 2026-06-25
- [x] E2E evidence committed to docs/evidence/57/ — 2026-06-25

## Decision Log
- 2026-06-25 — Badge radius: ≥999px (pill, A's proposal) over >0 (B). AC narrative says "proper badge/pill" — pill requires high radius. B's >0 permits barely-rounded square (sneaky-pass). Pill = strong modern signal.
- 2026-06-25 — Button hierarchy (run vs submit distinct): adopted B's SP3. Two identical buttons confuse user in learning tool. Run=primary, Submit=secondary. Load-bearing for v1 polish.
- 2026-06-25 — Token floor: ≥6 workspace-only `var(--bt-` (B's count, excludes :root) + no hardcoded hex (A's no-hex). Both kept — B's count testable + excludes :root; A's no-hex catches sneaky-pass B's count misses.
- 2026-06-25 — B's extra guards (SP6 mono, SP9 min-height, SP11 flex-row, SP10 picker-grouped): adopted all 4. Mono+pre-wrap=readability, min-height=empty-state, flex-row=layout, picker-grouped=visual unit. All load-bearing for "polished modern."
- 2026-06-25 — data-status selector count =4 (B's SP4): adopted. 4 states, 4 selectors. Catches missing states + class-based sneaky-pass.
- 2026-06-25 — Spacing scale naming: `--bt-space-xs/sm/md/lg` (B) over `--bt-space-1..6` (A). Semantic naming = design-system convention, self-documenting.
- 2026-06-25 — `--bt-color-surface-raised` (B): dropped. Output = code output → `--bt-color-surface-code` (AC-1) fits. Raised feel via `--bt-shadow-sm`. Avoids 3rd surface token.
- 2026-06-25 — `--bt-color-accent` vs `--bt-color-brand`: dropped accent; use brand + `--bt-color-brand-hover`. Single primary action color for learning tool. Brand=identity=action. Avoids duplication.

## Surprises & Discoveries
- **2026-06-25** — AC-1 forward-declared more tokens than just `--bt-space-xs`/`--bt-space-sm`. The `:root` block already contained `--bt-color-border` (`#d4d4d4`), `--bt-space-md`, `--bt-space-lg` (`1rem`, not the `1.5rem` AC-2 spec expected). Only 4 of the "9 new tokens" in the AC-2 spec needed adding: `--bt-color-status-idle`, `--bt-color-brand-hover`, `--bt-shadow-sm`, `--bt-radius-pill`. The other 5 were consume-only. Kept AC-1's `--bt-space-lg: 1rem` rather than changing to `1.5rem` per spec (would break AC-1's layout).
- **2026-06-25** — The `color-mix()` CSS function works for submit hover tint without needing a dedicated hover token. Avoids adding dead tokens. Supported in all modern browsers (Chrome 111+, Firefox 113+, Safari 16.2+) — blendtutor's target browsers (webR/Pyodide runtimes) all qualify.
- **2026-06-25** — `regex_lite` crate not in dependency tree despite `regex_lite_contains_pyodide_boot` function name suggesting otherwise. Had to add `regex-lite = "0.1"` to workspace + both crate dev-dependencies. The function name was just a prefix, not a crate reference.
- **2026-06-25** — The hex regex `#[0-9a-fA-F]{3,8}` falsely matched `#feedbac` inside the word "feedback" in CSS comments. Narrowed to exactly 3, 6, or 8 hex digits: `#[0-9a-fA-F]{6}(?:[0-9a-fA-F]{2})?\b|#[0-9a-fA-F]{3}\b`.

## Idempotence & Recovery
- Safe retry: re-run `cargo test -p blendtutor-core -p blendtutor-cli` after any change; rebuild site via `cargo run -p blendtutor -- build --target {webr|pyodide} <course> -o /tmp/bt-site` and re-run rodney IIFE probe.
- Rollback: revert `styles.css` workspace section (the `/* === workspace === */` block + AC-2's 9 added `:root` tokens). AC-1 foundation + header/footer rules remain. Shared JS untouched so runtime contract preserved.
