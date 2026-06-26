# E2E Evidence: Dark-mode `@media (prefers-color-scheme: dark)` overrides

**Issue:** #62 — Dark-mode prefers-color-scheme support

## What was verified

### 1. Both targets build successfully
- webR: `cargo run -- build --target webr crates/core/tests/fixtures/r-course -o /tmp/bt-e2e-webr-62`
- Pyodide: `cargo run -- build --target pyodide crates/core/tests/fixtures/python-course -o /tmp/bt-e2e-pyodide-62`

### 2. `styles.css` emitted with `@media (prefers-color-scheme: dark)` block
- File size: 13,019 bytes (both targets)
- SHA256: `3ab38c50ab30307b0d3b1fc5403db674a04114557e35e2cb1e4671139d4a7d2f`
- Contains the `@media (prefers-color-scheme: dark) { :root { ... } }` block with all 13 `--bt-color-*` overrides plus dark-mode idle pill override

### 3. Cross-target byte-identity
- `cmp` confirms webR and Pyodide `styles.css` are **byte-identical**
- Both copy files committed as evidence

### 4. Both `index.html` shells reference `styles.css`
- webR `index.html` contains `<link rel="stylesheet" href="styles.css">`
- Pyodide `index.html` contains `<link rel="stylesheet" href="styles.css">`

### 5. All 153 Rust tests pass
- `cargo test` — all tests pass, including `build_dark_mode_token_overrides` with both dark-mode and light-mode contrast checks

## Dark-mode values

| Token | Light | Dark (cycle 1) | Dark (cycle 2 fix) |
|-------|-------|----------------|--------------------|
| `--bt-color-surface` | `#ffffff` | `#1e1e1e` | `#1e1e1e` |
| `--bt-color-surface-code` | `#f5f5f5` | `#2d2d2d` | `#2d2d2d` |
| `--bt-color-text-primary` | `#1a1a1a` | `#e0e0e0` | `#e0e0e0` |
| `--bt-color-text-secondary` | `#555555` | `#a0a0a0` | `#a0a0a0` |
| `--bt-color-status-pass` | `#0a7d28` | `#4caf50` | **`#66bb6a`** |
| `--bt-color-status-fail` | `#c0202a` | `#f44336` | **`#ff5252`** |
| `--bt-color-status-running` | `#b06a00` | `#ff9800` | `#ff9800` |
| `--bt-color-brand` | `#1a1a1a` | `#e0e0e0` | `#e0e0e0` |
| `--bt-color-brand-hover` | `#000000` | `#ffffff` | `#ffffff` |
| `--bt-color-border` | `#d4d4d4` | `#808080` | `#808080` |
| `--bt-color-status-idle` | `#e8e8e8` | `#888888` | `#888888` |
| `--bt-color-success-bg` | `#e8f5e9` | `#1b3d1b` | `#1b3d1b` |
| `--bt-color-danger-bg` | `#fff3e0` | `#3d1b1b` | `#3d1b1b` |

## Review cycle 2 fixes

### Fix 1: Idle pill light-mode regression
- **Problem:** The cycle 1 fix set `color: var(--bt-color-surface)` unconditionally on the idle pill, regressing light mode to 1.23:1 (surface=#ffffff on status-idle=#e8e8e8)
- **Fix:** Default rule uses `color: var(--bt-color-text-secondary)` (6.08:1 in light mode). Dark-mode override inside `@media (prefers-color-scheme: dark)` block uses `color: var(--bt-color-surface)` (4.70:1 in dark mode)
- **Pinned by:** Light-mode test pair `"text-secondary on status-idle (light)"` at 4.5 threshold

### Fix 2: Verdict `<strong>` fails 4.5:1 in dark mode
- **Problem:** `status-pass (#4caf50) on success-bg (#1b3d1b)` = 4.37:1, `status-fail (#f44336) on danger-bg (#3d1b1b)` = 4.16:1. Both below 4.5:1 for normal text
- **Fix:** Darkened status-pass to `#66bb6a` (5.13:1 on success-bg, 7.05:1 on surface) and status-fail to `#ff5252` (4.80:1 on danger-bg, 5.22:1 on surface)
- **Pinned by:** New test pairs `"status-pass on success-bg"` and `"status-fail on danger-bg"` at 4.5 threshold

### Light-mode contrast regression guard
- Added parallel light-mode contrast check loop verifying 8 key pairs in light mode
- Would catch the cycle 1 idle-pill regression automatically in future

All WCAG AA contrast ratios verified at build time via `build_dark_mode_token_overrides` clause 8.
