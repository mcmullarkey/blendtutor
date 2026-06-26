# E2E Evidence: Dark-mode `@media (prefers-color-scheme: dark)` overrides

**Issue:** #62 — Dark-mode prefers-color-scheme support

## What was verified

### 1. Both targets build successfully
- webR: `cargo run -- build --target webr crates/core/tests/fixtures/r-course -o /tmp/bt-e2e-webr`
- Pyodide: `cargo run -- build --target pyodide crates/core/tests/fixtures/python-course -o /tmp/bt-e2e-pyodide`

### 2. `styles.css` emitted with `@media (prefers-color-scheme: dark)` block
- File size: 12,904 bytes (both targets)
- SHA256: `68af64a13fabc575c6a450ee9f004ef30e496dfda223d1bca764389445f769bc`
- Contains the `@media (prefers-color-scheme: dark) { :root { ... } }` block with all 13 `--bt-color-*` overrides

### 3. Cross-target byte-identity
- `cmp` confirms webR and Pyodide `styles.css` are **byte-identical**
- Both copy files committed as evidence

### 4. Both `index.html` shells reference `styles.css`
- webR `index.html` contains `<link rel="stylesheet" href="styles.css">`
- Pyodide `index.html` contains `<link rel="stylesheet" href="styles.css">`

### 5. All 157 Rust tests pass
- `cargo test` — all tests pass, including 10-clause `build_dark_mode_token_overrides`

## Dark-mode values

| Token | Light | Dark |
|-------|-------|------|
| `--bt-color-surface` | `#ffffff` | `#1e1e1e` |
| `--bt-color-surface-code` | `#f5f5f5` | `#2d2d2d` |
| `--bt-color-text-primary` | `#1a1a1a` | `#e0e0e0` |
| `--bt-color-text-secondary` | `#555555` | `#a0a0a0` |
| `--bt-color-status-pass` | `#0a7d28` | `#4caf50` |
| `--bt-color-status-fail` | `#c0202a` | `#f44336` |
| `--bt-color-status-running` | `#b06a00` | `#ff9800` |
| `--bt-color-brand` | `#1a1a1a` | `#e0e0e0` |
| `--bt-color-brand-hover` | `#000000` | `#ffffff` |
| `--bt-color-border` | `#d4d4d4` | `#808080` |
| `--bt-color-status-idle` | `#e8e8e8` | `#888888` |
| `--bt-color-success-bg` | `#e8f5e9` | `#1b3d1b` |
| `--bt-color-danger-bg` | `#fff3e0` | `#3d1b1b` |

All WCAG AA contrast ratios verified at build time via `build_dark_mode_token_overrides` clause 8.
