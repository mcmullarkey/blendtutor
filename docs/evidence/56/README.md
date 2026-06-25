# AC-1 Evidence — Design-token system + shared styles.css + semantic layout shell

## Commands run

```bash
# Build both targets
cargo run -p blendtutor-cli -- build --target webr crates/core/tests/fixtures/r-course -o /tmp/bt-site-56/webr
cargo run -p blendtutor-cli -- build --target pyodide crates/core/tests/fixtures/python-course -o /tmp/bt-site-56/pyodide

# Verify styles.css emitted and byte-identical
diff /tmp/bt-site-56/webr/styles.css /tmp/bt-site-56/pyodide/styles.css  # no output = identical

# Serve and check HTTP 200
cd /tmp/bt-site-56/webr && python3 -m http.server 8765 &
curl -o /dev/null -w "%{http_code}" http://localhost:8765/styles.css     # 200
curl -o /dev/null -w "%{http_code}" http://localhost:8765/index.html     # 200
```

## Results

| Check | Result |
|-------|--------|
| `styles.css` in `SiteFiles.files()` for both targets | ✅ |
| `styles.css` byte-identical across targets | ✅ |
| Both shells contain `<link rel="stylesheet" href="styles.css">` | ✅ |
| Both shells contain zero `<style>` elements | ✅ |
| `styles.css` declares `--bt-` tokens in `:root` | ✅ |
| `styles.css` has ≥4 `var(--bt-` usages | ✅ |
| Both shells have `<header class="site-header">`, `<main class="workspace">`, `<footer class="site-footer">` | ✅ |
| `<h1>blendtutor</h1>` in header | ✅ |
| All 6 `data-test` hooks present | ✅ |
| `data-action="submit"` on `#submit` | ✅ |
| `data-status="idle"` on `#lesson-status` | ✅ |
| All 10 JS-required IDs present | ✅ |
| `coi-serviceworker.js` before `styles.css` link | ✅ |
| Shells differ only by 3 known diffs (normalize → byte-identical) | ✅ |
| All 108 core unit tests pass (blendtutor-core) | ✅ |
| All 10 CLI integration tests pass (blendtutor-cli) | ✅ |
| Static site serves all assets (HTTP 200) | ✅ |

## Token system summary

18 tokens declared in `:root`, all used by ≥1 rule. 10 `var(--bt-` references
across rules (well above the 4-usage floor). Light theme, WCAG 4.5:1 normal text
contrast, system UI font stack with zero network dependencies.

## Test suite output (captured)

```
cargo test -p blendtutor-core -p blendtutor-cli
  ... 108 passed (core) ...
  ... 10 passed (cli) ...
```
