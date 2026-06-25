# Evidence: Issue #57 — Lesson workspace styling

## Summary

AC-2 (lesson workspace styling) implemented: 4 new design tokens in `:root` + workspace
component rules + pill status badge with 4 `data-status` states. Pure CSS — no JS changes.

## Test results

All 173 tests pass across both crates (0 failures):

```
blendtutor-core: 109 unit tests + 11 integration tests = 120
blendtutor-cli:  9 unit tests + 44 integration tests = 53
Total: 173 passed, 0 failed
```

## Build verification

### webr target (R course fixture)

```shell
$ cargo run -p blendtutor-cli -- build --target webr \
    crates/core/tests/fixtures/r-course -o /tmp/bt-site-57-webr
built 2 lesson(s) for webr into /tmp/bt-site-57-webr
```

### pyodide target (Python course fixture)

```shell
$ cargo run -p blendtutor-cli -- build --target pyodide \
    crates/core/tests/fixtures/python-course -o /tmp/bt-site-57-pyodide
built 1 lesson(s) for pyodide into /tmp/bt-site-57-pyodide
```

## styles.css byte-identity across targets

```
$ diff /tmp/bt-site-57-webr/styles.css /tmp/bt-site-57-pyodide/styles.css
  (no output — files identical)
Byte-identical: YES (both 7401 bytes)
```

## Workspace selectors present in emitted CSS

| Selector | Present |
|---|---|
| `.lesson-picker` | ✓ (3 occurrences) |
| `#lesson-title` | ✓ |
| `#lesson-prompt` | ✓ |
| `#submission` | ✓ (2: rule + focus) |
| `.controls` | ✓ (3: rule + both margins) |
| `#run` | ✓ (2: rule + hover) |
| `#submit` | ✓ (2: rule + hover) |
| `#lesson-status` | ✓ (5: base rule + 4 data-status rules) |
| `#output` | ✓ |

## Token usage

- `var(--bt-` total across entire file: 77
- `#lesson-status[data-status="..."]` rules: 4 (idle, running, pass, fail)
- `.status-*` class selectors: 0 (none — attribute selectors only)

## Server verification

```
HTTP http://localhost:8765/index.html → 200 (1928 bytes)
HTTP http://localhost:8765/styles.css  → 200 (7401 bytes)
```

All 9 workspace element IDs/classes present in HTML:
```html
class="workspace", class="lesson-picker", class="controls"
id="lesson-title", id="lesson-prompt", id="submission"
id="run", id="submit", id="lesson-status", id="output"
```

## Commits

```
fbe270f refactor: cargo fmt formatting on AC-2 test files
4f958e0 feat: style lesson workspace with token-driven rules + pill status badge
ee0facc test(red): add workspace CSS contract assertions per AC-2 spec
```
