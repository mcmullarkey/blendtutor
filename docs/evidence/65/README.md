# Issue #65 — E2E Evidence

Date: 2026-06-25
Branch: 65-integrate-cm6-editor
AC: Replace textarea with CodeMirror 6 editor in lesson sites (AC-2 from code-editor)

## Verification medium

`code` (build-time clauses 1-7) + `rodney` (runtime clauses 8-13).

- Build-time clauses 1-7: pinned by Rust tests in `crates/core/src/site/mod.rs`
  (`plan_site_shells_contain_semantic_regions` extended for clause 2;
  `plan_site_shells_load_codemirror_as_import` new for clauses 3-7; clause 1
  already pinned by `plan_site_emits_vendored_codemirror_bundle` from AC-1).
- Runtime clauses 8-13: rodney probes authored at
  `rodney-probes/cm6-integration.js`. Run by @builder-vision-probe (the coding
  builder does NOT run rodney).

## Test suite

`uv run cargo test -p blendtutor-core` — 130 tests pass (111 lib + 2 + 3 + 9 + 5).
Full output: `test-suite.log`.

Key new/extended tests:
- `plan_site_shells_contain_semantic_regions` — clause 2: both shells have
  `<div id="submission">` and NOT `<textarea id="submission">`.
- `plan_site_shells_load_codemirror_as_import` — clauses 3-7: data-test/id
  preserved + runSubmission contract; static `import { EditorView }` from
  codemirror.js; feedback.js reads via getSubmission() not .value; no innerHTML
  in runner core; both adapters pass `language:` field.

## Lint

- `uv run cargo fmt --check` — clean.
- `uv run cargo clippy -p blendtutor-core --lib --tests -- -D warnings` — clean.

## Build smoke test (both targets)

- webr: `uv run cargo run -- build --target webr crates/core/tests/fixtures/r-course -o /tmp/bt-webr-test` → `built 2 lesson(s) for webr`. Log: `build-webr.log`.
- pyodide: `uv run cargo run -- build --target pyodide crates/core/tests/fixtures/python-course -o /tmp/bt-pyodide-test` → `built 1 lesson(s) for pyodide`. Log: `build-pyodide.log`.

### Built output verification

Both built `index.html` files contain `<div id="submission"` (not textarea):
```
$ grep -o '<\(div\|textarea\) id="submission"' /tmp/bt-webr-test/index.html
<div id="submission"
$ grep -o '<\(div\|textarea\) id="submission"' /tmp/bt-pyodide-test/index.html
<div id="submission"
```

`codemirror.js` present in both built sites (687180 bytes, byte-identical):
```
$ ls -la /tmp/bt-webr-test/codemirror.js /tmp/bt-pyodide-test/codemirror.js
-rw-r--r--  687180  /tmp/bt-pyodide-test/codemirror.js
-rw-r--r--  687180  /tmp/bt-webr-test/codemirror.js
```

`lesson-runner-core.js` has the static import:
```
$ grep -c 'import { EditorView' /tmp/bt-webr-test/lesson-runner-core.js
1
```

Per-target language field:
```
$ grep 'language:' /tmp/bt-webr-test/lesson-runner.js
  language: "r",
$ grep 'language:' /tmp/bt-pyodide-test/lesson-runner.js
  language: "python",
```

`feedback.js` reads via getSubmission (2 occurrences — definition comment + call):
```
$ grep -c 'getSubmission' /tmp/bt-webr-test/feedback.js
2
```

## Runtime verification (rodney)

Rodney probes for clauses 8-13 (bidirectional sync write/read, R/Python mono
font + syntax tokens, no editor leak after switch cycle, graceful degradation)
are authored at `rodney-probes/cm6-integration.js`. They are executed by
@builder-vision-probe against served built sites — NOT by the coding builder.
