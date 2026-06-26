# Issue #64 — E2E Evidence

## AC
Vendor a pre-built CodeMirror 6 bundle (core + lang-r + lang-python + commands +
language) as a committed static asset at `crates/core/assets/shared/codemirror.js`,
wired into the site build pipeline via `include_str!` + `assemble`.

## Verification medium: `code` (cargo test)

## Evidence

### 1. Integration test (8-clause predicate) — PASS

```
$ uv run cargo test -p blendtutor-core --lib plan_site_emits_vendored_codemirror_bundle -- --nocapture

running 1 test
test site::tests::plan_site_emits_vendored_codemirror_bundle ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 109 filtered out
```

Full suite: 110 passed, 0 failed (see `run.log`).

The test `plan_site_emits_vendored_codemirror_bundle` exercises all 8 clauses:
1. `codemirror.js` present in SiteFiles for both `BuildTarget::Webr` and `BuildTarget::Pyodide`
2. byte-identical across targets (shared, not forked — §4.2)
3. `contents.len() > 10_000` (catches empty/comment-only stub)
4. contains `EditorView` (CM6 core ESM export — catches wrong-library/UMD)
5. contains evidence of `rLanguage` (lang-r) + `python` (lang-python) + `lineNumbers`
   + `highlightActiveLine` + `bracketMatching` + `indentWithTab`
6. no `new Worker(` (COOP/COEP isolation — no web workers)
7. deterministic order: after `styles.css` AND before any `lessons/` file
   (full positional invariant, not weak "after styles.css" proxy)
8. `CODEMIRROR_JS` const compiles — proved by the test running (`include_str!`
   refuses a missing asset at compile time — §1.3.1)

### 2. Bundle authoring (one-time, NOT build-time codegen)

The bundle was produced via one-time esbuild authoring — NOT part of
`blendtutor build` (ADR-0008: no build-time code execution). The committed
artifact at `crates/core/assets/shared/codemirror.js` is embedded at compile
time via `include_str!`, the same pattern as `STYLES_CSS` and
`COI_SERVICEWORKER_JS`. No build script was added to `Cargo.toml`.

**Note on `@codemirror/lang-r`:** The official `@codemirror/lang-r` package does
not exist on npm. The community package `codemirror-lang-r@0.1.1` (depends on
`lezer-r` parser + `@codemirror/language`) was used instead — it exports the
same `r()` function returning a `LanguageSupport`. Reported to Director.

### 3. Binary compiles with embedded bundle — PASS

```
$ uv run cargo build -p blendtutor-core

   Compiling blendtutor-core v0.1.0
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 2.65s
```

The 687KB bundle is embedded via `include_str!` into the binary's `.rodata`.
See `build.log`.

### 4. Bundle stats
- Size: 687180 bytes (671 KB)
- Format: ESM (`export { EditorView, bracketMatching, highlightActiveLine, indentWithTab, lineNumbers, python, r }`)
- No `new Worker(` (verified: 0 occurrences)
- No build script added to Cargo.toml
