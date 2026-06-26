# Issue #64 — E2E Evidence

## AC
Vendor a pre-built CodeMirror 6 bundle (core + lang-r + lang-python + commands + language) as a committed static asset at `crates/core/assets/shared/codemirror.js`, wired into the site build pipeline via `include_str!` + `assemble`.

## Verification medium: `code` (cargo test)

## Evidence

### 1. Integration test (8-clause predicate) — PASS
See `run.log`. The test `plan_site_emits_vendored_codemirror_bundle` exercises all 8 clauses:
1. `codemirror.js` present in SiteFiles for both `BuildTarget::Webr` and `BuildTarget::Pyodide`
2. byte-identical across targets (shared, not forked)
3. `contents.len() > 10_000` (catches empty/comment-only stub)
4. contains `EditorView` (CM6 core ESM export)
5. contains evidence of `rLanguage` (lang-r) + `python` (lang-python) + `lineNumbers` + `highlightActiveLine` + `bracketMatching` + `indentWithTab`
6. no `new Worker(` (COOP/COEP isolation)
7. deterministic order: after `styles.css` AND before any `lessons/` file (full positional invariant)
8. `CODEMIRROR_JS` const compiles (proved by test running — `include_str!` refuses missing asset at compile time)

Full suite: 110 passed, 0 failed.

### 2. Binary compiles with embedded bundle — PASS
See `build.log`. `cargo build` succeeds — the 687KB bundle is embedded via `include_str!` into the binary's `.rodata`.

### 3. Negative control — stub bundle caught
Replaced `codemirror.js` with an 8-byte stub (`// stub\n`). Test failed on clause 3:
```
codemirror.js must be a real bundle (>10_000 bytes), got 8 bytes
```
Real bundle restored (687180 bytes), test passes again.

### 4. Bundle authoring (one-time, NOT build-time codegen)
The bundle was produced via one-time esbuild authoring — NOT part of `blendtutor build` (ADR-0008: no build-time code execution). Steps:

```bash
mkdir -p /tmp/cm6-bundle && cd /tmp/cm6-bundle
npm init -y
npm install @codemirror/view @codemirror/state @codemirror/commands \
  @codemirror/language @codemirror/lang-python codemirror-lang-r esbuild
# entry.js re-exports: EditorView, lineNumbers, highlightActiveLine (view),
#   bracketMatching (language), indentWithTab (commands), r (lang-r), python (lang-python)
npx esbuild entry.js --bundle --format=esm \
  --outfile=crates/core/assets/shared/codemirror.js
```

**Note on `@codemirror/lang-r`:** The official `@codemirror/lang-r` package does not exist on npm. The community package `codemirror-lang-r@0.1.1` (depends on `lezer-r` parser + `@codemirror/language`) was used instead — it exports the same `r()` function returning a `LanguageSupport`. This is a deviation from the issue's literal `npm install @codemirror/lang-r` instruction, reported to Director.

### 5. Bundle stats
- Size: 687180 bytes (671 KB)
- Format: ESM (`export { EditorView, bracketMatching, highlightActiveLine, indentWithTab, lineNumbers, python, r }`)
- No `new Worker(` (verified: 0 occurrences)
- No build script added to Cargo.toml
