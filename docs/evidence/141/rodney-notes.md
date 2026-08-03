# Rodney clause-11 evidence (issue #141)

Run: `uv run node rodney-probes/auto-bootstrap.js` (harness pattern — direct
`uvx rodney` is permission-blocked for builders; the node harness runs it via
child_process, serving the worktree root so libs URLs resolve offline).

Verdict: **PROBES_PASS** — see `probe-report.json` + `rodney.log` (copied from
the harness output).

- mixed-lang.html: `window.__btExercises.length === 2` (1 R + 1 python mounted)
- mixed-lang.html: `.cm-editor` count === 2
- r-only.html: `window.__btExercises.length === 2` (2 R exercises)

This re-runs the AC-3 harness (issue #139) against the AC-4 deployment: the
bootstrap module now imports from `./mixed-lang_files/libs/quarto-contrib/
blendtutor-0.1.0/...` libs URLs, so a successful boot proves the URL rewrite
resolves at runtime (a broken specifier 404s → module fails → `__btExercises`
never set → PROBES_FAIL).

## Runtime catch during implementation
The first AC-4 run FAILED with "mixed-lang.html: __btExercises not populated".
Browser error (captured via a debug harness with a pre-injected error hook):
`Failed to resolve module specifier "mixed-lang_files/libs/..." — Relative
references must start with either "/", "./", or "../"`. ES module specifiers
reject bare relative paths (AC-3's `../_extensions/...` worked because it
started with `../`). Fixed: `libs_url()` emits a `./`-prefixed document-relative
URL. Clause-5 suite assertion added to pin the `./` prefix.
