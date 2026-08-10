# Decomposition: quarto-extension-install-path

**fast-track** — production bug: published extension fails at render time for every user who installs via `quarto add mcmullarkey/blendtutor`.

## Feature Goal

Fix the Quarto extension so it renders correctly regardless of install path. Today `blendtutor.lua` hardcodes `_extensions/blendtutor/assets/...` (lines 54/58); `quarto add mcmullarkey/blendtutor` installs to `_extensions/mcmullarkey/blendtutor/`, so Quarto's resource resolution lstat-fails on the nonexistent hardcoded path. The fix resolves asset paths relative to the filter script's own location (`PANDOC_SCRIPT_FILE`), removes the CI copy hack that masks the bug (`test_quarto_distribution.sh` clause 6), adds a render test against the REAL `quarto add` install path, and corrects the README install-path claim.

## AC Table

| AC | Description | Dependencies | Conflict Set | Risk |
|----|-------------|--------------|--------------|------|
| 1  | Resolve asset paths in `blendtutor.lua` relative to the filter script's own location (e.g. `PANDOC_SCRIPT_FILE` directory + `assets/`) instead of the hardcoded `_extensions/blendtutor/` prefix, so the filter works under in-repo `_extensions/blendtutor/`, installed `_extensions/mcmullarkey/blendtutor/`, and the demo-book `../_extensions/blendtutor/` reference; update any existing tests that assert the old hardcoded href/src string. | none | `_extensions/blendtutor/blendtutor.lua`, `scripts/tests/test_quarto_ux.py` (line ~392 asserts `href="_extensions/blendtutor/assets/styles.css"`), `rodney-probes/exercise-ux.js` (line ~160 path-handling comment) | med |
| 2  | Remove the clause-6 copy hack in `test_quarto_distribution.sh` (lines ~155-164) and add a render test that exercises the REAL `quarto add` install path — render a minimal `.qmd` (with a blendtutor div) in the temp dir where CI already runs `quarto add mcmullarkey/blendtutor`, asserting exit 0 with assets resolved from `_extensions/mcmullarkey/blendtutor/`; no copy workaround anywhere in the render path. | AC-1 | `scripts/tests/test_quarto_distribution.sh`, `.github/workflows/ci.yml` (quarto-distribution job, lines ~133-139), possibly a new minimal fixture `.qmd` under `quarto-fixture/` or inline heredoc in CI | med |
| 3  | Correct the README install section (line ~172) that claims the extension installs into `_extensions/blendtutor/`; state the actual path `_extensions/mcmullarkey/blendtutor/` and that asset resolution is install-path-independent. | none | `README.md` | low |

## Dependency DAG

```
AC-1 → AC-2
AC-3 (independent)
```

AC-2's real-install-path render test only passes once AC-1's path fix lands — serial dependency.
AC-3 is a doc fix, no shared files with AC-1/AC-2.

## Hot Conflict Files

- none identified — AC-1 (`blendtutor.lua`, `test_quarto_ux.py`), AC-2 (`test_quarto_distribution.sh`, `ci.yml`), AC-3 (`README.md`) touch disjoint file sets.

## Suggested Batch Schedule

- Batch 1 (parallel): AC-1, AC-3
- Batch 2 (sequential): AC-2 (depends on AC-1; without the Lua fix the new render test would fail red)

## Open Questions

- [needs-clarification] demo-book references the filter via a path OUTSIDE the project root (`filters: [../_extensions/blendtutor/blendtutor.lua]` in `demo-book/_quarto.yml`). With script-relative resolution, asset paths become `../_extensions/blendtutor/assets/...` — outside the Quarto project root. It is unverified whether Quarto copies/serves resources outside the project root. Speculators must pick one: (a) verify Quarto handles `..`-relative asset paths at render time (keep demo-book as-is), or (b) change demo-book to a short-name extension reference with the extension present under `demo-book/_extensions/` (but this reintroduces a copy — must NOT be a silent test hack; if used, document it as the local-dev install pattern and have AC-2 own real-install-path coverage). Default recommendation: (a) first; fall back to (b) only if Quarto rejects out-of-root resources.
- [spec-phase note, not user-blocking] The `<script src>` / `<link href>` values are BOTH Quarto resource-path inputs (lstat'd at render) and browser-facing URLs. Speculators must confirm the script-relative path stays project-relative after resolution so the emitted HTML URL matches where Quarto copies the asset into `_output`/`_site`.
- none requiring user input — decomposition otherwise complete.
