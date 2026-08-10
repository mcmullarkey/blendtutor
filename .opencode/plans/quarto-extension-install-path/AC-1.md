---
ac: 1
depends_on: []
risk: medium
status: complete
---

# AC-1: Script-relative asset path resolution in `blendtutor.lua` via `PANDOC_SCRIPT_FILE`

## Executable Spec
- **predicate:** all of:
  1. `quarto render quarto-fixture/ux.qmd --to html` exits 0; `quarto-fixture/ux.html` contains exact `href="../_extensions/blendtutor/assets/styles.css"`.
  2. `quarto render quarto-fixture/coi-true.qmd --to html` exits 0; output contains exact `src="../_extensions/blendtutor/assets/coi-serviceworker.js"`.
  3. Installed-layout: filter copied to `<tmp>/_extensions/mcmullarkey/blendtutor/blendtutor.lua` + qmd referencing it renders exit 0 AND output contains `href="_extensions/mcmullarkey/blendtutor/assets/styles.css"` AND NOT `href="_extensions/blendtutor/assets/styles.css"`.
  4. Emitted asset URLs are project-relative: match `^\.\.?/`, no leading `/`, no `//`, no `file:`, no drive letter, no backslash (rejects absolute `PANDOC_SCRIPT_FILE` leak).
  5. Resolving each emitted href from the rendered HTML file's directory yields an existing file on disk.
  6. `blendtutor.lua` contains no literal `_extensions/blendtutor/` string constant (source grep zero).
  7. `scripts/tests/test_quarto_ux.py:392` updated to assert the `../_extensions/...` href AND clauses 4–5 behaviorally — not constant-equality.
- **probe:**
  ```bash
  cd "$(git rev-parse --show-toplevel)"
  quarto render quarto-fixture/ux.qmd --to html && quarto render quarto-fixture/coi-true.qmd --to html
  uv run python scripts/tests/test_quarto_ux.py
  ! grep -n '"_extensions/blendtutor/' _extensions/blendtutor/blendtutor.lua
  T=$(mktemp -d) && mkdir -p "$T/_extensions/mcmullarkey/blendtutor/assets" && cp _extensions/blendtutor/blendtutor.lua "$T/_extensions/mcmullarkey/blendtutor/" && cp _extensions/blendtutor/assets/styles.css "$T/_extensions/mcmullarkey/blendtutor/assets/" && printf -- '---\nfilters: [_extensions/mcmullarkey/blendtutor/blendtutor.lua]\n---\n\n::: {.blendtutor language="r"}\nWrite code.\n\n```r\nx <- 1\n```\n:::\n' > "$T/test.qmd" && (cd "$T" && quarto render test.qmd --to html 2>/dev/null) && grep -qF 'href="_extensions/mcmullarkey/blendtutor/assets/styles.css"' "$T/test.html" && ! grep -qF 'href="_extensions/blendtutor/assets/styles.css"' "$T/test.html" && echo PASS || echo FAIL
  ```
- **negative:** (a) filter at installed path but href still hardcoded `_extensions/blendtutor/assets/...` → browser 404; (b) naive `PANDOC_SCRIPT_FILE:match("^(.*[/\\])")` when Quarto passes absolute script path (by-name `_extension.yml` install) → emits `/abs/.../assets/styles.css`, lstat passes, browser 404s — clause 4 rejects; (c) hardcoded constant retained as fallback — clause 6 rejects; (d) vacuous constant-equality test rewrite — clauses 6+7 reject.
- **verification:** code · real `quarto render` + grep + Python asserts. CI must run it (skip-if-missing allowed only locally).
- **fixture status:** EDIT `_extensions/blendtutor/blendtutor.lua:54,58` + `:385–408`; EDIT `scripts/tests/test_quarto_ux.py:392`; EDIT `rodney-probes/exercise-ux.js:158–183`; EDIT `docs/adr/0015-opt-in-coi-cross-origin.md:56`; existing `quarto-fixture/ux.qmd`, `coi-true.qmd` unchanged; NEW temp-dir fixture created inline by probe (installed layout).
- **rubric anchor:** §1.5, §3.2, §5

## Design Intent
- **Types / interfaces (§1):** Replace hardcoded `COI_SCRIPT_PATH`/`STYLES_CSS_PATH` constants with derived path from `PANDOC_SCRIPT_FILE`. Invariant: emitted URL = script's location expressed relative to project root, always matching actual install location.
- **Pure / effectful (§2):** Pure `resolve_asset_path(script_file, filename) -> string` (incl. absolute→project-relative conversion: prefer `quarto.utils.relative_to` under Quarto; pandoc fallback strips CWD prefix or slices from `_extensions/`). Effectful emission stays in `Pandoc()`.
- **Boundary cuts (§3):** Filter assumes nothing about install location — derives from own location. Behavior contract: in-repo → `_extensions/blendtutor/assets/...`; installed → `_extensions/mcmullarkey/blendtutor/assets/...`; demo-book/fixture → `../_extensions/blendtutor/assets/...`. All browser-usable.
- **Module responsibility (§4):** Fix contained in `blendtutor.lua` — no new module.
- **Function discipline (§5):** Single-responsibility resolver function; Windows `[/\\]` dir-extraction + emitted href normalized to `/` (code-review requirement, not test).

## Technical Context
- **Files likely touched:** `_extensions/blendtutor/blendtutor.lua` (54/58 constants, 385–408 usage), `scripts/tests/test_quarto_ux.py` (~392), `rodney-probes/exercise-ux.js` (158–183 symlink logic + comment), `docs/adr/0015-opt-in-coi-cross-origin.md:56` (doc path). `test_coi_filter.sh` verify-only (greps filename only). Static `tests/fixtures/*.html` unaffected.
- **Architecture notes:**
  - `PANDOC_SCRIPT_FILE` two forms (verified empirically, quarto 1.10.18): explicit YAML path → as written, relative to qmd dir; by-name `_extension.yml` contributes.filters → ABSOLUTE path. Absolute MUST be converted project-relative — Quarto does not rewrite `in-header` hrefs.
  - Test migration decision: extend existing `test_quarto_ux.py` (clauses 6–7); no separate `test_asset_paths.py` required.
  - 20 qmd files in quarto-fixture reference filter via `../_extensions/` paths; only `test_quarto_ux.py:392` asserts full href string.
  - **Out of scope (future work):** coi-serviceworker.js scope defaults to script URL dir; nested install paths leave page outside SW scope → COI headers never applied. Pre-existing, orthogonal to AC-1. Flag for future COI AC.
  - Demo-book full render belongs to AC-2; `quarto-fixture/ux.qmd` exercises identical `../` shape for AC-1.

## Dependencies
- Depends on: none. Blocks: AC-2. Conflict set: `_extensions/blendtutor/blendtutor.lua`. Risk: medium.

### Progress
- Issue: #129
- [x] spec resolved — pending implementation
- [x] test(red): behavioral asset-path checks added (46 total) — 11 new checks failed pre-fix — b62895f
- [x] docs: ADR-0018 script-relative asset path resolution — 19dbfb6
- [x] feat: resolve_asset_path() in blendtutor.lua, PANDOC_SCRIPT_FILE-derived constants — 532f3fa (green: 46/46 + full probe PASS)
- [x] docs: rodney-probes symlink comment + ADR-0015:56 updated — 9a7dba0
- [x] test: clause-4 guard refined (bare relative hrefs allowed, absolute leak rejected) — 03268d4
- [x] test: E2E evidence committed to docs/evidence/129/ — 18efda0
- [x] Regression: test_coi_filter.sh 15/15, test_quarto_filter.sh 17/17, test_quarto_render.sh 12/12, test_quarto_distribution.sh 23/23, test_sync_assets.sh 12/12, test_quarto_feedback.py 32/32, test_pyodide_adapter.sh 11/11, cargo nextest 301/301, demo-book render OK
- [x] Spec probe (all 7 clauses) PASS

### Decision Log
- resolver — probe set = union (B's in-repo 6-clause contract + A's installed-layout temp probe as clause 3); test migration extends existing test_quarto_ux.py (no new file); SW scope finding out of scope (future COI AC).
- builder — quarto.utils.relative_to does NOT exist in quarto 1.10.18 (verified: quarto.utils exposes resolve_path, resolve_path_relative_to_document only); pandoc.path is POSIX-only (Windows backslash → directory "." and is_absolute false). Chose own `[/\\]` dir-extraction + slice-from-_extensions/ for absolute paths, PWD-prefix strip last resort, backslash→/ normalization. Documented ADR-0018.
- builder — clause-4 guard: bare relative hrefs (`_extensions/mcmullarkey/...` installed layout) are valid project-relative URLs; guard rejects absolute leaks (leading /, //, file:, drive letter, backslash), not dot-prefixed-only shapes.

### Surprises & Discoveries
- AC-1 (quarto-extension-install-path): `quarto.utils.relative_to` — proposed in the design intent — does not exist in quarto 1.10.18; the `quarto.utils` table exposes `resolve_path`/`resolve_path_relative_to_document` but no `relative_to`. Switched to `pandoc.path`? No — `pandoc.path` is POSIX-only and mis-parses Windows backslash paths (`directory("C:\\...")` → `"."`). Final design: own `[/\\]` regex dir-extraction + slice from `_extensions/` for absolute PANDOC_SCRIPT_FILE, PWD-prefix strip as last resort.
- AC-1 (quarto-extension-install-path): by-name `_extension.yml` installs pass an ABSOLUTE PANDOC_SCRIPT_FILE whose root can differ from `$PWD` on macOS (`/private/var/...` vs `/var/...` — /tmp symlink) — a naive CWD-prefix strip fails; slicing from `_extensions/` is the robust conversion.
- AC-1 (quarto-extension-install-path): demo-book render emits `../_extensions/blendtutor/assets/styles.css` from `demo-book/_output/` — resolves to repo-root `_extensions/`, so the pre-existing `test_quarto_distribution.sh` clause-6 copy hack becomes harmless (AC-2 removes it).
- AC-1 (quarto-extension-install-path): `quarto render demo-book` mutated `demo-book/.gitignore` (added `**/*.quarto_ipynb`) — reverted; unrelated side effect, not committed.
- AC-1 (quarto-extension-install-path): spec clause 4's `^\.\.?/` regex contradicts clause 3's exact `href="_extensions/mcmullarkey/..."` (no dot prefix); interpreted clause 4 as the absolute-leak guard (no leading /, no //, no file:, no drive letter, no backslash) and encoded it as a function in the test.

### Idempotence & Recovery
- Safe retry: re-run probe commands; temp dir fixtures are self-contained.
- Rollback: `git checkout -- _extensions/blendtutor/blendtutor.lua scripts/tests/test_quarto_ux.py rodney-probes/exercise-ux.js docs/adr/0015-opt-in-coi-cross-origin.md`
