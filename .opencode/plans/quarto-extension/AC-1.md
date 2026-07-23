---
ac: 1
depends_on: none
risk: low
status: complete
---

## AC-1: Scaffold Quarto extension skeleton with no-op filter, fixture .qmd, and CI render harness

### Executable Spec
- **predicate:** Extension skeleton passes ALL of: (1) `_extensions/blendtutor/_extension.yml` contains `contributes.filters: [blendtutor.lua]`; (2) `blendtutor.lua` is valid Pandoc filter returning doc unmodified; (3) `quarto render quarto-fixture/minimal.qmd` exits 0 and produces HTML containing BOTH standard markdown content ("Hello from Quarto") AND blendtutor fenced-div content ("add(a, b)" AND "stopifnot(add(1, 2) == 3)"); (4) CI uses `quarto-dev/quarto-actions/setup@v2` with NO `continue-on-error`.
- **probe:** `bash scripts/tests/test_quarto_render.sh` (7 assertions: contributes.filters present, render exits 0, standard content survived, blendtutor content survived, CI setup@v2, no continue-on-error)
- **negative:** `_extension.yml` present but omits `contributes.filters` → structurally dead extension, CI green but filter never loads. Also: declares filter but file missing → Quarto errors.
- **verification:** code
- **fixture status:** _extension.yml (NEW), blendtutor.lua (NEW), quarto-fixture/minimal.qmd (NEW), scripts/tests/test_quarto_render.sh (NEW), .github/workflows/ci.yml (append)
- **rubric anchor:** §1.5 (refuse cheapest fake), §1.5.1 (absence assertions need positive companions), §3.2 (extension↔CLI boundary), §4.1 (module header)

### Design Intent
- §1: _extension.yml schema IS the type — missing contributes.filters is illegal state Quarto can't detect
- §2: No-op filter pure by construction; CI shell is effectful layer
- §3: Extension at _extensions/blendtutor/, loaded by Quarto CLI not Rust CLI
- §4: blendtutor.lua header names WHAT (no-op filter scaffold), WHERE (_extensions/blendtutor/), NOT (no AST transform, no HTML deps, no runtime JS)
- §5: Single no-op function, one job

### Technical Context
- Files: _extension.yml, blendtutor.lua, quarto-fixture/minimal.qmd, scripts/tests/test_quarto_render.sh, .github/workflows/ci.yml
- Quarto auto-discovers extensions in _extensions/. .qmd must NOT declare filters: [blendtutor] in frontmatter — relies on auto-discovery
- 7 error sinks: Quarto not installed, missing contributes.filters, invalid Lua, fixture syntax, content mangled, div swallowed, CI continue-on-error

### Dependencies
- Depends on: none
- Blocks: AC-2 through AC-11
- Conflict set: _extensions/blendtutor/**, quarto-fixture/**, scripts/tests/test_quarto_render.sh, .github/workflows/ci.yml

### Progress
- [x] Red: wrote test_quarto_render.sh (7 assertions), confirmed all fail (files absent)
- [x] Implement: created _extension.yml, blendtutor.lua, minimal.qmd
- [x] CI: appended quarto-render job to ci.yml
- [x] Green: all 10 assertions pass (quarto 1.6.42 local install)
- [x] Negative control: removed contributes.filters → assertion 1 fails
- [x] Committed: test(red) 45534f3, feat 984921e

### Decision Log
- 2026-07-22 — Filter wiring: take B's contributes.filters (extension-idiomatic) over A's .qmd frontmatter filters (bypasses extension mechanism)
- 2026-07-22 — Fixture name: take A's quarto-fixture/minimal.qmd over B's exercise.qmd (dedicated fixture dir more organized)

### Surprises & Discoveries
- Pandoc syntax highlighting wraps code tokens in `<span>` tags, breaking literal `grep -qF 'stopifnot(add(1, 2) == 3)'` on raw HTML. Fix: strip HTML tags via `sed 's/<[^>]*>//g'` before content-matching assertions. Without this, the stopifnot assertion false-fails even though content is present.
- YAML `_extension.yml` uses nested form (`contributes:\n  filters:\n    - blendtutor.lua`), not inline `contributes.filters: [blendtutor.lua]`. Test must check for `contributes` + `filters` keys separately, not literal `contributes.filters:` string.
- Quarto render produces `quarto-fixture/minimal_files/` directory (libs) alongside the HTML. Added `*_files/` to .gitignore.
- Quarto not installed on macOS without sudo (brew cask needs installer pkg). Downloaded pre-built binary tarball to temp dir for local testing.

### Idempotence & Recovery
- Safe retry: re-run scripts/tests/test_quarto_render.sh
- Rollback: delete _extensions/blendtutor/ directory
