---
ac: 2
depends_on: [1]
risk: medium
status: complete
---

# AC-2: Remove clause-6 copy hack; add hermetic working-tree render test exercising real `quarto add` org/repo install path

## USER DECISION (approved)
Adopt working-tree install simulation: cp checkout's `_extensions/blendtutor` → `$TMP/_extensions/mcmullarkey/blendtutor/` in a NEW hermetic script `scripts/tests/test_quarto_install_render.sh`. This deviates from AC text "no copy workaround anywhere in the render path" — justified: the old hack copied to the WRONG path (`_extensions/blendtutor/`) to MASK the bug; the simulation copies to the RIGHT org/repo path to SIMULATE the install, exercising the PR's own code. Literal AC reading (render in clause-12's `quarto add` temp dir) was rejected: that dir contains GitHub-main published code, a structural sneaky-pass confirmed against ci.yml:131-137.

## Executable Spec
- **predicate:** P1–P10, all shell-assertable:
  - **P1** Hack removed: `grep -qE 'cp .*_extensions|demo-book/_extensions' scripts/tests/test_quarto_distribution.sh` → FAIL (grep exit ≠ 0).
  - **P2** No masking copy anywhere in render path: `rg 'cp .*_extensions|demo-book/_extensions' scripts/tests .github/workflows/ci.yml` → only permitted match: copy of working-tree `_extensions/blendtutor` into a mktemp project at `_extensions/mcmullarkey/blendtutor/`. Any copy to `_extensions/blendtutor/` (non-org) or `demo-book/_extensions/` → FAIL.
  - **P3** Working-tree source: install-render test derives extension from repo checkout; must NOT contain `quarto add mcmullarkey/blendtutor` in the render path.
  - **P4** Real install-path render: minimal `index.qmd` in `$TMP` with `filters: [_extensions/mcmullarkey/blendtutor/blendtutor.lua]`; `quarto render --to html` exit 0.
  - **P5** Filter ran: output HTML contains `bt-exercise`.
  - **P6** Asset resolved, not exit-0 alone: HTML references `_extensions/mcmullarkey/blendtutor/assets/styles.css` AND `test -f "$TMP/_extensions/mcmullarkey/blendtutor/assets/styles.css"`.
  - **P7** No old-path leak: HTML does NOT contain `_extensions/blendtutor/assets` (non-org prefix).
  - **P8** COI path covered: minimal .qmd div sets `coi="true"`; HTML references `_extensions/mcmullarkey/blendtutor/assets/coi-serviceworker.js` AND file exists in `$TMP`.
  - **P9** Demo-book side-effect free: after `test_quarto_distribution.sh` runs, `test ! -d demo-book/_extensions`; reworked clause 6 asserts render exit 0 AND `test -f` on the asset href target referenced in rendered demo-book HTML.
  - **P10** CI wiring: quarto-distribution job runs new render test; `quarto add` temp-dir step retained (distribution-only proof); `Render demo book` step retained; no `continue-on-error` / `|| true` on render steps.
- **probe:** `bash scripts/tests/test_quarto_distribution.sh && bash scripts/tests/test_quarto_install_render.sh` (local without quarto: render clauses SKIP per existing pattern; structural P1/P2/P3/P10 always run)
- **negative:** (a) render test inside clause-12 GitHub temp dir → tests published code → P3+P7 catch; (b) hack reintroduced at non-org path → P2/P7; (c) pre-AC-1 hardcoded paths → P6/P8 RED; (d) filter not loaded → no `bt-exercise` → P5 fails; (e) exit-0-only assertions → P6/P9 file checks catch (Quarto does not validate emitted hrefs at render).
- **verification:** code · shell test suite, CI-gated (quarto-distribution job)
- **fixture status:** rework `scripts/tests/test_quarto_distribution.sh:155-174` | NEW `scripts/tests/test_quarto_install_render.sh` | `scripts/tests/test_quarto_ux.py:392` (migrate href pin — coordinated with AC-1) | `.github/workflows/ci.yml:117-151`
- **rubric anchor:** §1 (org/repo install path encoded as invariant P6/P7), §2 (P9 — no side effects on repo tree), §3 (test simulates real install layout)

## Design Intent
- **Types / interfaces (§1):** org/repo install path is the encoded invariant; old non-org path asserted absent (P7).
- **Pure / effectful (§2):** render tests fully effectful; assertions on effect outputs; P9 guarantees zero residue on repo tree.
- **Boundary cuts (§3):** two concerns separated — distribution proof (clause 12, GitHub `quarto add`) vs working-tree render correctness (new hermetic test).
- **Module responsibility (§4):** `test_quarto_distribution.sh` owns distribution predicate; NEW `test_quarto_install_render.sh` owns working-tree real-install-path render.
- **Function discipline (§5):** new script does one thing: mktemp → mkdir org/repo path → cp working-tree extension → minimal .qmd (blendtutor div, language="r", coi="true") → render → assert P4–P8 → cleanup. ko/SKIP only, never weak-ok fallback.

## Technical Context
- Files touched: `scripts/tests/test_quarto_distribution.sh:155-174`, NEW `scripts/tests/test_quarto_install_render.sh`, `scripts/tests/test_quarto_ux.py:392`, `.github/workflows/ci.yml:117-151`.
- `quarto add mcmullarkey/blendtutor` in CI installs from GitHub main — render tests of PR code MUST source from working tree.
- Quarto does NOT fail render on missing asset files — exit 0 insufficient; every render assertion must file-check the referenced href target.
- ci.yml `Render demo book` step currently relies on clause-6 copy side effect; post-AC-1 demo-book render resolves `../_extensions/blendtutor/assets/...` from repo root — step passes without copy (P9 + AC-1 dependency).
- `demo-book/_extensions` not gitignored — after AC-2 the dir is never created (P9).

## Dependencies
- Depends on: AC-1 (P4–P8 RED until Lua path fix lands). Blocks: none.
- Conflict set: `scripts/tests/test_quarto_distribution.sh`, `.github/workflows/ci.yml`, `scripts/tests/test_quarto_ux.py`. Risk: medium.

### Progress
- Issue: #130
- [x] spec resolved — pending implementation (batch 2, after AC-1 merges) — 2026-08-02
- [x] RED: test_quarto_install_render.sh written; P1/P2/P10 fail vs hack state; P4-P8 pass (AC-1 landed) — 2026-08-02
- [x] GREEN: hack removed, clause 7 reworked (exit 0 + href file-check), clauses renumbered 7-15, header fixed, P9 side-effect guard, ci.yml wired — 2026-08-02
- [x] Negative controls: hack reintro → P1/P2/P9 RED; hardcoded paths → P6/P7/P8 RED — 2026-08-02
- [x] E2E evidence committed docs/evidence/130/ (probe, both suites, 2 negative controls, regression suite) — 2026-08-02
- [x] PR created (branch 130-real-install-render-test) — 2026-08-02

### Decision Log
- user — approved working-tree install simulation over literal AC reading (GitHub-main sneaky-pass rejected); clause-12 `quarto add` retained as distribution-only proof; new file test_quarto_install_render.sh.
- resolver — adopted B's P1–P10; A's literal approach rejected after verification against ci.yml:131-137.
- builder — P2/P3 structural self-scans false-positive on their own source lines (the literal regex/install-command strings appear in comments and grep commands); fixed by assembling patterns from string fragments (`CP_PAT="cp .*_""extensions"`) and rewording comments — the test file never contains the contiguous pattern text.
- builder — P9 clause-6 rework resolves href targets relative to the project dir (demo-book/), not _output/; post-AC-1 PANDOC_SCRIPT_FILE-relative hrefs anchor at demo-book/../_extensions → repo-root checkout files exist. Matches spec "resolves from repo root".
- builder — clause renumbering (Group 2 6→7 … 11→12, Group 3 12→13 … 14→15) kills the AC-3 carryover "Clause 6" collision; spec P9's "reworked clause 6" = new clause 7 (behavior contract unchanged).

### Surprises & Discoveries
- CI clause-12 `quarto add` installs from GitHub main, not working tree (confirmed ci.yml:131-137).
- ci.yml `Render demo book` step implicitly relied on the copy hack's side effect.
- demo-book/_output/ is gitignored but demo-book/.gitignore is tracked and quarto render appends `**/*.quarto_ipynb` to it — must revert before every commit (trap confirmed twice during evidence runs).
- `quarto render demo-book` emits `../_extensions/blendtutor/assets/...` hrefs verbatim (filter-injected RawBlock, not rewritten by Quarto); browser resolution from _output/ would 404 but project-dir resolution hits the repo checkout — Quarto does not validate emitted hrefs, hence the file-check.
- A structural guard that greps for a pattern must not contain that pattern in its own source — variable-fragment assembly needed (self-scan false positives burned 4 red iterations).
- test_quarto_ux.py renders coi-book fixtures leaving untracked *_files/ dirs in quarto-fixture/ — cleaned before commit (not gitignored).

### Idempotence & Recovery
- Safe retry: re-run both test scripts; mktemp fixtures self-contained; P9 asserts no repo-tree residue.
- Rollback: restore clause-6 copy block in test_quarto_distribution.sh, delete test_quarto_install_render.sh, revert ci.yml step wiring.
