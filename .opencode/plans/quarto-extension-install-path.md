# Master Plan: quarto-extension-install-path

**fast-track** — production bug: published extension fails at render time for every user who installs via `quarto add mcmullarkey/blendtutor`.

## Feature Goal

Fix the Quarto extension so it renders correctly regardless of install path. Today `blendtutor.lua` hardcodes `_extensions/blendtutor/assets/...` (lines 54/58); `quarto add mcmullarkey/blendtutor` installs to `_extensions/mcmullarkey/blendtutor/`, so Quarto's resource resolution lstat-fails on the nonexistent hardcoded path. The fix resolves asset paths relative to the filter script's own location (`PANDOC_SCRIPT_FILE`), removes the CI copy hack that masks the bug (`test_quarto_distribution.sh` clause 6), adds a render test against the REAL `quarto add` install path, and corrects the README install-path claim.

## User Decisions

1. **AC-2:** Working-tree install simulation approved — cp checkout's `_extensions/blendtutor` → `$TMP/_extensions/mcmullarkey/blendtutor/` in a NEW hermetic script `scripts/tests/test_quarto_install_render.sh`. This deviates from AC text "no copy workaround anywhere in the render path" — justified: the old hack copied to the WRONG path (`_extensions/blendtutor/`) to MASK the bug; the simulation copies to the RIGHT org/repo path to SIMULATE the install, exercising the PR's own code. Literal AC reading (render in clause-12's `quarto add` temp dir) was rejected: that dir contains GitHub-main published code, a structural sneaky-pass confirmed against ci.yml:131-137.
2. **AC-3:** ADR-0017:54 stale line IS in scope: annotate it (one-line note that CI actually asserts the org/repo path `_extensions/mcmullarkey/blendtutor/`). ADR body history left intact.
3. **AC-1 merges before AC-3:** the install-path-independence sentence is only true once AC-1 lands. AC-3 may be BUILT in parallel (batch 1) but merges after AC-1.

## Dependencies

```
AC-1 → AC-2
AC-3 (independent build; merges after AC-1)
```

- AC-2's real-install-path render test only passes once AC-1's path fix lands — serial dependency.
- AC-3 is a doc fix; buildable in parallel with AC-1 (batch 1), but must MERGE after AC-1 (semantic — independence sentence documents AC-1 behavior).
- Hot conflict files: none — AC-1 (`blendtutor.lua`, `test_quarto_ux.py`, `rodney-probes/exercise-ux.js`, ADR-0015), AC-2 (`test_quarto_distribution.sh`, NEW `test_quarto_install_render.sh`, `ci.yml`), AC-3 (`README.md`, `docs/adr/0017`, `test_quarto_distribution.sh` Group 1 shared with AC-2 — serial batches avoid conflict).

## Batch Schedule

- **Batch 1 (parallel):** AC-1, AC-3
- **Batch 2 (sequential):** AC-2 (depends on AC-1; without the Lua fix the new render test would fail red)

---

## AC-1: Script-relative asset path resolution in `blendtutor.lua` via `PANDOC_SCRIPT_FILE`

---
ac: 1
depends_on: []
risk: medium
status: spec
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
- [ ] spec resolved — pending implementation

### Decision Log
- resolver — probe set = union (B's in-repo 6-clause contract + A's installed-layout temp probe as clause 3); test migration extends existing test_quarto_ux.py (no new file); SW scope finding out of scope (future COI AC).

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run probe commands; temp dir fixtures are self-contained.
- Rollback: `git checkout -- _extensions/blendtutor/blendtutor.lua scripts/tests/test_quarto_ux.py rodney-probes/exercise-ux.js docs/adr/0015-opt-in-coi-cross-origin.md`

---

## AC-2: Remove clause-6 copy hack; add hermetic working-tree render test exercising real `quarto add` org/repo install path

---
ac: 2
depends_on: [1]
risk: medium
status: spec
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
- [ ] spec resolved — pending implementation (batch 2, after AC-1 merges)

### Decision Log
- user — approved working-tree install simulation over literal AC reading (GitHub-main sneaky-pass rejected); clause-12 `quarto add` retained as distribution-only proof; new file test_quarto_install_render.sh.
- resolver — adopted B's P1–P10; A's literal approach rejected after verification against ci.yml:131-137.

### Surprises & Discoveries
- CI clause-12 `quarto add` installs from GitHub main, not working tree (confirmed ci.yml:131-137).
- ci.yml `Render demo book` step implicitly relied on the copy hack's side effect.

### Idempotence & Recovery
- Safe retry: re-run both test scripts; mktemp fixtures self-contained; P9 asserts no repo-tree residue.
- Rollback: restore clause-6 copy block in test_quarto_distribution.sh, delete test_quarto_install_render.sh, revert ci.yml step wiring.

---

## AC-3: Correct README install path to `_extensions/mcmullarkey/blendtutor/` + assert install-path-independence + annotate stale ADR-0017:54

---
ac: 3
depends_on: []
merge_after: [1]
risk: low
status: spec
---

# AC-3: Correct README install path to `_extensions/mcmullarkey/blendtutor/` + assert install-path-independence + annotate stale ADR-0017:54

## USER DECISIONS (approved)
1. ADR-0017:54 stale line IS in scope for AC-3: annotate it (one-line note that CI actually asserts the org/repo path `_extensions/mcmullarkey/blendtutor/`). ADR body history left intact.
2. Merge order: AC-1 MUST merge before AC-3 (the install-path-independence sentence is only true once AC-1 lands). AC-3 may be BUILT in parallel (batch 1) but merges after AC-1.

## Executable Spec
- **predicate:** given README.md install section, then
  1. install command survives: contains exact `quarto add mcmullarkey/blendtutor`
  2. correct path stated: contains exact `_extensions/mcmullarkey/blendtutor/`
  3. old wrong claim gone: does NOT contain `` your project's `_extensions/blendtutor/` `` (narrow unique phrase — catches sneaky-pass leaving :172 intact or relocating wrong sentence)
  4. install-path-independence stated: matches `install-path-independent|independent of.{0,40}install|regardless install|relative to.*filter|PANDOC_SCRIPT_FILE`
  5. ADR annotation present: `docs/adr/0017-quarto-extension-distribution.md` near line 54 contains a note that CI actually asserts `_extensions/mcmullarkey/blendtutor/` (org/repo path) — e.g. grep for `mcmullarkey/blendtutor` in the ADR file
- **probe:**
  ```bash
  grep -qF 'quarto add mcmullarkey/blendtutor' README.md \
  && grep -qF '_extensions/mcmullarkey/blendtutor/' README.md \
  && ! grep -qF "your project's \`_extensions/blendtutor/\`" README.md \
  && grep -qiE 'install-path-independent|independent of.{0,40}install|regardless install|relative to.*filter|PANDOC_SCRIPT_FILE' README.md \
  && grep -qF 'mcmullarkey/blendtutor' docs/adr/0017-quarto-extension-distribution.md
  ```
  README clauses land as NEW clause in `scripts/tests/test_quarto_distribution.sh` Group 1 (README), following existing `grep -qF '...' "$README"` + ok/ko pattern at :48-63.
- **negative:** current README.md:172 — contains `` your project's `_extensions/blendtutor/` `` (wrong path) and no independence mention; probe must exit non-zero. Sneaky-pass variants each caught by one clause: deleting command with sentence (clause 1), stating path without removing old sentence (clause 3), omitting independence note (clause 4), skipping ADR annotation (clause 5).
- **verification:** code · grep-based content assertion on README.md + ADR file
- **fixture status:** EDIT `README.md:170-172`; EDIT `docs/adr/0017-quarto-extension-distribution.md:~54` (annotation); NEW clause in `scripts/tests/test_quarto_distribution.sh` Group 1 (:48-63 exists, 5 clauses; additive, 0 migrated)
- **rubric anchor:** §4 (README is user-facing "where it fits" doc — WHERE must match Quarto reality); §1 (wrong-path invariant existed untested — new clause closes gap)

## Design Intent
- **Types / interfaces (§1):** wrong-path claim was invariant violation with no test; clause set encodes doc contract: command + correct path + no stale phrase + independence note + ADR annotation.
- **Pure / effectful (§2):** doc-content assertions are pure greps; behavioral proof (render succeeds on real install path) belongs to AC-2 — cross-reference, do not duplicate.
- **Boundary cuts (§3):** README owns user-facing install contract. `../_extensions/...` filter refs in quarto-fixture/demo-book are correct in-repo SOURCE paths — must NOT be "fixed". ADR history preserved; annotation only.
- **Module responsibility (§4):** README install section = project-level "where it fits" doc. Fix restores accuracy; independence sentence documents what extension does NOT depend on.
- **Function discipline (§5):** one grep per clause; each fails on exactly one sneaky-pass variant.

## Technical Context
- Files touched: `README.md:170-172` (destination sentence at :172; command at :167 stays), `docs/adr/0017-quarto-extension-distribution.md:~54` (annotation), `scripts/tests/test_quarto_distribution.sh` Group 1 (new clause after :48-63).
- `quarto add mcmullarkey/blendtutor` installs to `_extensions/mcmullarkey/blendtutor/` (Quarto org/repo convention; confirmed by ci.yml:138-139 and test_quarto_distribution.sh:320-323).
- Repo-wide sweep: README.md:172 is ONLY wrong user-facing claim. In-repo `_extensions/blendtutor/` refs (6 test/script files, quarto-fixture) are DEV source paths — NOT affected. docs/adr/0017:49, :8, docs/okf/*, docs/book/src/*, demo-book, CONTRIBUTING: clean.
- `test_quarto_distribution.sh` Group 1 clause numbering must be coordinated with AC-2's edits to the same file (AC-3 batch 1, AC-2 batch 2 → serial, low risk).

## Dependencies
- Depends on: none (build). Merge order: after AC-1 (semantic — independence sentence documents AC-1 behavior).
- Blocks: none. Conflict set: `README.md` (exclusive), `docs/adr/0017-quarto-extension-distribution.md` (exclusive), `scripts/tests/test_quarto_distribution.sh` (shared with AC-2, serial). Risk: low.

### Progress
- [ ] spec resolved — pending implementation (batch 1; merges after AC-1)

### Decision Log
- user — ADR-0017:54 annotation IS in scope (annotate, don't rewrite history); AC-1-before-AC-3 merge order enforced.
- resolver — union of 4 README clauses (A's 3 + B's command-survival guard + unique-phrase negative); B's missing standalone negative field recorded, not dropped.

### Surprises & Discoveries
- README.md:172 was the ONLY wrong user-facing install-path claim in the entire repo (swept docs/, ADRs, OKF bundles, demo-book, crate READMEs).

### Idempotence & Recovery
- Safe retry: re-run probe greps; all assertions idempotent.
- Rollback: `git checkout -- README.md docs/adr/0017-quarto-extension-distribution.md scripts/tests/test_quarto_distribution.sh`

---

## Open Questions

None remaining — all resolved:

- ~~[needs-clarification] demo-book `../_extensions/...` out-of-root reference~~ — RESOLVED: in-repo `../_extensions/blendtutor/` refs are correct DEV source paths, must NOT be "fixed" (AC-3 §3); demo-book render via `../_extensions/blendtutor/assets/...` from repo root exercised by AC-2 P9; AC-1 clause 3 proves installed layout.
- ~~[spec-phase note] script-relative href stays project-relative~~ — RESOLVED: AC-1 clause 4 asserts project-relative URL form; clause 5 asserts href target exists on disk from rendered HTML dir; PANDOC_SCRIPT_FILE absolute form converted project-relative (AC-1 Technical Context, verified quarto 1.10.18).
- ~~[none requiring user input]~~ — decomposition otherwise complete; all user decisions captured above.
