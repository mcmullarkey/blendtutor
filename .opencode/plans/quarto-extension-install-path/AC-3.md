---
ac: 3
depends_on: []
merge_after: [1]
risk: low
status: complete
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
- Issue: #131
- [x] spec resolved — pending implementation (batch 1; merges after AC-1) — 2026-08-02
- [x] RED: Group 1 Clause 6 added to test_quarto_distribution.sh, fails on 6b/6c/6d against current README — 2026-08-02
- [x] GREEN: README path corrected + independence note added; ADR-0017 annotated; full suite 28 passed / 0 failed — 2026-08-02
- [x] Evidence committed at docs/evidence/131/ — 2026-08-02

### Decision Log
- user — ADR-0017:54 annotation IS in scope (annotate, don't rewrite history); AC-1-before-AC-3 merge order enforced.
- resolver — union of 4 README clauses (A's 3 + B's command-survival guard + unique-phrase negative); B's missing standalone negative field recorded, not dropped.

### Surprises & Discoveries
- README.md:172 was the ONLY wrong user-facing install-path claim in the entire repo (swept docs/, ADRs, OKF bundles, demo-book, crate READMEs).
- Running test_quarto_distribution.sh mutates demo-book/.gitignore (quarto render auto-appends `**/*.quarto_ipynb`); reverted after test runs — builders must not stage it.

### Idempotence & Recovery
- Safe retry: re-run probe greps; all assertions idempotent.
- Rollback: `git checkout -- README.md docs/adr/0017-quarto-extension-distribution.md scripts/tests/test_quarto_distribution.sh`
