---
ac: 5
depends_on: [4]
risk: medium
status: complete
---

# AC-5: demo-book by-name install migration + e2e R/Python boot verification

## Executable Spec (resolver-merged, 12 clauses)
> AMENDED (Director decision, see Decision Log): clauses 5/6/8 re-written for
> the book-aware site_libs form + one new standalone-regression clause. The
> original _files-URL clauses were impossible: Quarto BOOK projects
> (type: book, output-dir) deploy html-dependency libs to a SHARED
> `_output/site_libs/` — never per-page `<stem>_files/`.
- predicate: given demo-book/ with by-name extension install at demo-book/_extensions/mcmullarkey/blendtutor, when _quarto.yml switched off out-of-root ../_extensions/blendtutor/blendtutor.lua filter path and quarto render demo-book --to html runs, then:
  1. Install committed: demo-book/.gitignore no longer ignores /_extensions/ (line 2 removed); by-name install demo-book/_extensions/mcmullarkey/blendtutor/ (extension.yml, blendtutor.lua, assets/) is git-tracked; /_output/ added to .gitignore in same edit.
  2. Extension currency: vendored blendtutor.lua contains markers add_html_dependency, BT_DEP_VERSION, libs-dir URL computation (permanent test greps); sync step verified build-time via cmp -s against _extensions/blendtutor/blendtutor.lua (NOT committed parity test — committed guard is marker-based only).
  3. By-name filter reference: demo-book/_quarto.yml filters: references mcmullarkey/blendtutor (full org/repo form, grep-asserted on filter VALUE not line number); NO ../_extensions substring.
  4. Render exit 0: quarto render demo-book --to html exits 0 for full book (index + r-exercises + python-exercises).
  5. AMENDED — Book libs URLs: rendered pages in demo-book/_output/ reference SHARED site_libs/quarto-contrib/blendtutor-<BT_DEP_VERSION>/ URLs (book mode: r-exercises imports exercise-runtime.js + webr-adapter.js, python-exercises imports exercise-runtime.js + pyodide-adapter.js, both pages styles.css); NO _extensions/ substring in filter-injected bootstrap specifiers for those assets.
  6. AMENDED — Files on disk (shared site_libs): under demo-book/_output/site_libs/quarto-contrib/blendtutor-<BT_DEP_VERSION>/: exercise-runtime.js + styles.css + codemirror.js always; webr-adapter.js present iff any R exercise in the book; pyodide-adapter.js iff any python exercise. File-check base dir is demo-book/_output/ (rendered-document-relative), NOT demo-book/.
  7. COI boundary (R page only): r-exercises.html (coi: true at demo-book/r-exercises.qmd:3) still loads coi-serviceworker.js via include_text; NO coi-serviceworker.js in the blendtutor-<version> libs dir. NOTE (book mode): Quarto REWRITES the in-header coi src to its own site_libs/quarto-contrib/quarto-project/mcmullarkey/blendtutor/assets/ copy — src is Quarto-managed; our add_html_dependency boundary (nothing coi in blendtutor-<version> libs dir) is the asserted invariant.
  8. Rodney boot, both pages: rendered _output/r-exercises.html AND _output/python-exercises.html each load with window.__btExercises.length === 2 and .cm-editor count === 2; R page entries all data-language="r", Python page all data-language="python". Probe asserts population only — never awaits WebR/Pyodide boot completion (CDN offline).
  9. P9 guard org/repo-aware: test_quarto_distribution.sh P9 updated — demo-book/_extensions/mcmullarkey/blendtutor/ explicitly allowed; only bare demo-book/_extensions/blendtutor/ (non-org path / copy-hack residue) fails.
  10. P2/P1 guards compatible: every new script/test reference to demo-book extension path includes full _extensions/mcmullarkey/blendtutor/ string so P2 allowlist (test_quarto_install_render.sh) passes; P1 updated to allow org/repo refs in distribution script.
  11. Existing suites green: test_quarto_distribution.sh, test_quarto_filter.sh, test_coi_filter.sh, test_quarto_ux.py, test_quarto_feedback.py, test_quarto_bootstrap.sh, test_quarto_install_render.sh, test_quarto_asset_deployment.sh all pass after switch.
  12. No render noise committed: after render + test run, git status shows no demo-book/_output/ or *_files/ artifacts; .gitignore edit made BEFORE any render (or render artifacts removed) — never stage render-mutated .gitignore.
  13. NEW — Standalone unchanged (AC-4 regression): test_quarto_asset_deployment.sh stays green (its clause 3/5/8 _files-form asserts pin standalone/default renders unchanged); new clause 8b adds a hermetic BOOK fixture asserting ./site_libs/... specifiers + site_libs files on disk, plus a standalone-with-output-dir fixture asserting <stem>_files form is KEPT (discriminator pin: quarto.project.output_directory == directory for ALL standalone renders, even with output-dir set — verified empirically).
- probe:
  # Sync step (fixture, build-time) — clause 2
  rsync -a --delete _extensions/blendtutor/ demo-book/_extensions/mcmullarkey/blendtutor/ && cmp -s _extensions/blendtutor/blendtutor.lua demo-book/_extensions/mcmullarkey/blendtutor/blendtutor.lua
  # Render + structural clauses 1-7, 9-12
  bash scripts/tests/test_quarto_distribution.sh
  # Clause 4 also runs standalone in CI (quarto-distribution job)
  quarto render demo-book --to html
  # Clause 8 — e2e boot, manual harness (CDN offline; rodney probes not CI-wired)
  uv run node rodney-probes/demo-book-bootstrap.js
  # Parity/scoping unchanged
  bash scripts/sync-quarto-assets.sh && git diff --exit-code
  python3 scripts/tests/verify_asset_scoping.py
- negative:
  1. Stale extension silent-pass — sync skipped; vendored copy keeps pre-AC-3/AC-4 include_text code; render exits 0 but exercises don't boot / assets 404. Killed by clause 2 (markers) + clause 8 (boot asserts).
  2. By-name fallback sneaky — _quarto.yml keeps ../_extensions/... or resolution silently falls back to out-of-root path; render passes, by-name install dead weight. Killed by clause 3 (grep no ../_extensions) + clause 9.
  3. Wrong-org P9 match — resolution matches _extensions/blendtutor outside mcmullarkey/. Killed by clause 3 full-form pin + clause 9.
  4. Hardcoded asset base — libs file-check anchored at demo-book/ instead of demo-book/_output/. Killed by clause 6 base-dir pin.
  5. Only-R-boots — probe checks R page only; pyodide path silently broken. Killed by clause 8 (BOTH pages, per-language data-language asserts, exact === 2 counts).
  6. COI leaked into libs — coi-serviceworker.js deployed via add_html_dependency under by-name install → SW scope breaks. Killed by clause 7.
  7. Render-noise commit — _output/ or *_files/ staged; .gitignore mutated by render and staged blindly; /_extensions/ un-ignore forgotten → install never committed → CI clone lacks extension. Killed by clauses 1 + 12.
  8. Line-anchor drift — test greps _quarto.yml:15 literally; any edit shifts line and false-fails. Clause 3 greps filter VALUE.
- verification: both — code (clauses 1-7, 9-12 via test_quarto_distribution.sh) · rodney (clause 8 real-browser boot, manual harness)
- fixture status: SYNC (mandatory, not assumption) demo-book/_extensions/mcmullarkey/blendtutor/ from _extensions/blendtutor/ — confirmed STALE (0 add_html_dependency matches vs 8 in source). MODIFIED demo-book/_quarto.yml (filters → mcmullarkey/blendtutor), demo-book/.gitignore (remove /_extensions/, add /_output/), scripts/tests/test_quarto_distribution.sh (href file-check base dir → $RENDER_HTML_DIR; marker/by-name/libs clauses; P9 guard org/repo-aware). NEW rodney-probes/demo-book-bootstrap.js (mirrors auto-bootstrap.js harness: EVIDENCE_DIR docs/evidence/<issue>/, wait only on __btExercises). demo-book/r-exercises.qmd + python-exercises.qmd: existing, unchanged. ci.yml: NO change (quarto-distribution job already runs distribution test + render).
- rubric anchor: §1.2 (by-name org/repo pin + currency markers), §2 (pure ref + greps; effectful render + rodney), §3.4 (consumer/vendored/_output seams; COI stays source-tree), §4.1 (demo-book = by-name consumer), §5 (one render command, one page-parametric probe, one test extension)

## Design Intent
- §1: full org/repo filter reference is the contract — "works via ../_extensions on my machine" unrepresentable; marker greps make stale-vendored-copy state unrepresentable at test time.
- §2: pure = yml reference, greps, gitignore state; effectful = quarto render + rodney browser boot.
- §3: demo-book/ (consumer) / _extensions/ (source of truth) / demo-book/_output/ (artifacts). COI stays source-tree (SW scope). Per-language adapter files cut at language joint.
- §4: demo-book = by-name install consumer, e2e proof of real quarto add flow; distribution test owns render+assert loop; sync script owns vendored-copy freshness.
- §5: one render command, one probe parametric over both pages, one test-file extension. No per-page probe files, no second render path.

## Technical Context
- Files likely touched: demo-book/_quarto.yml, demo-book/.gitignore, demo-book/_extensions/mcmullarkey/blendtutor/** (sync), scripts/tests/test_quarto_distribution.sh (:225-250 href base dir, :375-380 P9 guard, new clauses), NEW rodney-probes/demo-book-bootstrap.js.
- Architecture notes: by-name resolution verified — Quarto 1.10.18, book project (type: book, output-dir: _output); filters: [mcmullarkey/blendtutor] resolves to demo-book/_extensions/mcmullarkey/blendtutor/ (already present). Fallback if resolution fails: short-name filters: [blendtutor] — document in plan, do NOT spec both.
- Traps: (a) never rely on out-of-root fallback — full org/repo form only; (b) rodney asserts DOM population + .cm-editor count, never SharedArrayBuffer/boot completion; (c) probe loads demo-book/_output/<page>.html, never source qmd; (d) exercise counts pinned === 2 match current fixtures — fixture edits silently break pins; (e) .gitignore edit BEFORE renders or git add -f discipline; never stage render-mutated .gitignore; (f) P2 allowlist requires literal _extensions/mcmullarkey/blendtutor/ in any new cp/demo-book path reference; (g) assert filter value via grep, not line-number anchor.

## Dependencies
- Depends on: AC-4 (#141, PR #142 — libs-dir deployment), AC-3 (#139, PR #140 — bootstrap emission); AC-1/AC-2 merged (e96017b).
- Blocks: AC-7 (README quick-start documents verified end-state + COI caveat).
- Conflict set: demo-book/_quarto.yml, demo-book/.gitignore, demo-book/_extensions/mcmullarkey/blendtutor/, scripts/tests/test_quarto_distribution.sh, rodney-probes/demo-book-bootstrap.js — disjoint from AC-6 (quarto-fixture/, test_quarto_ux.py, test_quarto_feedback.py, validate-webr-adapter.js) → Batch 4 parallel-safe.
- Risk level: medium — no new mechanism; risks are sync discipline, rodney-on-real-project flakiness, .gitignore/render-noise hygiene.

## Decision Log
- resolver — staleness guard: markers in permanent test (B), cmp -s parity build-time only (A's diff -r couples every future source edit to fixture churn); filter ref: full org/repo mcmullarkey/blendtutor (kills wrong-org match, aligns P9); .gitignore: remove /_extensions/ line 2 + add /_output/ (currently absent — not ignored today); asset base dir: B correct — book output-dir _output confirmed, files at demo-book/_output/<page>_files/; P9 guard: allow org/repo subpath only; probe name: rodney-probes/demo-book-bootstrap.js; CI: no change (quarto-distribution job covers; rodney manual, 0 refs in ci.yml); P2 allowlist folded as trap.
- resolver — disagreement=minor; 8 divergences resolved by codebase verification; no user clarification needed.
- **Director — Option (a) book-aware libs_url (root-cause fix, replaces blocked _files clauses):** Quarto BOOK projects deploy html-dependency libs to SHARED _output/site_libs/ — never per-page <stem>_files/ — so AC-4's libs_url() <stem>_files URLs 404 in book renders → __btExercises never populates. libs_url() detects book renders via `quarto.project.output_directory ~= quarto.project.directory` (empirically verified quarto 1.10.18: book+output-dir differ; ALL standalone renders — with or without output-dir — equal) and emits ./site_libs/... URLs; standalone keeps <stem>_files/... (AC-4 regression net pins unchanged). Clauses 5/6/8 amended; new clause 13 standalone-regression. Touches _extensions/blendtutor/blendtutor.lua (AC-4 hot file) — approved.
- **Builder — clause 7 COI boundary (book-mode reality):** Quarto REWRITES the in-header coi src to its own site_libs/quarto-contrib/quarto-project/mcmullarkey/blendtutor/assets/coi-serviceworker.js copy in book renders (extension assets are copied into site_libs under quarto-project/). The coi src is therefore Quarto-managed in book mode, NOT source-tree _extensions/... The asserted invariant narrows to: coi-serviceworker.js NEVER deployed via our add_html_dependency into blendtutor-<version> libs dir + a coi src exists on the page. SW-scope caveat documented (Quarto's site_libs copy scope cannot cover _output/ pages — COI headers don't function in book mode; out of scope for boot/URLs issue, rodney asserts population only).

### Progress
- [x] spec resolved (resolver) — pending implementation
- [x] sync demo-book extension + migrate _quarto.yml + .gitignore — done (uncommitted)
- [x] Director root-cause decision: book-aware libs_url (Option a) — recorded above
- [x] red: extend test_quarto_distribution.sh (site_libs clauses 1-3,5-7,9; P9 org-aware) + test_quarto_asset_deployment.sh clause 8b (book + standalone-output-dir) + NEW rodney-probes/demo-book-bootstrap.js — confirmed RED (7 fails: _files URLs emitted in book, coi src rewritten)
- [x] green: libs_url() book-aware in _extensions/blendtutor/blendtutor.lua + rsync vendored sync (cmp -s + diff -rq parity) — distribution 55 passed
- [x] rodney clause 8 (both pages) — PROBES_PASS (__btExercises===2, .cm-editor===2, per-language data-language, population only)
- [x] regression: ALL suites green (distribution 55, asset-deployment 52, bootstrap 29, filter 22, coi 15, install-render 13, ux 46, feedback 32, sync --check ok, verify_asset_scoping 12)
- [x] evidence docs/evidence/143/ — render.log, site-libs-listing.txt, specifiers-r/python.txt, probe-report.json, rodney.log, test-suite.log
- [x] commits + push + PR — pending

### Surprises & Discoveries
- **AC-5 (filter-runtime-bootstrap): Quarto book projects NEVER produce `<page>_files/libs/` — spec clauses 5/6/8 impossible as written.** Verified in quarto.js source (`bookProjectType.libDir = "site_libs"`, inherits websiteProjectType) AND empirically (Quarto 1.10.18): `type: book` + `output-dir: _output` consolidates ALL add_html_dependency resources into the SHARED `_output/site_libs/quarto-contrib/blendtutor-0.1.0/` dir; per-page `<stem>_files/` dirs are NOT created (find shows zero `*_files` dirs). The filter's `libs_url()` (blendtutor.lua:137-157) emits `./<stem>_files/libs/quarto-contrib/...` specifiers — correct for standalone/default renders (verified: `type: default` → `index_files/libs/...`), WRONG for books → module imports 404 → `__btExercises` never populates → rodney clause 8 CANNOT pass. Pre-existing latent bug: demo-book boot-broken since AC-4 (no rodney probe ever ran on demo-book; distribution-test href grep `blendtutor/` never matched `blendtutor-0.1.0/` — sneaky pass via the COI shim src only). Director resolved: Option (a) make filter book-aware (emit site_libs URLs in book mode) — implemented.
- **Standalone render WITH output-dir: `quarto.project.output_directory` STAYS equal to `quarto.project.directory`** — output-dir is NOT reflected in output_directory for standalone/default projects (verified empirically: type: default + output-dir: build-out → output_directory == directory, libs still deploy to `<stem>_files/`, no site_libs dir). The book discriminator `output_directory ~= directory` is therefore SAFE for standalone-with-output-dir — the exact sneaky-pass shape a naive `output-dir`-based discriminator would misfire on.
- **Book-mode COI: Quarto REWRITES the in-header coi src to its own site_libs copy** — `src="site_libs/quarto-contrib/quarto-project/mcmullarkey/blendtutor/assets/coi-serviceworker.js"` (Quarto copies the by-name extension into site_libs under `quarto-project/`). The coi src is NOT the source-tree `_extensions/...` path in book renders; my first clause-7 assert (source-tree path) failed. Narrowed to the real invariant: coi-serviceworker.js absent from OUR blendtutor-0.1.0 libs dir + a coi src present. Consequence (documented): Quarto's site_libs SW copy has scope = its own dir → cannot cover _output/ pages → COI headers don't function in book mode. Out of scope for this issue (boot/URLs); rodney asserts population only.
- **My clause-5 loop initially over-asserted**: asserted BOTH adapter URLs on BOTH pages — but adapter imports are conditional (R page imports webr-adapter.js only, python page pyodide-adapter.js only). Fixed to per-page adapter assertions (R page asserts webr present + pyodide ABSENT; python page inverse).
- **Book site_libs path has NO `libs/` segment**: `_output/site_libs/quarto-contrib/<name>-<version>/` (standalone: `<stem>_files/libs/quarto-contrib/...`). Reusing the standalone `$LIBS_REL` var (`libs/quarto-contrib/...`) in the book fixture double-prefixed the path (`site_libs/libs/...`) — first clause-8b run failed on the wrong path, not the code.
- **P1 guard (test_quarto_install_render.sh) stale**: blanket-banned `demo-book/_extensions` literals in the distribution script — false-positived on the new org/repo by-name refs. Updated P1 to mirror P2's allowance (org/repo path with trailing slash OK; bare/non-org still fails).

### Idempotence & Recovery
- Safe retry: re-run rsync sync + render + probes (regenerated at test time).
- Rollback: git checkout -- demo-book/_quarto.yml demo-book/.gitignore scripts/tests/test_quarto_distribution.sh; git rm -r demo-book/_extensions/mcmullarkey/blendtutor
