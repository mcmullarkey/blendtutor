---
ac: 2
depends_on: [1]
risk: medium
status: complete
---

# AC-2: Extend docs.yml build job to render demo-book + demo-standalone into Pages artifact at /demo-book/ + /demo/, with root .nojekyll

## Executable Spec (resolver-merged, 11 clauses)
- predicate: given AC-1 merged (demo-standalone/ + scripts/fix-demo-coi-scope.sh exist), when .github/workflows/docs.yml build job runs on push, then:
  1. quarto setup: build job block contains quarto-dev/quarto-actions/setup@v2 (awk-extracted build job block, AC-1 cycle-2 lesson — file-wide grep insufficient). Confirmed absent today (docs.yml:41-82).
  2. build job renders demo-book: quarto render demo-book --to html in build job block
  3. build job renders demo-standalone: quarto render demo-standalone --to html in build job block
  4. bash scripts/fix-demo-coi-scope.sh demo-standalone runs AFTER the demo-standalone render step and BEFORE the artifact-copy steps and upload (line-order pin within build job block)
  5. demo-book assembled via dot-copy: cp -R demo-book/_output/. docs/book/book/demo-book/ — bare cp -R demo-book/_output dst creates dst/_output/ layer → wrong URL. Assembled artifact has docs/book/book/demo-book/index.html directly, NOT demo-book/_output/index.html. (VERIFIED: demo-book/_quarto.yml:3 pins output-dir: _output)
  6. demo-standalone assembled selectively into docs/book/book/demo/: index.html + index_files/ + coi-serviceworker.js ONLY — no .qmd sources, no _quarto.yml, no _extensions/ escape into the artifact
  7. explicit workflow step creates docs/book/book/.nojekyll at artifact ROOT (not nested) — must not rely on the local untracked file (VERIFIED: exists locally untracked; docs.yml has no step creating it)
  8. ALL new steps appear BEFORE actions/upload-pages-artifact@v5 (docs.yml:79-82); NO || true, NO continue-on-error: true on any new step; render/copy steps appear in the build job block only, NOT the deploy job block (deploy has no checkout/quarto)
  9. scripts/check-docs.sh mirrors the new steps (module-responsibility contract, check-docs.sh:10-13) and asserts the assembled layout: demo-book/index.html exists (not under _output/), demo/index.html exists, demo/coi-serviceworker.js exists + non-empty + byte-identical to vendored shim (cksum vs _extensions/blendtutor/assets/coi-serviceworker.js), demo/index.html coi src is exactly ./coi-serviceworker.js with NO _extensions/ substring in the coi script tag, docs/book/book/.nojekyll exists
  10. existing artifact survives: docs/book/book/index.html (mdBook), docs/book/book/api/blendtutor_core/index.html (rustdoc), docs/book/book/examples/{r,python}/index.html still exist after assembly — no rm -rf docs/book/book/* clobber
  11. CI wiring: ci.yml quarto-render job block runs scripts/tests/test_docs_pages_artifact.sh (awk job-block pin per AC-1 cycle-2 lesson)
- probe:
  bash scripts/tests/test_docs_pages_artifact.sh && bash scripts/check-docs.sh
  Test structure (test_docs_pages_artifact.sh):
  - Phase 1 structural (always runs): awk-extract build: job block from docs.yml → assert clauses 1-8 (setup, both renders, post-process, dot-copy literal, selective copy, .nojekyll step, ordering via line-number comparison vs upload step, absence of || true/continue-on-error, absence of render steps in deploy: block); awk-extract quarto-render job block from ci.yml → assert clause 11; grep check-docs.sh for mirrored commands (clause 9 structural half)
  - Phase 2 local-render (SKIP exit 0 if quarto OR mdbook OR cargo absent): run check-docs.sh end-to-end (full local render + assemble + assert per clauses 9-10) — mirrors AC-1 render-then-assert pattern
  - ok()/ko() PASS/FAIL counters, exit 1 on fail (AC-1 pattern)
- negative:
  1. quarto setup step missing → quarto render fails on clean runner → killed by clause 1
  2. || true / continue-on-error: true on render/copy/post-process step → silent-failure deploy of broken artifact → killed by clause 8 refusal-arm pin
  3. cp -R demo-book/_output docs/book/book/demo-book (no trailing /.) → extra _output/ layer → live URL becomes /demo-book/_output/index.html, /demo-book/ 404s → killed by clause 5 + check-docs demo-book/index.html existence assert
  4. COI post-process skipped, or run after copy → demo/index.html keeps ../_extensions/blendtutor/assets/coi-serviceworker.js src → SW scope = assets subdir, page never controlled → webR dead despite green "coi present" grep (AC-1/AC-5 trap) → killed by clauses 4, 9 (exact ./coi-serviceworker.js on ASSEMBLED artifact, not just source render)
  5. .nojekyll omitted from workflow (relying on local untracked file) or placed at nested path → Quarto's *_files/ dirs Jekyll-filtered if Pages source ever switches to branch mode → killed by clause 7 step pin + check-docs root-existence assert
  6. new steps placed AFTER upload-pages-artifact → artifact uploaded without demos → killed by clause 8 line-order pin
  7. render/copy steps placed in deploy job (no checkout, no quarto setup, OIDC-only) → killed by clause 8 build-job-block scoping + deploy-block absence assert
  8. rm -rf docs/book/book/* or overwrite during assembly → mdBook/rustdoc/examples clobbered → killed by clause 10 survival asserts
  9. demo-standalone copied wholesale → .qmd sources, _quarto.yml, _extensions/ leak into public Pages artifact → killed by clause 6 selective-copy pin + no-_extensions/ assert on artifact
  10. check-docs.sh not updated → local mirror diverges from CI, mirror contract (check-docs.sh:10-13) silently broken → killed by clause 9 structural grep inside phase 1
- verification: code · shell structural test (awk job-block pins on docs.yml/ci.yml + check-docs.sh mirror greps) + extended check-docs.sh local render/assemble/assert. No browser; live-URL proof is AC-3/AC-4
- fixture status: NEW scripts/tests/test_docs_pages_artifact.sh; EDIT .github/workflows/docs.yml (build job, steps inserted before upload step at docs.yml:79); EDIT scripts/check-docs.sh (extend build+mirror+assert sections); EDIT .github/workflows/ci.yml (one step in quarto-render job). Existing anchors: docs.yml:79-82 (upload step), ci.yml:59 (quarto setup@v2 pattern), test_demo_standalone_render.sh:115-120 (awk job-block extraction), check-docs.sh:10-13 (mirror contract), fix-demo-coi-scope.sh (AC-1 post-process), demo-book/_quarto.yml:3 (output-dir: _output — copy-semantics driver)
- rubric anchor: §2.1 (effectful render/copy shell thin; all assertions pure greps/test -f/cksum), §3.1 (URL nesting boundary /demo-book/ + /demo/ mirrors /api + /examples/{r,python}), §4.1 (check-docs.sh ↔ docs.yml mirror contract), §5.1 (one-step-one-concern workflow steps)

## Design Intent
- Types / interfaces (§1): artifact layout is the interface — nested dirs /demo-book/ + /demo/ typed by existence+content asserts in check-docs.sh
- Pure / effectful (§2): effectful = quarto renders + cp/touch steps; pure = every assertion (grep, test -f, cksum, awk extraction). No network, no browser at this layer
- Boundary cuts (§3): demo-book/ (book, static+Python, output-dir: _output) and demo-standalone/ (default, R+Python+COI, render-beside-source) are distinct render domains with distinct copy semantics (dot-copy vs selective copy)
- Module responsibility (§4): docs.yml build job owns CI assembly; check-docs.sh owns local mirror + predicate enforcement (contract check-docs.sh:10-13); fix-demo-coi-scope.sh keeps sole ownership of SW-scope seam
- Function discipline (§5): each new workflow step does one thing; post-process invoked once here (idempotent)

## Technical Context
- docs.yml build-job ordered step list (insert before upload step at docs.yml:79, after Python example build at :76-77):
  1. - uses: quarto-dev/quarto-actions/setup@v2
  2. render demo-book: quarto render demo-book --to html → output at demo-book/_output/
  3. render demo-standalone: quarto render demo-standalone --to html → output beside source
  4. bash scripts/fix-demo-coi-scope.sh demo-standalone
  5. mkdir -p docs/book/book/demo-book && cp -R demo-book/_output/. docs/book/book/demo-book/
  6. selective demo copy: mkdir -p docs/book/book/demo && cp demo-standalone/index.html docs/book/book/demo/ && cp -R demo-standalone/index_files docs/book/book/demo/ && cp demo-standalone/coi-serviceworker.js docs/book/book/demo/
  7. touch docs/book/book/.nojekyll
  8. (existing) Upload Pages artifact
- Copy semantics (verified catch): demo-book/_quarto.yml:3 pins output-dir: _output → bare cp -R demo-book/_output dst yields dst/_output/ (wrong). Dot-copy cp -R demo-book/_output/. dst/ flattens correctly. Demo-standalone renders beside source → selective copy excludes .qmd/_quarto.yml/_extensions/
- check-docs.sh extension: mirror steps 2-7 verbatim after the examples build legs (after :63), then assert clause-9/10 predicates; extend the workflow-grep loop (:103-116) with new needles (quarto setup, both renders, fix script, demo-book/_output/. dot-copy literal, .nojekyll)
- Artifact layout tree (target):
  docs/book/book/
  ├── .nojekyll              (root — NEW, workflow-created)
  ├── index.html             (mdBook — preserved)
  ├── api/                   (rustdoc — preserved)
  ├── examples/{r,python}/   (preserved)
  ├── demo-book/             (NEW — flattened _output, index.html at root)
  │   ├── index.html
  │   └── site_libs/ ...
  └── demo/                  (NEW — selective: index.html + index_files/ + coi-serviceworker.js)
      ├── index.html         (coi src = ./coi-serviceworker.js post-processed)
      ├── index_files/
      └── coi-serviceworker.js (byte-identical to vendored shim)
- Architecture notes: ci.yml edit deviates from decomposition conflict set ("ci.yml touched by AC-1 only") — safe post-AC-1-merge (append-only step in quarto-render job). Pages-source = GitHub Actions assumed per decomposition open question.

## Dependencies
- Depends on: AC-1 (merged, PR #151 — demo-standalone/ + scripts/fix-demo-coi-scope.sh + ci.yml quarto-render job)
- Blocks: AC-4 (verify-live job — same hot file), AC-5 (README links to /demo-book/ + /demo/)
- Conflict set: .github/workflows/docs.yml (serialized: AC-2 before AC-4 per decomposition DAG); .github/workflows/ci.yml (append-only, AC-1 merged — no live conflict); scripts/check-docs.sh (AC-2 sole owner)
- Risk level: medium — copy-semantics trap verified real; ordering/refusal-arm pins mechanical

## Decision Log
- resolver — test name: B's test_docs_pages_artifact.sh wins, A's check-docs.sh extension kept; resolved schema-gate drops (A negative/rubric/design missing — B supplies)
- resolver — copy semantics: B's _output dot-copy trap VERIFIED real (demo-book/_quarto.yml:3 output-dir: _output); dot-copy adopted
- resolver — quarto setup: docs.yml has NO setup today; mandatory addition (clause 1)
- resolver — ci.yml wiring deviates from decomposition conflict set; safe post-AC-1-merge (append-only)
- resolver — disagreement=minor; converged on all substance

### Progress
- [x] spec resolved (resolver) — pending implementation
- [x] red: test_docs_pages_artifact.sh + check-docs.sh extension — done (7c95c0d)
- [x] green: docs.yml build job steps — done (e5f2572)
- [x] evidence docs/evidence/<issue>/ — done (49524e6)
- [x] full local render: check-docs.sh end-to-end (cargo doc + mdBook + release example builds + quarto renders + assemble + asserts) — EXIT=0, 14/14 test pass

### Surprises & Discoveries
- AC-2 (github-pages-deploy): quarto render AUTO-CREATES `<dir>/.gitignore` (ignoring `/.quarto/` + `*.quarto_ipynb`) in demo-standalone/ — untracked render noise that appears on every local render. Root .gitignore already covers `.quarto/`, so the auto-file is redundant; must be rm'd before commit or it shows as untracked. Also: quarto-fixture nested *_files/ dirs (coi-book/chapter-coi_files) are NOT covered by the root `/quarto-fixture/*_files/` gitignore line (matches only depth-1) — test_coi_filter.sh renders leave untracked nested noise. Cleanup: rm -rf both before committing.
- AC-2 (github-pages-deploy): demo-book filter `mcmullarkey/blendtutor` resolves fine from the vendored `_extensions/blendtutor/` (no quarto add needed) — local render of demo-book + demo-standalone both exit 0, confirmed before writing the red test.
- AC-2 (github-pages-deploy): check-docs.sh is NOT run anywhere in CI — it is a pure local mirror (docs.yml keeps commands inline so the workflow is greppable). This is why the new test's Phase 1 greps check-docs.sh structurally: CI (quarto-render job) only enforces the structural half; the render/assert half runs locally via Phase 2 (SKIPs in CI since mdbook/cargo absent there).
- AC-2 (github-pages-deploy): zsh (macOS default) has no PIPESTATUS — `${PIPESTATUS[0]}` returns empty; use `$pipestatus` or run the script without a pipe to read exit codes.

### Idempotence & Recovery
- Safe retry: re-run check-docs.sh + test (renders idempotent).
- Rollback: git checkout -- .github/workflows/docs.yml .github/workflows/ci.yml scripts/check-docs.sh; rm scripts/tests/test_docs_pages_artifact.sh
