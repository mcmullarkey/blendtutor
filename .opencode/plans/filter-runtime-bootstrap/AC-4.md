---
ac: 4
depends_on: [3]
risk: high
status: in-progress
---

# AC-4: Deploy extension assets via add_html_dependency to libs/quarto-contrib/blendtutor-0.1.0 + rewrite bootstrap specifiers

## Executable Spec (resolver-merged, 13 clauses)
- predicate: given any qmd with ≥1 blendtutor exercise (no opt-out) rendered to HTML through the filter, when Pandoc() runs, then:
  1. Mechanism pin: blendtutor.lua contains exactly one quarto.doc.add_html_dependency({ name="blendtutor", version=BT_DEP_VERSION, stylesheets={"assets/styles.css"}, resources={...} }) call. NO scripts= key. NO _extension.yml resources: key. NO include_text("in-header", css_link) for styles.css remains (old path removed, not dual-injected).
  2. Conditional JS resources: resources table built conditionally — always assets/exercise-runtime.js + assets/codemirror.js; assets/webr-adapter.js iff has_r; assets/pyodide-adapter.js iff has_python.
  3. Deployed to libs: files physically exist at <stem>_files/libs/quarto-contrib/blendtutor-0.1.0/ — exercise-runtime.js, codemirror.js, styles.css always; webr-adapter.js present iff page has R exercises, ABSENT otherwise (same for pyodide/python). (Verified Quarto TS: quarto.js:128069-70 — quarto-contrib/<name>-<version>.)
  4. CSS via Quarto link: rendered HTML contains exactly one <link rel="stylesheet" href="<stem>_files/libs/quarto-contrib/blendtutor-0.1.0/styles.css">; no _extensions/.../assets/styles.css link present.
  5. Bootstrap specifiers rewritten: data-bt-bootstrap="auto" module's import specifiers reference <stem>_files/libs/quarto-contrib/blendtutor-0.1.0/<file>.js, computed from quarto.doc.output_file stem; NO _extensions/ substring and NO resolve_asset_path output in any JS/CSS specifier.
  6. No classic runtime script tag: rendered HTML contains NO <script src="...exercise-runtime.js"></script> classic tag (ES modules SyntaxError as classic scripts).
  7. COI boundary: coi="true" fixture still emits <script src="_extensions/.../assets/coi-serviceworker.js"> via include_text + resolve_asset_path; coi-serviceworker.js NOT in any libs dir.
  8. Stem correctness, nested + multi-page: hermetic render from a pages/ subdir yields libs at pages/index_files/libs/... and bootstrap specifier index_files/... (document-relative); quarto render quarto-fixture/coi-book --to html exits 0 with per-page libs dirs.
  9. Non-HTML gate: quarto render <fixture> --to latex → zero add_html_dependency calls (guarded by existing is_html_format()), zero _files/libs/ dirs created, zero bootstrap injection.
  10. Version pin single-sourced: BT_DEP_VERSION = "0.1.0" Lua constant equals _extension.yml:3 version and used in BOTH dependency declaration and emitted libs URL string.
  11. Runtime still boots — rodney: rendered quarto-fixture/mixed-lang.html → window.__btExercises.length === 2 (harness waits only on __btExercises, never boot completion — CDN offline risk).
  12. Asset parity intact: bash scripts/sync-quarto-assets.sh && git diff --exit-code passes; python3 scripts/tests/verify_asset_scoping.py passes (sources + sync script unchanged).
  13. Existing suites green post-migration: test_quarto_filter.sh, test_coi_filter.sh, test_quarto_ux.py, test_quarto_feedback.py, test_quarto_bootstrap.sh, test_quarto_install_render.sh all pass with rewritten libs-dir assertions.
- probe:
  bash scripts/tests/test_quarto_asset_deployment.sh    # NEW — clauses 1-10 (hermetic TMP renders: root, nested pages/, mixed-lang, r-only, latex-gate)
  bash scripts/tests/test_quarto_bootstrap.sh           # clause 5/7 — specifiers are libs URLs, no _extensions/, coi unchanged
  bash scripts/tests/test_quarto_install_render.sh      # P6 rewritten: CSS_REF/CSS_FILE → index_files/libs/quarto-contrib/blendtutor-0.1.0/styles.css
  uv run python scripts/tests/test_quarto_ux.py         # check_styles_css_loaded + 2 path checks → libs-dir
  uv run node rodney-probes/auto-bootstrap.js           # clause 11 — boot check on mixed-lang.html
  bash scripts/sync-quarto-assets.sh && git diff --exit-code   # clause 12
  python3 scripts/tests/verify_asset_scoping.py                # clause 12
- negative:
  1. _extension.yml gains resources: key — not a valid filter-extension mechanism; assets never deployed → clauses 1+3 kill.
  2. JS declared under scripts= — Quarto emits classic <script src>, ES module SyntaxErrors, __btExercises never set → clauses 1+6+11 kill.
  3. Bootstrap URL rewritten but files never deployed (no add_html_dependency / wrong relative path like styles.css vs assets/styles.css — Quarto silently skips) — URL greps pass, files 404 → clause 3 file-existence + clause 11 kill.
  4. Stem hardcoded (index_files/...) instead of derived from quarto.doc.output_file — breaks renamed/subdir output → clause 8 kills.
  5. Version drift: dep version, emitted URL, and _extension.yml disagree → libs dir ≠ specifier → 404 → clauses 3+10 kill.
  6. Old css_link include_text kept alongside dependency stylesheet — duplicate <link> → clause 4 "exactly one" kills.
  7. coi-serviceworker.js moved into libs — SW scope = script URL dir, COI silently breaks → clause 7 kills.
  8. Adapters deployed unconditionally — r-only page libs dir contains pyodide-adapter.js (dead file, deployment/import asymmetry vs AC-3 conditional imports) → clause 3 absence assertion kills.
  9. add_html_dependency called without is_html_format() guard — latex render creates spurious _files/libs/ → clause 9 kills.
- verification: code + rodney · method: NEW test_quarto_asset_deployment.sh (primary, clauses 1-10) + migrated existing suites + rodney harness for clause 11
- fixture status: existing — quarto-fixture/mixed-lang.qmd, r-only.qmd, pyodide.qmd, coi-book/chapter-coi.qmd, install-render TMP index.qmd. MODIFY — scripts/tests/test_quarto_bootstrap.sh (clause 6), scripts/tests/test_quarto_install_render.sh (P6/P7), scripts/tests/test_quarto_ux.py (3 path-check functions). NEW — scripts/tests/test_quarto_asset_deployment.sh; nested pages/ render is hermetic TMP (no committed fixture).
- rubric anchor: §1.1 (BT_DEP_VERSION single-sourced invariant), §1.2 (mechanism pin), §2 (filter pure; Quarto owns copy), §3.4 (source-tree vs libs boundary; coi/pyodide-CDN/feedback stay source-tree), §4.1 (header docs), §5 (one build_html_dependency, one build_bootstrap_script, one libs-URL computation)

## Design Intent
- §1: BT_DEP_VERSION single source for dep version, emitted URL, _extension.yml parity. Dependency table shape {name, version, resources, stylesheets} pinned; resources (copy-only) vs stylesheets (rewritten link) vs scripts (forbidden — classic tag). is_html_format() guard makes non-HTML deployment unrepresentable.
- §2: pure = stem computation from quarto.doc.output_file + specifier building; effectful = Quarto-core copy to libs dir. Filter never writes files.
- §3: vendored source tree (_extensions/<org>/blendtutor/assets/, sync-quarto-assets.sh-owned, unchanged) vs Quarto-owned output libs dir. coi-serviceworker.js stays include_text (SW scope); pyodide CDN stays include_text (external); exercise-feedback.js stays manual opt-in (BYOK, AC-7). Hand-written bootstraps (feedback/webr/ux.qmd) OUT of scope — AC-6.
- §4: blendtutor.lua declares dependency + emits rewritten bootstrap; Quarto copies + rewrites; exercise-runtime.js keeps transitive ./codemirror.js import (exercise-runtime.js:29-42) — codemirror ships in SAME libs dir; sync-quarto-assets.sh unchanged.
- §5: one build_html_dependency() (conditional resources table; guarded by has_blendtutor + is_html_format); one build_bootstrap_script() (rewritten specifiers only); one libs-URL helper; no resolve_asset_path fallback for JS/CSS; COI path untouched.

## Technical Context
- Files likely touched:
  - _extensions/blendtutor/blendtutor.lua — add BT_DEP_VERSION; replace css_link include_text (origin/main :534-542) with add_html_dependency; rewrite build_bootstrap_script() (:445-467) specifiers from resolve_asset_path (:130-132) to output_file-derived libs URLs; REMOVE STYLES_CSS_PATH (:124); KEEP COI_SCRIPT_PATH (:120) + coi include_text (:522-525). Header docs updated.
  - NEW scripts/tests/test_quarto_asset_deployment.sh — clauses 1-10.
  - MODIFY scripts/tests/test_quarto_bootstrap.sh (clause 6 specifier greps → libs), test_quarto_install_render.sh (P6 :242-253), test_quarto_ux.py (check_styles_css_loaded :401-435, check_installed_layout_asset_path :560, check_by_name_install_absolute_path :581).
  - _extension.yml, sync-quarto-assets.sh, verify_asset_scoping.py, CI — unchanged (clause 12 proves it).
- Quarto API notes (verified installed, /Applications/quarto):
  - add_html_dependency (init.lua:815-871): resources resolved via resolveFileDependencies, passed as external=true dependency; TS side copies resources only (quarto.js:128053-55) — no tag. stylesheets get rewritten <link>. scripts get classic tags — unusable for ES modules.
  - Output dir (quarto.js:128068-76): external → <stem>_files/libs/quarto-contrib/<name>-<version>; version omitted → <name> only.
  - attach_to_dependency (init.lua:874-911): silently no-ops if parent dep not yet injected — ordering trap; NOT used (resources mechanism wins).
  - head= string injected verbatim, not URL-rewritten — bootstrap URL computed by filter.
  - quarto.doc.output_file populated at Pandoc() (probe-verified) — URL anchor.
- Traps: (a) rendered *_files/ noise grows — every fixture page now emits *_files/libs/; builder must rm before commit per plan convention (coi-book *_files/ gitignore escape). (b) Local main is 1 commit behind origin/main (4fd547f = AC-3) — pull before building; blendtutor.lua line refs above are origin/main. (c) add_html_dependency silently skips missing files — always assert file-on-disk, never just the tag. (d) Quarto version drift breaks exact-URL assertions — pin via BT_DEP_VERSION constant in test greps where feasible.
- Memory note: rodney probes via uv run node rodney-probes/<name>.js harness (direct uvx rodney permission-blocked); harness serves worktree root → libs URLs resolve offline; wait only on __btExercises.

## Dependencies
- Depends on: AC-3 (#139, PR #140, commit 4fd547f — rewrites its emitted bootstrap; has_r/has_python/has_blendtutor/hasBootstrapDone/opt-out unchanged). AC-1, AC-2 merged.
- Blocks: AC-5 (demo-book by-name install e2e), AC-6 (fixture coexistence), AC-7 (README).
- Conflict set: _extensions/blendtutor/blendtutor.lua (hot, serialized), scripts/tests/test_quarto_bootstrap.sh, scripts/tests/test_quarto_install_render.sh, scripts/tests/test_quarto_ux.py, quarto-fixture rendered artifacts.
- Risk level: high — pins Quarto-internal libs naming, parses quarto.doc.output_file, migrates 3 test files with exact-URL assertions.

## Decision Log
- resolver — mechanism: A wins (resources= copy-only, quarto.js:128053-55; attach_to_dependency silently no-ops if parent dep missing — ordering trap); namespace: A wins (quarto-contrib/<name>-<version>, quarto.js:128068-76; B's libs/blendtutor factually wrong); codemirror: A wins (exercise-runtime.js:29-42 static import ./codemirror.js — must ship in same libs dir); conditional adapters: B wins (mirror AC-3 conditional imports, absence assertable); non-HTML gate: B adopted; probe file: B wins (NEW dedicated test file primary); rodney: A adopted (URL-rewrite regression needs runtime boot net).
- resolver — disagreement=minor; all 7 divergences resolved by code verification (quarto.js TS + origin/main blendtutor.lua); no load-bearing ambiguity remains; no user clarification needed.

### Progress
- [x] spec resolved (resolver) — pending implementation
- [x] red: NEW test_quarto_asset_deployment.sh + migrate 3 existing suites — done 2026-08-03
- [x] green: blendtutor.lua deployment + specifier rewrite — done 2026-08-03
- [x] rodney clause 11 — done 2026-08-03 (PROBES_PASS)
- [x] evidence docs/evidence/141/ — done 2026-08-03

### Surprises & Discoveries
- Quarto 1.10.18 add_html_dependency ERRORS on a missing resource file ("NotFound: lstat ...") rather than silently skipping — the spec's "silent skip" trap is version-dependent; file-on-disk assertions still the right discipline.
- quarto.doc.output_file is an ABSOLUTE path at Pandoc() time (e.g. /private/tmp/bt-probe/pages/index.html) — the libs URL helper must take the basename stem (index) for document-relative specifiers (index_files/...), not the full path.
- add_html_dependency resolves relative "assets/..." paths against the filter script's OWN directory (verified: filter at ext/filter.lua → assets resolved at ext/assets/), so by-name installs with absolute PANDOC_SCRIPT_FILE work unchanged.
- test_quarto_install_render.sh P2 guards ANY `cp ... _extensions` outside the org/repo mcmullarkey path — new hermetic-TMP helpers must copy to _extensions/mcmullarkey/blendtutor/ or P2 flags them (false-positive on a legitimate fixture-setup copy).
- **CRITICAL (rodney clause 11 caught):** ES module import specifiers MUST start with "/", "./", or "../" — a bare relative "mixed-lang_files/libs/..." throws "Failed to resolve module specifier" at runtime. The first AC-4 rodney run FAILED (PROBES_FAIL, __btExercises never set) because the bootstrap imported bare <stem>_files/... paths. AC-3's ../_extensions/... worked because ../ is a valid prefix. Fix: libs_url() emits "./"-prefixed document-relative URLs; clause-5 suite assertion pins the ./ prefix. <link href> tolerates bare paths; modules do not — the test must assert the ./ prefix, not just substring presence.
- rodney harness writes evidence to docs/evidence/139/ (its hardcoded EVIDENCE_DIR) — re-running it for AC-4 overwrites AC-3's committed evidence; revert those files and copy the report into docs/evidence/141/ instead of modifying the shared harness.

### Idempotence & Recovery
- Safe retry: re-run renders + probes (regenerated at test time).
- Rollback: git checkout -- _extensions/blendtutor/blendtutor.lua scripts/tests/; rm scripts/tests/test_quarto_asset_deployment.sh
