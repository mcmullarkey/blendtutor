---
ac: 7
depends_on: [4, 5]
risk: low
status: complete
---

# AC-7: README quick-start — zero-bootstrap flow + feedback opt-in + COI caveat (with test lockstep)

## Executable Spec (resolver-merged, 11 clauses)
- predicate:
  1. Quick-start zero-bootstrap flow — `quarto add mcmullarkey/blendtutor` + `filters:` by-name + explicit no-hand-written-bootstrap prose.
  2. Zero-bootstrap example integrity — quick-start fence contains NO script/module/scanExercises/start(/buildRegistry tokens.
  3. Opt-out documented — `bt-auto-bootstrap: false` literal + prose.
  4. Feedback opt-in — `exercise-feedback.js` + `mountAllFeedback`, manual, BYOK, NOT auto-mounted.
  5. COI book-mode caveat — does NOT function in `type: book`; SW scope can't cover `_output/`.
  6. Demo book COI honesty — :254 "COI configuration" overclaim removed.
  7. Stale mechanism removed — :174-176 "locates its assets relative to the filter script" dropped; keep install-path-independent OUTCOME, drop MECHANISM.
  8. No stale runtime bootstrap instructions.
  9. Command/version consistency — matches ci.yml:148 (full org/repo, no short-form), _extension.yml:3 version 0.1.0.
  10. Existing Group 1 clauses preserved (install cmd, both-lang syntax, BYOK, min Quarto, demo link, org/repo path, ADR annotation).
  11. Clause 6d lockstep — drops `relative to.*filter|PANDOC_SCRIPT_FILE` alternation, asserts new mechanism or outcome.
- probe: `bash scripts/tests/test_quarto_distribution.sh` (primary, CI-wired); greps: quarto add mcmullarkey/blendtutor present + no short form; bt-auto-bootstrap: false present; exercise-feedback.js|mountAllFeedback present; book mode|type: book present; no "locates its assets relative to the filter script"; no import .*exercise-runtime.js; no unqualified COI configuration; manual: quick-start fence has no script tag.
- negative: auto-bootstrap invisible (reader hand-writes); quick-start ships a bootstrap (double-start); opt-out undocumented; feedback mechanism invisible (key set, no UI); COI overclaim in demo book; stale mechanism survives (wrong debug path); test encodes stale claim (false-fail or docs lie); command drift (short-name install fails).
- verification: code + manual (prose tone, caveat clarity).
- fixture status: README.md (MODIFY) + scripts/tests/test_quarto_distribution.sh (MODIFY: extend Group 1, update clause 6d); NO new files.
- rubric anchor: §4.1, §5, §1, §3.

## Design Intent
- §1: full org/repo mcmullarkey/blendtutor + bt-auto-bootstrap: false literal pinned; stale mechanism removed.
- §2: pure = README text + grep assertions; effectful = quarto render (already in distribution test).
- §3: docs cut at verified-flow joints — quick-start / opt-out / feedback / COI caveat.
- §4: README names WHAT (verified end-state), NOT internal mechanism names.
- §5: extend ONE existing CI-wired test, update ONE stale clause.

## Technical Context
- README.md :153-254 → Installation :164 (zero-bootstrap quick-start + filters snippet, replaces stale mechanism :174-176), Authoring syntax, BYOK :217 (feedback opt-in subsection per quarto-fixture/feedback.qmd pattern), COI :228 (book-mode limitation), Demo book :243 (fix COI overclaim).
- scripts/tests/test_quarto_distribution.sh Group 1 — extended with clauses 7-12; clause 6d updated to outcome-level only.
- Verified anchors: ci.yml:148 `quarto add mcmullarkey/blendtutor --no-prompt`; demo-book/_quarto.yml:15; version 0.1.0 = _extension.yml:3 = BT_DEP_VERSION blendtutor.lua:137.
- COI wording mirrors AC-5 surprise #2 (Quarto rewrites coi src to own site_libs copy; SW scope can't cover _output/).

## Dependencies
- Depends on: #143 (AC-5, merged), #139 (AC-3 auto-bootstrap), #142 (AC-4 asset deployment), #145 (AC-6 fixture coexistence).
- Conflict set: README.md, test_quarto_distribution.sh.

## Progress
- [x] RED: extended test_quarto_distribution.sh Group 1 (clauses 7-12 + 6d lockstep) — confirmed fail (README lacked quick-start/opt-out/feedback/COI caveat; stale mechanism present) (2026-08-03)
- [x] GREEN: README.md Quarto Extension section — quick-start (by-name filter + zero-hand-written-bootstrap prose, fence token-clean), opt-out (bt-auto-bootstrap: false), feedback opt-in (exercise-feedback.js + mountAllFeedback, manual, BYOK), COI book-mode limitation (COI + Demo book sections), demo book COI honesty (:254 overclaim removed), stale mechanism replaced with outcome-level install-path independence, version 0.1.0 stated (2026-08-03)
- [x] Verify: test_quarto_distribution.sh 73 passed / 0 failed; regression suite green (test_quarto_filter.sh, test_coi_filter.sh, test_quarto_ux.py, test_quarto_feedback.py, test_quarto_bootstrap.sh, test_quarto_asset_deployment.sh, test_quarto_install_render.sh, test_quarto_render.sh, test_sync_assets.sh, verify_asset_scoping.py, sync-quarto-assets.sh drift check) (2026-08-03)
- [x] Manual review: prose tone natural, COI book-vs-standalone caveat clear, no internal API names (libs_url/add_html_dependency/PANDOC_SCRIPT_FILE) leak (2026-08-03)
- [x] Evidence: docs/evidence/147/ (grep new-present/old-absent + distribution + test-suite logs) (2026-08-03)

## Decision Log
- Quick-start fence extracted via awk region (`#### Quick start` → `#### Auto-bootstrap opt-out`) so clause 8 token-integrity greps scope to the quick-start example only, not the whole README.
- Feedback opt-in snippet imports ONLY exercise-feedback.js (never exercise-runtime.js) — keeps the `no import .*exercise-runtime.js` probe negative satisfiable while documenting the manual opt-in per feedback.qmd pattern.
- Clause 11 covers BOTH COI book-mode caveat AND demo book honesty in one clause (caveat appears in both sections per trap list).
- Version 0.1.0 pinned in installation prose to satisfy command/version consistency clause (matches _extension.yml:3).

## Surprises & Discoveries
- README's existing Authoring syntax section already uses nested 3-backtick fences (outer ``` containing ```r) — mirrored that style in the quick-start example; GFM treats ```r as a code-fence *info string*, not a closing fence, so the block renders literally.
- The opt-out subsection prose ("leaves bootstrapping to you") stays outside the awk-extracted quick-start region, so clause 8's token-integrity check cannot be tripped by it — region scoping is load-bearing, not the fence itself.
