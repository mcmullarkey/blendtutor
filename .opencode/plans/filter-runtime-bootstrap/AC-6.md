---
ac: 6
depends_on: [3, 4]
risk: medium
status: in-progress
---

# AC-6: Verify fixture hand-written bootstraps coexist with filter opt-out — no double-start, no clobber

## Executable Spec (resolver-merged, 9 clauses)
- predicate: given quarto-fixture/ux.qmd (mock adapter), webr.qmd, feedback.qmd — all carrying bt-auto-bootstrap: false — rendered with the AC-3/AC-4 filter, then:
  1. Zero auto-bootstrap on ALL THREE rendered pages: none of rendered ux.html/webr.html/feedback.html contains data-bt-bootstrap="auto". Closes load-bearing gap: test_quarto_bootstrap.sh:263-284 currently renders webr.qmd ONLY for zero-marker assert; ux/feedback verified YAML-side only.
  2. Exactly-one scanExercises module script per page: each rendered page has exactly one <script type="module"> importing scanExercises — the hand-written bootstrap, never two.
  3. Hand-written specifiers UNCHANGED — ../_extensions/blendtutor/assets/..., NOT libs-dir: AC-4 URL rewriting scoped to filter-injected bootstraps only; static HTML grep is authoritative (rodney is not — exercise-ux.js:158-188 ensureAssetSymlink masks specifier rewrites at runtime).
  4. Per-fixture adapter tokens preserved: ux keeps mock-adapter import, webr keeps webr-adapter.js, feedback keeps exercise-feedback.js.
  5. Real-page rodney population asserts — NO substitution: rodney navigates REAL rendered ux.html, webr.html, feedback.html and asserts window.__btExercises.length === 3 (ux), === 2 (webr), === 2 (feedback). CDN-safe: assert sync population (exercise-runtime.js:488, set before :491 boot await); never await boot completion. feedback-probe.js-style generateProbeHtml mock substitution (feedback-probe.js:188-216 — verified) is FORBIDDEN as evidence for this clause.
  6. Double-start console.warn spy: spy on console.warn across the 3 fixture pages; assert ZERO "[blendtutor] start() called twice" warns (exercise-runtime.js:436). Defense-in-depth behind clause 1 — catches runtime-emitted duplicates invisible to static grep.
  7. YAML static pin: all 3 fixture qmds grep-assert bt-auto-bootstrap: false (already at test_quarto_bootstrap.sh:255-261 — keep, extend loop unchanged).
  8. Migration trigger semantics: any failure of clauses 1-5 IS the migration trigger (rewrite hand-written bootstrap or fix filter); green path performs NO fixture edits. No "speculative migration".
  9. Regression suites green: test_quarto_ux.py, test_quarto_feedback.py, validate-webr-adapter.js, test_quarto_asset_deployment.sh, rodney-probes/exercise-ux.js all pass unmodified.
- probe:
  # EXTEND scripts/tests/test_quarto_bootstrap.sh clause 4 — generalize :263-284 render+assert block from webr-only to ux/webr/feedback: zero marker, exactly-one scanExercises, ../_extensions/ specifiers, per-fixture token
  bash scripts/tests/test_quarto_bootstrap.sh   # clauses 1-4, 7 (ALREADY CI-wired at ci.yml:103)

  # EXTEND rodney-probes/auto-bootstrap.js — add ux.html/webr.html/feedback.html real-page navigation (clauses 5-6); parameterize EVIDENCE_DIR (env, default docs/evidence/139 unchanged); AC-6 run overrides to docs/evidence/<AC-6-issue>/
  EVIDENCE_DIR=docs/evidence/<AC-6-issue> uv run node rodney-probes/auto-bootstrap.js   # clauses 5-6

  # Regression — unmodified
  uv run node rodney-probes/exercise-ux.js
  uv run python scripts/tests/test_quarto_ux.py
  uv run python scripts/tests/test_quarto_feedback.py
  uv run node scripts/tests/validate-webr-adapter.js
  bash scripts/tests/test_quarto_asset_deployment.sh
- negative:
  1. webr-only render gap — clause 4 block stays webr-only; ux/feedback double-start or lose bootstrap undetected. Killed by clause 1 (generalized loop).
  2. generateProbeHtml substitution — new rodney asserts reuse feedback-probe.js:188-216 mock-HTML pattern; mock DOM always passes while real page broken. Killed by clause 5 (real rendered pages only).
  3. AC-4 rewrite leak — filter rewrites hand-written specifiers to libs-dir URLs; symlink (ensureAssetSymlink) masks it in dev, 404s in real installs. Killed by clause 3 (static grep authoritative).
  4. Silent double-start — opt-out regresses; guard no-ops second start; population asserts still pass. Killed by clause 6 (warn spy) + clause 1.
  5. Opt-out silently dropped in qmd edit — auto-bootstrap re-emitted. Killed by clause 7 (static YAML pin).
  6. Boot-await flake — probe awaits WebR/Pyodide ready; CDN offline flakes CI. Killed by clause 5 (sync :488 population, never await :491).
  7. EVIDENCE_DIR clobber — re-running auto-bootstrap.js overwrites docs/evidence/139 (AC-3 evidence, hardcoded at :31). Killed by env parameterization + AC-6 override in probe.
  8. Render-noise commit — regenerated fixture HTML / *_files/ committed. Killed by fixture-status hygiene (rm before commit).
- verification: code + rodney · code = clauses 1-4, 7-9 (extended test_quarto_bootstrap.sh + suite regressions, CI-wired); rodney = clauses 5-6 (extended auto-bootstrap.js, real-page boot)
- fixture status: existing — quarto-fixture/ux.qmd, webr.qmd, feedback.qmd (opt-out present at line 4 of each, verified; NO edits unless clause 1-5 failure triggers migration per clause 8). MODIFY scripts/tests/test_quarto_bootstrap.sh (extend clause 4 render loop :263-284 → all 3 fixtures) and rodney-probes/auto-bootstrap.js (add 3 real-page asserts + warn spy + EVIDENCE_DIR env param). NO new files — scripts/tests/webr-probe.js exists but is vision-probe-only, NOT CI-wired; do NOT reuse or wire it. Regression-only: exercise-ux.js, test_quarto_ux.py, test_quarto_feedback.py, validate-webr-adapter.js, test_quarto_asset_deployment.sh.
- rubric anchor: §1.1 (exactly-one-bootstrap invariant; YAML pin makes opt-out disappearance unrepresentable), §2 (sync :488 population = pure fact asserted without effectful boot await), §3.4 (hand-written ../_extensions/ vs filter-injected libs-dir seam = the opt-out), §5 (extend existing harnesses; no bespoke probe file; no substituted HTML)

## Design Intent
- §1: bt-auto-bootstrap: false is the type-level switch making dual-bootstrap unrepresentable; exactly-one scanExercises import + zero-marker are its DOM-level invariant probes; static YAML pin makes opt-out removal unrepresentable.
- §2: pure = YAML pins, specifier strings, sync __btExercises set (:488); effectful = quarto render + rodney boot. CDN-safe strategy asserts the pure fact, never the effectful boot.
- §3: seam = opt-out. Hand-written bootstraps keep source-tree specifiers (AC-6 scope); filter-injected keep libs-dir specifiers (AC-4 scope). Per-fixture adapter contracts cut at the fixture joint.
- §4: fixture qmds document WHAT (hand-written bootstrap wins + opt-out), NOT (filter auto-start, libs rewriting). Extended bootstrap.sh clause 4 owns the coexistence contract; extended auto-bootstrap.js owns real-page runtime proof.
- §5: extend two existing wired harnesses rather than create NEW unwired files; one warn-spy assertion; page-parametric probe loop; no HTML substitution path.

## Technical Context
- Files likely touched: MODIFY scripts/tests/test_quarto_bootstrap.sh (clause 4 loop generalization — already CI-wired ci.yml:103, no new wiring needed); MODIFY rodney-probes/auto-bootstrap.js (ux/webr/feedback real-page asserts, warn spy, EVIDENCE_DIR env param at :31); regression-read 5 files (see fixture status); qmds untouched on green path.
- Traps: (a) ensureAssetSymlink (exercise-ux.js:158-188) masks specifier rewrites — static grep authoritative, rodney never verifies specifiers; (b) generateProbeHtml (feedback-probe.js:188-216) substitution — real pages only; (c) scripts/tests/webr-probe.js vision-only ambiguity — NOT the coverage vehicle; (d) EVIDENCE_DIR hardcode docs/evidence/139 (auto-bootstrap.js:31) — parameterize, AC-6 evidence → docs/evidence/<AC-6-issue>/; (e) render noise quarto-fixture/*_files/ — rm before commit; (f) rodney harness runs via uv run node; (g) exact count pins (=== 3/2/2) break intentionally on fixture exercise edits — loosen only via fixture-edit PR.
- Regression surface: ~9 files (2 MODIFY + 5 regression + 0-2 qmd + ci.yml check-only).

## Dependencies
- Depends on: AC-3 (#139/PR #140 — opt-out + filter emission), AC-4 (#141/PR #142 — libs-dir specifiers, non-clobber), AC-2 (double-start guard exercise-runtime.js:436 — clause 6 target).
- Blocks: none (AC-7 README documents pattern prose only).
- Conflict set: test_quarto_bootstrap.sh, auto-bootstrap.js, 3 fixture qmds, 5 regression files — DISJOINT from AC-5 (demo-book/, test_quarto_distribution.sh) → Batch 4 parallel-safe.
- Risk level: medium — no mechanism change; risk in probe quality (substitution/symlink masking), opt-out scoping regressions, render-noise hygiene.

## Decision Log
- resolver — opt-out key: both proposers wrote blendtutor-autostart: false — WRONG, corrected to bt-auto-bootstrap: false (matches AC-3); ux exercise count: A's ux===2 wrong, corrected to ===3 (verified fixture has 3 exercises); verification approach: EXTEND-over-NEW (test_quarto_bootstrap.sh + auto-bootstrap.js already CI-wired — AC-4 retro lesson: unwired tests escape); B's real-page + warn-spy adopted; feedback-probe generateProbeHtml substitution verified TRUE — real pages required; ensureAssetSymlink verified — static grep authoritative.
- resolver — disagreement=minor; all divergences resolved by codebase verification; no user clarification needed.
- builder — warn-spy injection mechanism: rodney 0.4.0 has NO addInitScript/console-capture (verified from bundled binary --help + uv cache source). Pre-load console.warn spy therefore cannot be installed via rodney post-navigation (module script runs during page load). Adopted serve-time injection: the harness's static server injects a passive observer <script> into the <head> of the REAL rendered ux/webr/feedback.html (spy records to window.__btWarnSpy, forwards to original warn). This is an OBSERVER, not generateProbeHtml-style bootstrap substitution (the real module script stays byte-identical; assertions run against real runtime). Documented in auto-bootstrap.js.
- builder — evidence metadata: probe-report.json hardcodes issue/branch (139/139-filter-auto-bootstrap). Parameterized: issue = basename(EVIDENCE_DIR), branch = `git rev-parse --abbrev-ref HEAD` — correct metadata for both AC-3 (docs/evidence/139) and AC-6 (docs/evidence/144) runs.

### Progress
- [x] spec resolved (resolver) — pending implementation
- [x] red: extend test_quarto_bootstrap.sh clause 4 to all 3 fixtures — 2026-08-03
- [x] green: verify zero-marker + exactly-one + specifiers + tokens on rendered pages — 2026-08-03 (39/0 pass; negative control: removing ux opt-out → FAIL zero-marker + FAIL exactly-one, reverted)
- [ ] rodney clauses 5-6 (real-page + warn spy) — pending
- [ ] evidence docs/evidence/144/ — pending
- [ ] regression suites green (exercise-ux.js, test_quarto_ux.py, test_quarto_feedback.py, validate-webr-adapter.js, test_quarto_asset_deployment.sh) — pending final run

### Idempotence & Recovery
- Safe retry: re-run renders + probes (regenerated at test time).
- Rollback: git checkout -- scripts/tests/test_quarto_bootstrap.sh rodney-probes/auto-bootstrap.js
