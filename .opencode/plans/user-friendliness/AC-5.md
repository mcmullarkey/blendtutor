---
ac: 5
depends_on: none
risk: med-high
status: complete
---

### AC-5 — demo-standalone removal
- Spec mode: spec-resolved
- Key files (full blast radius from repo-wide grep of `demo-standalone`):
  - `demo-standalone/` (delete dir)
   - `.github/workflows/docs.yml` — remove "Render demo-standalone", "Fix COI service-worker scope", "Assemble demo-standalone (/demo/)" steps; remove verify-live job entirely (user decision) (currently probes `DEPLOYED_URL + demo/`)
  - `.github/workflows/ci.yml` — remove the "Standalone demo render test" step (`test_demo_standalone_render.sh`)
  - `scripts/fix-demo-coi-scope.sh` (delete), `scripts/check-docs.sh` (remove demo legs)
  - `scripts/tests/test_demo_standalone_render.sh` (delete), `scripts/tests/test_docs_pages_artifact.sh` (remove demo pins)
   - `rodney-probes/pages-live.js`, `pages-live-core.js`, `pages-live-core.test.js` (probe targets /demo/) — verify-live job is DELETED per user decision (not repointed). If the pages-live suite's sole consumer is verify-live, delete the suite too; if other consumers exist (e.g. ci.yml PR checks), retarget them to /demo-book/. Builder/spec must verify consumers before choosing.
  - Historical `docs/evidence/150|152|153|170|199|212/` logs reference demo-standalone — DO NOT touch (historical records).
- Invariants/constraints:
  - `check-docs.sh` mirrors docs.yml (mirror-contract pinned by `test_docs_pages_artifact.sh`) — the two must stay in lockstep.
  - Quarto-distribution test pins a "demo link" in README (see AC-3) — that link points at /demo/ today; AC-5 must update it to /demo-book/ or an example site, coordinated with AC-3's README rewrite (README conflict → this AC's README edit orders with AC-3; keep the edit minimal: swap the URL only).
- Prior art: none — pure deletion + re-pointing.
- Verification: code (CI green: remaining tests after pin removal; mirror contract intact).
- Test seam: existing `test_docs_pages_artifact.sh` + `check-docs.sh`.

### Progress
- [x] red: test_docs_pages_artifact.sh updated to post-#227 contract (demo-standalone pins removed, absence pins added) — 14 FAIL against current tree, all absence pins (2026-09-08, commit b14651c)
- [x] green: demo-standalone/, fix-demo-coi-scope.sh, test_demo_standalone_render.sh, test_verify_live_wiring.sh, pages-live suite deleted; docs.yml/ci.yml/check-docs.sh/.gitignore edited — 35/35 PASS + check-docs.sh e2e exit 0 + distribution 91/91 (2026-09-08, commit 9e6667b)
- [x] evidence at docs/evidence/227/ (grep-proof, test-suite, check-docs-e2e, quarto-distribution logs)

### Decision Log
- pages-live suite DELETED (not retargeted): consumer analysis showed sole consumer = docs.yml verify-live job (+ its own wiring test test_verify_live_wiring.sh, deleted with it); ci.yml rodney-probes job runs key-page/feedback probes only. Matches the issue's delete-if-sole-consumer branch.
- deploy job `outputs.page_url` removed with verify-live — it was the output's sole consumer (environment url reads steps.deployment.outputs directly).
- test_verify_live_wiring.sh deleted beyond the issue's literal blast radius: it pinned the verify-live job + pages-live wiring; with the job deleted it pinned dead wiring and would fail CI. Its ci.yml step removed with it.
- README untouched (user decision — demo-link swap deferred to #225 to avoid README race with parallel AC-3 work). test_quarto_distribution.sh README pins verified still green (91/91).
- No ADR: pure deletion + re-pointing, no new interface/boundary (issue Design Intent: "Prior art: none").
- Absence pins grep for the literal tokens in docs.yml/check-docs.sh — explanatory comments in those files reworded to avoid the tokens so the pins stay meaningful (zero-occurrence contract).
- 2026-09-08 — review cycle 1 carryover: scripts/tests/test_demo_docs.sh c1/c3/c9 pins the deleted /demo/ URL + standalone capability claims (manual-run suite, not CI-wired). MUST update in lockstep with issue #225's README demo-link swap — otherwise stale spec silently pins a dead URL. (grep-proof.txt listing nit — 3 missing historical paths in docs/evidence/227/grep-proof.txt — noted, non-blocking, no fix scheduled.)

### Surprises & Discoveries
- The issue's blast radius missed scripts/tests/test_verify_live_wiring.sh (#156) — it pins the verify-live job and the pages-live wiring, so deleting verify-live without it would redden CI. Deleted it + its ci.yml step in the same change.
- feedback-probe.js / key-page-probe.js / rodney-chrome.sh carried comment-only citations to pages-live.js (pattern/line references); cleaned so no dangling refs to a deleted file remain.
- The absence pins initially failed on my own explanatory comments ("#227 removed the demo-standalone legs") — absence-by-grep pins require the token to be fully absent from the pinned file, so removal notes must not name the removed token in the pinned files.

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open
