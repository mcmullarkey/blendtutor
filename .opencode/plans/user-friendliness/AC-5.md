---
ac: 5
depends_on: none
risk: med-high
status: spec
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
- (none yet)

### Decision Log
- (none yet)

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open
