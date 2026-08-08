# Issue #199 — docs.yml assembles committed docs/evals/ into Pages artifact at /evals/

E2E evidence for AC-6 (smevals-eval-report). `verification: code` — shell tests
(structural pins + functional fixture sub-phase), CI-enforced via the
quarto-render job (ci.yml:140 runs `bash scripts/tests/test_docs_pages_artifact.sh`).

## Artifacts

- `test-suite.log` — full probe output: `bash scripts/tests/test_docs_pages_artifact.sh`
  → **23 passed, 0 failed** (exit 0).
- `check-docs-secondary.log` — secondary probe: `bash scripts/check-docs.sh`
  (local mirror) → exit 0, "docs: OK — merged site … evals at /evals/".

## What the probe asserts (predicate clauses)

Phase 1 (structural pins on `.github/workflows/docs.yml` build block):
- guard form `if [ -d docs/evals ]` present (clause 1)
- `[ -d docs/evals ] &&` shorthand ABSENT anywhere in the workflow (clause 1 —
  Actions runs steps under `bash -eo pipefail`; a trailing `&&` would exit 1
  when docs/evals is missing, reddening CI before AC-5 lands)
- within-step order guard < `rm -rf docs/book/book/evals` < `mkdir -p …` <
  dot-copy `cp -R docs/evals/. docs/book/book/evals/` (clause 2 — mkdir inside
  guard, no empty /evals/ nest pre-AC-5)
- step ordering demo-standalone assemble < evals guard < `.nojekyll` <
  `actions/upload-pages-artifact@v5` (clause 3 — evals lands before upload;
  all current paths preserved)
- no `|| true` / `continue-on-error: true` on build steps or the evals step
  (clause 4)
- deploy job block free of `evals` needles (clause 5 — deploy has no checkout)
- check-docs.sh mirror contract 11/11 needles (clause 6/7 — guard, dot-copy,
  double-nest assert)

Phase 3 (functional fixture sub-phase, clause 8):
- temp fixture `docs/evals/evals-fixture/index.html` inside the (real or
  simulated) committed docs/evals; guarded assemble snippet under
  `bash -euo pipefail` against scratch book_out → fixture HTML lands at
  `<scratch>/evals/evals-fixture/index.html` byte-identical, committed lesson
  nests alongside, and `! [ -e <scratch>/evals/evals ]` (no double-nest — bare
  cp would nest to `/evals/evals/<lesson>/` → 404)
- docs/evals absent → same snippet under `bash -e` exits 0 and creates no
  `evals/` dir (CI stays green pre-AC-5); fixture + scratch cleaned up via trap
- **committed docs/evals preservation pin**: the sub-phase moves pre-existing
  docs/evals aside (post-AC-5 it is committed source) and restores it
  byte-identical — a `rm -rf docs/evals` cleanup would delete real reports

## Negative cases exercised

- (a) docs/evals absent → guard exits 0, no evals nest (Phase 3 clause 8b)
- (b) `&&` shorthand → structural absence pin fails (Phase 1)
- (c) `|| true` on step → clause 4 pin fails
- (d) bare `cp -R docs/evals …` (no `/.`) → functional double-nest assert fails
  (Phase 3 clause 8a)
- (e) evals after .nojekyll/upload → ordering chain fails (Phase 1)
- (f) check-docs.sh not mirrored → MIRROR_OK 11-count fails (Phase 1)
- (g) `evals` needle in deploy block → deploy-leak assert fails (Phase 1)
- (h) mkdir outside guard → within-step order pin fails (Phase 1 clause 2)

## Regressions

- Phase 2 (`check-docs.sh` end-to-end local render + assemble + assert) passes —
  existing mdBook/rustdoc/examples/demo-book/demo artifact survives; no
  `rm -rf docs/book/book/*` clobber introduced (clause 9).

Generated 2026-08-07 on branch `199-evals-pages`.
