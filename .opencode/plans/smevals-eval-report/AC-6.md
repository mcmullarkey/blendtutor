---
ac: 6
depends_on: none (guard keeps job green pre-AC-5)
risk: medium
status: complete
---

## Final Spec (resolver-merged)

## AC-6
Extend .github/workflows/docs.yml to assemble committed docs/evals/ into the GitHub Pages artifact at /evals/ following the existing cp-into-docs/book/book pattern, guarded so the job stays green before any report exists. Preserve all current paths. Extend scripts/tests/test_docs_pages_artifact.sh to assert the evals nest.

### Executable Spec
- predicate clause 1: "docs.yml build job contains a step with guard form `if [ -d docs/evals ]; then … fi` (literal `if [ -d docs/evals ]` present) AND the shorthand `[ -d docs/evals ] &&` is ABSENT anywhere in the workflow (Actions runs steps under `bash -eo pipefail`; a trailing && chain exits 1 when the dir is missing → CI red before AC-5 lands)."
- predicate clause 2: "Within that step, line order is: guard < `rm -rf docs/book/book/evals` < `mkdir -p docs/book/book/evals` < literal dot-copy `cp -R docs/evals/. docs/book/book/evals/` (trailing `/.`; bare cp double-nests to `/evals/evals/<lesson>/` → 404)."
- predicate clause 3: "Step ordering in build block: demo-standalone assemble < evals guard < `.nojekyll` < `actions/upload-pages-artifact@v5` (evals content lands before single-artifact upload; all current paths preserved)."
- predicate clause 4: "No `|| true` / `continue-on-error` on or around the evals step."
- predicate clause 5: "Deploy-leak: needles `evals`, `docs/evals` absent from the deploy job block (deploy has no checkout)."
- predicate clause 6: "Mirror contract: `scripts/check-docs.sh` (under `set -euo pipefail`, line 14) contains the same guarded assemble against `$book_out` — `if [ -d docs/evals ]`, `rm -rf \"$book_out/evals\"`, `mkdir -p \"$book_out/evals\"`, `cp -R docs/evals/. \"$book_out/evals/\"` — plus a double-nest assert `! [ -e \"$book_out/evals/evals\" ]` inside the guard."
- predicate clause 7: "Test Phase 1 extends: L_EVALS_GUARD/L_EVALS_CP block_line needles, ordering chain (clause 3), `&&`-shorthand absence pin (clause 1), MIRROR_OK needle list +3 → count 8→11, deploy-leak list +`evals`."
- predicate clause 8: "Functional fixture sub-phase: temp fixture `docs/evals/evals-fixture/index.html`; run the guarded assemble snippet under `bash -euo pipefail` against a scratch book_out → fixture HTML lands at `<scratch>/evals/evals-fixture/index.html`, byte-identical, AND `! [ -e <scratch>/evals/evals ]`; then with fixture removed, same snippet under `bash -e` exits 0 and creates no `evals/` dir. Fixture + scratch cleaned up (trap)."
- predicate clause 9: "No regression: existing Phase 1/2 survival asserts stay green; no `rm -rf docs/book/book/*` clobber introduced."
- probe: "bash scripts/tests/test_docs_pages_artifact.sh (wired in ci.yml quarto-render job at ci.yml:128; ci.yml itself NOT modified by this AC). Secondary: bash scripts/check-docs.sh (local mirror; if-guard skips evals when docs/evals absent)."
- negative: "(a) docs/evals missing → guard snippet exits 0 under bash -e, no evals nest, CI stays green (functional, clause 8); (b) && shorthand instead of if/then/fi → structural absence pin fails; (c) || true on the step → fail; (d) bare `cp -R docs/evals docs/book/book/evals/` (no `/.`) → functional double-nest assert fails; (e) evals step after .nojekyll or after upload → ordering chain fails; (f) check-docs.sh not mirrored → MIRROR_OK 11-count fails; (g) `evals` needle in deploy block → deploy-leak assert fails; (h) mkdir outside guard → empty /evals/ nest published pre-AC-5 → within-step order pin fails."
- verification: "code · shell test (structural pins + functional fixture sub-phase), CI-enforced via quarto-render job"
- fixture status: "NEW — runtime temp fixture (mkdir -p docs/evals/evals-fixture + echo HTML), removed via trap after assertion"
- rubric anchor: "§4.1 mirror contract (check-docs.sh mirrors docs.yml build steps); §5.1 guarded single-responsibility step"

### Design Intent
- **Types / interfaces (§1):** Guard predicate `[ -d docs/evals ]` encodes the two legal repo states (pre-/post-AC-5); no illegal "half-assembled evals" state reachable.
- **Pure / effectful (§2):** Step is effectful shell; kept thin and idempotent (`rm -rf` + `mkdir` + dot-copy), mirroring api precedent.
- **Boundary cuts (§3):** Assemble logic lives in docs.yml build job + check-docs.sh mirror only; deploy job untouched; ci.yml untouched.
- **Module responsibility (§4):** check-docs.sh header/assert comments extended to name the `/evals/` contract and the `/.` double-nest trap, matching existing demo-book annotation style.
- **Function discipline (§5):** One guarded step does one thing; test gains one fixture sub-phase with trap cleanup; each assertion independently failing.

### Decisions on Flagged Divergences
- **mkdir placement:** inside guard (adversarial wins — no empty /evals/ nest published pre-AC-5).
- **Stale-content removal:** `rm -rf docs/book/book/evals` inside guard, before mkdir (api precedent docs.yml:67 — docs/evals is committed source; lesson deletion is a committed deletion; cp-only never removes stale reports).
- **Guard form:** if/then/fi only; `&&` shorthand pinned absent (verified: Actions run: = bash -eo pipefail).
- **Fixture sub-phase:** inline functional snippet in test (catches cp semantics) coupled to docs.yml via literal needle pins (catches drift between replicated snippet and workflow).

### Technical Context
- `.github/workflows/docs.yml` — insert evals step between line 111 (end of demo-standalone assemble) and line 113 (.nojekyll):
  ```yaml
  - name: Assemble evals into artifact (/evals/)
    run: |
      if [ -d docs/evals ]; then
        rm -rf docs/book/book/evals
        mkdir -p docs/book/book/evals
        cp -R docs/evals/. docs/book/book/evals/
      fi
  ```
- `scripts/check-docs.sh` — insert guarded evals assemble + double-nest assert after line 84 (demo assemble), before the assert block ~line 88:
  ```bash
  if [ -d docs/evals ]; then
    rm -rf "$book_out/evals"
    mkdir -p "$book_out/evals"
    cp -R docs/evals/. "$book_out/evals/"
    test ! -e "$book_out/evals/evals" || { echo "docs: evals double-nest" >&2; exit 1; }
  fi
  ```
- `scripts/tests/test_docs_pages_artifact.sh` — Phase 1: L_EVALS_GUARD/L_EVALS_CP via existing block_line() (line 85), ordering chain extension (demo-assemble < evals-guard < nojekyll < upload), `! grep -qF '[ -d docs/evals ] &&'` absence pin on full workflow, MIRROR_OK needles +3 (`if [ -d docs/evals ]`, `docs/evals/.`, `"$book_out/evals"`) with count 8→11 (line 237), deploy-leak needle list +`evals`; new fixture sub-phase with trap cleanup per predicate clause 8.
- Architecture notes: GH Actions run: executes `bash --noprofile --norc -eo pipefail` — if/then/fi is the only safe guard form. Root .nojekyll covers underscore paths under /evals/; no per-subdir file needed. Builder must confirm smevals output nesting locally with throwaway `uvx smevals build -o /tmp/eval-test` (AC-5 coupling).

### Dependencies
- **Depends on:** none at build time (guard keeps job green pre-AC-5); logically delivers user value only after AC-5 commits docs/evals/<lesson>/.
- **Blocks:** none (evidence-link consumption is AC-7/report-side, soft).
- **Conflict set:** .github/workflows/docs.yml, scripts/check-docs.sh, scripts/tests/test_docs_pages_artifact.sh — all AC-6-exclusive in this feature; no serialization needed. ci.yml NOT modified.
- **Risk level:** medium — wrong guard form reds CI on every push until AC-5 lands (mitigated by clause 1 absence pin + clause 8 functional green-when-absent test); otherwise low blast radius.

### Progress
- [x] Speculators A+B returned (2026-08-07)
- [x] Resolver merged (2026-08-07)
- [x] RED: test extended (L_EVALS_* needles, &&-absence pin, MIRROR_OK 8→11, deploy-leak +evals, Phase 3 fixture sub-phase) — failed on all 5 evals needles + 1 test bug (2026-08-07)
- [x] GREEN: docs.yml guarded evals step + check-docs.sh mirror + double-nest assert — 21/21 pass (2026-08-07)
- [x] Committed: 116fbf7 test(red) + 5f4ad51 feat (branch 199-evals-pages)

### Decision Log
- 2026-08-07 — Guard form if/then/fi (set -e safe); && shorthand pinned absent
- 2026-08-07 — mkdir inside guard (no empty nest pre-AC-5)
- 2026-08-07 — rm -rf before mkdir (api precedent; stale lesson reports removed)
- 2026-08-07 — Dot-copy literal cp -R docs/evals/. (double-nest trap)
- 2026-08-07 — Fixture cleanup removes WHOLE docs/evals dir, not just evals-fixture subdir: `rm -rf docs/evals "$SCRATCH"` in trap + clause 8b. Removing only the fixture subdir leaves an empty docs/evals/ → `[ -d docs/evals ]` stays true → mkdir runs → clause 8b false-positives "nest created despite absent".

### Surprises & Discoveries
- Clause 8b negative needed `rm -rf docs/evals` (the WHOLE temp dir), not just the fixture subdir: a leftover empty docs/evals/ dir keeps `[ -d docs/evals ]` true, so the guard body runs and the "no nest created when absent" assert false-positives. The guard's predicate is dir-existence, not content-non-emptiness.
- Phase 2 check-docs.sh runs fully locally (quarto + mdbook + cargo all installed): ~2-3min wall (cargo doc + 2 release example builds). CI quarto-render job gets full coverage too, not just Phase 1 pins.

### Idempotence & Recovery
- Safe retry: re-run bash scripts/tests/test_docs_pages_artifact.sh; additive step + test clauses
- Rollback: remove evals step from docs.yml + check-docs.sh mirror + test pins