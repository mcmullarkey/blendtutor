# Decomposition: github-pages-deploy

## Feature Goal
Deploy the blendtutor Quarto demo to the repo's existing GitHub Pages site (mcmullarkey.github.io/blendtutor/, currently served from `docs/book/book` artifact by `.github/workflows/docs.yml`) and prove with rodney probes against the LIVE deployed URL that exercises mount editors and actually execute code in-browser for BOTH Python and R. COI is dead in Quarto `type: book` renders (AC-5 empirical finding; SW scope cannot cover pages rendered into `_output/` with assets under `site_libs/`), so interactive R requires standalone documents (`coi: true` YAML, SW script stays in source tree per ADR-0015). The deployment therefore ships BOTH the demo-book (static fallback display + interactive Python) AND standalone demo page(s) with `coi: true` (interactive R + Python).

## AC Table
| AC | Description | Dependencies | Conflict Set | Risk |
|----|-------------|--------------|--------------|------|
| 1  | Add standalone interactive demo page(s) (new `demo-standalone/` dir, `type: default` render, R exercise with `coi: true` YAML + Python exercise) with a render/asset-resolution test following the `scripts/tests/test_quarto_render.sh` pattern | none | `demo-standalone/**` (new), `scripts/tests/test_demo_standalone_render.sh` (new), `.github/workflows/ci.yml` (add one step to existing quarto-render job) | medium |
| 2  | Extend docs.yml build job to render demo-book AND demo-standalone into the Pages artifact (nested paths, e.g. `/demo-book/` and `/demo/`), mirroring the existing `/api` and `/examples/{r,python}` nesting pattern; add `.nojekyll` to artifact root (Quarto emits underscore-prefixed `*_files/` dirs; belt-and-suspenders even though deploy-pages action skips Jekyll) | AC-1 | `.github/workflows/docs.yml` | medium |
| 3  | Add live-URL rodney probe harness (`rodney-probes/pages-live.js`, parameterized by `DEPLOYED_URL` env, defaulting to local static server for local runs) that asserts: editors mount (`.cm-editor` count), `__btExercises` populated, `window.crossOriginIsolated === true` on COI page, and REAL execution — click `.bt-run-btn` / drive adapter run → `.bt-status[data-status]` reaches pass|fail terminal state — for R (webR, awaits real CDN boot) AND Python (pyodide); writes evidence report under `docs/evidence/<branch>/` | AC-1 | `rodney-probes/pages-live.js` (new), `docs/evidence/**` (generated, gitignored evidence dirs are per-branch) | high |
| 4  | Wire a `verify-live` job into docs.yml that runs after `deploy` (needs: deploy), installs uv + node deps, and executes the AC-3 probe against `${{ steps.deployment.outputs.page_url }}`; probe failure fails the job (post-deploy alarm, NOT a gate — deploy already happened) | AC-2, AC-3 | `.github/workflows/docs.yml` | medium |
| 5  | Update README demo-book/deployment docs with live URLs, what works where (book = Python interactive + static fallback; standalone = R + Python interactive), and the book-mode COI limitation pointer (partially documented at README.md:303-306, 320-321 — extend, don't duplicate) | AC-2 | `README.md` | low |

## Dependency DAG
AC-1 → AC-2 → AC-4
AC-1 → AC-3 → AC-4
AC-2 → AC-5

## Hot Conflict Files
- `.github/workflows/docs.yml`: touched by AC-2 (build steps) and AC-4 (verify job) — serialize: AC-2 lands before AC-4.
- `.github/workflows/ci.yml`: touched by AC-1 only (single new test step). No other AC touches it.
- `README.md`: AC-5 only. No conflict.
- `demo-book/` qmds: NOT modified by any AC (deployed as-is). No conflict.

## Suggested Batch Schedule
- Batch 1 (sequential): AC-1 (foundation — everything else needs the standalone pages)
- Batch 2 (parallel): AC-2 (docs.yml build/deploy), AC-3 (probe harness, locally testable against local server with internet)
- Batch 3 (parallel): AC-4 (docs.yml verify job — same batch as AC-5; docs.yml hot file safe because AC-2 merged in batch 2), AC-5 (README)

## Open Questions
- [needs-clarification] Confirm repo Pages source is "GitHub Actions" (docs.yml already deploys successfully today, so almost certainly yes — but the demo will 404 if someone switched it to branch-based deploys). Verify in repo Settings → Pages before AC-2 merges; no code change needed either way.
- [needs-clarification] URL layout: proposed nesting is `/demo-book/` (book) + `/demo/` (standalone) under the existing site root, mirroring `/api` and `/examples/`. Alternative: top-level separate prefixes. Decomposer recommends the mirror pattern; flag if user wants different paths (affects AC-2 paths + AC-5 links).
- [needs-clarification] Should the live-verify job also run on `workflow_dispatch` and PRs touching demo/extension files? Deploy only fires on main/staging pushes today; a PR cannot exercise the live probe pre-merge. Decomposer recommends: verify-live runs only post-deploy (push/workflow_dispatch); AC-3's local mode (`DEPLOYED_URL` unset → local static server with real CDN boots) is the PR-time safety net.
- [needs-clarification] webR boot on live Pages can take 30–90s cold (CDN fetch + SAB init + COI reload cycle). Probe timeout budget: decomposer recommends generous per-boot wait (~120s) with evidence capture; flag if CI wall-time is a concern (verify-live adds ~2–4 min to docs deploys).
- Non-question (verified): pyodide does NOT require COI — Python exercises work in the deployed book as-is. R requires it → standalone page. Deployment method (`upload-pages-artifact` + `deploy-pages`) does not run Jekyll, but `.nojekyll` costs nothing and guards against future source-mode switches.
