# Evidence — Issue #153 (github-pages-deploy AC-3: live/local rodney probe harness)

Builder-phase evidence: pure-core unit tests + harness source. The rodney
browser run itself (probe-report.json + rodney.log) is produced by the
vision-probe agent after this commit (verification: rodney — the builder does
not run rodney/browser).

## Harness source listing

| File | Lines | Role |
|------|-------|------|
| `rodney-probes/pages-live.js` | 656 | Effectful harness: renders demo-standalone/ if missing, serves worktree root :8088, drives uvx rodney (execFileSync — direct uvx rodney bash is permission-blocked), asserts P1-P5, writes report |
| `rodney-probes/pages-live-core.js` | 174 | Pure core (§2): verdict closed enum, assertion-record construction, path normalization local `/demo-standalone/` vs live `/demo/`, timeout budgets (COI 30s / webR 120s / pyodide 60s), report assembly — zero side effects |
| `rodney-probes/pages-live-core.test.js` | 235 | Pure-part unit tests (node:test, no rodney/DOM) — 21 tests, all green |

## How to run

Pure-part unit tests (builder gate):

```bash
uv run node --test rodney-probes/pages-live-core.test.js
```

Local-mode probe (vision-probe agent; serves rendered demo-standalone/ on
:8088 — localhost is a secure context so the SW registers):

```bash
uv run node rodney-probes/pages-live.js local
```

Live-mode probe (post-deploy, AC-4; DEPLOYED_URL is used verbatim, trailing
slash stripped; curl HEAD pre-checks coi-serviceworker.js at the deployed
root):

```bash
DEPLOYED_URL=https://<user>.github.io/<repo>/demo/ uv run node rodney-probes/pages-live.js live
```

## Assertion map (predicate → probe function)

- **P1** CM6 mounts + registry — `probeP1Mounts()`: vacuous guard first
  (`__btExercises` defined — PR #123 pattern), `.get('bt-exercise-0'/'1')`
  non-null, 2 `.cm-editor` each with a `.cm-content` child; textarea-only
  degradation recorded as distinct `cm6_fallback` finding, not hard fail
- **P2** COI gate — `probeP2Coi()`: poll ≤30s THROUGH the SW self-reload
  cycle (sessionStorage.coiReloadedBySelf) until `crossOriginIsolated ===
  true` AND `controller !== null`; timeout → `coi_failure` verdict, P3/P4
  skipped with recorded reason
- **P3** R exec — `probeP3RExec()`: `setEditorContent` incl.
  `print(add(1,2))` → click `.bt-run-btn` → ≤120s webR → data-status="pass"
  AND `.bt-output` contains "3" (status-only insufficient — webR ignores
  checks)
- **P4** Python bidirectional — `probeP4PyExec()`: correct square → pass
  ≤60s; incorrect `return 0` → fail + "Check error" (proves checks execute;
  always-pass adapter caught)
- **P5** — `writeReport()`: exit 0 + probe-report.json (verdict, per-assertion
  {name, status, details}, actual R+Python outputs, crossOriginIsolated,
  controller, boot timings, DEPLOYED_URL/"local")

## Verdict schema

Closed enum: `pass | coi_failure | exec_failure | cm6_fallback_noted`
Per-assertion status: `pass | fail | skip` (skip only with recorded reason)

## Artifacts

- `test-suite.log` — `uv run node --test` full output (21/21 pass)
- `probe-report.json` + `rodney.log` — written by the harness run
  (vision-probe phase; not committed in the builder commit)
