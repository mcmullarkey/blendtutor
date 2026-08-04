---
ac: 3
depends_on: [1]
risk: medium
status: complete
---

# AC-3: Live/local rodney probe harness asserting real R+Python execution, COI isolation, CM6 mounts, structured evidence

## Executable Spec (resolver-merged, P1-P5)
- predicate:
  - P1: window.__btExercises.length === 2 AND __btExercises.get(<id>) non-null for both exercise ids AND document.querySelectorAll('.cm-editor').length === 2 AND each .cm-editor has a .cm-content child (real CM6, not fake div). If .cm-editor count is 0 but textarea count is 2 → record cm6_fallback as distinct finding (not hard fail; execution still valid via textarea).
  - P2: BEFORE any execution attempt: poll (up to 30s, through SW self-reload cycle gated on sessionStorage.coiReloadedBySelf) until crossOriginIsolated === true AND navigator.serviceWorker.controller !== null. Timeout → record coi_failure verdict distinct from exec_failure and skip P3/P4.
  - P3: R exercise — inject solution content including print(add(1,2)) via __btExercises.get(<r-id>).setEditorContent(...), click .bt-run-btn, poll (up to 120s webR boot+exec) until .bt-status[data-status="pass"]. REQUIRED: .bt-output textContent contains computed value "3" (status-only insufficient — webR adapter ignores checks param, pure definition yields output:""; fake adapter {output:"",ok:true} must fail).
  - P4: Python exercise — BIDIRECTIONAL: (P4a) inject correct square solution → Run → .bt-status[data-status="pass"] within 60s pyodide budget; (P4b) inject incorrect body (return 0) → Run → .bt-status[data-status="fail"]. P4b proves adapter executes user code + runs checks; always-pass adapter caught.
  - P5: Exit 0 AND probe-report.json written with: verdict, per-assertion {name, status, details}, actual .bt-output textContent for both R and Python runs, crossOriginIsolated value, SW controller presence, boot timings (COI stabilization / webR / pyodide), DEPLOYED_URL value or "local".
- probe:
  # PR-time (local static server serving rendered demo-standalone/, port 8088; localhost = secure context so SW works)
  uv run node rodney-probes/pages-live.js local
  # Post-deploy (live GitHub Pages)
  DEPLOYED_URL=https://<user>.github.io/<repo>/demo/ uv run node rodney-probes/pages-live.js live
  Harness internals: execFileSync("uvx", ["rodney", ...]) (established pattern — direct uvx rodney bash permission-blocked); rodney js subcommand (NOT eval; no reload subcommand — SW self-reload handles it); curl HEAD pre-check for coi-serviceworker.js at served root; path normalization local /demo-standalone/ vs live /demo/.
- negative:
  1. Status-only sneaky-pass: fake webR adapter {output:"",ok:true} → P3 fails (no "3" in .bt-output)
  2. Always-pass adapter: pyodide returning ok regardless → P4b fails (incorrect square must produce data-status="fail")
  3. COI-skipped: asserting execution without COI gate → P2 must pass first; coi_failure recorded distinctly, never silently skipped
  4. Pre-reload false-negative: asserting crossOriginIsolated before SW self-reload completes → poll through reload cycle up to 30s before verdict
  5. Fake editor div: <div class="cm-editor"> without CM6 → P1 .cm-content child check fails
  6. Unwired run button / vacuous guard: probe must assert __btExercises defined and non-empty before any if(adapter)-guarded block executes (PR #123 vacuous-pass pattern)
  7. Local/live structure mismatch: probe passing locally but live path wrong → path normalization + curl HEAD pre-check on coi-serviceworker.js at each mode's root
  8. Boot timeout too short: webR cold boot 30-90s → 120s budget (approved in decomposition); premature timeout misclassified as exec_failure
- verification: rodney · uv run node rodney-probes/pages-live.js {local|live} → exit 0 + committed probe-report.json + rodney.log
- fixture status: NEW rodney-probes/pages-live.js (mirrors demo-book-bootstrap.js harness pattern); renders demo-standalone/ itself if HTML missing; render outputs gitignored
- rubric anchor: §2 (pure/evidence-record separated from effectful browser run — probe-report.json is the pure artifact), §4 (probe module header documents what it asserts + what NOT, e.g. visual layout)

## Design Intent
- Types / interfaces (§1): report schema closed enum: verdict ∈ {pass, coi_failure, exec_failure, cm6_fallback_noted}; per-assertion status ∈ {pass, fail, skip} — skip only with recorded reason
- Pure / effectful (§2): effectful shell = rodney browser driving + static server; pure core = assertion evaluation + report construction from captured DOM snapshots
- Boundary cuts (§3): probe harness owns serving + driving + reporting; does not modify exercise-runtime.js or demo-standalone/ source (AC-1 surface untouched)
- Module responsibility (§4): pages-live.js = "assert real execution + COI on deployed-or-local demo; NOT layout/visual assertions, NOT runtime unit behavior (covered by earlier probes)"
- Function discipline (§5): one function per P-clause (assertMounts, assertCOI, runRExec, runPyExecBidirectional, writeReport); each returns {status, details} record; no shared mutable probe state

## Technical Context
- Files likely touched: NEW rodney-probes/pages-live.js; evidence outputs under docs/evidence/<issue>/ (probe-report.json, rodney.log)
- Architecture notes:
  - Execution driving: __btExercises.get(id).setEditorContent(code) (exercise-runtime.js:370) then click .bt-run-btn (L317); poll .bt-status data-status closed set idle→running→pass|fail (L302,384); read .bt-output (L356) for computed value
  - webR adapter ignores checks param → pure definitions output "" → solution content must print(add(1,2)) so "3" appears in output
  - pyodide adapter emits "ok" literal on pass; bidirectional case (P4b return 0) proves checks actually run
  - COI reload cycle: coi-serviceworker.js registers SW → self-reloads once, gated sessionStorage.coiReloadedBySelf → poll through it (30s) before asserting crossOriginIsolated/controller
  - Timeouts: COI 30s / webR boot+exec 120s / pyodide 60s (approved in decomposition)
  - Local mode: serve worktree root :8088 (localhost = secure context, SW functional); live mode: DEPLOYED_URL verbatim, path local /demo-standalone/ vs live /demo/
- Selectors verified: .bt-run-btn (L317), .bt-status + data-status (L302,350-351,384), .bt-output (L356), entry.setEditorContent(code) (L370), .bt-solution-btn wiring (L339-343)

## Dependencies
- Depends on: AC-1 (merged — demo-standalone/ rendered page, COI via fix-demo-coi-scope.sh)
- Blocks: none (terminal verification AC)
- Conflict set: none — new file only
- Risk level: medium (webR cold-boot timing variance on CI runners; mitigated by 120s budget + local-mode PR-time net)

## Decision Log
- resolver — B subsumes A on all 10 divergence points; A negative/rubric/design-intent dropped as unenumerated
- resolver — real-execution evidence: webR ignores checks param → output "" for pure definitions → R solution must include print(add(1,2)) so .bt-output contains "3" (status-only insufficient); Python bidirectional (correct → pass, incorrect return 0 → fail) proves checks execute
- resolver — COI assertion sequencing: crossOriginIsolated + controller !== null BEFORE execution; poll through SW reload cycle (30s); coi_failure distinct from exec_failure
- resolver — setEditorContent (public registry API, L370) over .bt-solution-btn click (cleaner for injecting solution + incorrect code)
- resolver — disagreement=minor; no load-bearing conflict
- builder — pure core extracted to pages-live-core.js (§2): verdict enum, record construction, path normalization, timeout budgets, report assembly unit-tested with uv run node --test (no rodney needed). Harness requires the core.
- builder — live-mode blank-page fix: navigateToPage() bootstraps from the LOCAL blank page (established rodney pattern — open panics on heavy loads) in BOTH modes, so the static server starts in live mode too; prior draft omitted startServer() in live mode, which would break live navigation.
- builder — Chrome launch-flag fix (post-rebase on #154 merge): rodney 0.4.0 hardcodes `--single-process` (main.go:356) + go-rod `--disable-site-isolation-trials`/`--disable-features=site-per-process` → permanently break P2 crossOriginIsolated. Committed scripts/rodney-chrome.sh wrapper (strips flags, execs real Chrome via REAL_CHROME → macOS → Linux → Chromium.orig fallback) + pages-live.js sets ROD_CHROME_BIN to it; rodney pinned to ==0.4.0 via uvx --from in all 6 probe harnesses (shared rodney() helper — sibling rule). Vision-probe's earlier pass relied on an OUT-OF-REPO wrapper; the committed harness is now self-sufficient.

### Progress
- [x] spec resolved (resolver) — pending implementation
- [x] red: pages-live.js harness — DONE (pure-core unit tests first, confirmed red on missing module)
- [x] green: local mode PROBES_PASS — harness + core implemented; pure-part tests 21/21 green; rodney run is vision-probe phase (builder does not run rodney)
- [ ] live mode post-deploy (AC-4) — pending
- [x] evidence docs/evidence/153/ — DONE (README, test-suite.log; probe-report.json + rodney.log produced by vision-probe run)
- [x] Chrome launch-flag fix — DONE (scripts/rodney-chrome.sh wrapper + ROD_CHROME_BIN in pages-live.js + rodney==0.4.0 pin in all 6 probes + wrapper smoke tests; 26/26 node tests green; vision-probe re-verifies PROBES_PASS with committed wrapper)

### Surprises & Discoveries
- Previous builder session left uncommitted WIP: complete 656-line pages-live.js, stale coi_failure probe-report.json/rodney.log (from an unauthorized rodney run at 06:41), demo-standalone/.gitignore. Reused the harness + .gitignore, discarded stale evidence (vision-probe regenerates fresh), and retrofitted BDD (pure-core tests first).
- Registry `__btExercises` is an Array with a `.get(id)` method (`find(e => e.id === id) || null`, exercise-runtime.js buildRegistry) — both `.get()` and `.find()` work; P1 uses `.get('bt-exercise-0'/'1')` (rendered payload ids confirmed in demo-standalone/index.html), P3/P4 use `.find(x => x.element.dataset.language === ...)`.
- demo-standalone/index.html + coi-serviceworker.js + index_files/ are already gitignored by ROOT .gitignore (lines 50-52); the untracked demo-standalone/.gitignore adds `/.quarto/` + `**/*.quarto_ipynb` (quarto render internals). Committed.
- Live-mode bug found in prior WIP: navigateToPage() opens the localhost blank page in both modes but main() only started the static server in local mode → live navigation would fail. Fixed by starting the server in both modes (blank page must resolve even when asserting the deployed URL).
- No pyproject.toml in repo — repo-level Python gates (pytest/ty/ruff) don't apply; JS gate is `uv run node --test`.
- rodney's Chrome flags are a silent COI killer: --single-process + --disable-site-isolation-trials/--disable-features=site-per-process make crossOriginIsolated permanently false; the only escapes are ROD_CHROME_BIN binary swap or connect API. No flag override exists — the wrapper is the only repo-committable fix.
- This machine has no /Applications/Google Chrome.app (macOS path absent) — the wrapper falls back to ~/.cache/rod/browser/chromium-1321438/.../Chromium.orig; on machines without the rodney cache, REAL_CHROME env is the override. Verified empirically: --help with no REAL_CHROME exec'd Chromium.orig (resolved, no error), fake-REAL_CHROME smoke test shows 3 flags stripped, order preserved.
- 6 probe files duplicate the rodney() execFileSync helper; pinning rodney==0.4.0 in only pages-live.js would leave the drift risk in 5 siblings (v1/v2 divergence pattern) — pinned all 6.

### Idempotence & Recovery
- Safe retry: re-run probe (renders demo-standalone if HTML missing).
- Rollback: rm rodney-probes/pages-live.js + pages-live-core.js + pages-live-core.test.js
