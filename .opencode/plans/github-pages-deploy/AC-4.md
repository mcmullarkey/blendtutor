---
ac: 4
depends_on: [2, 3]
risk: high
status: complete
---

# AC-4: verify-live post-deploy alarm job in docs.yml + rodney COI-flag workaround

## Executable Spec (resolver-merged, 16 clauses)
- predicate: given AC-2 merged (deploy job, step id `deployment`) and AC-3 merged (`rodney-probes/pages-live.js` with DEPLOYED_URL env + live arg + exit-code contract), when .github/workflows/docs.yml is structurally inspected (awk job-block extraction, per test_demo_standalone_render.sh:115 pattern — PR #151 cycle-2 lesson), then:
  1. `^  verify-live:` exists as top-level job key in docs.yml, block start line > deploy block end line (job declared after deploy)
  2. `needs: deploy` appears INSIDE the verify-live job block (awk-extracted, NOT file-wide grep)
  3. deploy job block declares job-level `outputs:` with `page_url: ${{ steps.deployment.outputs.page_url }}` (step outputs do NOT cross job boundaries — without this needs.deploy.outputs.page_url is empty)
  4. `DEPLOYED_URL` env in verify-live references `needs.deploy.outputs.page_url` (NOT steps.deployment.outputs) AND includes `demo` path suffix — page_url yields site ROOT with trailing slash; harness expects demo dir root
  5. probe invocation contains `pages-live.js live` (live mode — NOT local)
  6. NO `continue-on-error` key anywhere in verify-live job block
  7. probe `run:` line contains NO exit-code swallow (`|| true`, `|| exit 0`, `|| :`, `; true`, `&& true`)
  8. verify-live job does NOT declare `if: always()` (runs only when deploy succeeds)
  9. `actions/checkout` appears in verify-live job block (deploy job does NOT checkout; probe file must exist on runner)
  10. `astral-sh/setup-uv` step present in verify-live job block (`uvx rodney` requires uv)
  11. node available: `actions/setup-node` step present (defensive pin over ubuntu-latest default)
  12. deploy job block does NOT declare `needs: verify-live` (post-deploy alarm, NOT a gate — inversion check)
  13. ALL assertions use awk job-block extraction — NOT file-wide `grep -q` on docs.yml (PR #151 cycle-1 regression guard)
  14. runtime COI fix: `scripts/rodney-chrome.sh` committed + executable; wrapper strips `--single-process`, `--disable-site-isolation-trials`, and the whole `--disable-features=...` arg from argv, then execs real Chrome with resolution order `$REAL_CHROME` → macOS /Applications/Google Chrome.app/Contents/MacOS/Google Chrome → Linux /usr/bin/google-chrome → Chromium.orig fallback; preserves rodney-supplied --no-sandbox --disable-dev-shm-usage. (rodney 0.4.0 hardcodes poison flags unconditionally — rodney main.go:356, go-rod launcher.go:75,91; no upstream fix; without wrapper crossOriginIsolated never true → AC-3 P2 coi_failure → verify-live permanently red)
  15. ROD_CHROME_BIN wired to the wrapper: EITHER verify-live job env sets `ROD_CHROME_BIN` → scripts/rodney-chrome.sh (belt-and-suspenders), OR the harness sets it itself. NOTE: post-AC-3-fix, pages-live.js sets ROD_CHROME_BIN to the wrapper unless caller overrides (verified at 9a50c01) — harness-level setting satisfies this clause; job-level explicit set optional. Test may grep either location.
  16. rodney pinned: probe invocation in pages-live.js uses `execFileSync("uvx", ["--from", "rodney==0.4.0", "rodney", ...])` (prevents silent drift to a future rodney that changes flags/behavior; landed at 9a50c01)
- probe:
  bash scripts/tests/test_verify_live_wiring.sh
  Test structure (NEW file, AC-1/AC-2 ok()/ko() counter pattern, exit 1 on fail):
  - Phase 1 workflow structural: awk-extract verify-live: and deploy: blocks from docs.yml; per-clause grep assertions clauses 1-12 against correct block; line-number compare clause 1 ordering; awk-extract quarto-render: block from ci.yml → assert test_verify_live_wiring.sh step present (wiring, AC-2 clause 11 pattern)
  - Phase 2 wrapper unit test (no browser): stub real Chrome via REAL_CHROME=/bin/echo; run bash scripts/rodney-chrome.sh --no-sandbox --single-process --disable-site-isolation-trials --disable-features=site-per-process --disable-dev-shm-usage about:blank; assert argv (a) no --single-process/--disable-site-isolation-trials/--disable-features, (b) keeps --no-sandbox --disable-dev-shm-usage + trailing about:blank (clause 14)
  - Phase 3 env + pin static assertions: grep for ROD_CHROME_BIN wiring (clause 15 — either job env OR harness set; check pages-live.js for ROD_CHROME_BIN + wrapper path); grep rodney-probes/pages-live.js for literal `--from` + `rodney==0.4.0` (clause 16)
  - Phase 4: none — live probe execution is the workflow's job at deploy time; first real push to main confirms end-to-end (manual, one-time)
- negative (13):
  1. verify-live job omitted → clause 1
  2. probe wired into deploy job (no separate alarm job) → clauses 1 + 12
  3. `needs: build` instead of `needs: deploy` → runs pre/concurrent with deploy, page_url empty, probe 404s → clause 2
  4. deploy outputs: block missing → needs.deploy.outputs.page_url empty → DEPLOYED_URL = demo/ relative → probe curls wrong root → clause 3
  5. DEPLOYED_URL = page_url verbatim (no /demo/) → harness navigates to site root (mdBook), no .cm-editor → verify-live ALWAYS red → clause 4
  6. DEPLOYED_URL hardcoded instead of needs-output → breaks org rename / branch preview → clause 4
  7. probe invoked as pages-live.js local → local static server, never touches live URL → silent-pass → clause 5
  8. continue-on-error: true → alarm silenced (most dangerous sneaky-pass) → clause 6
  9. `uv run node ... || true` → exit swallowed → clause 7
  10. if: always() → runs on failed/skipped deploy → false alarm every time → clause 8
  11. no actions/checkout → probe file absent → infra failure masks demo health → clause 9
  12. file-wide grep -q verify-live in test → passes if job commented out or needs: deploy lands in wrong job → clause 13
  13. runtime COI: wrapper missing/not executable, OR ROD_CHROME_BIN unwired → rodney 0.4.0 launches Chrome with poison flags → crossOriginIsolated never true → AC-3 P2 coi_failure → verify-live PERMANENTLY red from deploy #1 → clauses 14 + 15. Sub-case: wrapper strips only exact site-per-process → combined --disable-features arg partially survives → Phase 2 full-arg-strip assertion kills
- verification: code · shell structural test (awk YAML + wrapper argv stub) + manual (one-time: first real push to main triggers verify-live green)
- fixture status: EDIT .github/workflows/docs.yml (deploy job gains outputs: after header, keep needs: build; NEW verify-live job appended at EOF); NEW scripts/tests/test_verify_live_wiring.sh; NEW scripts/rodney-chrome.sh (landed at 9a50c01 on AC-3 branch — confirm present, EDIT if wrapper committed but executable bit or strip logic differs); EDIT .github/workflows/ci.yml (append one step in quarto-render job after AC-2's test step); rodney-probes/pages-live.js pin (landed at 9a50c01 — READ-ONLY unless pin regression)
- rubric anchor: §4.1 (job-boundary responsibility — verify-live = run AC-3 probe against live URL, fail job on probe failure; NOT gate deploy, NOT implement probe logic), §5.1 (one job one concern, one ok/ko per clause), §3.1 (alarm-not-gate job seam), §1.1 (awk job-block extraction makes assertion-belongs-to-this-job unrepresentable-as-wrong)

## Design Intent
- Types / interfaces (§1): job-level outputs: page_url is the typed cross-job interface; clause 3 pins exact expression so contract cannot drift from environment-url reference at docs.yml:96. ROD_CHROME_BIN env is the typed interface between CI job and rodney launcher
- Pure / effectful (§2): pure = awk extraction + grep assertions on YAML + wrapper argv stub test; effectful = actual probe run (browser, network, live site) — exercised only on real deploy, delegated to AC-3
- Boundary cuts (§3): deploy (publishes) vs verify-live (post-deploy alarm) split at job seam; URL composition page_url + /demo/ marks site-root vs demo-nest boundary; wrapper script isolates rodney's Chrome-launch defect at the binary-swap seam (no rodney fork)
- Module responsibility (§4): docs.yml deploy job owns publishing; verify-live owns post-deploy verification + runner environment; rodney-chrome.sh owns flag-sanitization only; test owns CI enforcement; probe harness stays AC-3-owned
- Function discipline (§5): each verify-live step one thing (checkout, setup-uv, setup-node, single probe run line); wrapper does one thing (argv filter + exec); one ok/ko per clause

## Technical Context
- Files likely touched: .github/workflows/docs.yml:84-100 (deploy block + append verify-live); .github/workflows/ci.yml:54-119 (quarto-render test step); NEW scripts/tests/test_verify_live_wiring.sh; scripts/rodney-chrome.sh (EXISTS at 9a50c01 — verify, don't recreate); rodney-probes/pages-live.js (pin + ROD_CHROME_BIN landed at 9a50c01 — READ-ONLY unless regression)
- Architecture notes:
  - rodney 0.4.0 poison flags UNCONDITIONAL (main.go:356 + go-rod launcher.go:75,91); no flag override; only ROD_CHROME_BIN binary-swap or rodney connect escape. Binary-swap chosen (validated); rodney connect rejected (externally-managed Chrome lifecycle in CI, more moving parts)
  - On ubuntu-latest, go-rod downloads own Chromium r1321438 (~120-150MB per run) — wrapper redirects to preinstalled /usr/bin/google-chrome, eliminating download AND poison flags
  - Harness post-fix (9a50c01): pages-live.js sets ROD_CHROME_BIN → wrapper unless caller set — self-sufficient on any machine; verify-live job explicit set is optional belt-and-suspenders
  - COI on Pages achieved via coi-serviceworker.js v0.1.7 shim (SW re-serves with COOP/COEP); shim works ONLY if Chrome doesn't cripple site isolation — clause 14 load-bearing
  - COEP credentialless + CDN risk (webR cdn.r-wasm.org, pyodide cdn.jsdelivr.net): live-only; failure → AC-3 exec_failure (correct, distinct alarm)
  - CDN propagation race post-deploy: AC-3 curl HEAD pre-check responsibility; optional sleep 5 (not a clause)
  - Probe wall-time ~4.5min (COI 30s + webR 120s + pyodide 60s); recommend timeout-minutes: 15 (not a clause)
  - Evidence: harness writes probe-report.json + rodney.log; optional upload-artifact step (not a clause)
  - verify-live job header comment: "Post-deploy alarm — runs AC-3 probe against live URL; failure is NOT a gate (deploy already published)."
- AC-3 dependency note: AC-3 (issue #153) branch 153-live-probe has the wrapper + pin at 9a50c01 — merge AC-3 BEFORE AC-4 (AC-4 depends on it; conflict set: rodney-probes/pages-live.js + scripts/rodney-chrome.sh serialized)

## Dependencies
- Depends on: AC-2 (docs.yml deploy job + step id deployment — MERGED #154); AC-3 (pages-live.js committed with wrapper + pin + DEPLOYED_URL/live/exit-code contract — branch 153-live-probe, must merge first)
- Blocks: none (terminal alarm AC)
- Conflict set: .github/workflows/docs.yml (AC-2 first — merged; AC-4 appends); .github/workflows/ci.yml (AC-2 first — merged; AC-4 appends); rodney-probes/pages-live.js + scripts/rodney-chrome.sh (AC-3 owns; AC-4 READ-ONLY after AC-3 merge)
- Risk: high (raised from medium) — cross-job output mechanism new wiring AND runtime COI failure mode would make job permanently red from deploy #1; clauses 14-16 mitigation is fix-now. Residual live-only risks (COEP/CDN) are AC-3's domain and alarm-correct

## Decision Log
- resolver — naming: test_verify_live_wiring.sh over A's test_verify_live_job.sh (matches wiring-test vocabulary + test_demo_standalone_render.sh convention)
- resolver — clause base: B's P1-P13 adopted wholesale (subsumes A's 7; B adds checkout/if:always/awk-mandate sneaky-passes). A's line-ordering merged INTO clause 1
- resolver — NEW clauses 14-16 + negative 13: merged from runtime-COI research finding — NEITHER proposer had it; without it job permanently red. Risk medium → high
- resolver — B's P11 documented-reliance → hardened to A's explicit setup-node requirement
- resolver — rodney connect rejected (more moving parts than binary-swap)
- resolver — disagreement=minor; all resolved w/o user input
- Director — clause 15 adjusted post-resolver: AC-3 fix (9a50c01) makes harness set ROD_CHROME_BIN itself; job-level explicit set optional. Test may grep either.

### Progress
- [x] spec resolved (resolver) — done
- [x] red: test_verify_live_wiring.sh — 2026-08-04: 10 pass / 9 fail (verify-live job absent on main), commit d264a36
- [x] green: docs.yml verify-live job + deploy outputs — 2026-08-04: 19/19 pass, commit 8574764
- [x] evidence docs/evidence/156/ — 2026-08-04: test-suite.log + verify-live-job-listing.txt + wrapper-argv-capture.txt, commit (evidence)

### Surprises & Discoveries
- Absence-grep false positives: the verify-live job header comment literally contains "No continue-on-error / || true / if: always()", so block-wide absence greps for clauses 6/8 matched the PROSE, not keys. Fixed by stripping comment-only lines (`grep -vE '^[[:space:]]*#'`) before the key greps and scoping clause 7 to the awk-extracted probe RUN LINE (spec pins the run line, not the block). The green run caught the false positives; red run couldn't (empty block → absent greps trivially passed). Lesson: absence clauses on YAML must match code (keys / run lines), never prose — comment-stripping is the cheap guard.
- Evidence-capture zsh trap: the wrapper argv evidence script passed flags via an unquoted `$ARGV` variable; zsh does NOT word-split unquoted variables, so the wrapper received the whole flag list as ONE argument and stripped nothing (blob arg doesn't match any strip pattern). The wrapper itself is correct — the test (run under bash with literal args) proves 0 occurrences of each poison flag. Evidence capture must use literal args, not variable expansion, when run from a zsh shell tool.
- Clause 3 deploy outputs placement: job-level `outputs:` inserted after `runs-on:` in the deploy header, before `permissions:` — keeps `needs: build` and step id `deployment` intact (AC-2 contract preserved; test_docs_pages_artifact.sh still 14/14).

### Idempotence & Recovery
- Safe retry: re-run test_verify_live_wiring.sh (structural, idempotent).
- Rollback: git checkout -- .github/workflows/docs.yml .github/workflows/ci.yml; rm scripts/tests/test_verify_live_wiring.sh
