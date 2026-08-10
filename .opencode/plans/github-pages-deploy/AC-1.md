---
ac: 1
depends_on: []
risk: high
status: complete
---

# AC-1: Standalone interactive demo — demo-standalone/ renders R + Python with page-covering COI service worker

## Executable Spec (resolver-merged, 7 clauses)
- predicate: given demo-standalone/ (Quarto type: default project, repo-root-relative filter reference, coi: true YAML), when quarto render demo-standalone --to html runs followed by the COI scope post-process, then:
  1. render exits 0 → demo-standalone/index.html exists
  2. filter loaded: HTML contains exactly 2 bt-exercise divs — 1 language="r" + 1 language="python" (load-proving guard; pinned counts for AC-3 live probe)
  3. libs deployed standalone-form: demo-standalone/index_files/libs/quarto-contrib/blendtutor-0.1.0/ contains exercise-runtime.js, codemirror.js, styles.css, webr-adapter.js, pyodide-adapter.js (both adapters — page has both languages)
  4. bootstrap emitted: <script type="module" data-bt-bootstrap="auto"> with ./index_files/libs/... specifiers; NO site_libs/ and NO _extensions/ substring in specifiers
  5. COI activated: exactly ONE coi-serviceworker.js script src in HTML (YAML coi: true → has_coi → injection; exactly-one pins hasCoiDone dedup)
  6. COI scope coverage (SCOPE RESOLUTION): post-processed coi src is exactly ./coi-serviceworker.js (page-root relative — NOT ../-prefixed, NOT _extensions/... subdir); demo-standalone/coi-serviceworker.js exists, is non-empty, AND contains navigator.serviceWorker. Rationale (verified coi-serviceworker.js:103): n.serviceWorker.register(window.document.currentScript.src) passes NO scope option → default scope = SW script URL directory → SW must sit at page root to control index.html
  7. CI + config: .github/workflows/ci.yml quarto-render job runs scripts/tests/test_demo_standalone_render.sh; demo-standalone/_quarto.yml statically pinned type: default (book mode kills COI per AC-5), NO output-dir
- probe:
  bash scripts/tests/test_demo_standalone_render.sh
  (test itself: static-grep _quarto.yml type: default + filters: [../_extensions/blendtutor/blendtutor.lua]; rm -f demo-standalone/*.html demo-standalone/*_files/; quarto render demo-standalone --to html assert exit 0; run bash scripts/fix-demo-coi-scope.sh demo-standalone (post-process: rewrite coi src → ./coi-serviceworker.js, copy shim to page root); then grep/assert clauses 1-6; ok()/ko() PASS/FAIL counters, exit 1 on fail, SKIP exit 0 if quarto absent — mirrors scripts/tests/test_quarto_render.sh:1-252)
- negative:
  1. coi src left as ../_extensions/blendtutor/assets/coi-serviceworker.js (raw filter emission) → SW URL /_extensions/blendtutor/assets/ → scope covers assets dir only, page never controlled → webR dead despite green "coi present" grep (AC-5 book trap re-surfaced). Killed by 6 (exact-./coi-serviceworker.js match, not just "no ../")
  2. coi src = _extensions/mcmullarkey/blendtutor/assets/coi-serviceworker.js (by-name vendored form) → SW URL /demo/_extensions/.../assets/ → scope = assets subdir, still NOT /demo/index.html. B's self-contained claim verified FALSE — self-contained ≠ page-covering. Killed by 6
  3. post-process rewrites src but forgets the file copy (or copies zero-byte/wrong file) → 404 or silent SW registration failure. Killed by 6 (exists + non-empty + navigator.serviceWorker content)
  4. render exits 0 but filter never loaded → raw content, no bt-exercise. Killed by 2
  5. book discriminator leak → bootstrap ./site_libs/... (issue #143 trap) → 404 on Pages. Killed by 3-4
  6. hasCoiDone dedup broken → duplicate coi script tags → double SW registration + reload loop. Killed by 5 exactly-one
  7. type: book regression or output-dir set → COI inert / emitted src mismatches output location (resolve_asset_path not output-dir-aware, blendtutor.lua:95-120). Killed by 7 static pin
  8. Python exercise dropped → page R-only, AC says R + Python. Killed by 2 (language="python" pin)
- verification: code · shell render test (rendered HTML greps + file existence + post-process — no browser; live SW-control + runtime webR/pyodide proof is AC-3's rodney probe)
- fixture status: NEW demo-standalone/index.qmd (one page, R + Python exercises, coi: true — copy .blendtutor syntax from demo-book/r-exercises.qmd:22-53, demo-book/python-exercises.qmd:15-50); NEW demo-standalone/_quarto.yml; NEW scripts/fix-demo-coi-scope.sh (scope-fix mechanism — post-process, filter untouched); NEW scripts/tests/test_demo_standalone_render.sh; NEW demo-standalone/coi-serviceworker.js (produced by post-process copy, gitignored with render output); EDIT .github/workflows/ci.yml (one step in quarto-render job); EDIT .gitignore (/demo-standalone/*.html, /demo-standalone/*_files/, /demo-standalone/coi-serviceworker.js, /demo-standalone/.quarto/). Existing anchors: _extensions/blendtutor/assets/coi-serviceworker.js (shim source), scripts/tests/test_quarto_render.sh:113-115 (assertion-2b explicit-path filter convention), quarto-fixture/mixed-lang.qmd (R+Python coexistence pattern)
- rubric anchor: §1.1 (coi: true YAML → has_coi typed opt-in, ADR-0015 option 3; type: default pin makes book-mode-COI-kill unrepresentable), §3.1 (standalone-vs-book libs_url discriminator; SW-scope boundary — dir boundary IS domain boundary), §5.1 (hasCoiDone one-activation dedup)

## Design Intent
- Types / interfaces (§1): coi: true YAML boolean → has_coi at blendtutor.lua:628-633; filters: [../_extensions/blendtutor/blendtutor.lua] explicit-path reference (by-name discovery fails for standalone; explicit path is assertion-2b convention)
- Pure / effectful (§2): effectful = quarto render + post-process script only; all assertions pure greps / test -f over rendered HTML. No network, no browser
- Boundary cuts (§3): demo-standalone/ (interactive R+Python, COI required) separate from demo-book/ (static + Python, no COI) — AC-5 proved per-page conditional COI in book mode impossible. SW scope boundary: script-URL dir vs page dir is THE seam; post-process owns crossing it, filter stays untouched (0 test migrations)
- Module responsibility (§4): demo-standalone/ owns demo content; scripts/fix-demo-coi-scope.sh owns the SW-scope contract (single documented place where coi src is rewritten — header must state WHY: register-default-scope mechanics); test_demo_standalone_render.sh owns the AC contract; ci.yml owns execution
- Function discipline (§5): post-process does two things only (rewrite src, copy file) and is idempotent; test = one assertion per concern, ok()/ko() pattern

## Technical Context
- Files likely touched: demo-standalone/index.qmd (NEW), demo-standalone/_quarto.yml (NEW), scripts/fix-demo-coi-scope.sh (NEW), scripts/tests/test_demo_standalone_render.sh (NEW), .github/workflows/ci.yml (EDIT, quarto-render job ~L108), .gitignore (EDIT)
- SW scope mechanics (empirically verified): coi-serviceworker.js:103 = n.serviceWorker.register(window.document.currentScript.src).then(...) — single argument, NO {scope} option → default scope = SW script URL directory. Filter emission (resolve_asset_path blendtutor.lua:95-128, injected :664-674) yields _extensions/.../assets/coi-serviceworker.js under BOTH reference forms (repo-root-relative → ../_extensions/...; by-name vendored → project-relative _extensions/mcmullarkey/...). Both put the SW in a subdir → scope never covers index.html. Conclusion: post-process to page root is mandatory regardless of filter form; therefore choose repo-root-relative filter (no vendoring, sync-quarto-assets.sh untouched, no asset-parity propagation surface). Comment at blendtutor.lua:122-127 ("SW scope is script URL's directory… never deploy to libs dir") corroborates — page-root deployment honors the same invariant.
- Render config: type: default, NO output-dir (render beside qmd → deterministic demo-standalone/index.html + index_files/); render output + copied SW gitignored, test repeatable
- Architecture notes: one page, both exercises, coi: true (user-approved decomposition singular-page reading; ADR-0015:33 — pyodide benefits from COI; COEP-vs-pyodide-CDN risk is real → AC-3 live rodney probe is the safety net, flagged not resolved)

## Dependencies
- Depends on: none (foundation AC)
- Blocks: AC-2 (deploy artifact), AC-3 (live probe needs pages + page-covering COI), AC-4 (verify job), AC-5 (README links)
- Conflict set: demo-standalone/** (new, none); scripts/fix-demo-coi-scope.sh (new); scripts/tests/test_demo_standalone_render.sh (new); .github/workflows/ci.yml (AC-1 only per decomposition); .gitignore (low-contention append)
- Risk level: high — scope-fix is foundation-level; decomposition premise "standalone = working path" was unverified at emission level and required a new mechanism (post-process) the plan did not anticipate

## Decision Log
- resolver — SW scope dispute resolved empirically: coi-serviceworker.js:103 register(currentScript.src) has NO scope option → default scope = SW script URL dir → SW must sit at PAGE ROOT to control index.html. Both filter reference forms (repo-root-relative ../_extensions/... AND by-name vendored _extensions/mcmullarkey/...) put SW in a subdir → neither covers the page. B's "self-contained" claim verified FALSE — self-contained ≠ page-covering. Resolution: A's post-process approach wins — scripts/fix-demo-coi-scope.sh rewrites coi src → ./coi-serviceworker.js + copies shim to page root; filter untouched (0 test migrations); repo-root-relative filter reference (no vendoring, sync-quarto-assets.sh untouched).
- resolver — dual-consumer COI preserved: one page (R + Python, coi: true) per user-approved decomposition; COEP may block pyodide CDN fetches; AC-3 live probe is the catch; fallback = two pages if AC-3 shows pyodide blocked.
- resolver — plan-premise correction: decomposition treated standalone COI as solved; it is not without the post-process step. AC-2 deploy wiring MUST run fix-demo-coi-scope.sh before upload.
- resolver — disagreement=minor; load-bearing dispute resolved by empirical verification.

### Progress
- [x] spec resolved (resolver) — pending implementation
- [x] red: test_demo_standalone_render.sh — 6 FAIL (missing files) as expected (2026-08-03)
- [x] green: demo-standalone/ pages + fix-demo-coi-scope.sh — 24/24 PASS (2026-08-03); regression suite green (9 shell/py tests + sync + verify_asset_scoping)
- [x] evidence docs/evidence/150/ — test-suite.log + coi-scope-snippet.html + shim-listing.log (2026-08-03)

### Surprises & Discoveries
- AC-1 (github-pages-deploy): index.qmd's own prose mentions "coi-serviceworker.js" as documentation → raw-occurrence counting in the test double-counted prose + real script. Fix: count `<script[^>]*coi-serviceworker\.js[^>]*>` script TAGS only — the dedup concern is duplicate script tags, not filename mentions. Uncovered at green run (found 3, expected 1).
- AC-1 (github-pages-deploy): the vendored coi-serviceworker.js shim is MINIFIED — it uses `n.serviceWorker` (alias), never the literal `navigator.serviceWorker` the spec asserted. Byte-identity (cksum vs vendored source) + presence of `register(window.document.currentScript.src)` (the no-scope call) are stronger wrong-file kills than the literal string.
- AC-1 (github-pages-deploy): spec pinned inline `filters: [../_extensions/blendtutor/blendtutor.lua]`; I first wrote YAML block form (filters:\n  - ...) — equivalent YAML but failed the static grep. Switched to inline form to match spec literally; demo-book/_quarto.yml keeps block form (different file, untouched).

### Idempotence & Recovery
- Safe retry: re-run render + post-process + test (idempotent).
- Rollback: git checkout -- .gitignore .github/workflows/ci.yml; rm -rf demo-standalone/ scripts/fix-demo-coi-scope.sh scripts/tests/test_demo_standalone_render.sh
