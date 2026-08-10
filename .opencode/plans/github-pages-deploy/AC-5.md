---
ac: 5
depends_on: [2]
risk: low
status: spec
---

# AC-5: README demo/deployment docs — live URLs, what-works-where, COI book-mode limitation (extend-in-place)

## Executable Spec (resolver-merged, 11 clauses)
- predicate (awk-scoped `### Demo book` → `## License` section unless noted; README.md at AC-2-merged HEAD):
  1. Both live URLs exact literals, trailing slash pinned: `https://mcmullarkey.github.io/blendtutor/demo-book/` AND `https://mcmullarkey.github.io/blendtutor/demo/`
  2. Book capabilities: demo section contains Python-interactive claim AND literal phrase `static fallback`
  3. Standalone capabilities: demo section states interactive R (webR) AND interactive Python
  4. COI book-mode limitation survives + names `type: book`: demo section contains `type: book` AND phrase matching `COI does not (function|take effect|work)`
  5. Book explicitly does NOT run R: statement matching `R exercises.*(don't|do not|cannot|not).*(run|execute)|R exercises.*unavailable|editors mount but execution` — generic COI-doesn't-function insufficient
  6. Pyodide accuracy guard (whole-README): matches `pyodide.*(do not|doesn't|no).*COI|Pyodide-only.*do not need COI`
  7. No stale /examples/ conflation: demo section does NOT match `mcmullarkey.github.io/blendtutor/examples/` (lines 143-146 examples sites = Rust-binary, different thing)
  8. Extend-don't-duplicate (two count pins): `grep -cF 'COI does not function in Quarto' README.md` == 1 AND `grep -c 'Book-mode limitation' README.md` == 1 (existing blockquote :303-306 extended in place; no verbatim restatement at :320-322, :338-340, or new text)
  9. Region pin: both live URLs appear at line >= 288 and < 342 — extends existing demo/COI region, no new top-level parallel section
  10. ADR pointer: README links `docs/adr/0015-opt-in-coi-cross-origin.md` AND `test -f` that file passes (ADR-0015:67-69 defers runtime-scope resolution to AC-5)
  11. Existing distribution-doc pins survive (test_quarto_distribution.sh README group): `python3 -m http.server 8000` present (:316 pin); `COI configuration` absent (:281 pin); `PANDOC_SCRIPT_FILE` absent (:291 pin); `type: book` present README-wide (:266 pin); `demo-book/` relative link target (:311) still exists as directory
- probe:
  bash scripts/tests/test_demo_docs.sh   # NEW; ok()/ko() counters, exit 1 on any fail
  awk-scope demo section: awk '/^### Demo book/,/^## License/' README.md
  c1: grep -qF exact URL literals (scoped); c2-3/c5: grep -E keyword/regex pins (scoped); c4: grep -qF 'type: book' + grep -E 'COI does not (function|take effect|work)' (scoped); c6: grep -E pyodide-COI accuracy (whole README); c7: ! grep -qF 'mcmullarkey.github.io/blendtutor/examples/' (scoped); c8: test "$(grep -cF 'COI does not function in Quarto' README.md)" -eq 1 && test "$(grep -c 'Book-mode limitation' README.md)" -eq 1; c9: grep -nF URLs | awk line-range 288<=n<342; c10: grep -qF 'docs/adr/0015-opt-in-coi-cross-origin.md' README.md && test -f docs/adr/0015-opt-in-coi-cross-origin.md; c11: grep -qF 'python3 -m http.server 8000'; ! grep -qF 'COI configuration'; ! grep -qF 'PANDOC_SCRIPT_FILE'; grep -qF 'type: book'; test -d demo-book
  NO network calls — pure grep/awk/test -f/test -d. Live-URL runtime validity = AC-3/AC-4 rodney probes' job.
- negative (9, clause-mapped):
  1. Wrong URL (missing scheme/trailing slash/repo segment, swapped /demo-book/ ↔ /demo/) → killed by c1
  2. Capability mapping swapped/incomplete (book claims R; standalone omits Python or R; static fallback omitted) → killed by c2/c3
  3. Vacuous URL grep (URLs + nothing else) or table-only answer without explicit R-doesn't-run → killed by c2/c3/c5
  4. COI limitation deleted to make room, or restated verbatim in new live-demos text (4th duplicate block) → killed by c4 + c8 (counts != 1)
  5. Pyodide-needs-COI lie → killed by c6
  6. Stale /examples/ entry presented as demo → killed by c7
  7. New parallel top-level deployment section instead of extending :288-340 → killed by c9
  8. ADR link typo'd/renamed → killed by c10 (file-exists); demo-book/ dir or :311 link removed → killed by c11 (test -d)
  9. AC-5 prose drops python3 -m http.server 8000 or introduces COI configuration / PANDOC_SCRIPT_FILE → test_quarto_distribution.sh README group regresses → killed by c11. Probe misuse: curl/HTTP-HEAD live URLs → pre-deploy 404 flake → prohibited (source-grep only). Prose clarity/tone → N/A — subjective, PR-review judgment
- verification: code · shell content-check (no browser, no network, no deploy dependency)
- fixture status: EDIT README.md (:288-340 region — extend blockquote :303-306, Demo book :308-322, Viewing :324-336, third mention :338-340; red-commit prose). NEW scripts/tests/test_demo_docs.sh. EDIT (comment-only) scripts/tests/test_quarto_distribution.sh:266-269 — delegate note to test_demo_docs.sh; grep pin kept. No data fixture — README itself is the fixture
- rubric anchor: §1 (URL literals + absence-pins encode capability invariant), §2.1 (pure content-check; effectful live-URL proof deferred to AC-3/AC-4), §3.1 (book vs standalone capability boundary as domain seam), §4.1 (README names what works where AND what-NOT; single-owner extend-in-place)

## Design Intent
- Types / interfaces (§1): URL strings ARE the interface — exact literals pinned. Invariant book != R-interactive encoded as testable content assertion (c5). Absence-pins (COI configuration, PANDOC_SCRIPT_FILE) keep internal mechanics from leaking
- Pure / effectful (§2): docs = pure content; probe = pure grep/test -f. Live-URL existence/execution effectful → OUT of this AC by design (AC-3 local, AC-4 verify-live own it). Mixing = flake on deploy timing
- Boundary cuts (§3): demo-book/ (static+Python, no COI) vs demo/ (R+Python, COI active) = distinct capability domains with distinct URLs. Docs reflect seam, not blurred single demo entry
- Module responsibility (§4): Demo book section owns ALL demo instructions (local serve + live URLs + capability mapping); COI section owns mechanics + ADR-0015 pointer. what-NOT (R doesn't run in book) load-bearing as what — #1 user confusion prevented. Extend-don't-duplicate = single-owner rule; test_demo_docs.sh owns AC-5 contract, test_quarto_distribution.sh owns distribution-doc contract — edits must satisfy BOTH
- Function discipline (§5): one clause per concern in test; README paragraph = one purpose. No parallel truth-source (c9)

## Technical Context
- Files likely touched: README.md (EDIT :288-340); NEW scripts/tests/test_demo_docs.sh; scripts/tests/test_quarto_distribution.sh (comment-only EDIT :266-269); optional ci.yml append (DEVIATION from decomposition ci.yml-AC-1-only — AC-2 + check-docs.sh precedent; flag)
- Architecture notes: Book (demo-book/, type: book): Quarto rewrites in-header coi src to site_libs/quarto-contrib copy; SW scope = own dir → cannot cover _output/ pages → COI dead → webR SAB unavailable → R editors mount, execution fails. Pyodide no COI → Python runs. Static fallback = server-rendered title/prompt/template/hints. Standalone (type: default): coi: true + fix-demo-coi-scope.sh → SW at page root → COI functions → webR runs; index.qmd R (22-44) + Python (46-65). Live URLs nested under existing Pages site root per AC-2 clauses 5-6 (mirror /api + /examples nesting). Third limitation mention :338-340 NOT named in AC — extend it too or stale. README anchors: 138-152 old /examples/ Rust-binary sites (out of scope, c7 guard); 288-301 COI section (:301 pyodide anchor c6); 303-306 primary blockquote; 320-321 second mention; 336 static-fallback note; 338-340 third mention. No markdown link checker configured — c10/c11 file-exists substitute. Existing test overlap: test_quarto_distribution.sh README group 12+ greps (:131, :266, :271, :276, :281, :291, :296, :316, :321, :326-329, :833) — prose must not regress them; :266-269 vacuous type: book presence check superseded by c4 scope → extend comment to delegate, do NOT retire (retire loses distribution-group coverage)
- Merge-order gate (process, NOT test assertion): /demo-book/ + /demo/ resolve only after AC-2 deploys live. If AC-5 merges before AC-2 Pages deploy, README ships dead links. Probe source-grep only (no curl) — dead-link guarantee shifts to Director process gate: AC-5 PR body states depends-on-AC-2-deploy-live; Director blocks merge until AC-2 verify-live passes (verify-live = AC-4, deploy #1)

## Dependencies
- Depends on: AC-2 (MERGE — #154 done; artifact paths land; capability semantics transitively from AC-1, also merged)
- Blocks: none (terminal docs AC)
- Conflict set: README.md (sole owner per decomposition — BUT hidden shared contract: test_quarto_distribution.sh README group greps README prose); scripts/tests/test_quarto_distribution.sh (comment edit); NEW test file
- Risk: low

## Decision Log
- resolver — predicate backbone: B's 9 clauses (anti-fake pins c5/c6/c7/c9 = sneaky-pass catchers)
- resolver — distribution-pin survival: merged as c11 (full pin list incl. test -d demo-book) from A
- resolver — ADR pointer: merged as c10 (B omitted; ADR-0015:67-69 defers to AC-5 → load-bearing)
- resolver — duplication guard: both count pins kept (A body-phrase + B heading — complementary)
- resolver — capability window scope: B's awk section-scope over A's ±6-line URL window (cleaner; c9 pins region anyway)
- resolver — test name: test_demo_docs.sh (matches repo convention)
- resolver — :266-269 overlap: extend-delegate (comment), keep grep, no retire
- resolver — disagreement=minor; all resolved w/o ambiguity. Merge-order gate flagged to Director (process, not user)

### Progress
- [ ] spec resolved (resolver) — pending implementation
- [ ] red: test_demo_docs.sh — pending
- [ ] green: README extension — pending
- [ ] evidence docs/evidence/<issue>/ — pending

### Idempotence & Recovery
- Safe retry: re-run test_demo_docs.sh (idempotent).
- Rollback: git checkout -- README.md scripts/tests/test_quarto_distribution.sh; rm scripts/tests/test_demo_docs.sh
