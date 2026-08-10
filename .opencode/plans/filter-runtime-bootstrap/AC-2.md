---
ac: 2
depends_on: []
merge_with: [1]
risk: medium
status: in-progress
---

# AC-2: start() adapter-map dispatch keyed by data-language + synchronous double-start guard

## USER DECISIONS (approved)
1. start() accepts adapter MAP ONLY — single-adapter signature REMOVED, all 8 fixture call sites migrated (no backward-compat wrapper).
2. The `|| runtime.language || "r"` fallback at exercise-runtime.js:419 is REMOVED — missing data-language → skip + warn, never default.
3. Merge coupling: AC-1 + AC-2 land in same batch — do not ship AC-2 without AC-1 (fallback removal means pre-AC-1 emitted HTML gets exercises skipped; verified safe in-repo since all test HTML regenerates at test time).

## Executable Spec (USE THIS MERGED VERSION — from resolver):
- predicate: start(registry, adapters) accepts adapter map keyed by language string ONLY. Fixture mixed-runtime.html with 4 exercises (R, python, R, attribute-less) + two instrumented mocks {r: mockR, python: mockPy}:
  1. routing: R Run → mockR.calls grows, mockPy unchanged; python Run → mockPy grows, mockR unchanged; two R exercises route to SAME mockR instance.
  2. double-start guard: module-level flag set SYNCHRONOUSLY at entry (before mount loop, window.__btExercises, any await); two fire-and-forget start() calls → each adapter bootCount === 1, exactly one console.warn matching /called twice|already started/, no re-mount (.cm-editor count stays 3), resolves without throw.
  3. fallback removal: attribute-less exercise SKIPPED with warn, absent from window.__btExercises.
  4. map validation: unknown language not in map → skip + warn, others mount, start() resolves; map key ≠ adapter.language → console.error + skip that adapter; non-function boot/run → console.error + skip.
  5. boot: every adapter gets exactly one boot() via Promise.all.
  6. source greps: `|| runtime.language || "r"` absent; `start(registry, adapters)` present; guard assignment precedes first await; no static import of webr-adapter/pyodide-adapter into exercise-runtime.js.
- probe: `uv run node rodney-probes/mixed-runtime.js && uv run node scripts/tests/validate-runtime.js` (mixed-runtime.js = node rodney harness on waitFor/asyncTest pattern from rodney-probes/pyodide-adapter.js; fixture calls start() twice unawaited, spies console.warn, rodney-clicks Run buttons, asserts calls/bootCount/__btExercises.length === 3/warn count === 1)
- negative: (a) cheapest fake (single-adapter + fallback + no guard) fails 3 ways: python routes through R adapter; attribute-less runs as R; second start() double-boots bootCount===2. (b) guard set after first await → race probe catches. (c) per-exercise adapter instantiation → same-instance assertion fails.
- verification: rodney (node harness rodney-probes/mixed-runtime.js) + Node source greps in validate-runtime.js
- fixture status: NEW tests/fixtures/mixed-runtime.html (3 attributed + 1 attribute-less); NEW rodney-probes/mixed-runtime.js; MODIFY tests/fixtures/mock-adapter.js (createMockAdapter({name="mock", language="r"}={}) + bootCount); MIGRATE 8 call sites to {lang: adapter} maps: tests/fixtures/runtime.html:70, runtime-edge.html:89, pyodide.html:50, webr-runtime.html:62, rodney-probes/feedback-probe.js:205, quarto-fixture/webr.qmd:38, feedback.qmd:45, ux.qmd:54; STRING PINS: validate-runtime.js:97 + :125-127, validate-pyodide-adapter.js:266, runtime-probe.js:190 (comment); optional prose: docs/adr/0014-*.md:55,78, exercise-feedback.js:670
- rubric anchor: §1.2, §2.2, §3.2, §3.4, §5.3

## Design Intent
- §1: adapter map {[language]: Adapter} closed set — "exercise with no adapter" unrepresentable at call site; runtime refuses (skip+warn). Single-adapter shape eliminated, not wrapped.
- §2: dispatch resolution pure; only guard-set, warn, boot, mount effectful. Guard is 2-state machine — bool correct.
- §3: runtime owns dispatch over INJECTED instances only; must NOT statically import real adapters (adapter-agnostic, mocks injectable). Adapter loading belongs to AC-3 bootstrap (dynamic import). NO new adapter-router.js module (§4.2 anti-name).
- §4: exercise-runtime.js header gains: "NOT adapter loading, NOT language defaulting — missing data-language exercises are skipped, never defaulted."
- §5: start() = guard → validate map → mount/dispatch → boot. Routing ≤3 lines inside existing loop.

## Technical Context
- Files: exercise-runtime.js:416-431 (signature, guard at entry, routing :419, Promise.all boot :429), mock-adapter.js:22-27, 8 fixture call sites, validate-runtime.js:97,125-127, validate-pyodide-adapter.js:266, runtime-probe.js:190.
- Interface conformance verified: all three adapters implement {name, language, boot(), run()} (webr factory, pyodide singleton, mock factory).
- Adapter module top-levels side-effect-free (webr.mjs dynamically imported in ensureBooted(); pyodide CDN injected in bootPyodide() only) — static import of adapter modules safe, COI can't break Python-only pages via module scope.
- data-language dual-consumer: LANG_EXT editor extension (:50) AND dispatch. Language in map but absent from LANG_EXT mounts editor without highlighting but executes — document.
- No committed rendered artifacts — all test HTML regenerated at test time.

## Dependencies
- Depends on: none. Blocks: AC-3. Merge coupling: same batch as AC-1 (fallback removal unsafe without AC-1's attribute emission in any branch that renders HTML with old filter).
- Conflict set: exercise-runtime.js, mock-adapter.js, 8 fixture call sites (AC-6 keeps hand bootstraps — migration not wasted). Risk: medium.

### Progress
- [x] spec resolved — pending implementation
- [x] implemented (issue #136) — 2026-08-03: red probe 247eea1, feat bd43dd8, migration e899cf6, evidence 5d49882. Probe 14/14 PROBES_PASS; validate-runtime 49 pass; full regression green.
- [ ] PR review + merge (Director; same-batch as AC-1 #135)

### Decision Log
- user — approved migrate-all (no wrapper) + fallback removal.
- resolver — guard sync-at-entry adopted (A's end-of-function set has race hole); Promise.all boot confirmed; map-key mismatch validation adopted; B's mixed-runtime fixture (attribute-less + race probes) adopted over A's 2-exercise version.
- builder — implementation: window.__btExercises exposes only MOUNTED (filtered) registry, not the full scan (spec clause 3 requires attribute-less absent); usable-adapter validation runs once up-front (boot/run functions + map-key==adapter.language) so skipped adapters neither dispatch nor boot; boot dedupes map values via Set so duplicate instances boot once.
- builder — clause-4 page is harness-GENERATED (quarto-fixture/_mixed-validation.html, gitignored) rather than a second committed fixture; committed fixture stays exactly the spec'd 4-exercise shape.

### Surprises & Discoveries
- All 3 adapters already implement conformant {name, language, boot(), run()} interfaces.
- Adapter modules are top-level side-effect-free (verified) — static imports safe.
- window.__btExercises set mid-function (:425) — NOT usable as double-start guard.
- blendtutor.lua emit_widget does NOT emit data-language — pre-AC-1 rendered HTML has no data-language, so existing rodney probes (exercise-ux, feedback) skip ALL exercises on the 136 branch. Confirmed the documented AC-1+AC-2 merge coupling. Verified probes pass with AC-1 output shape simulated (data-language added to rendered divs).
- runtime-probe.js (AC-4) asserted both R+python runs land on ONE adapter.calls — impossible under per-language dispatch with map-key==adapter.language validation. Required updating the probe to track two mocks (assertions 6/9/13) — beyond the spec's "comment pin" note but mandatory for the migration.

### Idempotence & Recovery
- Safe retry: re-run probe; fixtures regenerated at test time.
- Rollback: git checkout -- _extensions/blendtutor/assets/exercise-runtime.js tests/fixtures/ quarto-fixture/ rodney-probes/ scripts/tests/validate-runtime.js scripts/tests/validate-pyodide-adapter.js
