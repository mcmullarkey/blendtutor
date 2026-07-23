---
ac: 5
depends_on: AC-4
risk: medium
status: in-progress
---

## AC-5: webR shared-boot adapter — lazy boot, per-run Shelter isolation, shared instance, degraded channel

### Executable Spec
- **predicate:** 2 R exercises on page WITHOUT COI (crossOriginIsolated === false). Before first Run, no webr.wasm fetch (lazy boot — verified via performance.getEntriesByType('resource')). First Run shows boot progress, Exercise 1 (x <- 5; print(x)) output contains [1] 5. Second Run shows NO boot progress (shared instance), Exercise 2 (print(exists("x"))) output contains [1] FALSE (per-run Shelter isolation).
- **probe:** rodney open webr.qmd + resource timing API + click Run + assert boot progress + shared instance + isolation
- **negative:** Reused Shelter → x leaks (exists("x") returns TRUE). New WebR per exercise → second boot progress shown (wastes memory).
- **verification:** rodney
- **fixture status:** quarto-fixture/webr.qmd (NEW, 2 R exercises), webr-adapter.js (NEW)
- **rubric anchor:** §1.2, §1.5, §2.1, §2.4, §3.1, §3.4, §3.5, §4.1, §5.1

### Design Intent
- §1: Boot state explicit enum (uninitialized/booting/ready/failed) — not nullable. Adapter implements {name, language, boot(), run()}.
- §2: 3 effectful ops: bootR (CDN+WASM), evalInShelter (per-run Shelter), teardownR (cleanup). Boot promise dedup.
- §3: Adapter is module boundary. Shelter isolation is per-run boundary. installPackages scope = boot phase.
- §4: webr-adapter.js owns webR boot + per-run Shelter eval + teardown. NOT exercise UI, NOT prompt construction.
- §5: boot() deduplicates promise. run() wraps Shelter in try/finally. Concurrent run guard.

### Technical Context
- Files: webr-adapter.js (NEW), webr.qmd (NEW)
- CDN: https://webr.r-wasm.org/latest/webr.mjs (dynamic import)
- No COI → no SharedArrayBuffer → degraded channel (slower, single-threaded)
- Shelter: webR's Shelter class provides variable isolation per eval scope
- Boot progress surfaced via callback/event to exercise-runtime

### Dependencies
- Depends on: AC-4 (exercise-runtime multi-exercise contract)
- Blocks: AC-9 (COI is optional optimization)
- Conflict set: exercise-runtime.js, blendtutor.lua
- Risk: medium

### Resolved Decisions
- Concurrent Run: REJECT WITH ERROR. Second Run click while first is executing → show error message "Another exercise is already running". Run button stays disabled during execution.
- installPackages scope: BOOT-PHASE ONLY. Packages installed once during webR/Pyodide boot, shared across all exercises. Matches current static-site behavior.
- CDN timeout UX: ERROR MESSAGE ONLY. Show "webR failed to boot: <error>" in exercise status area. Run button stays disabled. User must reload page to retry. No retry button, no auto-retry.

### Progress
- [x] Create branch 110-webr-shared-boot-adapter
- [x] Red: write validate-webr-adapter.js (54 structural assertions), webr-probe.js (8 rodney assertions), webr-runtime.html (HTML fixture)
- [x] Implement: create webr-adapter.js with lazy boot, Shelter isolation, concurrent guard, boot promise dedup
- [x] Create quarto-fixture/webr.qmd with 2 R exercises
- [x] Green: validate-webr-adapter.js passes (54/54), existing tests unaffected (130 total pass)
- [x] Evidence: test-suite.log + pandoc-render.log committed to docs/evidence/110/
- [ ] Rodney probe verification (builder-vision-probe dispatch needed)
- [ ] PR created

### Decision Log
- 2026-07-22 — B's exists("x") → FALSE concrete test over A's vague "references variable"
- 2026-07-22 — B's resource timing API for lazy boot verification
- 2026-07-22 — A's explicit crossOriginIsolated === false assertion
- 2026-07-22 — Boot state machine (observable behavior, not enum names)

### Surprises & Discoveries
- 2026-07-23 — boot() must be a no-op for lazy boot. exercise-runtime.js calls boot() in start() at page load. If boot() fetched WASM, the lazy boot invariant (no WASM before first Run) would be violated. Solution: boot() is empty, actual boot deferred to first run() via internal ensureBooted(). This is the key architectural insight — the adapter protocol's boot() is repurposed as a "register adapter" hook, not a "start WASM" hook.
- 2026-07-23 — webR Shelter API: captureR() returns {result, output} where output is array of {type, data}. Used captureR with evalR+toString fallback for robustness across webR versions.
- 2026-07-23 — blendtutor.lua was modified by issue #111 (pyodide CDN injection). Had to revert the change on this branch — the pyodide changes belong on branch 111, not 110.

### Idempotence & Recovery
- Safe retry: re-run rodney probe
- Rollback: delete webr-adapter.js
