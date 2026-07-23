---
ac: 4
depends_on: AC-2, AC-3
risk: high
status: in-progress
---

## AC-4: Multi-exercise runtime core — DOM scan, per-exercise registry, N editors, mock-adapter injection

### Executable Spec
- **predicate:** 3 exercises (R + Python + empty-template) mounted with mock adapter. 16 assertions: registry is Array+get(id) length 3, each entry {id, editorView, element, payload}, 3 distinct cm-editor instances, empty-template survives, malformed JSON isolation, per-exercise Check sends correct checks, per-exercise data-status isolation, per-exercise getSubmission distinct, per-exercise run isolation, mock not leaked (window.__bt undefined), no global #submission, concurrent run safety, duplicate-id defense (skip+warn), graceful degradation (CM6 fail→textarea), adapter injection seam (start(registry, runtime)).
- **probe:** rodney with runtime.html (3 exercises + mock) + runtime-edge.html (edge cases)
- **negative:** Singleton survives via null-check gate → all exercises share one editor. Fails registry-length, editor-count, distinct-instances assertions.
- **verification:** rodney
- **fixture status:** tests/fixtures/runtime.html (NEW), tests/fixtures/runtime-edge.html (NEW), tests/fixtures/mock-adapter.js (NEW)
- **rubric anchor:** §1.1, §1.5, §3.2, §3.4, §5.1, §5.3

### Design Intent
- §1: Registry is Array + get(id). Entry shape {id, editorView, element, payload}. editorView nullable only under degradation.
- §2: DOM scan + mount effectful. Pure helpers (buildRegistry, parsePayload) testable without browser.
- §3: NEW file exercise-runtime.js — FORK of lesson-runner-core.js, not modification. Kills 3 singletons: module-level editorView, #submission, window.__bt.
- §4: Owns scan+mount+wire. NOT execution (delegated to adapter), NOT feedback (AC-7), NOT filter (AC-2).
- §5: scanExercises, parsePayload, mountEditor, wireExercise, buildRegistry — each one job.

### Technical Context
- Files: exercise-runtime.js (NEW), runtime.html, runtime-edge.html, mock-adapter.js
- Forks lesson-runner-core.js (365 lines). Reuses: CM6 extension builder, HighlightStyle, cursor theme, degradation pattern. Kills: module-level editorView (line 147), #submission (line 121), window.__bt (line 308), start(runtime) single-adapter signature (line 306).
- Runtime adapter protocol {name, language, boot(), run(code, checks, packages)} reused unchanged.

### Dependencies
- Depends on: AC-2 (HTML contract), AC-3 (vendored CodeMirror)
- Blocks: AC-5, AC-6, AC-7, AC-8
- Conflict set: exercise-runtime.js (AC-7/AC-8 modify), blendtutor.lua (AC-6/AC-9 add hooks)
- Risk: high

### Progress
- [x] RED: rodney probe scripts (runtime-probe.js 13 assertions, runtime-edge-probe.js 3 assertions) + HTML fixtures (runtime.html 3 exercises, runtime-edge.html 5 edge cases) + mock-adapter.js + validate-runtime.js (35 structural checks) (2026-07-23)
- [x] GREEN: exercise-runtime.js implemented — fork of lesson-runner-core.js, kills 3 singletons (module-level editorView, #submission, window.__bt), per-exercise registry with get(id), adapter injection seam start(registry, runtime), per-exercise degradation, concurrent run safety, duplicate ID defense, malformed JSON isolation (2026-07-23)
- [ ] Rodney verification (builder-vision-probe)

### Decision Log
- 2026-07-22 — B's 3-exercise+edge-case coverage subsumes A's 2
- 2026-07-22 — Registry get(id) needed by AC-7 feedback
- 2026-07-22 — start(registry, runtime) cleaner than start(adapter)
- 2026-07-22 — lesson-runner-core.js NOT modified (fork, not refactor)

### Surprises & Discoveries
- AC-2's blendtutor.lua does NOT emit `data-language` on the output div.bt-exercise — only `class="bt-exercise"`. The issue says the HTML contract includes `data-language`, but the actual filter output omits it. Resolved by including `data-language` in test fixtures (hand-crafted HTML) and having exercise-runtime.js read `data-language` from the div, falling back to `runtime.language`. Production filter update deferred to a future AC (AC-6 adds hooks to blendtutor.lua).
- CM6 failure simulation: exercise-runtime.js imports EditorView directly from codemirror.js via ES module import, so it cannot be monkeypatched from outside. Resolved by adding `data-cm-fail="true"` test hook attribute on the div — the runtime checks this attribute and throws inside the try-catch to simulate CM6 failure. This is a test-only hook that doesn't affect production (no production div will have this attribute).
- Rodney not available locally — probe scripts written as self-contained JS files with assert functions. Builder-vision-probe will execute them in a browser context. Local validation done via Node.js structural checks (syntax, exports, HTML structure, singleton-killed verification).

### Idempotence & Recovery
- Safe retry: re-run rodney probe
- Rollback: delete exercise-runtime.js
