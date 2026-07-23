---
ac: 7
depends_on: AC-4
risk: medium
status: complete
---

## AC-7: Per-exercise BYOK LLM feedback — shared key, per-exercise scoping, ?provider= override

### Executable Spec
- **predicate:** 9 clauses: (1) key entered once — shared sessionStorage key slot (provider-scoped, not exercise-scoped); (2) per-exercise scoping — each exercise has its own feedback container, no singleton #feedback; (3) key reused — readKey reads from shared slot, second exercise skips key prompt; (4) provider switch — PROVIDERS map + storeProvider/readProvider + provider chooser; (5) ?provider= override — providerBaseUrl honors localhost-only override, rejects non-local/credentialed; (6) llm_evaluation_prompt ABSENT — never in JS or qmd; (7) fetch spy (STUDENT_CODE fences) — buildPrompt emits fences, backends call fetch; (8) concurrent — per-exercise _feedbackRunning guard; (9) UI visibility — feedback button + container per-exercise.
- **probe:** rodney with feedback.qmd + fetch spy
- **negative:** Silently skips fetch. Cross-exercise bleed. llm_evaluation_prompt leaks.
- **verification:** rodney + code
- **fixture status:** feedback.qmd (NEW), exercise-feedback.js (NEW), test_quarto_feedback.py (NEW)
- **rubric anchor:** §1.2, §1.3.1, §1.5, §3.2, §3.4, §5.1

### Design Intent
- §1: FeedbackConfig type (PROVIDERS map); PromptBuilder pure function (buildPrompt); sessionStorage key contract (provider-scoped slots).
- §2: buildPrompt() pure; fetch effectful; key storage effectful. No module-level effectful code — pure layer importable in Node.js.
- §3: Pure layer separate from effectful shell; feedback separate from runtime (exercise-runtime.js).
- §4: exercise-feedback.js owns feedback lifecycle. NOT execution, NOT filter, NOT editor.
- §5: buildPrompt returns string; fetch returns Promise; no implicit state. mountFeedback(entry) per-exercise.

### Technical Context
- Files: _extensions/blendtutor/assets/exercise-feedback.js (NEW), quarto-fixture/feedback.qmd (NEW), scripts/tests/test_quarto_feedback.py (NEW)
- Fork of feedback.js (crates/core/assets/shared/feedback.js) — kills 3 singletons: getElementById("feedback"), window.__bt.getSubmission(), module-level submit wiring.
- Pure layer ported byte-identical: neutralize, buildPrompt, parseModels, modelRoster, feedbackRequest, toVerdict, fireworksRequest, fireworksToVerdict, providerBaseUrl.
- Shared sessionStorage: key slots are provider-scoped (fireworks_api_key, anthropic_api_key), NOT exercise-scoped.
- Per-exercise: mountFeedback(entry) creates button + container inside div.bt-exercise. mountAllFeedback(registry) called after start(registry, adapter).
- ?provider= override: localhost-only gate, rejects non-local/credentialed (key-exfil defense).

### Dependencies
- Depends on: AC-4 (per-exercise registry — window.__btExercises, entry.getSubmission())
- Conflict set: exercise-feedback.js (NEW), exercise-runtime.js (serialize before AC-8)
- ADR: ADR-0014

### Progress
- [x] RED: test_quarto_feedback.py (9 clauses) — confirmed fail (2026-07-23)
- [x] Create feedback.qmd fixture (2 R exercises + feedback import) (2026-07-23)
- [x] Implement exercise-feedback.js — port pure layer, rewrite effectful shell per-exercise (2026-07-23)
- [x] GREEN: 32 tests pass (source patterns + Node.js behavioral + qmd structure) (2026-07-23)
- [x] ADR-0014 written (2026-07-23)
- [x] CI step added to .github/workflows/ci.yml (2026-07-23)
- [x] Evidence saved to docs/evidence/112/test-suite.log (2026-07-23)

### Decision Log
- 2026-07-23 — Option 2 chosen (per-exercise containers + shared sessionStorage). Kills cross-exercise bleed at UI layer; shared provider-scoped slots satisfy "key entered once, reused."
- 2026-07-23 — No module-level effectful code: applyEmbeddedKey() moved into mountAllFeedback. Pure functions importable in Node.js without side effects — testable without browser.
- 2026-07-23 — Per-exercise concurrent guard (_feedbackRunning) prevents overlapping requests on SAME exercise. Different exercises have independent guards and can fetch concurrently.

### Surprises & Discoveries
- Source-pattern checks false-positive on comment lines: the header comments documenting "what we killed" (getElementById("feedback"), window.__bt.getSubmission()) matched the source-pattern checks. Fixed by adding _strip_comments() helper that removes // comment lines before pattern matching. Lesson: when checking "pattern X must NOT appear in source," strip comments first or the documentation of the kill triggers the kill detector.
