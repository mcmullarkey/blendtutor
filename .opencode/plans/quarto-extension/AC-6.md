---
ac: 6
depends_on: AC-4
risk: medium
status: complete
---

## AC-6: Pyodide shared-boot adapter — lazy boot, per-run fresh globals, CDN injected by filter

### Executable Spec
- **predicate:** 7 clauses: (1) CDN injection — exactly one pinned pyodide.js script tag via hasDoneSetup guard; (2) single boot — loadPyodide called once (idempotent boot promise); (3) lazy trigger — no WASM before first Run; (4) per-run fresh globals — exercise 2 can't see exercise 1's variables; (5) error surface — boot fail, Python error, package fail, loadPyodide undefined each surface as structured error; (6) PyProxy cleanup — namespace.destroy() in finally; (7) coexistence with webR — independent, zero shared state.
- **probe:** rodney with pyodide.qmd (2 Python exercises) + mixed-lang.qmd (R+Python) + spy loadPyodide
- **negative:** boot() returns Promise.resolve() without loading Pyodide. run() short-circuits to {ok:true} without evaluating code. Second exercise passes → shared globals leaked.
- **verification:** code + rodney
- **fixture status:** pyodide.qmd (NEW), mixed-lang.qmd (NEW), pyodide-adapter.js (NEW), rodney-probes/pyodide-adapter.js (NEW)
- **rubric anchor:** §1.2, §1.3.1, §2.1, §2.4, §3.4, §4.1, §5.1

### Design Intent
- §1: Boot state 3-state machine (null/pending/resolved). Adapter exports {name, language, boot(), run()}.
- §2: bootPyodide effectful (idempotent). createNamespace pure. normalizeError pure. Thin effectful shell.
- §3: Adapter is protocol seam. Filter (Lua) owns CDN script injection. Bidirectional contract: Lua emits pinned URL, JS expects window.loadPyodide.
- §4: pyodide-adapter.js owns Pyodide boot + per-run namespace isolation. NOT webR, NOT LLM, NOT DOM scanning.
- §5: 3 functions: bootPyodide, createNamespace, normalizeError. run() orchestrates: boot → createNamespace → eval → destroy in finally.

### Technical Context
- Files: pyodide-adapter.js (NEW), blendtutor.lua (additive hook — CDN script dep), pyodide.qmd, mixed-lang.qmd
- CDN: https://cdn.jsdelivr.net/pyodide/v0.27.2/full/pyodide.js (classic non-module script)
- Per-run isolation: pyodide.toPy({}) creates fresh globals. namespace.destroy() in finally.
- loadPackage cumulative (packages shared across runs, globals fresh)
- Coexistence: webR uses ES module import, Pyodide uses classic script global — different boot mechanisms

### Dependencies
- Depends on: AC-4 (runtime adapter contract)
- Blocks: AC-10 (demo book needs Python exercises)
- Parallel with: AC-5 (webR adapter — separate files, no conflict)
- Conflict set: blendtutor.lua (additive hook), exercise-runtime.js, pyodide-adapter.js (new)
- Risk: medium

### Progress
- [x] RED: test fixtures (pyodide.qmd, mixed-lang.qmd), rodney probe, Node.js validation — confirmed fail (2026-07-23)
- [x] Implement pyodide-adapter.js — 3-state boot, per-run fresh globals, error surface, PyProxy cleanup (2026-07-23)
- [x] Modify blendtutor.lua — hasDoneSetup guard + has_python flag for CDN injection (2026-07-23)
- [x] GREEN: all tests pass (Node.js validation + shell test) (2026-07-23)
- [x] CI step added to .github/workflows/ci.yml (2026-07-23)

### Decision Log
- 2026-07-22 — B's 7 adversarial clauses merged with A's happy path
- 2026-07-22 — Mixed-lang fixture for coexistence test (B addition)
- 2026-07-22 — loadPackage cumulative flagged as pattern detector (fresh globals ≠ fresh packages)

### Surprises & Discoveries
- Pandoc Lua filter execution order: Div() runs BEFORE Pandoc(), not after. This means Pandoc() sees already-transformed RawBlocks, not original Divs. Required setting has_python flag in Div() and reading it in Pandoc() — could not scan doc.blocks for Div elements in Pandoc() because they were already transformed to RawBlock by the time Pandoc() runs. Verified with debug output showing call order: Div -> Div -> Pandoc.
- The validation script's regex for `type="module"` matched a comment in the adapter source that said `NOT type="module"`. Fixed by changing the comment to `NOT type=module` (without quotes around module).

### Idempotence & Recovery
- Safe retry: re-run rodney probe
- Rollback: delete pyodide-adapter.js
