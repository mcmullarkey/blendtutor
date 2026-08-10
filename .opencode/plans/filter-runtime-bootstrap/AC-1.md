---
ac: 1
depends_on: []
risk: low
status: complete
---

# AC-1: emit_widget() emits data-language="r|python"; filter-output verification asserts per-index pairing + pins runtime read

## Executable Spec (USE THIS MERGED VERSION — from resolver):
- predicate (6 clauses): given quarto-fixture/filter.qmd (4 exercises: r, python, r, r) rendered to HTML:
  1. Widgets extract with attr-tolerant regex `<div class="bt-exercise"[^>]*>\s*<script type="application/json">(.*?)</script>`; ≥4 widgets; 9-key JSON payload contract intact.
  2. EVERY bt-exercise div carries exactly one data-language="r" or "python"; count of data-language attrs === widget count (all-or-none).
  3. PER-INDEX pairing matches filter.qmd order [r, python, r, r] — idx 1 (filter.qmd:36) MUST be python. Catches hardcoded-"r" fakes.
  4. Static pin: exercise-runtime.js still reads `entry.element.dataset.language ||` at :419.
  5. Static pin: verify_filter_output.py no longer contains exact matcher `<div class="bt-exercise">`.
  6. Refusal arms unchanged: filter-invalid-lang.qmd + filter-missing-lang.qmd render zero bt-exercise + WARNING, div passthrough.
- probe: canonical `bash scripts/tests/test_quarto_filter.sh` (CI harness); direct debug loop: quarto render quarto-fixture/filter.qmd --to html + inline python3 regex assertions + python3 scripts/tests/verify_filter_output.py quarto-fixture/filter.html
- negative: (a) omit attr → clause 2 fails div[0]; (b) hardcode "r" → clause 3 fails idx 1; (c) attr emitted but runtime read removed → clause 4 fails; (d) attr leaks to latex → existing test_quarto_filter.sh latex assertion.
- verification: code
- fixture status: MODIFY scripts/tests/verify_filter_output.py (:46-48 regex → attr-tolerant + per-index assertions); MODIFY scripts/tests/test_quarto_filter.sh (data-language block + static pins). quarto-fixture/filter.qmd unchanged; hand fixtures already conformant.
- rubric anchor: §1, §2, §3, §4, §5

## Design Intent
- §1: data-language ∈ {"r","python"} enforced by validate_language() (:205-207) before emission (:364-368) — injection-safe by construction.
- §2: emit_widget(payload, lang) stays pure; threading lang adds no I/O.
- §3: data-language IS the emitter↔runtime contract — SOLE language carrier (payload has NO language key, 9-key contract unchanged). Runtime fallback || runtime.language || "r" becomes defense-in-depth only.
- §4: blendtutor.lua owns emission; exercise-runtime.js owns consumption (:419 → mountEditor :420 → editorExtensions :82-84).
- §5: emit_widget widens 1→2 params; Div() threads validated lang (:358) at call site :391.

## Technical Context
- Files: blendtutor.lua (:322-327, :391), verify_filter_output.py (:46-48), test_quarto_filter.sh (extend), quarto-fixture/filter.html (regen, not committed).
- lang validated before emit_widget; refusal arms :359-362/:364-368 pinned by clause 6. has_python gate (:372-374) unaffected.
- Only committed consumer of old shape: verify_filter_output.py regex. Hand fixtures already carry data-language. sync-quarto-assets.sh syncs codemirror/styles/coi only.
- filter.qmd 4-exercise order load-bearing — do not reorder.

## Dependencies
- Depends on: none. Blocks: AC-3. Conflict set: blendtutor.lua, verify_filter_output.py, test_quarto_filter.sh. Risk: low.

### Progress
- [x] spec resolved — pending implementation (2026-08-03)
- [x] test(red): attr-tolerant regex + per-index language assertions + static pins (42756a5)
- [x] feat: emit_widget(payload, lang) threads validated lang (47ce120)
- [x] docs: E2E evidence 6/6 clauses (086138c)

### Decision Log
- resolver — adopted B's 6-clause predicate + test_quarto_filter.sh harness; kept A's inline one-liner as dev debug loop; both proposers corrected context error (payload has NO language key — attribute is sole carrier).
- builder — no new ADR: emit_widget widening is a simple param thread, contract (dataset.language) already exists in runtime; ADR-0019 left unused.
- builder — shell-level count uses `data-language="[^"]*"` (not `[rp]`) — `[rp]` matches single char only and misses `"python"`. Guarded with `|| true` for set -o pipefail (zero-match grep returns 1).

### Surprises & Discoveries
- AC context claim "payload JSON already contains language" was factually wrong (build_payload emits 9 keys, no language key) — caught by both speculators independently.
- `set -o pipefail` + `grep -o ... | wc -l` kills test_quarto_filter.sh silently on zero matches — pipeline returns grep's exit 1, `set -e` aborts before the ko. Red run surfaced it; `|| true` fixes. Any future no-match regression will now FAIL loudly instead of dying silently.
- sed negative-control trap: `sed 's/emit_widget(payload, lang)/emit_widget(payload, "r")/'` matches BOTH the function definition param list AND the call site — replaced definition param with a string literal → invalid Lua → render exit 1. Scope sed to the full call line (`return emit_widget(...)`) for controls.
- verify_filter_output.py widget floor bumped 3→4 to match spec clause 1 "≥4 widgets" (filter.qmd has exactly 4, order load-bearing).

### Idempotence & Recovery
- Safe retry: re-run probe commands; rendered HTML regenerated at test time.
- Rollback: git checkout -- _extensions/blendtutor/blendtutor.lua scripts/tests/verify_filter_output.py scripts/tests/test_quarto_filter.sh
