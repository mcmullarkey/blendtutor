---
ac: 2
depends_on: AC-1
risk: medium
status: complete
---

## AC-2: Lua filter parses .blendtutor divs into widget HTML with embedded 9-key SiteLesson JSON

### Executable Spec
- **predicate:** Given filter.qmd with ≥3 exercise divs (full R, minimal Python, empty), quarto render --to html produces div.bt-exercise with script[type=application/json] for EACH. JSON has ALL 9 SiteLesson keys (id, title, prompt, code_template, checks, packages, solution, hints, gotchas). Full exercise: prompt has <code>, code_template has <-, checks array len 2, solution has "a + b", hints has <-, gotchas null, packages [], llm_evaluation_prompt ABSENT, id distinct, title non-empty. Non-HTML: no bt-exercise in PDF + warning. Invalid/missing language: warning + skip.
- **probe:** scripts/tests/test_quarto_filter.sh (renders + Python JSON assertions + PDF grep + warning grep)
- **negative:** Filter emits llm_evaluation_prompt (leaks prompt IP). Hardcoded JSON. Omits packages/gotchas (contract drift). Non-HTML emits widget. Silent default for invalid language.
- **verification:** code
- **fixture status:** filter.qmd (3 exercises), filter-invalid-lang.qmd, filter-missing-lang.qmd, verify_filter_output.py, test_quarto_filter.sh (all NEW)
- **rubric anchor:** §1.2, §1.3.1, §3.2, §4.1, ADR-0008

### Design Intent
- §1: Closed language set {r,python}. 9-key SiteLesson contract. Absent fields = null/[] (never omitted). llm_evaluation_prompt NEVER emitted.
- §2: Pure AST→AST transform. pandoc.write for prompt rendering is pure.
- §3: Cut at markdown↔HTML-widget joint. JSON payload IS the Rust↔JS contract.
- §4: blendtutor.lua owns detection + emission. NOT execution, NOT asset injection, NOT adapter lifecycle.
- §5: Split helpers: detect_exercise, validate_language, parse_inner_blocks, build_payload, emit_widget, should_skip_format.

### Technical Context
- Files: blendtutor.lua (filter body), filter.qmd, filter-invalid-lang.qmd, filter-missing-lang.qmd, verify_filter_output.py, test_quarto_filter.sh
- RawBlock("html", ...) for widget HTML. pandoc.write for prompt HTML rendering.
- packages attribute: comma-separated → array. Absent → [].
- Non-HTML: check FORMAT global, warning + return div unchanged.

### Dependencies
- Depends on: AC-1
- Blocks: AC-4 (runtime consumes .bt-exercise HTML contract), AC-10
- Conflict set: blendtutor.lua (AC-4/6/9 append), quarto-fixture/*.qmd

### Resolved Decisions
- id/title: AUTO-GENERATED. id = `bt-exercise-<index>`, title = `Exercise N`. Authors do NOT specify via div attributes. Duplicate-ID sneaky-pass is vacuous (auto-generated IDs are always unique).
- .gotchas: SUPPORTED. Filter parses `::: {.gotchas}` inner block mirroring `.hints`. Absent → null. Present → raw markdown text.

### Progress
- [x] RED: test_quarto_filter.sh + verify_filter_output.py + 3 fixture .qmd files written (2026-07-23)
- [x] GREEN: blendtutor.lua filter body implemented — Div detection, language validation, inner block parsing, 9-key JSON payload, widget emission (2026-07-23)
- [x] All 16 test assertions pass (HTML render, non-HTML skip, invalid/missing lang skip) (2026-07-23)
- [x] CI workflow updated with quarto-filter test step (2026-07-23)
- [x] Negative control: llm_evaluation_prompt leakage caught by verify_filter_output.py (2026-07-23)

### Decision Log
- 2026-07-22 — 9-key SiteLesson contract (B correct, A missed packages+gotchas)
- 2026-07-22 — 3 fixtures (B correct, A had 1) for test isolation
- 2026-07-22 — Python JSON assertions (B correct, A had shell-only grep)
- 2026-07-23 — Module-level exercise counter reset in Pandoc() for multi-document safety; Pandoc() kept as near-no-op for AC-1 compatibility (grep checks for `function Pandoc(doc)` + `return doc`)
- 2026-07-23 — Test script supports both quarto (CI) and pandoc (local dev) via RENDER_TOOL detection; pandoc fallback enables local TDD without quarto cask (needs sudo)
- 2026-07-23 — Hints/gotchas rendered to markdown via pandoc.write(doc, 'markdown') with trailing whitespace stripped; prompt rendered to HTML via pandoc.write(doc, 'html')

### Surprises & Discoveries
- Quarto cask install via `brew install quarto` requires sudo (pkg installer), which is not available in non-interactive shells. Pandoc (formula, no sudo) was installed instead for local TDD. The Lua filter is a standard pandoc Lua filter — `pandoc --lua-filter` produces identical widget HTML. The test script detects quarto vs pandoc and uses whichever is available, so CI (quarto) and local dev (pandoc) both work.
- Pandoc Lua filter traversal is bottom-up (children before parent), but inner .hints/.gotchas divs don't have the "blendtutor" class so they pass through unchanged. The outer .blendtutor div is processed in document order, so the module-level exercise counter increments correctly.
- `pandoc.write(doc, 'markdown')` adds a trailing newline; stripped with `gsub('%s+$', '')` to produce clean hints/gotchas strings.
- Pandoc converts the `language` div attribute to `data-language` in HTML output when the div is returned unchanged (invalid/missing language skip case). This is cosmetic — the test checks for absence of `bt-exercise`, not the div's rendered form.
- PR review cycle 1 found XSS via `</script>` in JSON-embedded string values: json_escape handled \ " \n \r \t but NOT <. HTML parser closes `<script>` at first `</script>` regardless of type attr. Fixed by escaping `<` to `\u003c`. Also added C0 control char escape (`%c` → `\uXXXX`) for JSON spec compliance. Test: 4th exercise fixture with `</script>` in code_template — before fix, JSON parsing failed (truncated by `</script>`); after fix, parses correctly.
- PR review cycle 1 found elseif chain in parse_inner_blocks silently dropped gotchas when a Div had both `.hints` and `.gotchas` classes. Fixed by changing `elseif` to separate `if` statements. Test: 4th exercise fixture with `::: {.hints .gotchas}` div — before fix, gotchas was null; after fix, both hints and gotchas are set.

### Idempotence & Recovery
- Safe retry: re-run scripts/tests/test_quarto_filter.sh
- Rollback: revert blendtutor.lua to no-op stub from AC-1
