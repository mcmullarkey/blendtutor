---
ac: 7
depends_on: "AC-2 (mountKeyPage + .blendtutor-key API), AC-3 (vendored lua + key-page.js in demo-book/_extensions, merged PR #174)"
risk: low
status: complete
---

# AC7 — Add demo-book API key page chapter

## Executable Criterion

- **P1:** `demo-book/api-key.qmd` (NEW) with fenced div `::: {.blendtutor-key}` + prose (what Fireworks key is, where to get it — Fireworks console, security model: browser localStorage only, Authorization Bearer to api.fireworks.ai only, never logged; HTTP-serve note: `file://` breaks localStorage + ES modules).
- **P2:** `demo-book/_quarto.yml` `book.chapters` lists `- api-key.qmd` BEFORE `- r-exercises.qmd`.
- **P3:** `bt-key-page: api-key.html` scalar in front matter of BOTH `r-exercises.qmd` AND `python-exercises.qmd`.
- **P4:** `demo-book/index.qmd` BYOK section Fireworks-only: literal `Fireworks` + link `api-key.html` + ZERO `ANTHROPIC_API_KEY`.
- **P5 (probe):** `quarto render demo-book --to html` exits 0; `_output/api-key.html` exists + contains `class="blendtutor-key"`; api-key/r-exercises/python-exercises `.html` each contain `__btConfig`.
- **Test placement:** extend `scripts/tests/test_quarto_distribution.sh` — structural arms (no quarto) + render arm INSIDE clause 7's post-render block (reuses `$RENDER_HTML_DIR`, skips when quarto missing).

## Progress

- [x] Red: extended test script with 7 structural + 2 render arms; confirmed 9 fails (api-key.qmd absent, no registration, no bt-key-page meta, index stale, api-key.html absent). `2026-08-07`
- [x] Green: wrote api-key.qmd, registered in _quarto.yml, added bt-key-page meta to both exercise chapters, rewrote index BYOK; render arm tripped on div shape (see Surprises) then passed. `2026-08-07`
- [x] Full suite: `test_quarto_distribution.sh` 89 passed / 0 failed; `uv run pytest -q` 8 passed. Evidence at `docs/evidence/168/` (test-suite.log, render.log, render-proof.md). `2026-08-07`

## Decision Log

- **api-key.qmd div content:** empty div initially — rendered as `<section class="level3 blendtutor-key">` when a `###` heading sat inside (book heading numbering), breaking P5's literal `class="blendtutor-key"` grep. AND empty div tripped clause 9's empty-exercise-div awk (its `/::: \{\.blendtutor/` pattern matches `.blendtutor-key` too). Resolution: short fallback paragraph inside the div — pandoc emits `<div class="blendtutor-key">` (exact) and clause 9 sees non-empty content. mountKeyPage injects its own UI over it anyway.
- **No ADR:** AC-7 is declarative content/config — no new interface or boundary (per spec note, ADR-0022 "unlikely"; skipped).
- **Single commit** (content + test): one logical feature, avoids shipping a failing red commit to the PR branch.

## Surprises & Discoveries

- **Quarto heading-numbering mutates fenced div classes:** `::: {.blendtutor-key}` with a heading inside renders as `<section id=... class="level3 blendtutor-key" data-number=...>` — the spec's literal `grep -qF 'class="blendtutor-key"'` fails on real output. Headingless div+paragraph renders exact `<div class="blendtutor-key">`. Lesson: for exact-class render greps, keep fenced divs heading-free.
- **Clause 9 empty-div awk collides with `.blendtutor-key`:** the pre-existing empty-exercise-div detector matches ANY `.blendtutor` fenced div including the key page's; an empty key div fails clause 9. Non-empty div content satisfies both the detector and the exact-class grep.
- **Vendored lua already current on main:** AC-3's merge (#174) had synced demo-book/_extensions — `__btConfig` + `keyPageUrl = "api-key.html"` emitted on all 3 chapter pages on first render; no stale-vendored-copy trip.
