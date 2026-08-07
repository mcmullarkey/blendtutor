---
topic: demo-book-exercise-authoring
created: 2026-08-07
slices: [188]
---

# Demo-book exercise authoring (blendtutor `.blendtutor` divs)

- 2026-08-07 (#188): Exercise div authoring contract in `demo-book/*.qmd`:
  - Prompt prose must be Para/Plain only before the first code block —
    `blendtutor.lua:418` collects ONLY `Para`/`Plain` blocks there; bullet
    lists, headers, and blockquotes are silently dropped from the LLM prompt.
    Enumerate multi-step tasks inline, one paragraph.
  - First plain code block = `code_template`; blocks with `.checks` class =
    checks; `.solution` block = solution (button replaces editor content, so
    it must be self-contained — includes the dataset again). No `.checks` for
    R exercises: the webR adapter ignores them entirely (webr-adapter.js:194).
  - `rodney-probes/demo-book-bootstrap.js` hardcodes exercise counts (`=== 2`)
    in the R-page clause (and python clause) — adding an exercise requires
    migrating the count, or CI/probe goes red. Verify the "untouched" clause
    byte-identical-to-HEAD, not by counting `=== 2`: each clause has 4 such
    occurrences (2 assert bodies + 2 message strings).
- 2026-08-07 (#188): R print-contract grading — solution stdout labels
  `print("Step N: ...")` are the only signal the LLM grades beyond code.
  `print(agg)` on an aggregate() data.frame sorts rows alphabetically;
  whole-number revenues render as `96.0` (R preserves the `.0` in fixed-width
  column output) — parse with numeric comparison, not string equality.
- 2026-08-07 (#188): Verification pattern that works: extract the LAST
  `{.r .solution}` block via awk, run `Rscript --vanilla`, assert exit 0 +
  step labels + exact aggregate. Negative controls (omit a step → aggregate
  changes) prove each wrangling step is load-bearing.
- 2026-08-07 (#188): `test_quarto_filter.sh` rendering `quarto-fixture/coi-book`
  leaves untracked `chapter-*_files/` sidecar dirs in the worktree — remove
  before committing (a stray `git add -A` would sweep them).
