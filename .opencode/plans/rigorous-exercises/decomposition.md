# Decomposition: rigorous-exercises

## Feature Goal

Add ONE rigorous multi-step "pure data wrangling" exercise to each of `demo-book/r-exercises.qmd` and `demo-book/python-exercises.qmd`, appended as Exercise 3 in each file, leaving the two existing trivial exercises per file untouched. Purpose: stress-test blendtutor's LLM feedback quality on a realistic multi-step wrangling task (messy data: NAs, wrong types, inconsistent categories, duplicate rows — no modeling) versus the current trivial single-expression exercises (add, greet, square, is_even). Content-only qmd changes — no extension, runtime, or LLM-prompt changes.

## Shared Exercise Design (same dataset concept in both languages — DECIDED)

**Why same concept:** enables apples-to-apples comparison of LLM feedback quality across the R (prose-judged) and Python (check-graded) grading modes. One dataset, one wrangling spec, two language realizations.

**Dataset concept:** a ~20-30 row messy sales-transactions log embedded inline as a literal in the `code_template` (R: `data.frame(...)` or `read.csv(text = ...)`; Python: CSV string + `StringIO` or `pd.DataFrame` dict literal). Deliberate mess:

1. **Missing values** — `NA` / `None`/empty strings in `price` and/or `quantity`.
2. **Wrong types** — `price` column character/string due to entries like `"$12.50"` and `"unknown"`.
3. **Inconsistent categories** — `"Electronics"`, `"electronics"`, `"ELECTRONICS "`, `"Elec"` variants in a `category` column.
4. **Duplicate rows** — 2-3 exact duplicate transaction rows.

**Wrangling steps (spec, ~5 steps, scaffolded with blanks in template):**

1. Inspect the raw data (print structure/head).
2. Clean `price` to numeric (strip `$`, coerce `"unknown"` to missing).
3. Normalize `category` to a canonical set (trim + lowercase + map alias `"elec"` → `"electronics"`).
4. Drop rows with missing price/quantity (drop, not impute — deterministic expected answer).
5. Drop exact duplicate rows.
6. Final step: aggregate total revenue (`price * quantity`) per category; print the result.

**R print strategy (AC-1):** webR adapter IGNORES `.checks` blocks, so grading is LLM judgment on prose + code + captured `.bt-output`. The prompt MUST explicitly instruct the learner to print intermediate results after each cleaning step (e.g., "print the number of NA prices before and after cleaning", "print the unique category values after normalization") plus the final aggregated table. **Omit `.checks` blocks entirely in the R exercise** — they are dead config under webR and would imply grading that never runs.

**Python check strategy (AC-2):** `.checks` run as real Pyodide assertions. Multiple `.checks` blocks allowed; use them to verify: cleaned frame has no NA in `price`/`quantity`, no duplicate rows remain, `category` values ⊆ canonical set, and the final per-category revenue dict/series equals the expected literal values. Checks reference learner-visible variable names fixed by the template scaffold (e.g., `clean_df`, `revenue_by_cat`).

**LLM stress-test angle:** a 5-6 step pipeline lets students make subtle intermediate errors (normalize-before-dedupe vs after, impute-instead-of-drop, off-by-one category alias) that produce plausible-looking final output. Tests whether the LLM catches intermediate-step errors, not just the final answer, within the 1024-token `max_tokens` budget. Also tests partial-credit behavior on a half-completed multi-step attempt.

**Hints:** include a nested `{.hints}` div in both exercises (multi-step tasks need scaffolding; existing exercises inconsistently include hints — this one should).

## AC Table

| AC | Description | Dependencies | Conflict Set | Risk |
|----|-------------|--------------|--------------|------|
| 1  | Append rigorous R data-wrangling exercise (Exercise 3) to `demo-book/r-exercises.qmd` per shared design: messy sales dataset inline in template, 5-6 wrangling steps with blanks, explicit print-per-step instructions (no `.checks`), one `.solution`, `{.hints}` div | none | `demo-book/r-exercises.qmd` | med |
| 2  | Append rigorous Python data-wrangling exercise (Exercise 3) to `demo-book/python-exercises.qmd` per shared design: same messy sales dataset concept inline in template, same 5-6 wrangling steps with blanks, real `.checks` assertions (no NAs, no dupes, canonical categories, exact expected revenue values), one `.solution`, `{.hints}` div, `packages="pandas"` attr | none | `demo-book/python-exercises.qmd` | med |

**Risk rationale (both med, not low):** single-file content changes, but "rigorous" design quality is the real work — the expected final values must be hand-verified, the R exercise's prose must be LLM-judgeable (unambiguous print contract), and the Python expected revenue literals must exactly match the dataset after cleaning.

## Dependency DAG

```
AC-1 (R exercise)     — independent
AC-2 (Python exercise) — independent
```

No edges. The two ACs share only the design spec above (recorded in this file, not in code).

## Hot Conflict Files

- none identified — AC-1 touches only `demo-book/r-exercises.qmd`, AC-2 touches only `demo-book/python-exercises.qmd`. YAML headers (`coi: true`, `bt-key-page`) already correct; untouched.

## Suggested Batch Schedule

- Batch 1 (parallel): AC-1, AC-2

Both are content authoring against the shared design spec in this file; no ordering constraint.

## Verification Notes (for speculators/builders)

- Deterministic check available: `quarto render demo-book/r-exercises.qmd` and `quarto render demo-book/python-exercises.qmd` must succeed, and the `.blendtutor` div must parse (existing filter; no runtime change).
- Builder self-validation: serve the rendered page, paste the `.solution` into the editor, run it, confirm expected printed output (R) and green checks (Python). Hand-verify expected revenue values from the dataset literal BEFORE writing the checks/solution.
- CAUTION (from memory): a ~10GB `target/` Rust build cache in the repo root slows `quarto render` ~45x (1.3s fresh → 70-95s). If a render-based verification times out locally, `du -sh target`, move it aside, re-run, restore. CI unaffected.
- Both exercises are UI-touching (new rendered content on live demo-book) — vision probe applies, but the deterministic render+run check carries most of the weight.

## Open Questions

- [needs-clarification] Publish path: after the qmd edits land, who re-renders/publishes demo-book to GitHub Pages (CI on merge, or manual `quarto publish gh-pages`)? Feature scope says "content-only qmd changes" — assuming render/publish is a separate existing mechanism, but if the builder must also trigger deployment, that step needs to be in scope.
- [decided, not open] Same dataset concept in both languages: YES (decided above for cross-language LLM feedback comparability).
- [decided, not open] R exercise `.checks` blocks: OMIT (webR ignores them; dead config would imply grading that never runs).
