---
ac: 1
depends_on: none (parallel with AC-2; shares canonical CSV dataset contract)
risk: medium
status: complete
---

## AC-1 Final Spec (resolved): Rigorous R data-wrangling Exercise 3 — demo-book/r-exercises.qmd

Append ONE rigorous multi-step pure-data-wrangling R exercise (Exercise 3) to demo-book/r-exercises.qmd. Leave Exercises 1-2 (lines 1-56) byte-identical. No .checks blocks (webR adapter ignores them — dead config). Grading = LLM judgment on prose + code + captured .bt-output.

### Executable Spec
- **predicate:** ALL hold:
  1. demo-book/r-exercises.qmd contains exactly 3 `::: {.blendtutor language="r"}` divs, exactly 3 plain ```r code blocks, exactly 3 ```{.r .solution} blocks, exactly 2 ```{.r .checks} blocks (Exercise 3 OMITS .checks — count stays 2), exactly 1 `## Exercise 3:` heading, exactly 1 {.hints} div in the Exercise 3 region.
  2. Exercises 1–2 byte-identical — git diff on lines 1–56 shows additions only.
  3. Exercise 3 code_template embeds the canonical 24-row CSV (below) via read.csv(text = "...", stringsAsFactors = FALSE).
  4. Solution correctness: extracted .solution block run via Rscript --vanilla exits 0 and printed aggregate contains exactly 3 category rows — books 96, clothing 190, electronics 172.5 (hand-verified; stopifnot abs(...) < 1e-9 per value).
  5. Print contract: solution stdout contains ALL six step labels "Step 1:" … "Step 6:" (≥6 named prints, distinct labels).
  6. Prompt prose is Para/Plain ONLY — no BulletList/Header/BlockQuote before the first CodeBlock (blendtutor.lua:418 silently drops them; LLM would see no task).
  7. Solution drops NA rows (no imputation) and drops exact full-row duplicates (dupes share order_id); every wrangling step load-bearing (omitting any step changes the aggregate: alias-miss → 4th group elec 37.5; dedup-miss → electronics 226, books 111; NA-miss → phantom rows).
  8. Rodney migration: rodney-probes/demo-book-bootstrap.js R-page clause asserts === 3 at the __btExercises.length, .cm-editor, and payload-script assertions (was === 2); comments updated. rg '=== 2' in the R-page clause block returns zero matches. Python-page clause (L222-243) left at === 2 — that is AC-2's migration, not this one.
  9. YAML frontmatter (lines 1–5) untouched.
- **probe:** quarto render demo-book/r-exercises.qmd succeeds; structural counts (rg -c blendtutor language="r" → 3, ```r → 3, {.r .solution} → 3, {.r .checks} → 2, ## Exercise 3: → 1); awk no-.checks-in-Ex3-region; git diff additions-only; Rscript solution run asserting 96/190/172.5 + 6 step labels (fallback if no local Rscript: hand-verified arithmetic is the contract, structural grep + quarto render + rodney webR run); awk no-bullet/header/blockquote between Ex3 opener and first ```r; uv run node rodney-probes/demo-book-bootstrap.js local → clause 8 green.
- **negative:** (A) .checks block added "for documentation"; (B) prompt as markdown bullet list (silently dropped → empty LLM task); (C) dataset without exact full-row dupes sharing order_id → dedup no-op sneakypass; same trap NA-drop/normalize/alias; (D) imputation instead of NA-drop; (E) solution = blanks only (solution button replaces editor content — must be complete: dataset + all 6 steps); (F) <6 named step-labeled prints; (G) expected aggregates differ from 96/190/172.5; (H) Exercises 1–2 altered/renumbered; (I) bootstrap.js still === 2 on R-page clause → CI red; (J) quarto render errors.
- **verification:** code (structural grep/awk + quarto render + Rscript run) · rodney secondary (bootstrap.js clause 8 post-migration)
- **fixture status:** demo-book/r-exercises.qmd:1-56 byte-identical + NEW append (~90-120 lines); rodney-probes/demo-book-bootstrap.js R-page clause MIGRATE 2→3 (comment L8-9, L189; asserts L197-198, L201-202, L218)
- **rubric anchor:** §1 (each step load-bearing; exact aggregates are the invariant) · §2 (pure wrangling, base R only, standalone in webR) · §4 (prompt prose IS the grading contract; parser-drop = silent contract violation)

### Canonical Dataset (BINDING — identical CSV consumed by AC-2 Python via pd.read_csv(io.StringIO(CSV)))
```
order_id,product,category,price,quantity
1,Widget,Electronics,$12.50,3
2,Gadget,electronics,$8.00,2
3,Book A,Books,$15.00,1
4,Shirt,Clothing,$20.00,2
1,Widget,Electronics,$12.50,3
5,Gadget,ELECTRONICS ,unknown,2
6,Book B,books,$15.00,
7,Pants,Clothing ,$25.00,1
8,Widget,Elec,$12.50,3
2,Gadget,electronics,$8.00,2
9,Book C,Books,15.00,2
10,Shirt,clothing,$20.00,2
11,Hat,Clothing,$10.00,
12,Gadget,electronics,$8.00,1
13,Book D,books,$18.00,2
14,Widget,Electronics,$ 5.00,4
15,Pants,clothing ,$25.00,1
16,Book E,Books,$15.00,1
17,Gadget,ELECTRONICS,$8.00,2
18,Widget,electronics,$12.50,3
19,Shirt,Clothing,$20.00,2
3,Book A,Books,$15.00,1
20,Book F,books,unknown,1
21,Hat,clothing,$10.00,2
```
- 24 rows, 5 cols. Dupes: rows 5/10/22 = exact full-row dupes of rows 1/2/3 (share order_id 1/2/3). NA: row 6 (unknown price), rows 7/13 (empty quantity), row 23 (unknown price). 17 rows kept.
- 3 canonical categories after normalize: books, clothing, electronics (lowercase). Alias map: only elec → electronics.
- **Expected revenue (hand-verified, exact IEEE754): books 96, clothing 190, electronics 172.5. Total 458.5.**
- Mess variants each test a DISTINCT step: price "$12.50" / "15.00" bare / "unknown" / "$ 5.00" (space after $ — gsub("[$ ]","")); category case+whitespace variants; "Elec" truncation alias; empty quantity → NA.
- Step-omission signatures: alias-miss → 4 groups (spurious elec 37.5); dedup-miss → electronics 226, books 111 (total 527); NA-miss → phantom rows; price-clean-miss → all NA/error.

### Solution shape (reference — complete runnable code, base R)
```r
sales <- read.csv(text = "<CSV above>", stringsAsFactors = FALSE)
print("Step 1: raw data"); print(sales)
sales$price <- as.numeric(gsub("[$ ]", "", sales$price))
print("Step 2: cleaned price"); print(sales)
sales$category <- trimws(tolower(sales$category))
sales$category[sales$category == "elec"] <- "electronics"
print("Step 3: normalized category"); print(sales)
sales <- sales[!is.na(sales$price) & !is.na(sales$quantity), ]
print("Step 4: dropped NA rows"); print(sales)
sales <- sales[!duplicated(sales), ]
print("Step 5: dropped exact duplicates"); print(sales)
sales$revenue <- sales$price * sales$quantity
agg <- aggregate(revenue ~ category, data = sales, FUN = sum)
print("Step 6: revenue per category"); print(agg)
```

### code_template / prose / hints
- code_template = same read.csv block + Step 1 print given, blanks ___ at steps 2–6.
- Prompt prose = plain paragraphs enumerating 6 steps inline, explicit print contract ("print after each step with a label like print('Step 2: cleaned price')"), "drop NA (do NOT impute)" + "drop exact duplicates" + alias requirement, expected STRUCTURE only ("exactly 3 normalized categories: books, clothing, electronics" — never numeric values).
- Hints div: markdown lists OK (hints rendered separately, not parser-constrained): gsub()/as.numeric() (unknown→NA automatically); trimws()+tolower(); named vector c(elec="electronics"); is.na() subset; duplicated(); aggregate(revenue~category, FUN=sum).

### Design Intent
- §1: dataset engineered so each of 6 steps is load-bearing; exact aggregates (96/190/172.5) are the invariant.
- §2: wrangling pure, base R only, no I/O beyond print() — print is the effectful evidence channel, ONLY signal LLM grades beyond code.
- §3: joint = the DATASET (shared CSV string), not syntax — mirrors Python AC for valid cross-language LLM-feedback comparison.
- §4: self-contained single .blendtutor div, additive; prompt prose MUST be Para/Plain (parser constraint blendtutor.lua:418).
- §5: 6 steps, one transformation each, printed once, top-to-bottom script.

### Technical Context
- Files touched: demo-book/r-exercises.qmd (append after line 56; YAML 1-5 + Exercises 1-2 untouched) + rodney-probes/demo-book-bootstrap.js (R-page clause only: L8-9 comment, L189 comment, L197-198, L201-202, L218).
- Gotchas: blendtutor.lua:418 drops BulletList/Header/BlockQuote from prompt; first plain code block = code_template (second plain block silently dropped); webR captureR wraps capture.output (print/cat captured); per-run Shelter isolation → solution self-contained; $ needs NO JSON escaping; webR prints 172.5 cleanly.
- NOT affected: pages-live.js (/demo/ standalone — vestigial); bootstrap.js fix-index clause (1R+1Py, unaffected); bootstrap.js python clause (AC-2 scope); Rust suite (examples/ YAML, not demo-book).
- quarto render requires local Quarto — verify availability; if absent, structural greps + rodney local run are the gate.
- Rscript may be absent locally — hand-verified arithmetic is the PINNED contract; do NOT re-derive expected values.
- When editing bootstrap.js, touch ONLY the R-page clause sites; python clause and fix-index clause out of scope.

### Dependencies
- Depends on: none. Blocks: AC-2 (consumes canonical dataset identically). Conflict set: demo-book/r-exercises.qmd, rodney-probes/demo-book-bootstrap.js (R-page clause; disjoint line ranges from AC-2's python clause).

### Progress
- [x] Spec resolved (2026-08-07) — canonical CSV pinned, rodney migration mandated
- [x] Implementation committed (2026-08-07, branch 188-rigorous-r-exercise)
  - test(red) b50111a: verify_solution.sh + red-run.log (fails: 2 divs, Exercise 3 absent)
  - feat 5bbc084: r-exercises.qmd Exercise 3 (137 insertions, Ex1-2 byte-identical)
  - test c07a50d: bootstrap.js R clause 2→3 (python clause byte-identical to HEAD)
  - docs a2b8028: E2E evidence (solution run, payload probes, negative controls, suite logs)
  - Gates green: quarto render (single + full book), test_quarto_{render,filter,bootstrap,distribution}.sh
    (12/27/74/91 passed), Rscript 4.5.1 solution run, aggregate 96/190/172.5

### Decision Log
- 2026-08-07 — Dataset: speculator-b's portable CSV wins over speculator-a's data.frame and Python dict literal: one CSV string works verbatim in read.csv(text=) AND pd.read_csv(io.StringIO()); cross-language comparability is the feature's whole point.
- 2026-08-07 — Rodney bootstrap.js migration mandated (hidden blast radius found by speculator-b: === 2 hardcoded → CI red if not migrated).
- 2026-08-07 — Prompt prose MUST be Para/Plain (blendtutor.lua:418 drops BulletList/Header from LLM prompt).
- 2026-08-07 — Implementation: template blanks as `# Step N: <action>` comment + `___` line per step (step 1 fully given); solution = complete self-contained base-R script (Shelter isolation). Verifier kept in docs/evidence/188 (not a repo test file — plan's file surface is qmd + bootstrap.js only).
- 2026-08-07 — Python-clause "untouched" check pinned as byte-identical-to-HEAD, not a `=== 2` count: the clause contains 4 `=== 2` occurrences (2 assert bodies + 2 message strings), so a naive count-of-2 check fails.

### Surprises & Discoveries
- webR adapter ignores .checks blocks entirely (webr-adapter.js:194) — R exercises cannot be assertion-graded in-browser.
- blendtutor.lua:418 collects ONLY Para/Plain before first CodeBlock — markdown lists in prompt prose silently vanish from the LLM task.
- rodney-probes/demo-book-bootstrap.js hardcodes exercise counts (=== 2) — adding an exercise breaks CI without any qmd change.
- R's data.frame print renders 96.0 as "96" but keeps 172.5 — parse aggregate with numeric comparison ($3+0 == 96), not string equality.
- aggregate() sorts result rows alphabetically (books, clothing, electronics) — stable order for output parsing.
- quarto render in the worktree has no ~10GB target/ slowdown — that pathology is specific to the main repo; worktree render is fast.
- Rendering quarto-fixture coi-book via test_quarto_filter.sh leaves untracked `chapter-*_files/` sidecar dirs; remove before commit (git add -A would sweep them).
- Negative controls empirically confirm plan's predicted omission signatures: alias-miss → spurious elec 37.5; dedup-miss → electronics 226 / books 111; impute-miss → books 139.75 / electronics 200.

### Idempotence & Recovery
- Safe retry: re-run quarto render + structural greps + Rscript solution check; bootstrap.js local run. All deterministic.
- Rollback: revert the qmd append + the bootstrap.js 2→3 edits (single commit each).