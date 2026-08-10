# Plan: rigorous-exercises

## Feature Goal
Add ONE rigorous multi-step pure-data-wrangling coding exercise as Exercise 3 in each of demo-book/r-exercises.qmd and demo-book/python-exercises.qmd, to stress-test how blendtutor's LLM feedback performs on rigorous tasks vs the current trivial exercises. Both exercises use the SAME canonical messy sales CSV (24 rows: order_id/product/category/price/quantity; mess = string prices with $/unknown/space-after-$ / bare, case+whitespace category variants + Elec alias, empty quantities, 3 exact dupes sharing order_id; expected revenue after clean: books 96, clothing 190, electronics 172.5) — cross-language LLM-feedback comparability is the feature's whole point.

## Publish Path (resolved)
CI deploys demo-book to GitHub Pages on merge to main (.github/workflows/docs.yml). No manual publish step.

## AC Table
| AC | Title | File(s) | Risk | Depends on | Status |
|----|-------|---------|------|------------|--------|
| 1 | Rigorous R data-wrangling Exercise 3 (no .checks; print-contract grading) | demo-book/r-exercises.qmd + rodney-probes/demo-book-bootstrap.js (R clause) | medium | none | spec |
| 2 | Rigorous Python data-wrangling Exercise 3 (pandas, 5 .checks) | demo-book/python-exercises.qmd + rodney-probes/demo-book-bootstrap.js (py clause) + scripts/tests/test_python_exercise3.py + ci.yml | medium | none | spec |

## Dependency DAG
```
AC-1 (R)     — independent (shares canonical CSV contract with AC-2)
AC-2 (Python) — independent (consumes canonical CSV identically)
```
No edges. Shared file rodney-probes/demo-book-bootstrap.js but DISJOINT clauses (AC-1: R-page clause L197-218; AC-2: python clause L222-243) — coordinate in same batch, no conflict.

## Batch Schedule
Batch 1 (parallel): AC-1 + AC-2 — two worktrees, both builders dispatched together.

## Canonical Dataset (binding for both ACs)
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

## Open Questions
- None blocking. (Publish path resolved; all spec disagreements resolved.)

## Per-AC Specs
### AC-1 — (full spec from AC-1.md, verbatim after the frontmatter — copy the entire "AC-1 Final Spec (resolved)" section)

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
- [ ] Spec resolved (2026-08-07) — canonical CSV pinned, rodney migration mandated

### Decision Log
- 2026-08-07 — Dataset: speculator-b's portable CSV wins over speculator-a's data.frame and Python dict literal: one CSV string works verbatim in read.csv(text=) AND pd.read_csv(io.StringIO()); cross-language comparability is the feature's whole point.
- 2026-08-07 — Rodney bootstrap.js migration mandated (hidden blast radius found by speculator-b: === 2 hardcoded → CI red if not migrated).
- 2026-08-07 — Prompt prose MUST be Para/Plain (blendtutor.lua:418 drops BulletList/Header from LLM prompt).

### Surprises & Discoveries
- webR adapter ignores .checks blocks entirely (webr-adapter.js:194) — R exercises cannot be assertion-graded in-browser.
- blendtutor.lua:418 collects ONLY Para/Plain before first CodeBlock — markdown lists in prompt prose silently vanish from the LLM task.
- rodney-probes/demo-book-bootstrap.js hardcodes exercise counts (=== 2) — adding an exercise breaks CI without any qmd change.

### Idempotence & Recovery
- Safe retry: re-run quarto render + structural greps + Rscript solution check; bootstrap.js local run. All deterministic.
- Rollback: revert the qmd append + the bootstrap.js 2→3 edits (single commit each).

### AC-2 — (full spec from AC-2.md, verbatim after the frontmatter — copy the entire "AC-2 Final Spec (resolved)" section)

## AC-2 Final Spec (resolved): Rigorous pandas data-wrangling Exercise 3 — demo-book/python-exercises.qmd

Append ONE rigorous multi-step pure-data-wrangling Python exercise (Exercise 3) to demo-book/python-exercises.qmd. Leave Exercises 1-2 byte-identical + YAML frontmatter (L1-3) untouched. Div attr packages="pandas". Python HAS real .checks grading (Pyodide) — 5 .checks blocks assert the wrangling outcome.

### Executable Spec
- **predicate:** (a) demo-book/python-exercises.qmd contains exactly 3 {.blendtutor language="python"} divs; Exercises 1–2 byte-identical; YAML frontmatter L1–3 untouched; Exercise 3 div carries packages="pandas". (b) Exercise 3 prompt prose is Para/Plain-only (no line before the first code fence matching ^\s*[-*+]\s, ^#{1,6}\s, or ^>\s) AND names the required techniques (pd.to_numeric, str.strip/str.lower, alias map elec→electronics, dropna, drop_duplicates, groupby) AND mandates output names clean_df + revenue_by_cat. (c) Extracting Exercise 3's {.python .solution} + all 5 {.python .checks} blocks and exec'ing solution then checks sequentially in ONE namespace (mirroring pyodide-adapter.js:252–266 semantics: code first, checks in order, first exception = fail) with pandas → ALL checks pass. (d) Second exec runs solution only and asserts revenue_by_cat.round(2).to_dict() is byte-equal to the literal embedded in check 4 (transcription drift caught both directions; literal GENERATED from solution output, cross-checked against hand-verified {books: 96.0, clothing: 190.0, electronics: 172.5}). (e) Third exec runs a hardcoded sneaky-pass (3-row from-scratch clean_df + hardcoded revenue_by_cat Series with the correct dict) → at least one check FAILS. (f) rodney-probes/demo-book-bootstrap.js python clause asserts === 3 for __btExercises.length, .cm-editor count, and payload-script count (R clause untouched — AC-1's half). (g) Pyodide 0.27.2 parity: rendered Exercise 3 — Check on scaffold → no Python error; Check on solution → green.

- **probe:**
  - PRIMARY (CPython): uv run python scripts/tests/test_python_exercise3.py — extracts Exercise 3 section via regex; asserts (b) prose Para/Plain + technique names; execs solution+5 checks (c); execs solution-only dict-parity vs check-4 literal (d); execs hardcode sneaky-pass expecting failure (e); asserts sum(revenue)==458.5 self-consistency.
  - STRUCTURAL: quarto render demo-book && uv run node rodney-probes/demo-book-bootstrap.js → python-clause asserts must read === 3.
  - PYODIDE PARITY (secondary, rodney/manual on rendered page): python -m http.server 8087 & ; uvx --from rodney==0.4.0 rodney start; rodney open http://localhost:8087/demo-book/_output/python-exercises.html; click Check on Exercise 3 scaffold → no Python error; Solution → Check → green pass.
- **negative:** (1) prose as BulletList/Header/BlockQuote → blendtutor.lua:418 silently drops it → LLM gets empty/partial task (probe (b) fails). (2) alias-map skip leaves elec in categories → check 3 fails. (3) fillna/impute keeps NA rows contributing revenue → check 1 or check 4 fails. (4) uncoerced "unknown"/empty price → object dtype → price*quantity TypeError. (5) hardcoded revenue_by_cat leaving clean_df raw → checks 1/2 fail. (6) tiny 3-row from-scratch clean_df → check 5 (len==17) fails. (7) builder hand-transcribes expected revenue wrong → dict-parity probe (d) fails. (8) dedup-before-normalize → ELECTRONICS /electronics variants survive as distinct rows → check 3 or len check fails.
- **verification:** code · CPython probe primary (scripts/tests/test_python_exercise3.py, stdlib+pandas, no browser) + rodney/manual · Pyodide 0.27.2 parity secondary (rendered demo-book, Check scaffold→solution)
- **fixture status:** demo-book/python-exercises.qmd:1-53 (append-only; Exercises 1–2 + frontmatter untouched) + NEW scripts/tests/test_python_exercise3.py + NEW CI step in .github/workflows/ci.yml (one-liner, precedent: python3 scripts/tests/test_quarto_feedback.py ci.yml:82)
- **rubric anchor:** §2 (solution+checks pure pandas core; Pyodide effectful shell verified separately), §3 (exercise div is the unit; checks are the contract), §5 (each check ONE assertion with message)

### Canonical Dataset (binding — IDENTICAL CSV AC-1's R consumes via read.csv(text=...); Python via pd.read_csv(io.StringIO(CSV)))
24 rows, 5 cols (order_id,product,category,price,quantity). Dupes: 3 exact full-row (order_id 1/2/3 shared). NA: rows with unknown price (2), empty quantity (2). 17 rows kept. 3 canonical categories: books, clothing, electronics; alias map {elec: electronics} only. **Expected revenue (hand-verified IEEE754): books 96.0, clothing 190.0, electronics 172.5; total 458.5.** Mess variants test distinct steps: $12.50 standard, 15.00 bare, $ 5.00 (space after $ → [$ ] strip), unknown→NaN; category case+whitespace variants; Elec alias; empty quantity→NA. (Full CSV verbatim in AC-1.md + plan; builder copies byte-identical.)

### The 5 Checks (exact text — order cheapest-diagnostic-first; adapter surfaces only FIRST failure, so each carries an f-string message)
```python
# check 1 — no NA
assert clean_df['price'].notna().all() and clean_df['quantity'].notna().all(), \
    f"NA rows remain in clean_df: {clean_df[clean_df[['price','quantity']].isna().any(axis=1)].index.tolist()}"

# check 2 — no dupes
assert clean_df.duplicated().sum() == 0, \
    f"{int(clean_df.duplicated().sum())} duplicate rows remain in clean_df"

# check 3 — canonical categories
assert set(clean_df['category'].unique()) <= {'books', 'clothing', 'electronics'}, \
    f"non-canonical categories: {sorted(set(clean_df['category'].unique()) - {'books','clothing','electronics'})}"

# check 4 — exact revenue (literal GENERATED from solution run, cross-checked vs hand-verified)
assert revenue_by_cat.round(2).to_dict() == {'books': 96.0, 'clothing': 190.0, 'electronics': 172.5}, \
    f"revenue mismatch: got {revenue_by_cat.round(2).to_dict()}"

# check 5 — row count (defeats tiny-hardcode sneaky-pass)
assert len(clean_df) == 17, f"expected 17 clean rows, got {len(clean_df)}"
```

### Solution shape (reference — complete runnable code)
```python
import pandas as pd
import io

csv = """order_id,product,category,price,quantity
<canonical CSV verbatim>"""
sales = pd.read_csv(io.StringIO(csv))

# Step 1: inspect
print(sales.head())
print(sales.dtypes)

# Step 2: clean price
sales['price'] = pd.to_numeric(sales['price'].astype(str).str.replace(r'[$ ]', '', regex=True), errors='coerce')

# Step 3: normalize category
sales['category'] = sales['category'].str.strip().str.lower()
alias = {'elec': 'electronics'}
sales['category'] = sales['category'].map(lambda c: alias.get(c, c))

# Step 4: drop NA rows (drop, NOT impute)
clean_df = sales.dropna(subset=['price', 'quantity'])

# Step 5: drop exact duplicates
clean_df = clean_df.drop_duplicates()

# Step 6: aggregate revenue per category
clean_df['revenue'] = clean_df['price'] * clean_df['quantity']
revenue_by_cat = clean_df.groupby('category')['revenue'].sum()
print(revenue_by_cat)
```

### code_template / prose / hints
- code_template = import pandas/io + CSV string + pd.read_csv(io.StringIO(...)) + Step 1 print given, blanks ___ at steps 2–6. Final vars MUST be named clean_df + revenue_by_cat.
- Prompt prose = plain paragraphs (Para/Plain ONLY — no lists/headers/blockquotes; blendtutor.lua:418 drops them from LLM prompt) enumerating 6 steps inline, naming required techniques (pd.to_numeric, str.strip/str.lower, alias map elec→electronics, dropna, drop_duplicates, groupby), "drop NA (do NOT impute)", "drop exact duplicates", expected STRUCTURE only ("exactly 3 normalized categories") NOT numeric values, print contract, mandated output names clean_df + revenue_by_cat.
- Hints div (markdown lists OK): pd.to_numeric(errors='coerce') turns non-numeric into NaN; str.strip().str.lower(); alias map {'elec':'electronics'}; dropna(subset=['price','quantity']); drop_duplicates(); groupby('category')['revenue'].sum().
- Pipeline ORDER load-bearing: normalize BEFORE dedup (else ELECTRONICS /electronics variants survive as distinct rows).

### Design Intent
- §1: mandated output names = type-level contract; len(clean_df)==17 encodes row-arithmetic invariant (24 − 4 NA − 3 dupes = 17, NA/dupe disjoint).
- §2: solution+checks pure pandas (CPython probe, no browser); Pyodide effectful shell verified separately — CPython-green/Pyodide-red divergence is a REAL failure mode (pandas WASM build 0.27.2).
- §3: exercise div is the unit; .checks blocks are the grading contract; prose is the ONLY spec visible to the LLM (solution/hints not sent).
- §4: prose MUST self-document required techniques (LLM can't see checks' intent, only source); probe file owns regression guarding — qmd owns content.
- §5: each check = ONE assertion, ONE property, with message; len check separate.

### Technical Context
- Files touched: demo-book/python-exercises.qmd (append after L53); rodney-probes/demo-book-bootstrap.js (python clause ONLY, L222–243: two ===2→===3 + NEW payload-script assert ===3); NEW scripts/tests/test_python_exercise3.py; .github/workflows/ci.yml (one new step; must install pandas — check ci.yml env setup for existing python3 steps); demo-book/_output/ regenerated via quarto render demo-book (not committed if gitignored — check).
- Gotchas: blendtutor.lua:418 Para/Plain-only prompt extraction; pyodide-adapter.js:252–266 sequential-check semantics (first exception surfaces only → check order load-bearing); exercise-feedback.js:122–140 sends checks SOURCE to LLM (answer-key leak acknowledged, mitigated by prose technique-naming); drop_duplicates() default = all columns; pd.read_csv(io.StringIO(...)) — empty quantity parses as NaN natively; empty-string quantity → NaN verify dropna catches; $ 5.00 (space after $) needs r'[$ ]' regex (plain '\$' leaves ' 5.00' — to_numeric handles leading space? verify in probe; literal is GENERATED so discrepancy surfaces immediately).
- LLM answer-key leak: check-4 literal visible in prompt → sophisticated hardcode (exact dict + valid 17-row clean_df) passes ALL checks; LLM sole remaining defense. Residual risk ACCEPTED for learning exercise; prose technique-naming is the mitigation.
- CPython pandas ≠ Pyodide 0.27.2 WASM build — mandatory rodney/manual smoke before done.

### Dependencies
- Depends on: none (canonical CSV contract inherited from AC-1 resolution — consume identically). Blocks: none.
- Conflict set: demo-book/python-exercises.qmd, rodney-probes/demo-book-bootstrap.js (python clause only — disjoint from AC-1's R clause), .github/workflows/ci.yml (additive).

### Progress
- [ ] Spec resolved (2026-08-07) — canonical CSV inherited, 5 checks pinned, probe file + CI step mandated

### Decision Log
- 2026-08-07 — Dataset: canonical portable CSV wins (AC-1 resolver) — identical string in pd.read_csv(io.StringIO()) and R read.csv(text=). Rejects speculator-a's 27-row dict literal (unportable).
- 2026-08-07 — Committed probe file scripts/tests/test_python_exercise3.py wins over ad-hoc verification: repo precedent scripts/tests/*.py + one-line CI step (ci.yml:82) = zero config friction; durable regression guard.
- 2026-08-07 — B's adversarial structure adopted: 5 checks (adds len==17 hardcode-defeat), f-string messages (adapter surfaces only first failure), generated-not-transcribed expected values, Pyodide parity verification.
- 2026-08-07 — Expected values GENERATED from solution run by builder, cross-checked vs hand-verified 96.0/190.0/172.5 — never hand-transcribed.

### Surprises & Discoveries
- pd.read_csv(io.StringIO(...)) parses empty quantity fields as NaN natively — dropna catches them.
- pyodide-adapter.js surfaces only the FIRST check failure — check ordering and f-string messages are load-bearing for debug UX.
- The check-4 expected-value literal is visible to the LLM (exercise-feedback.js sends checks SOURCE) — answer-key leak is structural, mitigated by prose technique-naming.

### Idempotence & Recovery
- Safe retry: re-run uv run python scripts/tests/test_python_exercise3.py + quarto render + bootstrap.js local. All deterministic.
- Rollback: revert the qmd append + bootstrap.js python-clause edits + (if committed) probe file + CI step.