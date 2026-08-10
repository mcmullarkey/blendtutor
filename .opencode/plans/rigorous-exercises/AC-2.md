---
ac: 2
depends_on: none (parallel with AC-1; consumes canonical CSV dataset contract identically)
risk: medium
status: complete
---

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
- [x] Spec resolved (2026-08-07) — canonical CSV inherited, 5 checks pinned, probe file + CI step mandated
- [x] Implemented (2026-08-07) — Exercise 3 appended to python-exercises.qmd (append-only, Ex1-2 byte-identical); probe 26/26 green; quarto render demo-book clean; bootstrap.js python clause 2→3 + payload assert; CI step added

### Decision Log
- 2026-08-07 — Dataset: canonical portable CSV wins (AC-1 resolver) — identical string in pd.read_csv(io.StringIO()) and R read.csv(text=). Rejects speculator-a's 27-row dict literal (unportable).
- 2026-08-07 — Committed probe file scripts/tests/test_python_exercise3.py wins over ad-hoc verification: repo precedent scripts/tests/*.py + one-line CI step (ci.yml:82) = zero config friction; durable regression guard.
- 2026-08-07 — B's adversarial structure adopted: 5 checks (adds len==17 hardcode-defeat), f-string messages (adapter surfaces only first failure), generated-not-transcribed expected values, Pyodide parity verification.
- 2026-08-07 — Expected values GENERATED from solution run by builder, cross-checked vs hand-verified 96.0/190.0/172.5 — never hand-transcribed.
- 2026-08-07 (builder) — CI pandas: quarto-render job's python3 steps install nothing; PEP 668 blocks bare pip on ubuntu-latest → added astral-sh/setup-uv@v5 + `uv run --with pandas python3 scripts/tests/test_python_exercise3.py` (mirrors rodney-probes job pattern).
- 2026-08-07 (builder) — Check/solution/code_template source kept ASCII-only: blendtutor.lua json_escape's `[%c]` gsub matches bytes 0x80-0x9F in non-C locale, mangling multi-byte UTF-8 in RAW code-block JSON (checks/solution). Prose em-dashes survive (pandoc entity-escapes rendered HTML), but `errors='coerce'` got smart-quoted → curly quotes → same mojibake; backticked code terms so pandoc leaves ASCII quotes inside code spans.

### Surprises & Discoveries
- pd.read_csv(io.StringIO(...)) parses empty quantity fields as NaN natively — dropna catches them.
- pyodide-adapter.js surfaces only the FIRST check failure — check ordering and f-string messages are load-bearing for debug UX.
- The check-4 expected-value literal is visible to the LLM (exercise-feedback.js sends checks SOURCE) — answer-key leak is structural, mitigated by prose technique-naming.
- blendtutor.lua json_escape mangles non-ASCII in RAW code-block text (checks/solution/code_template): `s:gsub("[%c]", ...)` matches bytes 0x80-0x9F when Lua runs in a non-C locale, so a UTF-8 em-dash (E2 80 94) in a check comment rendered as U+FFFD + literal `\u0080\u0094`. Prose/hints survive because pandoc HTML-entity-escapes them before json_string. Rule: keep check/solution/code_template source pure ASCII (AC-1 R exercise has the same risk if it puts non-ASCII in code).
- Pandoc smart typography converts `'coerce'` → curly quotes inside prose → also mojibake'd by the same json_escape path. Fix: backtick code terms (`errors='coerce'`) so pandoc leaves them as ASCII inside <code> spans. Verified: zero non-ASCII in all payload fields after fix.

### Idempotence & Recovery
- Safe retry: re-run uv run python scripts/tests/test_python_exercise3.py + quarto render + bootstrap.js local. All deterministic.
- Rollback: revert the qmd append + bootstrap.js python-clause edits + (if committed) probe file + CI step.