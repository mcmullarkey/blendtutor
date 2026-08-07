#!/usr/bin/env python3
r"""Executable spec for issue #189 — AC-2 rigorous pandas data-wrangling exercise.

Verifies the AC-2 predicate against demo-book/python-exercises.qmd:
  (a) exactly 3 {.blendtutor language="python"} divs; Exercises 1-2
      byte-identical; YAML frontmatter untouched; Exercise 3 div carries
      packages="pandas".
  (b) Exercise 3 prompt prose is Para/Plain-only (no line before the first
      code fence matching ^\s*[-*+]\s, ^#{1,6}\s, or ^>\s) AND names the
      required techniques (pd.to_numeric, str.strip/str.lower, alias map
      elec→electronics, dropna, drop_duplicates, groupby) AND mandates output
      names clean_df + revenue_by_cat AND gives expected STRUCTURE only
      (3 canonical categories, never numeric values).
  (c) Extracting Exercise 3's {.python .solution} + all 5 {.python .checks}
      blocks and exec'ing solution then checks sequentially in ONE namespace
      (mirroring pyodide-adapter.js:252-266 semantics: code first, checks in
      order, first exception = fail) with pandas → ALL checks pass.
  (d) Second exec runs solution only and asserts
      revenue_by_cat.round(2).to_dict() equals the literal embedded in check 4
      (transcription drift caught both directions; the literal was GENERATED
      from the solution run, cross-checked against hand-verified
      {books: 96.0, clothing: 190.0, electronics: 172.5}).
  (e) Third exec runs a hardcoded sneaky-pass (3-row from-scratch clean_df +
      hardcoded revenue_by_cat Series with the correct dict) → at least one
      check FAILS.
  (f) sum(revenue_by_cat) == 458.5 self-consistency.

Negative: prose as list/header/blockquote (empty LLM task); alias-miss leaves
elec; fillna impute; uncoerced unknown/empty price → TypeError; hardcoded
revenue_by_cat with raw clean_df; tiny from-scratch clean_df; wrong
hand-transcribed expected revenue; dedup-before-normalize.

Usage: uv run --with pandas python3 scripts/tests/test_python_exercise3.py
"""

from __future__ import annotations

import ast
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
QMD_PATH = REPO_ROOT / "demo-book" / "python-exercises.qmd"

PASS = 0
FAIL = 0


def ok(msg: str) -> None:
    global PASS
    PASS += 1
    print(f"  PASS: {msg}")


def ko(msg: str) -> None:
    global FAIL
    FAIL += 1
    print(f"  FAIL: {msg}")


# ---------------------------------------------------------------------------
# Embedded fixtures
# ---------------------------------------------------------------------------

# Exercises 1-2 + frontmatter must stay BYTE-IDENTICAL (append-only file).
# Pinned here so the probe fails the moment anyone rewrites the existing
# exercises or the YAML header.
PREFIX_PIN = """---
title: Python Exercises
bt-key-page: api-key.html
---

# Python Exercises

This chapter demonstrates Python exercises using Pyodide (Python in the
browser). No COI is needed for Pyodide-only pages — it boots on the main
thread.

## Exercise 1: Square function

Write a function `square(n)` that returns the square of a number.

::: {.blendtutor language="python"}
Write a function `square(n)` that returns `n * n`.

```python
def square(n):
    ___
```

```{.python .checks}
assert square(3) == 9
```

```{.python .solution}
return n * n
```
:::

## Exercise 2: Even check

Write a function `is_even(n)` that returns `True` if `n` is even.

::: {.blendtutor language="python"}
Write a function `is_even(n)` that returns `True` if `n` is even.

```python
def is_even(n):
    ___
```

```{.python .checks}
assert is_even(4) == True
assert is_even(7) == False
```

```{.python .solution}
return n % 2 == 0
```
:::
"""

# The canonical 24-row messy sales CSV — byte-identical to AC-1's R exercise.
CANONICAL_CSV = """order_id,product,category,price,quantity
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
21,Hat,clothing,$10.00,2"""

# Hand-verified expected revenue (IEEE754): books 96, clothing 190,
# electronics 172.5 — total 458.5; 24 − 4 NA − 3 dupes = 17 rows kept.
EXPECTED_REVENUE = {"books": 96.0, "clothing": 190.0, "electronics": 172.5}

EX3_DIV = '::: {.blendtutor language="python" packages="pandas"}'
HEADING_RE = re.compile(r"^## Exercise 3:.*$", re.MULTILINE)
CODE_TEMPLATE_RE = re.compile(r"^```python\n(.*?)^```", re.MULTILINE | re.DOTALL)
CHECKS_RE = re.compile(
    r"^```\{\.python \.checks\}\n(.*?)^```", re.MULTILINE | re.DOTALL
)
SOLUTION_RE = re.compile(
    r"^```\{\.python \.solution\}\n(.*?)^```", re.MULTILINE | re.DOTALL
)
# Para/Plain negatives — any of these at line start means the lua filter
# (blendtutor.lua:418) drops the block from the LLM prompt.
BLOCK_START_RE = re.compile(r"^(\s*([-*+])\s|#{1,6}\s|>\s|\d+\.\s)")


def check_structure(qmd: str) -> None:
    """Predicate (a): exactly 3 python blendtutor divs; Ex1-2 byte-identical;
    Exercise 3 div carries packages='pandas'."""
    python_divs = re.findall(r'::: \{\.blendtutor language="python"[^}]*\}', qmd)
    if len(python_divs) == 3:
        ok(f"exactly 3 blendtutor language='python' divs (found {len(python_divs)})")
    else:
        ko(f"expected 3 blendtutor language='python' divs, found {len(python_divs)}")

    if qmd.startswith(PREFIX_PIN):
        ok("Exercises 1-2 + YAML frontmatter byte-identical (append-only)")
    else:
        ko("Exercises 1-2 or YAML frontmatter altered — file must be append-only")

    if EX3_DIV in qmd:
        ok('Exercise 3 div carries packages="pandas"')
    else:
        ko('Exercise 3 div missing packages="pandas"')


def extract_ex3(qmd: str) -> str:
    """Return the Exercise 3 section (from its heading to EOF)."""
    m = HEADING_RE.search(qmd)
    if not m:
        return ""
    return qmd[m.start() :]


def check_section_shape(section: str) -> None:
    """Predicate (a)+(c) shape: heading, 5 checks, 1 solution, 1 hints div,
    code_template embedding the canonical CSV."""
    if HEADING_RE.search(section):
        ok("Exercise 3 section heading present")
    else:
        ko("Exercise 3 heading missing")

    checks = CHECKS_RE.findall(section)
    if len(checks) == 5:
        ok("exactly 5 .checks blocks")
    else:
        ko(f"expected 5 .checks blocks, found {len(checks)}")

    solutions = SOLUTION_RE.findall(section)
    if len(solutions) == 1:
        ok("exactly 1 .solution block")
    else:
        ko(f"expected 1 .solution block, found {len(solutions)}")

    if "::: {.hints}" in section:
        ok("Exercise 3 has a {.hints} div")
    else:
        ko("Exercise 3 missing {.hints} div")

    templates = CODE_TEMPLATE_RE.findall(section)
    if len(templates) == 1:
        ok("exactly 1 plain ```python code_template block")
    else:
        ko(f"expected 1 plain code_template block, found {len(templates)}")
        return

    template = templates[0]
    if "pd.read_csv(io.StringIO(csv))" in template:
        ok("code_template reads the CSV via pd.read_csv(io.StringIO(csv))")
    else:
        ko("code_template missing pd.read_csv(io.StringIO(csv))")
    if CANONICAL_CSV in template:
        ok("code_template embeds the canonical 24-row CSV byte-identically")
    else:
        ko("code_template does not embed the canonical CSV byte-identically")
    if CANONICAL_CSV in solutions[0]:
        ok("solution embeds the canonical 24-row CSV byte-identically")
    else:
        ko("solution does not embed the canonical CSV byte-identically")


def check_prose(section: str) -> None:
    """Predicate (b): prose is Para/Plain-only, names the required techniques
    and output names, and gives structure-only expectations (never values)."""
    div_idx = section.find(EX3_DIV)
    fence_idx = section.find("```python", div_idx)
    if div_idx == -1 or fence_idx == -1 or fence_idx < div_idx:
        ko("prose region (div opener → first code fence) not found")
        return
    prose = section[div_idx:fence_idx]
    prose = prose.split("\n", 1)[1] if "\n" in prose else prose

    bad_lines = [
        line
        for line in prose.split("\n")
        if line.strip() and BLOCK_START_RE.match(line)
    ]
    if not bad_lines:
        ok("prose is Para/Plain-only (no list/header/blockquote line starts)")
    else:
        ko(
            f"prose contains block-start lines (silently dropped from LLM prompt): {bad_lines[:3]}"
        )

    required_tokens = [
        "pd.to_numeric",
        "str.strip",
        "str.lower",
        "elec",
        "alias",
        "dropna",
        "drop_duplicates",
        "groupby",
        "clean_df",
        "revenue_by_cat",
        "impute",
        "duplicates",
    ]
    missing = [t for t in required_tokens if t.lower() not in prose.lower()]
    if not missing:
        ok("prose names all required techniques + output names")
    else:
        ko(f"prose missing required technique/output names: {missing}")

    leaked = [v for v in ("96", "190", "172.5", "458.5") if v in prose]
    if not leaked:
        ok("prose gives expected STRUCTURE only (no numeric revenue values leaked)")
    else:
        ko(f"prose leaks numeric expected values (answer key): {leaked}")

    if (
        "books" in prose.lower()
        and "clothing" in prose.lower()
        and "electronics" in prose.lower()
    ):
        ok("prose names the 3 canonical categories")
    else:
        ko("prose does not name the 3 canonical categories")


def check_checks(checks: list[str]) -> None:
    """Each check must be ONE assertion with an f-string message — the
    pyodide adapter surfaces only the first failure (pyodide-adapter.js:252-266),
    so messages are the learner's only diagnostic."""
    for i, check in enumerate(checks, start=1):
        if check.count("assert") == 1 and ('f"' in check or "f'" in check):
            ok(f"check {i}: single assertion with f-string message")
        else:
            ko(f"check {i}: must be one assertion with an f-string message")


def check_4_literal(checks: list[str]) -> dict:
    """Extract the expected-revenue dict literal from check 4."""
    m = re.search(r"==\s*(\{.*?\})", checks[3], re.DOTALL)
    if not m:
        ko("check 4: expected-revenue dict literal not found")
        return {}
    try:
        literal = ast.literal_eval(m.group(1))
    except (ValueError, SyntaxError):
        ko("check 4: expected-revenue dict literal is not a valid Python literal")
        return {}
    if isinstance(literal, dict):
        ok("check 4: expected-revenue dict literal extracted")
        return literal
    ko("check 4: expected-revenue literal is not a dict")
    return {}


def exec_solution_and_checks(solution: str, checks: list[str]) -> None:
    """Predicate (c): exec solution then checks sequentially in ONE namespace
    (mirroring pyodide-adapter.js semantics: code first, checks in order,
    first exception = fail)."""
    ns: dict = {}
    try:
        exec(compile(solution, "<exercise3-solution>", "exec"), ns)  # noqa: S102
        for i, check in enumerate(checks, start=1):
            exec(compile(check, f"<exercise3-check-{i}>", "exec"), ns)  # noqa: S102
    except AssertionError as err:
        ko(f"solution + checks: assertion failed — {err}")
        return
    except Exception as err:  # noqa: BLE001 — probe surfaces any exec failure
        ko(f"solution + checks: exec raised {type(err).__name__}: {err}")
        return
    if "clean_df" in ns and "revenue_by_cat" in ns:
        ok("solution defines clean_df and revenue_by_cat")
    else:
        ko("solution must define clean_df and revenue_by_cat")
    ok("solution + all 5 checks exec to green in one namespace")


def exec_solution_dict_parity(solution: str, literal: dict) -> None:
    """Predicate (d): solution-only exec must reproduce check 4's literal —
    transcription drift caught in both directions."""
    ns: dict = {}
    try:
        exec(compile(solution, "<exercise3-solution>", "exec"), ns)  # noqa: S102
    except Exception as err:  # noqa: BLE001
        ko(f"solution-only exec raised {type(err).__name__}: {err}")
        return
    actual = ns["revenue_by_cat"].round(2).to_dict()
    if actual == literal:
        ok(f"dict-parity: solution output {actual} == check-4 literal")
    else:
        ko(
            f"dict-parity: solution output {actual} != check-4 literal {literal} — hand-transcription drift"
        )
    if abs(sum(actual.values()) - 458.5) < 1e-9:
        ok("sum(revenue_by_cat) == 458.5 (self-consistency)")
    else:
        ko(f"sum(revenue_by_cat) == {sum(actual.values())}, expected 458.5")


SNEAKY_PASS = """import pandas as pd
clean_df = pd.DataFrame({
    "order_id": [1, 2, 3],
    "product": ["A", "B", "C"],
    "category": ["books", "clothing", "electronics"],
    "price": [10.0, 20.0, 15.0],
    "quantity": [1, 1, 1],
    "revenue": [96.0, 190.0, 172.5],
})
revenue_by_cat = pd.Series({"books": 96.0, "clothing": 190.0, "electronics": 172.5})
"""


def check_sneaky_pass_defeated(checks: list[str]) -> None:
    """Predicate (e): a hardcoded 3-row clean_df + correct revenue dict must
    trip at least one check (len(clean_df)==17 is the hardcode-defeat)."""
    ns: dict = {}
    try:
        exec(compile(SNEAKY_PASS, "<sneaky-pass>", "exec"), ns)  # noqa: S102
    except Exception as err:  # noqa: BLE001
        ko(f"sneaky-pass setup raised {type(err).__name__}: {err}")
        return
    failures = 0
    failed_idx = []
    for i, check in enumerate(checks, start=1):
        try:
            exec(compile(check, f"<sneaky-check-{i}>", "exec"), ns)  # noqa: S102
        except AssertionError:
            failures += 1
            failed_idx.append(i)
    if failures >= 1:
        ok(
            f"hardcoded sneaky-pass defeated ({len(checks) - failures} passed, checks {failed_idx} failed)"
        )
    else:
        ko("hardcoded sneaky-pass passed ALL checks — no hardcode-defeat guard")


def main() -> int:
    print(
        "=== AC-2 Rigorous Python data-wrangling exercise — test_python_exercise3.py ===\n"
    )

    qmd = QMD_PATH.read_text()

    print("-- Predicate (a): structure --")
    check_structure(qmd)

    section = extract_ex3(qmd)
    if not section:
        ko("Exercise 3 section not found in python-exercises.qmd")
        print(f"\n=== Results: {PASS} passed, {FAIL} failed ===")
        return 1 if FAIL > 0 else 0

    print("\n-- Section shape --")
    check_section_shape(section)

    checks = CHECKS_RE.findall(section)
    solutions = SOLUTION_RE.findall(section)

    print("\n-- Predicate (b): prose contract --")
    check_prose(section)

    print("\n-- Check discipline --")
    check_checks(checks)

    print("\n-- Predicate (d): check-4 literal extraction --")
    literal = check_4_literal(checks)

    print("\n-- Predicate (c): solution + checks exec --")
    if solutions:
        exec_solution_and_checks(solutions[0], checks)
    else:
        ko("no solution block to exec")

    print("\n-- Predicate (d)+(f): dict-parity + sum --")
    if solutions and literal:
        exec_solution_dict_parity(solutions[0], literal)

    print("\n-- Predicate (e): hardcode sneaky-pass --")
    check_sneaky_pass_defeated(checks)

    print(f"\n=== Results: {PASS} passed, {FAIL} failed ===")
    return 1 if FAIL > 0 else 0


if __name__ == "__main__":
    sys.exit(main())
