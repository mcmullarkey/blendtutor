# ADR-0024: Misconception blueprint, a `CaseLabel` that projects to polarity, and check coverage

- Status: Accepted
- Date: 2026-11-08
- Supersedes: the "single label type" consequence of ADR-0007 (polarity stays
  as `ExpectedVerdict`; the case label is now a separate `CaseLabel`)

## Context

An eval suite is the custom benchmark for a lesson, and a benchmark needs a
blueprint: the parts of the construct it is meant to sample. For feedback on
code, those parts are **misconceptions**. Each incorrect item should exhibit
exactly one, so we can ask, per misconception, whether the lesson's checks can
detect it (ADR-0023's audit) and later whether the grader names it.

A misconception tag only makes sense on an `incorrect` item. Where that
invariant lives matters, because `ExpectedVerdict` is shared: it is both the
author's label and the polarity the grader's verdict projects to, and
`score_case` compares the two with `==`.

## Options

1. **`EvalCase { expected, misconception: Option<_> }` plus a parse-time check.**
   Least churn, but "tagged and correct" stays constructible, so every other
   constructor (smevals, tests) has to remember the rule.
2. **Put the tag inside `ExpectedVerdict::Incorrect`.** This makes the state
   unrepresentable, but it breaks polarity. The grader's verdict has no tag, so
   `Incorrect { M2 } != Incorrect { None }`, and every tagged case would silently
   score as a mismatch.
3. **A separate `CaseLabel { Correct, Incorrect { misconception: Option<Id> } }`
   with `polarity() -> ExpectedVerdict`.** The illegal state is unrepresentable,
   and scoring, `classify`, and the audit keep working on polarity unchanged.

## Decision

Option 3. `EvalCase` holds a private `label: CaseLabel` and exposes
`expected()` (the polarity) and `misconception()`. The suite gains an optional
`blueprint.misconceptions` map (`id → label`, in document order). Parsing
refuses a tag whose id is not in the blueprint, and a tag on a `correct` case,
naming the case in both errors. Suites without a blueprint parse unchanged.

The audit adds a coverage row per blueprint misconception, built from
`caught`/`items` counts. An item is **caught** when at least one check `Fail`s.
A `NotRun` item counts toward `items` but not `caught`: a crashing item says
nothing about whether the checks detect the misconception.
`MisconceptionCoverage` is `NoItems | Covered | Partial | Uncovered`, derived
from the counts and never stored independently. Coverage gaps exit 0; only label
violations fail the audit.

## Consequences

- An uncovered misconception means only the LLM grader can catch it. The
  blueprint therefore shows where grader evaluation has to concentrate.
- About a dozen call sites move from the `case.expected` field to the
  `expected()` accessor, and from struct literals to `EvalCase::new`.
- `smevals_gen` still emits only polarity. Carrying tags into the smevals report
  is deferred.
