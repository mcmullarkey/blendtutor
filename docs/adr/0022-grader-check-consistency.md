# ADR-0022: Grader–check consistency — a per-case signal separate from polarity accuracy

- Status: Accepted
- Date: 2026-11-08

## Context

`blendtutor eval` scores one thing per case: whether the LLM grader's verdict
polarity matches the author's `expected` (ADR-0007). That number depends on the
author's labels being right, and with 2–10 hand-written cases it says nothing
about *why* a grader fails.

The pipeline already produces a second, deterministic measure of the same
construct ("does the code meet the spec"): each lesson's `checks`, classified
`Pass` / `Fail` / `NotRun` by `grade::run_checks`. `run_eval` discards them and
keeps only the verdict. This is the first slice of treating feedback as a
measurement instrument (see the construct map below). Checks and verdict are two
indicators, and theory constrains how they may disagree:

- A `Fail` or `NotRun` check is **sufficient** evidence of "incorrect". A
  `correct` verdict alongside one means an instrument is broken: the grader, the
  check, or the author's label.
- All checks passing is **necessary, not sufficient**. Success criteria such as
  "uses `cat()`" are often not encoded as checks, so an `incorrect` verdict here
  is plausible grading, not a contradiction.
- No checks means **no evidence**. That is not agreement.

## Options

1. **Boolean `consistent` flag.** Simple, but it collapses "no checks" into
   either true (false confidence) or false (false alarm), and it puts the
   asymmetric mirror case on the same footing as a real contradiction.
2. **Fold consistency into `matched`/accuracy.** This hides the signal. It also
   cannot catch a *mislabeled item*: the author marks broken code `correct`, the
   grader agrees, `matched` is true, and only the failing checks show the error.
3. **A four-state `Consistency` sum type, derived at scoring time, kept separate
   from `matched`:** `Unchecked | Consistent | Discordant(kind) | Violated(kind)`.

## Decision

Option 3. `eval::Consistency` is computed by a pure function of the verdict
polarity and the check outcomes, and `CaseResult::score` is its only producer.
It is never set independently, the same rule that already holds for `matched`
(§1.1). It reaches the wire as
`"consistency": {"status": "<state>", "kind": "<kind>"}`, where `kind` is
present only for `discordant`/`violated`. That is additive, so the existing keys
of `eval-report.json` keep their meaning.

Severity is encoded in the variant rather than in a flag: `Violated` is a
logical contradiction, and `Discordant` is worth a look but expected at some
rate whenever checks are incomplete.

## Construct map (scope for later slices)

Polarity accuracy measures only *verdict validity*. Later slices measure the
other constructs, each with its own operational definition and evidence:
diagnostic accuracy, grounding in execution output, solution leakage,
actionability, criterion alignment, and tone. They are listed here so each one
lands as its own signal instead of being blended into one "quality" score.

## Consequences

- An eval can flag a broken grader **or a broken eval item** with no judge and no
  human labels.
- An LLM-only lesson reports consistency as not assessable, never as 100%.
- `CaseResult::score` takes the check outcomes, so `run_eval` threads them
  through from `RunReport`.
- The human report adds a `Consistency:` headline and per-row call-outs for
  violations and discordances. The `eval_human` snapshot changes on purpose.
