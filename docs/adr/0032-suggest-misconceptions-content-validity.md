# ADR-0032: `suggest-misconceptions`, a content-validity helper for eval blueprints

- Status: Accepted
- Date: 2026-11-08

## Context

The misconception blueprint (ADR-0024) defines what a lesson's eval suite is
meant to sample. Its content validity is only as good as the author's
imagination: a mistake nobody lists never gets an item, so neither the checks
nor the grader are ever measured on it. Authors need help finding the gaps,
and that help has to be validated itself.

## Options

1. **Leave it to authors and the course skill.** No product change, but no way
   to measure how good the suggestions are.
2. **An LLM judge of blueprint completeness.** Gives a score, but not an
   actionable list.
3. **A suggester that proposes uncovered misconceptions, each with a runnable
   example checked against the lesson's checks,** validated by leave-one-out
   recall on a real blueprint.

## Decision

Option 3. `blendtutor suggest-misconceptions <lesson>` sends the grader model
the task, success criteria, checks, and the blueprint's existing labels, and
asks for up to 5 other misconceptions with minimal example submissions. Each
example runs through the lesson's checks and is reported as `caught`,
`not_caught` (only the grader can catch it), `does_not_run`, or `unchecked`.
Nothing is written to the suite: a person decides what enters the blueprint.
The provider call goes through a shared `extract::<T>` that `request_feedback`
now also uses, so key guards and error mapping cannot drift.

## Validation (lesson 03, deepseek-v4p1-flash-1017, 2026-11-08)

Leave-one-out: for each of the 8 blueprint misconceptions, remove it and its
items, run the suggester once, and judge whether any suggestion names the
hidden one. **Recall was 5/8 (95% Wilson CI 31–86%).** All three
misconceptions the checks cannot see (hard-coding, ignoring the argument,
reversing the vector) were recovered. The misses were mechanical mistakes
(returning the input, a missing final expression, a wrong function name),
consistent with the prompt's instruction to prefer conceptual mistakes. The
runs also repeatedly proposed new, check-invisible gaps (e.g. inferring the
scale maximum from the data, in 5 of 8 runs).

## Consequences

- Authors get a concrete, runnable list of candidate gaps, already sorted by
  whether the checks would see them.
- Recall is measured, but precision (are suggestions real, distinct mistakes?)
  needs human review and is not yet measured.
- One run per held-out item; the recall estimate itself has run-to-run
  variance (ADR-0031). A larger validation would repeat runs and cover more
  lessons.
