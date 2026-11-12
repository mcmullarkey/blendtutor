# ADR-0034: Judge existing feedback from an items file

## Status

Accepted (2026-11-10). Extends ADR-0033.

## Context

ADR-0033's judge runs inside `eval --judge`, which grades each case afresh and
then judges the feedback it just produced. To validate the judge against
people, the judge and the people must see the *same* feedback. A fresh grading
run cannot provide that: the grader is stochastic, so its feedback differs from
the feedback a person already labeled. It also pays for grader calls and R
runs that judging does not need.

The first real run (lesson 03, `feedback_guidance` in place) passed 25/25 on
actionability. With no failing feedback in the set, that result cannot tell a
working judge from one that always says yes. The labeled items include
baseline feedback that pastes the solution, which the construct calls a fail,
so those items are the natural negatives the actionability judgment lacks.

## Options

1. **Re-run `eval --judge` and label its output.** This keeps one command, but
   every validation round needs new labels, and labels never carry over to a
   changed judge prompt, which is what prompt optimization needs.
2. **Seed the grader with recorded feedback.** This replays feedback through
   the eval pipeline by stubbing the grader. The judge sees real data, but it
   ties judging to lesson files and test infrastructure.
3. **A `judge-feedback` command over an items file.** Each item is data: `id`,
   `task`, `submission`, `feedback`, and optionally `misconception` and
   `control` (the mismatched label). No lesson, grader, or runner is involved.

## Decision

Option 3. `blendtutor judge-feedback <items.json> [--samples N] [--judge-model M]`
reads a JSON array of items. It applies `judge_case` exactly as `eval --judge`
does, and reports each item's judgment by `id` with the same `JudgeSummary`.
The control label is supplied by the item, not derived, because an items file
has no blueprint. Whoever exports the items computes it with the same
next-in-blueprint rule. The judge is the provider default model unless
`--judge-model` is given.

The construct definitions are fixed before any agreement is computed. In
particular, "handing over a complete working solution does not count as a next
step" is the same sentence in the judge prompt and in the labeling desk.

## Consequences

- The same labeled items can be re-judged after every prompt change, so
  automated prompt optimization can score candidates against fixed human
  labels without new labeling.
- Item ids carry through to the output, so agreement with human labels is a
  join on `id`. That join is the next slice.
- Items carry their own task text, so one file can mix lessons.
- The item schema is a second way into the judge, so it has to stay in step
  with `judge_case`'s inputs.

## Amendment (2026-11-12): checkpoint and resume

Two real runs over the 50 labeled items died partway: once on a dropped
connection and once when the machine slept. Because the report was written
only at the end, every paid call before the failure was lost. Now
`--checkpoint <file.jsonl>` appends each item's judgment as soon as it is made
(flushed to disk), and a re-run skips items already in the file. Core exposes
`judge_item` and `ItemsReport::of` so the file handling stays in the CLI shell.
The report keeps items-file order whether or not the run resumed.
