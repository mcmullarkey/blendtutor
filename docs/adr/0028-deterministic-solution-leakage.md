# ADR-0028: Deterministic solution-leakage measurement in `eval`

- Status: Accepted
- Date: 2026-11-08

## Context

The brainstorm's construct map lists **solution leakage** as a construct
separate from verdict validity: feedback should help the learner fix the code
without handing over a working answer. Dogfooding lesson 03 (31 items) showed
why it matters. The grader was 93.5% accurate, and a regex for the lesson's
answer (`6 - x`) found a working solution in 24 of 25 incorrect-case messages.
A regex only works for one lesson, so the measure needs a general operational
definition. It should also be deterministic, so it needs no judge and no
judge validation.

## Options

1. **"Contains a code block."** Cheap, but it counts hints with illustrative
   code, and wrong code, as leaks.
2. **An LLM judge ("does this message give away the answer?").** General, but
   it is a new instrument that needs human-label validation (Approach C) before
   its numbers count.
3. **Execution:** feedback leaks when one of its fenced code blocks, appended
   to the learner's submission, makes the lesson's checks pass when the
   submission alone did not.

## Decision

Option 3. Appending to the submission keeps the learner's own setup (lesson 03
rebuilds `survey_data`) and asks the question that matters: if the learner
pasted the grader's code in, would they be done?

A case is **assessable** only when it is labeled incorrect, the lesson has
checks, and the submission alone fails a check (`Fail`). Otherwise there is
nothing for the code to fix, or no way to tell. Its outcome is `Leaked` or
`Withheld`. Withheld covers both "no code" and "code that doesn't fix it."
Extracting fenced blocks (any language tag) is pure. Running each
`submission + "\n" + block` through `run_checks` happens in `run_eval`'s
effectful loop. On the wire, a per-case `"leakage"` appears only when the case
is assessable, and a report-level `leakage: {leaked, assessable, incorrect,
interval}` appears only when at least one case is.

## Consequences

- Leakage is measured for free, in any language, with no judge.
- **Selection.** Items the checks cannot distinguish (submission already
  passes) and crashing items (`NotRun`) are excluded. The rate is unbiased only
  if the grader leaks equally on those, which is doubtful, since the excluded
  ones are the subtle conceptual mistakes. The report therefore always shows
  assessable *n* next to the number of incorrect cases. Stronger checks fix
  coverage (ADR-0024) and leakage assessability at once.
- Each fenced block costs one extra check run per assessable case.
- A partial hint that fixes one of several failing checks is not a leak. This
  measures *complete* fixes only.
