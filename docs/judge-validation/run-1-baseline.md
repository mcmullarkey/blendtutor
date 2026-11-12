# Judge validation, run 1: the unoptimized judge against human labels

2026-11-12. Judge prompt as of commit `449f94a` (ADR-0033). Data in
[`write-less-code-r-03/`](write-less-code-r-03/).

## Setup

- **Items:** 50 pieces of real grader feedback on lesson 03 ("Write a
  Function") submissions that the grader called incorrect. 25 came from the
  baseline grading prompt and 25 from the candidate prompt with
  `feedback_guidance`, both from the earlier `eval-compare` run. Every item is
  tagged with its blueprint misconception (M1–M8).
- **Human labels:** one labeler (the author), blind to which prompt wrote the
  feedback and to every judge verdict. Items were shuffled. The labeler saw the
  same construct definitions as the judge.
- **Judge:** Kimi K3.1 (`accounts/fireworks/models/kimi-k3p1-pro`), a
  different model family from the DeepSeek grader. Each criterion is a separate
  call, with the explanation written before the verdict, and each verdict is the
  majority of 3 samples. Command: `blendtutor judge-feedback`, then
  `blendtutor judge-agreement`.
- **Actionability definition, as given to both the judge and the labeler:**
  "Pass only if a learner could act on it right away: it says what to change,
  look at, or try next in this code. Fail if it only says the answer is wrong,
  only encourages, or stays so general it would fit any submission. Handing over
  a complete working solution does not count as a next step."

## Results

| Criterion | Pairs | TPR | TNR |
|---|---|---|---|
| Diagnosis | 50 | 50/50 (93%–100%) | 0/0, not measurable |
| Actionability | 50 | 18/18 (82%–100%) | **9/32 (16%–45%)** |

The intervals are 95% Wilson intervals. The diagnosis control (the same
feedback judged against a mismatched misconception) passed 0/50 (0%–7%). Judge
self-consistency was 140/150 unanimous judgments.

| Grading prompt | Human | Judge | n |
|---|---|---|---|
| baseline | fail | fail | 9 |
| baseline | fail | pass | 16 |
| candidate | fail | pass | 7 |
| candidate | pass | pass | 18 |

## What it means

- **The actionability judge is lenient.** It passed everything the labeler
  passed but caught only 9 of 32 fails. A 25/25 actionability result on an
  all-guided run (`eval --judge`) could not have revealed this, because it had
  no failing items to miss.
- **The disagreement is a construct gap, not noise.** 30 of the 32 fail notes
  say the feedback "leaks the answer instead of helping the learner think
  through the next step." The judge reads "a complete working solution"
  narrowly. On `cmp-b-03` it wrote: "Although it includes the complete one-line
  function, the explicit replacement instruction makes the feedback
  actionable." The labeler's construct is broader: stating the answer
  expression (`6 - x`) or the exact edit is a leak even when the task text
  states the formula. The judge is also highly self-consistent (140/150), so it
  applies the narrow reading reliably.
- **Diagnosis is unvalidated in one direction.** The labeler passed all 50, so
  there is no human-fail class and TNR cannot be estimated. Specificity rests
  on the control alone.

## Limits

- One labeler, who also wrote the construct. There is no inter-rater
  agreement yet.
- One lesson. Baseline and candidate items share submissions, so the 50 items
  are not independent and the effective n is smaller than the intervals assume.
- 32 human fails bound how precisely TNR can be estimated.
- Token usage is not logged, so cost is estimated, not measured.

## Next

Automated prompt optimization (ProTeGi, arXiv:2305.03495) on the actionability
definition. Items are split by misconception (seeded, in `split.json`): **dev**
is M2, M4, M5, M7, M8 (30 items, 19 human fails) and **test** is M1, M3, M6 (20
items, 13 human fails). Optimization sees dev only. Run 1 on each split, for
comparison:

| Split | Actionability TPR | Actionability TNR |
|---|---|---|
| dev | 11/11 (74%–100%) | 6/19 (15%–54%) |
| test | 7/7 (65%–100%) | 3/13 (8%–50%) |
