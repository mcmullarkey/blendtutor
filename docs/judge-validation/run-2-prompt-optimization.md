# Judge validation, run 2: optimizing the actionability definition

2026-11-12. Protocol pre-registered in ADR-0036; the run 1 baseline is in
[run-1-baseline.md](run-1-baseline.md). Data in
[`write-less-code-r-03/`](write-less-code-r-03/).

## What was compared

Three actionability definitions, each scored once on the held-out **test**
split (M1, M3, M6: 20 items, 7 human passes, 13 human fails). Kimi K3.1 judged
with a 3-sample majority, as in run 1.

1. **v1:** the built-in definition. These are run 1's test numbers.
2. **hand:** v1 plus the labeler's stated rule ("giving `6 - x` directly is the
   answer, not the thought process"), written before the search
   (`definitions/hand.txt`).
3. **APO:** the best of 9 definitions from a ProTeGi search on **dev** (M2, M4,
   M5, M7, M8: 30 items). It ran 3 rounds, with a beam of 2, 2 gradients per
   parent, minibatches of 4 errors, and 1 sample per judgment. The optimizer
   saw only dev items and the labeler's dev notes (`apo/`).

## Results on test

| Definition | TPR | TNR | Balanced |
|---|---|---|---|
| v1 | 7/7 (65%–100%) | 3/13 (8%–50%) | 0.62 |
| hand | 7/7 (65%–100%) | **13/13 (77%–100%)** | 1.00 |
| APO | 5/7 (36%–92%) | **13/13 (77%–100%)** | 0.86 |

The intervals are 95% Wilson intervals.

## What it means

- **Both rewrites fixed the lenient judge.** TNR went from 3/13 to 13/13, and
  the intervals do not overlap (8%–50% vs 77%–100%). The construct gap from run
  1 was real and fixable through the definition alone.
- **APO did not beat writing the rule down.** Hand and APO tie on TNR. On TPR
  (7/7 vs 5/7) the intervals overlap, so the comparison is inconclusive, not a
  loss for APO.
- **APO found the rule unaided.** Its gradient on round 1's candidate named the
  gap ("the answer-leak safeguard is too narrow… does not cover exact fixes,
  corrected fragments, key expressions"). Its best definition says "a key
  solution expression remains leakage even when quoted from or attributed to the
  prompt", which is the labeler's `6 - x` rule, recovered from dev notes alone.
- **APO overgeneralized that rule.** Both test misses are feedback the labeler
  passed:
  - `cmp-c-25` quotes `6 - x` back from the learner's own code
    (`print(6 - x)`). That is not a leak, but the rule's wording catches it.
  - `cmp-c-03` gives a Socratic hint ("the reversal of an N-point scale is
    computed relative to N + 1"), which the judge read as revealing the
    constant.

  The next construct refinement is to treat restating the learner's own code
  as not leakage.
- **The dev score was inflated.** The best dev candidate scored 0.974 on dev
  and 0.86 on test. Picking the best of 9 noisy 1-sample scores on 30 items
  rewards luck. This is why the test split exists.

## Limits

- **The hand arm is not cleanly held out.** I wrote `hand.txt` after reading
  run 1's disagreements on all 50 items, test items included (the `6 - x`
  example comes from `cmp-b-03`, an M1 test item). Its 13/13 is therefore
  optimistic. The APO arm is the clean one: the optimizer never saw test items
  or notes.
- One labeler and one lesson. Baseline and candidate items share submissions,
  so the effective n is smaller than 20.
- The test split has now been used. Changing the definition again (for
  example, the learner's-own-code refinement) needs newly labeled items to
  validate it, not a re-score of this test set.
- The search made 270 judge calls (9 definitions × 30 dev items) plus about
  15 optimizer calls, and the test scoring 120 more. Tokens are not logged, so cost is not measured.

## Decision

Adopt neither definition as the built-in yet. Write a v2 definition that
combines the hand rule with the "the learner's own code is not leakage"
refinement, update the labeling desk to the same text, and validate it on
labeled items from a second lesson before shipping it.
