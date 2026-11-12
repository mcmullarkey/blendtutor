# ADR-0036: Optimizing the actionability definition against human labels

## Status

Accepted (2026-11-12). Builds on ADR-0033 to ADR-0035.

## Context

Run 1 ([judge validation](../judge-validation/run-1-baseline.md)) found the
actionability judge lenient: TPR 18/18 but TNR 9/32. The labeler's fail notes
show a construct gap. The labeler counts stating the answer expression (`6 - x`)
or the exact edit as a leak, while the judge reads "complete working solution"
narrowly. The labeler has since stated the rule outright: giving `6 - x`
directly is the answer, not the thought process, so it fails.

We want to know whether automated prompt optimization (ProTeGi, Pryzant et al.
2023, arXiv:2305.03495) closes the gap. Tuning a prompt against the same labels
it is scored on would overstate the gain, so the protocol has to keep test
labels out of the loop.

## Options

1. **Hand-edit the definition and re-score all 50.** This is cheap, but the
   reported agreement would be measured on the data that motivated the edit.
2. **ProTeGi on all 50, then report the dev score.** This has the same flaw,
   and the optimizer makes it worse.
3. **ProTeGi on a dev split, with one final score on a held-out test split,
   compared against a hand-edited control.**

## Decision

Option 3, pre-registered here before any optimization runs:

- **Interface.** `judge-feedback --criterion actionability` judges actionability
  alone. `--actionability-definition <file>` replaces the built-in definition
  (`ACTIONABILITY_DEFINITION`) with a candidate. A candidate is plain text, so
  the optimizer runs outside the binary and scores through it.
- **Split.** By misconception, seeded (`split.json`): dev is M2, M4, M5, M7, M8
  (30 items) and test is M1, M3, M6 (20 items). Baseline and candidate feedback
  on one submission always land together.
- **Search, on dev only.** ProTeGi uses error minibatches from dev, an LLM
  "gradient" (why the definition misjudged them, given the labeler's notes),
  edits applied against that gradient, and beam selection. The selection metric
  is balanced accuracy, (TPR + TNR) / 2, so collapsing to "always fail" cannot
  win. Search samples once per judgment to save cost.
- **Arms scored once on test**, with 3 samples, as in run 1:
  1. v1, the built-in definition (run 1's test numbers);
  2. **hand-edited**: v1 plus the labeler's stated rule, written before the
     search;
  3. **APO-best**: the top dev candidate.
  The hand-edited arm is the control. It asks whether APO beats simply writing
  the rule down.
- **Reporting.** TPR and TNR per arm with Wilson intervals. Test n is small (13
  human fails), so overlapping intervals are reported as inconclusive, not as a
  win.

## Consequences

- The binary stays free of optimizer logic. The optimizer is a script that
  calls `judge-feedback` and `judge-agreement`.
- A winning definition still needs the desk's definition updated to match
  before any future labeling, or human and judge constructs drift apart again.
- Test labels are used once. Another round of optimization needs new labeled
  items, not a re-score of this test set.
