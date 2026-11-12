# ADR-0035: Judge agreement as TPR and TNR per criterion

## Status

Accepted (2026-11-11). Builds on ADR-0033 and ADR-0034.

## Context

The feedback judge (ADR-0033) is only useful if its verdicts track people's.
`judge-feedback` (ADR-0034) produces the judge's verdicts on the same item ids
people labeled blind in the labeling desk. What's left is to compare them, in
a way that says what kind of mistakes the judge makes.

The base rates are skewed. Most guided feedback passes actionability, so a
judge that always says yes scores high on raw accuracy. ADR-0033's control
answers this for diagnosis, but not for actionability.

## Options

1. **Raw accuracy.** One number, but it is dominated by the majority class, so
   it hides a judge that never says no.
2. **Cohen's κ.** It corrects for chance, but it is still one number. It is
   unstable when one class is rare, and it doesn't say which way the judge
   errs.
3. **TPR and TNR per criterion, each with a Wilson interval.** TPR is how often
   the judge passes what people pass, and TNR is how often it fails what people
   fail. They separate the two error directions, and each is a proportion with
   an honest interval at small n. The disagreeing items are listed by id for
   error analysis.

## Decision

Option 3. `blendtutor judge-agreement <judged.json> <labels.json>` reads
`judge-feedback --format json` output and a JSON array of labels (`id`, and
`diagnosis` and/or `actionability` as `"pass"`/`"fail"`; other fields such as
`note` are ignored). It pairs them on `id`, separately for each criterion, and
reports:

- `pairs`, the TPR and TNR (`hits`/`of` with a Wilson interval), and the
  disagreements;
- `unlabeled` (judged ids with no label at all) and `unjudged` (labeled ids the
  judge never saw), so silent data loss in the join shows up.

It makes no provider call and changes nothing, so it can be re-run as labels
accumulate. A yes-sayer shows up as a TNR near 0, whatever its accuracy.

## Consequences

- TNR on actionability is the measurement that the 25/25 ceiling result could
  not give. It needs failing items, such as the baseline feedback that pastes
  solutions.
- The disagreement list is what automated prompt optimization critiques.
  Optimizing on it needs a dev/test split (by misconception) so the reported
  agreement is not the agreement that was optimized. That split is a later
  slice.
- The diagnosis control is not part of this report. It is a known negative by
  construction, not a human label, so `judge-feedback` still reports it.
