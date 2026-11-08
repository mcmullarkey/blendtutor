# ADR-0029: `eval-compare`, a paired, pre-registered ship decision between two grading prompts

- Status: Accepted
- Date: 2026-11-08

## Context

The goal of the measurement work is answering "is the new feedback prompt
better?" with evidence a ship decision can rest on. ADRs 0022–0028 gave each
eval run several constructs (accuracy, per-misconception detection, leakage)
with honest small-n intervals. Comparing two prompts still meant eyeballing two
reports. Two failure modes are common:

- **Point-estimate comparison.** "Both got 6/6, and the new one leaks less, so
  ship." Equal observed accuracy on few items does not rule out a large drop.
- **Unpaired comparison.** Treating the two runs as independent samples throws
  away the fact that both arms grade the *same* items, which is the largest
  source of variance.

## Options

1. **Two `eval` runs plus a diff of headline numbers.** No inference.
2. **Unpaired two-proportion intervals.** Valid but much wider than necessary.
3. **A paired design with Newcombe's score interval for a difference of paired
   proportions (method 10), plus a pre-registered decision rule.**

## Decision

Option 3. `blendtutor eval-compare <baseline> <candidate>` grades the
baseline's eval suite with both lessons' prompts, case by case. For each
construct it reports the counts per arm, the paired change (candidate −
baseline), and a 95% Newcombe interval. That interval is built from each arm's
Wilson interval and the phi correlation between arms. It is closed-form,
consistent with ADR-0025, and stays inside [−1, 1] at small n.

- **Accuracy:** `non_inferior` when the lower bound exceeds −margin.
- **Leakage** (cases assessable per ADR-0028; assessability depends only on the
  item, so both arms share the set): `improved` when the upper bound is below 0,
  `worse` when the lower bound is above 0, otherwise `not_shown_improved`.
- **Decision:** `ship` iff accuracy is non-inferior and leakage, when
  assessable, is improved. Otherwise `hold`, with the reasons. Exit 0 = ship,
  3 = hold. Exit 2 is clap's usage-error code, and exit 1 is reserved for
  errors.
- `--margin` (default 0.05) must be strictly between 0 and 1. `--model` applies
  to both arms.
- The candidate must have the same language and checks as the baseline, or the
  comparison is refused before any grading: it would compare lessons, not
  graders.

## Consequences

- "No evidence of harm" is never reported as "no harm." At the default 5-point
  margin, showing non-inferiority needs roughly 75 items even when every item
  agrees, so small suites will usually hold. That is the honest answer, and
  the margin is a choice to make **before** looking.
- The decision rule is fixed: accuracy is the guardrail and leakage the target.
  Choosing a different target construct is a later extension.
- Each comparison costs two eval runs.
