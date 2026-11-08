# ADR-0025: Per-misconception grader detection, reported with Wilson score intervals

- Status: Accepted
- Date: 2026-11-08

## Context

ADR-0024 tags incorrect eval items with blueprint misconceptions, and `audit`
reports which ones the lesson's checks catch. The next question is the one the
eval exists for: **does the grader catch each misconception?** This matters
most for misconceptions the checks leave uncovered.

Every tagged item is labeled `incorrect`, so "caught by the grader" means the
grader's verdict polarity is `incorrect`, which is polarity scoring restricted
to one tag. No judge is needed.

Item counts per misconception are small (often 1–5), so the number has to be
honest at small n. A bare rate ("2/2 = 100%") overstates certainty.

## Options

1. **Bare rate.** Simple, and misleading at the n authors actually have.
2. **Wald interval** p̂ ± z·√(p̂(1−p̂)/n). The textbook formula, but its width is
   zero at p̂ = 0 or 1, so it reports 2/2 as [100%, 100%]. It fails exactly
   where small n most needs an honest answer, and its coverage is poor
   throughout small n.
3. **Clopper–Pearson (exact).** Guaranteed coverage but conservative
   (2/2 → [16%, 100%]), and it needs a beta quantile function, which means a
   new dependency or hand-rolled numerics.
4. **Wilson score interval.** A closed form, never degenerate (2/2 →
   [34%, 100%]), with near-nominal coverage at small n. It is the usual
   recommendation for binomial proportions.

## Decision

Option 4, at 95% (z = 1.96). `EvalReport` gains `by_misconception`: one row per
blueprint entry, in blueprint order, with `id`, `label`, `caught`, `items`, and
`interval: [lo, hi]`. `interval` is absent when `items` is 0. That row is a gap
in the item bank, not a 0% rate. `CaseResult` now carries the case's
misconception (absent when untagged). `CaseResult::score` takes the full
`CaseLabel`, and `EvalReport::new` takes the blueprint, so the rows are derived
from the cases and never set independently.

## Consequences

- Authors see when they need more items: a wide interval is the signal.
- **The interval assumes independent items.** Generated variants of a single
  seed, or near-duplicate items, are correlated, so the interval is too narrow
  for them. Once generated items carry provenance (a later slice), reporting
  should cluster by seed or at least flag the count of independent seeds.
- The headline accuracy is still a bare rate. Giving it an interval too is a
  natural follow-up, kept out of this slice.
