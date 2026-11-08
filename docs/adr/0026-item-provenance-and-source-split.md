# ADR-0026: Item provenance (`source: hand | generated`) and grader accuracy split by source

- Status: Accepted
- Date: 2026-11-08

## Context

Rigorous per-misconception numbers (ADR-0025) need more items than authors
will write by hand. The plan is to have the `blendtutor-course` skill generate
items from the blueprint, gated by `audit` (ADR-0023/0024). Generated items
are a new instrument, so they need their own validity check. A generator can
write items that are easier than real mistakes, or worse, items that share the
grader's blind spots, especially when both are the same model family. Either
problem would make prompt comparisons on generated items misleading, and
neither shows up in pooled accuracy.

## Options

1. **No provenance.** Generated and hand items are pooled, so there is no way
   to detect generator bias.
2. **Separate files** (`eval_<lesson>.generated.yaml`). Provenance comes from
   the file, but every command needs a second sibling-file convention, and mixed
   suites can't be expressed.
3. **A per-case `source: hand | generated` field** (closed enum, default
   `hand`), with `eval` reporting accuracy per source when any generated item is
   present.

## Decision

Option 3. `ItemSource` is a closed enum. An unknown token is a parse error
naming the case. A missing field means `hand`, so every existing suite is
unchanged. `CaseResult` carries the source, and the wire omits it for `hand`
so prior reports keep their shape. `EvalReport` gains `by_source`: one row per
source (`hand`, then `generated`) with `matched`, `items`, and a Wilson
`interval`. It is emitted only when the suite contains a generated item.

Source is independent of every derived field, so `CaseResult` takes it through
a `with_source` builder instead of widening `score` again.

## Consequences

- A hand-vs-generated gap is a **screening signal**, not a verdict. It can mean
  easier items, shared blind spots, a different misconception mix (compare
  within a misconception first), or bad hand labels (run `audit`).
- Shared blind spots are the dangerous case: they hide improvements, not just
  shift the level. The skill should generate with a different model family
  than the grader and keep a hand-written anchor set.
- Generated variants of one seed are correlated, so the Wilson interval is too
  narrow for them. A `seed` field for clustering is deferred.
- `audit` does not yet warn when a generated item crashes (`incorrect` +
  `NotRun` is consistent). That is the next gate to add.

## Amendment (2026-11-08): audit warns on tagged items that never run

The gate gap listed above is now closed. `LabelAudit` derives
`warning: Option<AuditWarning>` at construction. It is
`TaggedItemNeverRan` when the case has a misconception tag and its checks
came back `NotRun`. The wire carries `"warning": "tagged_item_never_ran"` only
when present, and the human report lists each warned case. This is a
**warning, not a violation**: `audit` still exits 0, because a crash
contradicts nothing. The item just can't show its misconception. Untagged
crashing items are legitimate syntax-error cases and get no warning. The
warning applies regardless of `source`, since a hand-written tagged item that
crashes is equally useless as evidence.
