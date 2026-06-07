# ADR-0007: Eval expected verdict — a polarity-only type, not the message-carrying runtime `Verdict`

- Status: Accepted
- Date: 2026-06-07

## Context

Issue #12 introduces the eval-case model (`core::eval`): an `EvalSuite` of
`EvalCase`s, each pairing a synthetic student `submission` with the verdict the
grader *should* return. It ports the ten ground-truth cases from
`evals/eval_fireworks_vitals.R`, replacing that file's
`^The code is (in)correct` target regex with a typed `expected` field.

The Slice-12 plan proposed reusing the runtime `llm::Verdict` for `expected`, so
"author intent and runtime verdict share one type". But `llm::Verdict` is
`Correct { message: String } / Incorrect { message: String }` (ADR-0006): it
carries the **learner-facing feedback message** and derives no `Deserialize` —
the private `Feedback` DTO does, and `Verdict` is built from it via
`From<Feedback>`. Eval ground truth has no message; it is pure **polarity**
(`correct` / `incorrect`). And eval scoring (Slice 13) compares polarity, not
message text. So the plan's premise — that `Verdict` is a simple two-variant
tag — does not hold against the code as built.

## Options

1. **Reuse `llm::Verdict`, deserialize `correct` → `Verdict::Correct { message: "" }`.**
   Honors the plan literally. But it forces an always-empty `message` on every
   expected case — a redundant state (§1.1) — and `Verdict`'s `PartialEq` is
   then useless for scoring, since an expected `Correct { message: "" }` never
   equals an actual `Correct { message: "<real feedback>" }`. Also needs a
   hand-written `Deserialize` to map a scalar onto a struct variant.
2. **A polarity-only `ExpectedVerdict { Correct, Incorrect }` in `core::eval`.**
   No vestigial field (§1.1); an unknown verdict is rejected at the parse
   boundary (§1.2) naming the offending case (§1.4). Cost: a second
   verdict-shaped type — but it models a genuinely different thing (an author's
   expected polarity vs a runtime grade that also carries feedback prose).
3. **Extract a shared `Polarity` out of `llm::Verdict`** (`Verdict { polarity, message }`)
   and reuse it in `core::eval`. Truly one type for the polarity. But it
   reshapes `llm::feedback` and the `run::VerdictTag` wire mapping — scope well
   beyond this slice.

## Decision

Option 2. `expected` is a dedicated `ExpectedVerdict` sum type owned by
`core::eval`. The structural parse reads each case's verdict token and maps it to
a variant; an unrecognized token is a typed `EvalParseError` naming the case,
never a coerced default.

## Consequences

- Eval cases carry no vestigial empty message, and an illegal verdict is
  unrepresentable past the parse boundary (§1.2, §1.4) rather than a silently
  defaulted `Incorrect`.
- `core::eval` depends on `lesson` but **not** on `llm::Verdict`: the
  one-directional `eval → llm` coupling the plan anticipated is not needed at the
  model layer. Scoring (Slice 13) may introduce it by mapping a runtime
  `Verdict` to its polarity and comparing against `ExpectedVerdict`.
- Two verdict-shaped types coexist (`llm::Verdict`, `eval::ExpectedVerdict`).
  Their names and module homes keep them distinct; they model different domains.
  Accepted deliberately. If the Slice-13 polarity mapping proves load-bearing,
  revisit via Option 3 (extract a shared `Polarity`) — recorded here so the
  reuse intent is not lost.
- Diverges from the Slice-12 plan note ("reuse `llm::Verdict`"), which assumed a
  tag-only `Verdict`. This ADR is the record of that divergence; the issue's
  "covered by ADR-0002" pointer was a mis-reference (ADR-0002 is the git-hook
  mechanism).
