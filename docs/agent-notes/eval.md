---
topic: eval
created: 2026-06-07
slices: [12]
---

The eval-case model (`core::eval`): the typed shape the `eval` command will later
score. Owns `EvalSuite`/`EvalCase`/`ExpectedVerdict` + `parse_eval_suite`. See
ADR-0007 for the verdict-representation decision.

- 2026-06-07 (#12): **`expected` is a polarity-only `ExpectedVerdict { Correct,
  Incorrect }`, NOT the runtime `llm::Verdict`** (ADR-0007). The Slice-12 plan
  said to reuse `llm::Verdict` so author intent and runtime verdict share one
  type — but that premise is stale: `llm::Verdict` is `Correct { message } /
  Incorrect { message }` and derives **no** `Deserialize` (the private
  `Feedback` DTO does; `Verdict` is `From<Feedback>`). Eval ground truth is pure
  polarity (no message), and Slice-13 scoring compares polarity, so reuse would
  force an always-empty `message` (§1.1) and a `PartialEq` scoring can't use.
  Decision reconciled with the user before writing red. `core::eval` therefore
  does **not** depend on `llm::Verdict`; a polarity mapping is Slice-13's job.
- 2026-06-07 (#12): **Two-phase parse = private `Raw*` DTOs → public domain
  types**, mirroring the `llm` layer's `Feedback → Verdict` boundary (ADR-0006).
  `RawSuite`/`RawCase` (private, `#[derive(Deserialize)]`, `expected: String`)
  deserialize the structure; then each token validates into `ExpectedVerdict`.
  This is **load-bearing**: it is the only way the error names the *logical case
  index*. A direct `serde_saphyr::from_str::<EvalSuite>` reports the YAML **line**
  (`line 22: unknown variant maybe`), not "case 2" — serde fails atomically with
  no logical index. `parse_eval_suite` enumerates the raw cases so
  `EvalParseError::UnknownVerdict { index, token }` can carry both. The public
  types (`ExpectedVerdict`/`EvalCase`/`EvalSuite`) are intentionally serde-free
  domain values; only the private DTOs touch serde.
- 2026-06-07 (#12): **Valid verdict spellings live once** in
  `ExpectedVerdict::CORRECT_TOKEN`/`INCORRECT_TOKEN`, used as the match patterns
  in `from_token` *and* interpolated into the `UnknownVerdict` Display hint, so
  the accepted set and the "expected …" error can't drift (associated `&str`
  consts are valid match patterns).
- 2026-06-07 (#12): **`.gitignore` gotcha.** The repo's `evals/` rule (the
  top-level R working dir) was unanchored, so it also ignored Rust eval fixtures
  under `crates/*/tests/fixtures/evals/`. Anchored to `/evals/` — fixtures track,
  the R dir stays ignored. Watch for this if adding eval fixtures elsewhere.
- 2026-06-07 (#12): Fixtures in `crates/core/tests/fixtures/evals/`:
  `eval_fireworks_vitals.yaml` (10 cases ported from
  `evals/eval_fireworks_vitals.R`, each `submission` + `expected:
  correct|incorrect`); `eval_fireworks_vitals_bad_verdict.yaml` (a 4-case subset
  with case index 2's `expected: maybe`) for the rejection path.
- 2026-06-07 (#12): **Deferred to Slice 13** — `parse_eval_suite` accepts an
  empty `cases:` list (structurally valid). Whether a suite must have ≥1 case is
  a semantic rule for the scoring slice, not modeled here.
