---
topic: eval
created: 2026-06-07
slices: [12, 13]
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
- 2026-06-07 (#13): **`eval` drives the *full* `run_lesson`, not just
  `request_feedback`.** Decided with the user against the lighter feedback-only
  path: each case's submission is executed through the real interpreter, graded,
  then sent for a verdict — the exact pipeline `run` uses, so "feedback evaluated
  is feedback shipped" holds literally (§3.2). Consequence: eval (and its
  integration tests) need `Rscript`, so the cli tests skip via `rscript_absent()`
  and the demo-lesson eval cases must be **cleanly-running** R (`cat("alpha\n")`,
  not pseudocode), or a launch failure becomes `EvalRunError` instead of a
  verdict. The original `eval_fireworks_vitals.R` was text-only; v0 here is not.
- 2026-06-07 (#13): **Scoring is the pure core, `run_eval` the thin shell**
  (§2.3/§2.4), both in `core::eval`. `score_case(&ExpectedVerdict,
  &ExpectedVerdict) -> bool` is exact polarity equality (no substring slack);
  `aggregate(&[CaseResult]) -> f64` is matched/total and returns **0.0 for an
  empty suite** (not `0/0` NaN — serde_json cannot serialize NaN, so the artifact
  would otherwise fail to emit). `CaseResult::score(expected, &Verdict)` is the
  *only* constructor — it maps the runtime verdict to polarity and derives
  `matched`, so a `matched` flag inconsistent with the polarities is
  unrepresentable (§1.1). `EvalReport::new` likewise derives `accuracy`.
- 2026-06-07 (#13): **`ExpectedVerdict` reconciliation realized.** Slice 12 left
  the `llm::Verdict` → polarity mapping to this slice; it lands as
  `impl From<&Verdict> for ExpectedVerdict` (message dropped — v0 scores polarity
  only). `ExpectedVerdict` gained a hand-written `Serialize` (→ the canonical
  `token()`, now `pub`) so the JSON spelling can't drift from the YAML/accepted
  set, and `token()` is the single source for YAML + JSON + the human render.
- 2026-06-07 (#13): **Case numbering split is intentional.** The human report and
  `EvalRunError` Display are **1-based** (`case N`, matching how an author counts
  their YAML); Slice-12's `EvalParseError::UnknownVerdict` stays **0-based** (a
  parse-time logical index, its tested contract untouched). The `index` field is
  zero-based throughout; only Display adds `+1`. Commented inline so it reads as a
  choice, not a bug. A future slice could unify on 1-based (would change #12's
  error test).
- 2026-06-07 (#13): **No `mockd` crate (still).** The issue's executable-spec
  probes invoked `cargo run -p blendtutor-test-support --bin mockd`; that crate
  was never built (Slice 11 chose in-test wiremock — see [[run]]). Eval reuses the
  same seam: `mount_feedback_for(server, needle, …)` routes a distinct scripted
  verdict per submission by `body_string_contains(needle)`, where `needle` is a
  token `build_prompt` fences verbatim into the request body. The
  `BLENDTUTOR_PROVIDER_URL` const moved from `commands::run` to `commands::mod`
  (`PROVIDER_URL_VAR`) so `run` and `eval` share one source.
- 2026-06-07 (#13): **Sibling-suite convention is realized in the cli, not core.**
  `eval <lesson.yaml>` reads `eval_<file_name>` next to the lesson
  (`commands::eval::sibling_suite_path`, prefix the whole file name with
  `eval_`). core::eval still never touches the filesystem (§2.1); the path
  derivation + read live at the cli edge.
