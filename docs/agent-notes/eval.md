---
topic: eval
created: 2026-06-07
slices: [12, 13]
acs: [1, 2, 3, 5]
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
  (2026-09-08, #228: the R working dir and its scripts are retired — the
  `/evals/` rule was removed from `.gitignore` and the untracked `evals/` tree
  deleted; the fixture-anchoring lesson above stands for any future ignore
  rules.)
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
- 2026-08-08 (#AC-1): **`CaseResult` gained `feedback_message: String`**,
  captured in `CaseResult::score` — the only constructor — so the message can
  never be inconsistent with the verdict it came from; both `Verdict` variants
  carry `message` (there is NO public `message()` accessor; read inline via
  exhaustive match, feedback.rs untouched). Serialize-only derive: additive
  JSON field, `eval-course.sh`'s jq (`.cases` selects) unaffected. `--case N`
  single-case selection is **1-based** (human-report numbering), and the
  ORIGINAL suite index is threaded through the filter so
  `EvalRunError::CaseOutOfRange { requested, suite_size }` reports the user's
  number, never a post-filter index; the range check lives inside `run_eval`
  (pure core), clap exits 2 for parse errors and 1 only for out-of-range.
- 2026-08-08 (#AC-2): **The smevals eval-dir generator is pure Rust in
  `core::smevals_gen`** — `generate_eval_dir(&Lesson, &EvalSuite, lesson_id)
  -> Vec<(PathBuf, String)>`, the FIRST YAML *emitter* in the codebase
  (serde-saphyr is parse-only). Generated dir is `<course>/.smevals/`
  (dotdir, `**/.smevals/` rule — provably no collision with committed
  `docs/evals/`); an empty suite is an Err, not a vacuous pass. Two
  implementation deviations from plan: (1) emission uses double-quoted
  scalars with full control-char escaping, NOT block scalars — serde-saphyr
  0.0.27 `|+`/`|-` chomping cannot round-trip arbitrary strings byte-exact
  (the injection round-trip test pins byte-identity regardless); (2)
  `runner:`/`checker:` paths resolve relative to the config/grader FILE, so
  the effectful `write_eval_dir` recomputes the relative prefix by walking to
  the repo root — the pure fn cannot know course depth. Provider default
  bumped to `accounts/fireworks/models/deepseek-v4-flash-0731` (matches the
  browser BYOK pin).
- 2026-08-08 (#AC-3): **smevals runner (run.sh) + deterministic polarity
  checker (check_polarity.sh)** — the runner reads `SMEVALS_TASK_*` env,
  shells `blendtutor eval <lesson> --case N --format json` under
  `timeout ${SMEVALS_TIMEOUT:-120}` (env-overridable, so the stub test
  exercises the timeout path in ~2s), and exposes the verdict via a
  **line-1 stdout header** (`verdict: <polarity>` — fail-closed: a missing or
  malformed header is a transient failure). Retry is transient-only
  (exit-1/empty-stdout/malformed-JSON/timeout-124 — never on a mismatch
  verdict), 3 attempts, sleep-2 backoff. `check_polarity.sh` treats `.actual`
  as the source of truth (`matched` derives from `score_case`, never set
  independently) and compares exact equality — no substring slack. No
  `env -i`: `FIREWORKS_API_KEY` must reach the `blendtutor` subprocess.
- 2026-08-08 (#AC-5): **`blendtutor eval-report <lesson>` is a thin
  orchestration shell** (subcommand, not a script): generate `.smevals/` via
  AC-2's generator → `uvx smevals==0.2.0 run -g` → `uvx smevals==0.2.0 build
  -o docs/evals/.<lesson>.tmp/` → atomic rename into `docs/evals/<lesson>/`.
  **Grade-fail is evidence, not a gate:** a `run` exiting non-zero *with*
  recorded `runs/` still proceeds to build and exits 0 (a low accuracy is a
  result); only a run that exited non-zero with NO runs fails the command,
  naming the `run` stage. A build failure always fails naming `build` and
  discards the temp, so a prior committed report survives — build-into-temp +
  rename, NOT docs.yml's rm -rf precedent (local re-runs don't lose data).
  `.smevals/` is cleaned before each generate (stale runs are false
  evidence); lesson id = file stem via the shared course-root helper; a
  missing `uvx` is a clean stage-named error with an install hint, never a
  panic. The committed `docs/evals/01_seed_data/` report doubles as the
  real-key smoke evidence.
- 2026-11-08 (eval-measurement slice 1): **Grader–check consistency** (ADR-0022).
  `eval::consistency::classify(&actual_polarity, &[CheckOutcome])` is pure and
  total. It first collapses the checks into a private `CheckEvidence`
  (`NoChecks | AllPassed | NotRun | Failed`) so an empty slice can't fall into
  "all passed" by vacuous truth, then matches exhaustively. It classifies the
  **actual** verdict, never `expected`. `CaseResult::score` takes the checks
  (`&[]` for LLM-only lessons) and derives `consistency` the same way as
  `matched`. Wire: additive `{"status", "kind"?}`; `build` reads only
  `accuracy`, so `eval-report.json` consumers are unaffected. Fixture
  `eval_consistency/` has one case per category, and case 2 is a deliberately
  mislabeled item (`matched` true, violated).
- 2026-11-08 (eval-measurement slice 2): **Label audit** (ADR-0023).
  `blendtutor audit <lesson>` → `core::eval::audit_labels`, which runs
  `run_checks` per case, builds no provider, and needs no key. It reuses
  `classify(&expected, &checks)`: the same truth table, applied to a
  different instrument. Exit 1 iff `LabelAuditReport::has_violation()`.
  CLI wording goes through a shared `Claimant { Grader, Label }`, so eval
  says "graded" and audit says "labeled"; the integration test asserts audit
  output never contains "graded". The integration test runs with the API key
  env removed and asserts the mock provider saw zero requests.
- 2026-11-08 (eval-measurement slice 3): **Misconception blueprint + check
  coverage** (ADR-0024). `EvalCase` now holds a private `CaseLabel { Correct,
  Incorrect { misconception: Option<MisconceptionId> } }`. Read it with
  `expected()` (polarity) and `misconception()`, and build it with
  `EvalCase::new(s, ExpectedVerdict::X.into())` / `EvalSuite::new(cases)`.
  `ExpectedVerdict` is now `Copy`. The tag lives outside `ExpectedVerdict` on
  purpose: putting it in `Incorrect` breaks `score_case`'s `==` against the
  untagged grader polarity. `blueprint.misconceptions` is deserialized by a
  custom `MapAccess` visitor that keeps document order (a BTreeMap would put
  M10 before M2) and refuses duplicate ids. "Caught" = at least one check
  `Fail`; `NotRun` counts toward items but never toward caught (the negative
  control flipped exactly `a_crashing_item_is_counted_but_not_caught`).
  Fixture `eval_blueprint/`: M2 (print instead of return) is a real R gap,
  because `print()` returns its argument, so the checks pass.
- 2026-11-08 (eval-measurement slice 4): **Grader detection per misconception**
  (ADR-0025). `CaseResult::score` takes a `CaseLabel` (callers pass
  `ExpectedVerdict::X.into()`), and `EvalReport::new(blueprint, cases)` derives
  `by_misconception: Vec<DetectionRow>`, where "caught" = actual polarity is
  Incorrect. `eval::wilson(successes, n)` is a 95% Wilson score interval
  (`None` at n = 0), chosen over Wald, which is zero-width at 0/n and n/n.
  `by_misconception` is `skip_serializing_if = "Vec::is_empty"` so untagged
  reports stay byte-identical (the `eval_json_shape_is_byte_stable` pin still
  holds), and `interval` is omitted when items = 0. The fixture
  `eval_blueprint/` gained `# zz_*` comment tokens for mock keying, which
  doesn't change check outcomes.
- 2026-11-08 (eval-measurement slice 5): **Item provenance** (ADR-0026).
  `ItemSource { Hand (default), Generated }`. `EvalCase::with_source` /
  `CaseResult::with_source` are builders (source is independent of every
  derived field, so `score` didn't widen again). The wire omits `source` for
  `hand`, and `by_source` is emitted only when some case is generated, which
  the `hand_only_suites_get_no_split` / `eval_omits_the_source_split…` guard
  tests pin. Negative-control gotcha: a perl sabotage of `split` silently
  missed because rustfmt had reflowed the guard; the Edit-tool redo failed
  exactly those 2 guards. Generation itself lives in the `blendtutor-course`
  skill ("Generate more items"): per misconception × surface facet,
  `source: generated`, audit gate, human review of uncovered-misconception
  items, and a different model family than the grader.
- 2026-11-08 (eval-measurement slice 6): **Audit crash warning** (ADR-0026
  amendment). `LabelAudit::warning()` is `Some(TaggedItemNeverRan)` iff the case
  is tagged and any check outcome is `NotRun`. Untagged crashes, tagged items
  with no checks, and tagged items that ran don't warn. Warnings never change
  the exit code. The human report interleaves warnings with disagreements per
  case, in case order. Fixture `eval_crash/`.
- 2026-11-08 (eval-measurement slice 7): **`--model` / `Grader`** (ADR-0027).
  `Grader { provider, model: ModelId }` replaces the bare `ProviderChoice` on
  `request_feedback` / `run_lesson` / `run_eval`. `ModelId: FromStr` refuses
  blank ids, so clap exits 2. `commands::grader_for(Option<ModelId>)` is the
  single CLI resolver. The default was then bumped to
  `deepseek-v4p1-flash-1017`, because Fireworks retired 0731 (ADR-0016
  amendment); both the Rust and JS pins moved, and `check-model-alignment.sh`
  enforces they match. Hazard: `scripts/tests/test_sync_assets.sh` runs
  `git checkout -- _extensions/blendtutor/assets/` and silently reverted the
  uncommitted JS bump once.
- 2026-11-08 (dogfood, write-a-function, 31 items, deepseek-v4p1-flash-1017):
  accuracy 29/31; the two misses are both M6 `print()` items, where the
  grader *noticed* the print but followed success_criteria, which never forbid
  it (an underspecified rubric, not grader error). Verdicts were 31/31
  identical across two runs. Hand 4/4 vs generated 25/27. 6 of 8
  misconceptions were at ceiling, so accuracy has little headroom for prompt
  comparison. A regex proxy found a working `6 - x` solution in 24/25
  incorrect-case feedback messages (solution leakage), which is the
  construct with the most headroom.
