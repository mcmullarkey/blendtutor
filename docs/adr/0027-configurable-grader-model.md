# ADR-0027: A configurable grader model (`--model`) via a `Grader` value

- Status: Accepted
- Date: 2026-11-08

## Context

The grading model was a compile-time constant per provider
(`ProviderChoice::default_model`). In November 2026 the pinned Fireworks
checkpoint `deepseek-v4-flash-0731` stopped resolving (`404 Model not found`),
which broke every `run` and `eval` with no workaround short of a rebuild.

The measurement work also needs the model to be a variable. A grader is a
configuration (provider × model × prompt), and comparing two configurations is
the point of a later `eval-compare` slice. The model belongs in that
configuration, not in a constant.

## Options

1. **A `BLENDTUTOR_MODEL` env var read inside `request_feedback`.** Small, but
   it makes the model ambient global state that a comparison of two models
   in one process cannot use.
2. **One more `model: Option<&str>` parameter** on `request_feedback`,
   `run_lesson`, and `run_eval`. Explicit, but it widens three signatures with a
   loose string, and "empty model" stays representable.
3. **A `Grader { provider, model: ModelId }` value** that replaces the bare
   `ProviderChoice` parameter on all three, with `ModelId` refusing an empty or
   blank id at parse time.

## Decision

Option 3. `Grader::default_for(provider)` uses the provider's default model, and
`Grader::with_model(ModelId)` overrides it. `ModelId` implements `FromStr`, so
clap rejects `--model ""` as a usage error (exit 2) before any request.
`run` and `eval` take `--model <id>`. The provider's default stays the single
source for the no-flag case and for `smevals_gen`.

## Consequences

- A vanished model is now a one-flag workaround, not an outage.
- A comparison of two graders is two `Grader` values, with no shared mutable
  state.
- The default can still go stale. Bumping it remains a deliberate, test-pinned
  change (ADR-0016), now separate from being able to run at all.
- The browser BYOK path already has its own model selection (ADR-0009) and is
  unaffected.
