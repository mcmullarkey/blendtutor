# ADR-0006: LLM feedback via rig-core — BYOK, structured Verdict, hardened prompt

- Status: Accepted
- Date: 2026-06-06

(Issue #10 and the plan call this "ADR-0004" — that number was already assigned
to course-manifest discovery before this slice; the LLM-provider ADR is 0006.)

## Context

Issue #10 ports the R package's LLM feedback (`R/fireworks_integration.R`: a
Fireworks/OpenAI-compatible call whose forced `respond_with_feedback(is_correct,
feedback_message)` tool contract returns a structured grade) into `core`. The
`run` and `eval` commands both consume this: given a lesson, a submission, its
captured output, and the per-check outcomes, return a typed correct/incorrect
verdict with a message. We need to decide how the provider boundary is built,
how the model's structured output is typed, how a missing key is handled, and how
the prompt is assembled given student code is untrusted input flowing to an LLM.

## Options

1. **Hand-roll the boundary.** A `Provider` trait, a `reqwest` client per
   provider, and manual parsing of `choices[0].message.tool_calls[0].function.
   arguments` JSON. Full control, but we own HTTP, retries, schema→tool
   derivation, and tool-call extraction — exactly the plumbing every LLM client
   re-implements, and the surface where malformed-response bugs hide.
2. **Adopt rig-core (0.38.1) as the abstraction layer.** rig supplies the
   provider clients (`providers::openai`, `providers::anthropic`) and an
   `Extractor<M, T>` that derives a tool from `T`'s JSON schema, forces the tool
   call, and deserializes its arguments into `T`. We write the pure prompt, the
   DTO, the `Verdict` mapping, and the provider choice — not the HTTP or the
   parsing. Cost: a heavy dependency tree (reqwest/rustls/tokio) and coupling to
   rig's API shape, contained behind our own seam.

## Decision

Option 2. rig-core is an implementation detail of `core::llm`; callers depend on
`request_feedback` + `Verdict`, never on rig types (§3.4).

- **Structured output (§1.2).** Boundary DTO `Feedback { is_correct: bool,
  feedback_message: String }` derives `serde::{Deserialize, Serialize}` +
  `schemars::JsonSchema` (rig's `Extractor` bounds `T: JsonSchema + Deserialize +
  Serialize`) and **no `Default`**, so an omitted field is a deserialize error,
  never a silently-defaulted grade. `Feedback` maps at the boundary into the
  domain sum type `Verdict { Correct { message }, Incorrect { message } }`, so the
  bool's meaning lives in the variant — "correct with no message" and
  contradictory states are unrepresentable, and there is no public `is_correct`.
- **Provider selection (§3.4).** `ProviderChoice { Fireworks /* default */,
  Anthropic }`. Fireworks is OpenAI-compatible: rig's `openai::Client` with its
  base URL overridden to `https://api.fireworks.ai/inference/v1`, reading
  `FIREWORKS_API_KEY`. Anthropic is rig's native `anthropic::Client`, reading
  `ANTHROPIC_API_KEY`. Adding a provider is one enum arm.
- **Missing-key guard (§1.3.1).** Our own boundary check reads the *active*
  provider's key var and returns a typed error naming that var (and telling the
  user to set it) when it is unset **or empty**, before the rig client is built or
  any socket opens. A typed error impl'ing `std::error::Error` — `core` stays
  `anyhow`-free (ADR-0001).
- **Prompt input (reconciliation).** The spec's `ExecResults`/`{name, passed}`
  shape does not exist; slice #9 produced `ExecutionResult` (one run) and
  `Vec<CheckOutcome>` (per check, no name, index-paired with `lesson.checks`). We
  bundle them as `ExecResults { output: ExecutionResult, outcomes:
  Vec<CheckOutcome> }` and `build_prompt(&Lesson, &Submission, &ExecResults) ->
  Prompt` pairs `lesson.checks[i]` with `outcomes[i]` by index.
- **Hardened prompt (§2.1–§2.3).** `build_prompt` is pure and fixture-free. It
  builds a **fresh structured prompt** (task from `exercise.prompt`, the
  submission inside a single delimiter fence, captured output and check results in
  two labeled sections) rather than substituting the lesson's
  `llm_evaluation_prompt` template. Every interpolated value is neutralized so a
  student cannot forge a fence or label: each structural token appears exactly
  once. `request_feedback` (driving the Extractor) is the only effectful function.
- **Test seam.** The base-URL override is the test seam: rig's openai client is
  pointed at a `wiremock` server (request shape + extraction + error paths). A
  small `live`-tagged suite hits real providers, excluded from the default
  nextest lane (mirrors the runner slice's skip-with-notice convention).

## Consequences

- `core::llm` owns prompt build + client construction + `Feedback`→`Verdict`
  mapping; it does not run code or score evals (§4.1). Swapping providers touches
  one `ProviderChoice` arm (§3.4).
- rig-core pulls reqwest/rustls into the tree and `tokio` gains the LLM surface;
  build times grow. Providers are **not** feature-gated in 0.38.1, so no extra
  Cargo features are needed.
- rig API shape (load-bearing, non-obvious, pinned to 0.38.1): `Extractor<M, T>`
  has two generics, obtained via `client.extractor::<Feedback>(model).build()`;
  the base URL is set with `openai::Client::builder().api_key(..).base_url(..)
  .build()?` (not `from_url`, which is Llamafile's); `schemars` is **1.x** and
  rig does not re-export it (we add `schemars = "1"`); the Extractor forces an
  internal **`submit`** tool, so the wiremock tool-call body uses
  `function.name = "submit"`, not the R package's `respond_with_feedback` — rig's
  abstraction supersedes that hand-rolled contract.
- The lesson's `llm_evaluation_prompt` and its slice-#4-hard-validated
  `{student_code}` placeholder are **not consumed** by `build_prompt` in v1. The
  placeholder validation still stands (ADR-0003 only mandates its presence, which
  this does not contradict), but the authored eval-prompt text is currently
  unused. Consuming it (or relaxing the validation) is a future decision, flagged
  so the unused field is not mistaken for a defect.
