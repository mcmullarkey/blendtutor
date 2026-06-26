---
topic: llm
created: 2026-06-06
slices: [10]
---

The LLM feedback layer (`core::llm`): a pure injection-hardened prompt builder and
the rig-core provider client that turns a model's tool call into a typed `Verdict`.
See ADR-0006 for the decision record.

- 2026-06-06 (#10): `core::llm` is split into three submodules with the public
  names re-exported from `mod.rs`, so callers keep `core::llm::build_prompt` /
  `core::llm::OPEN_CODE`: `prompt` (pure — constants, `Submission`, `ExecResults`,
  `Prompt`, `build_prompt`), `feedback` (effectful — `Verdict`, the private
  `Feedback` DTO, `FeedbackError`, `request_feedback`), `provider`
  (`ProviderChoice`). The split realizes the §2.1/§2.2 pure-vs-effectful cut and
  was forced by roborev re-flagging the flat module's §4.3 surface on two
  consecutive commits (second occurrence promotes — don't defer a third time).
- 2026-06-06 (#10): **rig-core 0.38.1 crate gotchas (load-bearing).** The lib name
  is **`rig_core`**, not `rig` — `use rig_core::...`. It **re-exports schemars**
  (`pub use schemars`) and depends on **schemars 1.x** (not 0.8); the extractor's
  `T` must derive `JsonSchema + Deserialize + Serialize` (the `ExtractorBuilder`
  bound adds `Serialize` — easy to miss). `Extractor<M, T>` has **two** generics.
- 2026-06-06 (#10): **The Responses-vs-Completions trap.** `openai::Client`
  defaults to the OpenAI **Responses API** (`OpenAIResponsesExt`), whose response
  envelope differs from chat-completions. Fireworks (and any OpenAI-compatible
  provider) speaks **chat completions**, so you MUST call `.completions_api()`
  before `.extractor::<T>(model)`, or the response won't parse. Build with the
  type-state builder: `openai::Client::builder().api_key(k).base_url(u).build()?`
  (`build()` returns `http_client::Result`; not `from_url`, that's Llamafile's).
  `api_key` accepts any `S: Into<String>` (the key wrapper is `BearerAuth` /
  `AnthropicKey`). Anthropic's `.extractor()` comes from the `CompletionClient`
  trait (`use rig_core::prelude::CompletionClient`); openai's completions extractor
  is inherent.
- 2026-06-06 (#10): **rig's `Extractor` forces an internal tool named `submit`**
  (`SUBMIT_TOOL_NAME = "submit"`) and filters the returned tool_calls by that exact
  name — so a wiremock chat-completions body must use
  `function.name = "submit"`, NOT the R package's `respond_with_feedback`. The
  `function.arguments` field is a JSON-**encoded string** (rig accepts stringified
  or object form). The base-URL override pointed at a `wiremock::MockServer` is the
  whole test seam; no real provider is hit in the default lane.
- 2026-06-06 (#10): **`FeedbackError` mirrors rig's `ExtractionError` taxonomy** so
  a caller can act on the kind: `MissingApiKey{var}` (our guard), `Client` (build
  failure), `Completion` (transport/auth/model — `ExtractionError::CompletionError`,
  retry/fix-key), `Extraction` (malformed/missing field or no tool call —
  `DeserializationError`/`NoData`, the model's output is bad). The
  `From<ExtractionError>` match is deliberately **exhaustive with no wildcard**: a
  new rig variant must fail to compile until mapped.
- 2026-06-06 (#10): **The missing-key guard (`require_api_key`) fires before the
  rig client is built** (§1.3.1) and treats **unset OR empty** as missing, naming
  the active provider's var (`FIREWORKS_API_KEY` / `ANTHROPIC_API_KEY`). Tested by
  pointing the client at a live mock and asserting `received_requests().len() == 0`
  on the error path — proves no socket I/O. Env-mutating tests are `#[serial]`
  (serial_test) and `std::env::set_var`/`remove_var` are `unsafe` in edition 2024.
- 2026-06-06 (#10): **`build_prompt` does NOT use the lesson's
  `llm_evaluation_prompt` template.** It builds a fresh structured prompt (task
  from `exercise.prompt`, a single fenced submission, captured-output + check
  sections). The `{student_code}` placeholder that slice #4 hard-validates is
  therefore **currently unused** (recorded in ADR-0006, not a defect). Every
  interpolated value is neutralized so a student can't forge a fence/label — each
  structural token stays count==1. Layout pinned by
  `tests/snapshots/llm__build_prompt.snap`.
- 2026-06-06 (#10): **Spec drift reconciled.** The issue's `ExecResults` /
  `[{name, passed}]` check shape never existed; slice #9 produced
  `ExecutionResult` (one run, no checks) + `Vec<CheckOutcome>` (no name,
  index-paired with `lesson.checks`). `core::llm::ExecResults` bundles
  `{ output: ExecutionResult, outcomes: Vec<CheckOutcome> }`; `build_prompt` pairs
  `lesson.checks[i]` with `outcomes[i]` by index but **iterates over `outcomes`**
  so a verdict is never silently dropped if a hand-built `ExecResults` desyncs.
- 2026-06-06 (#10): **Known gap — the Anthropic happy path has no wiremock
  coverage.** Only its key guard is tested. Its native messages-API response shape
  (a `content` array of `tool_use` blocks, `input` as a JSON object — not a
  stringified `arguments`) differs from the OpenAI envelope `mount_tool_call`
  builds, so it needs its own fixture. Grep `TODO(anthropic-happy-path)` in
  `feedback.rs` when picking this up.
