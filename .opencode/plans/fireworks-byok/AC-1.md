---
ac: 1
depends_on: none
risk: low
status: complete
---

# AC1 — Fireworks BYOK feedback backend (byokFireworks)

## Executable Criterion

- **predicate:** `feedback.js` defines a `byokFireworks({ baseUrl, apiKey })` factory returning a `FeedbackBackend` `{ name, getFeedback(prompt, model) }` (interchangeable with `byokAnthropic`, same `Verdict` seam). Posts `${baseUrl}/chat/completions` with `Authorization: Bearer` header, OpenAI-compatible request body (`parameters` not `input_schema`, `tool_choice: { type: "function", function: { name } }`), and response mapper that `JSON.parse`s `choices[0].message.tool_calls[0].function.arguments`. Named fallback model const `FIREWORKS_MODEL` mirrors `MODEL`. The 9 anti-copy-paste tripwires ship: `byokFireworks(`, `/chat/completions`, `Bearer`, `"Authorization"`, `parameters`, `"type": "function"`, `tool_calls`, `choices`, `JSON.parse`. File does NOT contain `fireworks_api_key` or `/v1/v1/chat/completions`.
- **probe:** `cargo test -p blendtutor-cli --test build -- build_webr_ships_the_byok_fireworks_feedback_seam`
- **negative:** Copy `byokAnthropic` verbatim, rename to `byokFireworks`, swap `/v1/messages` → `/chat/completions` and `x-api-key` → `Bearer`, but keep Anthropic `input_schema`, `tool_choice:{type:"tool"}`, and `data.content[].input` parsing (no `JSON.parse`). The 9-token tripwire catches it.
- **verification:** code · Rust `#[test]` token-gate in `crates/cli/tests/build.rs`.
- **fixture status:** NEW — `crates/cli/tests/build.rs` new fn `build_webr_ships_the_byok_fireworks_feedback_seam` at line 424.
- **rubric anchor:** §1.2 (Verdict type reused across backends), §3.4 (FeedbackBackend seam), §4.2 (feedback.js owns the browser BYOK concern), §5 (pure request builder + pure response mapper + effectful getFeedback shell).

## Design Intent

- **Types / interfaces (§1):** The `FeedbackBackend` seam (`{ name, getFeedback(prompt, model) }`) stays unchanged; `byokFireworks` returns the same shape as `byokAnthropic`.
- **Pure / effectful (§2):** `fireworksRequest(prompt, model)` and `fireworksToVerdict(data)` are pure; only `byokFireworks().getFeedback()` performs I/O (fetch → error-check → parse → map).
- **Boundary cuts (§3):** Fireworks backend lives alongside Anthropic in `feedback.js`; no changes to the submit flow, handleSubmit, providerBaseUrl, or model picker.
- **Module responsibility (§4):** `feedback.js` continues to own the entire browser BYOK feedback concern; the Fireworks section is a self-contained block that does not reach into other modules.
- **Function discipline (§5):** `fireworksRequest` builds one request shape; `fireworksToVerdict` maps one response shape; `byokFireworks` composes them in a thin effectful shell.

## Technical Context

- **Files touched:** `crates/core/assets/shared/feedback.js` (added `FIREWORKS_MODEL` const, `fireworksRequest`, `fireworksToVerdict`, `byokFireworks`; updated header comment); `crates/cli/tests/build.rs` (added `build_webr_ships_the_byok_fireworks_feedback_seam`)
- **Architecture notes:** `byokFireworks` is a standalone addition that does NOT modify `handleSubmit` routing — that belongs to AC2. The `FIREWORKS_MODEL` const carries `accounts/fireworks/models/deepseek-v4-flash` (named const, not inline literal).
- **Cross-AC contract:** Fireworks base URL: `https://api.fireworks.ai/inference/v1` (carries trailing `/v1`); AC1 uses `{baseUrl}/chat/completions`. Anthropic base URL: `https://api.anthropic.com` (no `/v1`); uses `{baseUrl}/v1/messages`.

## Dependencies

- **Depends on:** none (Batch 1, first slice).
- **Blocks:** AC2 (provider chooser + routing), AC3 (model discovery).
- **Conflict set:** `crates/core/assets/shared/feedback.js`, `crates/cli/tests/build.rs`.
- **Risk level:** low — mirrors existing Anthropic backend pattern; no security-critical changes.

### Progress
- [x] 2026-06-23 — Red: wrote `build_webr_ships_the_byok_fireworks_feedback_seam` test (9 tripwires + model const + structural pins + mapping pins + negative assertions) — confirmed fail
- [x] 2026-06-23 — Inner loop: added `FIREWORKS_MODEL` const, `fireworksRequest`, `fireworksToVerdict`, `byokFireworks` factory to `feedback.js`; updated header comment to name the second backend
- [x] 2026-06-23 — Green: new Fireworks test passes; all 9 build tests pass; all 102 core unit tests pass; all CLI integration tests pass
- [x] 2026-06-23 — Committed and created PR

### Decision Log
- 2026-06-23 — No ADR needed: the Fireworks backend follows the exact same pattern as the existing byokAnthropic backend (established by ADR-0006/0008). A new ADR would only restate what the code already documents.
- 2026-06-23 — Used quoted property names (`"type": "function"`) for consistency with the test's token assertions; JS unquoted shorthand (`type: "function"`) would also produce a functionally identical object but would not match the `"type": "function"` source token.

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run `cargo test -p blendtutor-cli --test build -- build_webr_ships_the_byok_fireworks_feedback_seam` after any `feedback.js` edit.
- Rollback: `git revert` the AC1 commit.
