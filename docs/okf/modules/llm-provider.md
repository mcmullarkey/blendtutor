---
type: Module
title: llm::provider
description: Closed set of LLM backend metadata.
resource: crates/core/src/llm/provider.rs
tags: [rust, llm, provider, closed-enum]
timestamp: 2026-06-25
pure: true
---

# Responsibility

Enumerates supported LLM backends and their metadata (env var, base URL, default model). Does NOT perform HTTP and does NOT own the API key.

# Interface

- `#[derive(Default)] enum ProviderChoice { Fireworks (default), Anthropic }`
- `pub fn key_var(self) -> &'static str` → `"FIREWORKS_API_KEY"` / `"ANTHROPIC_API_KEY"`
- `pub fn default_base_url(self) -> &'static str`
- `pub fn default_model(self) -> &'static str`

# Dependencies

- Consumed by [llm-feedback](/modules/llm-feedback.md), [run](/modules/run.md), [eval](/modules/eval.md).

# Invariants

- Enum, not string (§1.2). Exhaustive matches, no wildcard — a new provider forces new arms everywhere.
- CLI `run`/`eval` and the browser BYOK path share the Fireworks default (`deepseek-v4-flash`); Anthropic still differs — CLI `claude-sonnet-4-5` vs browser `claude-opus-4-8` — see [js-runtime-seam](/interfaces/js-runtime-seam.md).

# Pure/Effectful

Pure: metadata only.

# Citations

- `crates/core/src/llm/provider.rs`
