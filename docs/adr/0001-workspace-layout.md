# ADR-0001: Workspace layout — `core` library + `cli` binary

- Status: Accepted
- Date: 2026-06-06

## Context

The blendtutor Rust rewrite must author and run R/Python lessons from a CLI
today, and a Tauri desktop GUI is a planned future surface (deferred, see the
feature plan). Both surfaces need the same domain logic: the lesson model,
language runners, grading, and LLM providers. We must decide how to lay out
the crates so that domain logic is reusable without being entangled with
argument parsing or terminal rendering.

## Options

1. **Single crate** — one `blendtutor` binary crate with the domain logic in
   modules. Simplest to start. A future GUI would have to either depend on a
   binary crate's `lib` target bolted on later, or talk to the CLI over a
   process boundary (spawn + parse stdout / JSON-RPC).
2. **Workspace: `blendtutor-core` (lib) + `blendtutor-cli` (bin)** — domain
   logic lives in a library crate; the binary is a thin shell that parses
   arguments and delegates. A future GUI links `blendtutor-core` directly.
3. **Many domain crates** (separate `lesson`, `runner`, `provider` crates) —
   maximal separation, but premature: the module boundaries are not yet proven
   and the churn of splitting/merging crates early is high.

## Decision

Option 2. A two-crate workspace:

- `blendtutor-core` — pure domain logic and effectful adapters. Knows nothing
  about clap or terminals.
- `blendtutor-cli` — a thin binary (`blendtutor`) that parses arguments with
  clap and delegates into `core`.

The dependency edge only ever points **cli → core**; `core` never imports
`cli`. A future Tauri GUI becomes a third workspace member depending on
`core`, reusing the domain logic with no process boundary or serialization
layer between front end and logic.

## Consequences

- The GUI can reuse `core` directly — the main reason to pay the workspace
  cost now rather than retrofitting a `lib` split later.
- Pure/effectful separation is reinforced by the crate boundary: rendering and
  argument parsing physically cannot leak into `core`.
- Small upfront cost: two manifests, a workspace root, and the discipline of
  keeping the binary thin.
- Internal module boundaries inside `core` (Option 3's concern) stay cheap to
  refactor — they are module moves within one crate, not crate splits.
