# Architecture

`blendtutor` is a Cargo workspace of two crates:

- **`blendtutor-core`** — the reusable library: the lesson model, language
  runners (R and Python), grading, and LLM providers, built up slice by slice.
  It holds all domain logic and is free of `anyhow`; error reporting lives at the
  CLI edge.
- **`blendtutor-cli`** — a thin command-line shell (binary `blendtutor`) that
  parses arguments and delegates to `core`.

The dependency only ever points **cli → core**; `core` never imports `cli`
(ADR-0001). That boundary is what lets a future GUI reuse `core` directly.

## How the docs mirror the code

This book is structured to track the `core` module boundaries: as top-level
modules land (lesson model, runners, grading, providers), each gets a narrative
chapter here, while the generated [API reference](./api-reference.md) documents
the corresponding public surface. Prose explains *why* a module exists and how it
fits; rustdoc documents *what* each item is. Keeping them in their native tools —
and merging the rustdoc output under [`/api`](./api/blendtutor_core/index.html) at
deploy time — avoids duplicating the API surface by hand.
