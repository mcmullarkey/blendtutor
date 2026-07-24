# ADR-0017: Quarto extension distribution via `quarto add` with same-repo demo book

- Status: Accepted
- Date: 2026-07-23

## Context

The blendtutor Quarto extension lives in `_extensions/blendtutor/` within the
main repository. Authors need a way to install it into their own Quarto
projects, and they need documentation and a demo book to learn the authoring
syntax.

The distribution mechanism must be:
- **Native to Quarto** — authors should not need to manually copy files.
- **Verifiable in CI** — the install path must be tested, not assumed.
- **Self-documenting** — the README is the primary onboarding interface (§4).

Three concerns drive this ADR:

1. **Install mechanism**: `quarto add <org>/<repo>` vs. manual `_extensions/`
   copy vs. a separate distribution repo.
2. **Demo book location**: same repo vs. separate repo vs. no demo book.
3. **CI verification**: how to prove `quarto add` works without a live GitHub
   clone in every CI run.

## Options

1. **Manual `_extensions/` copy with no CI verification.** Authors clone the
   repo and copy `_extensions/blendtutor/` into their project. Simplest, but
   untestable — there's no guarantee the extension installs cleanly via
   Quarto's native mechanism. Violates §5 (no verification path).

2. **Separate distribution repo with `quarto add`.** Create a dedicated
   `blendtutor-quarto` repo containing only `_extensions/blendtutor/`. Authors
   run `quarto add mcmullarkey/blendtutor-quarto`. Clean separation (§3), but
   doubles the maintenance surface — every asset sync must target two repos,
   and the demo book would need its own home.

3. **Same-repo distribution via `quarto add` with CI verification.** The
   extension stays in `_extensions/blendtutor/` in the main repo. Authors run
   `quarto add mcmullarkey/blendtutor`. CI verifies the install in a temp
   directory. The demo book lives in `demo-book/` in the same repo.

## Decision

Option 3 — same-repo distribution via `quarto add` with CI verification.

- **Install command**: `quarto add mcmullarkey/blendtutor` — Quarto's native
  extension discovery clones the repo and copies `_extensions/blendtutor/`
  into the target project. No manual file copying.

- **CI verification**: A `quarto-distribution` CI job creates a temp directory
  via `mktemp -d`, runs `quarto add mcmullarkey/blendtutor --no-prompt`, and
  asserts `_extensions/blendtutor/_extension.yml` and `blendtutor.lua` exist.
  No `continue-on-error` — a failed install fails the job.

- **Demo book**: `demo-book/` in the same repo, with `_quarto.yml`
  (`project: type: book`), R and Python exercise chapters, COI config, and no
  `llm_evaluation_prompt` (per ADR-0006 — prompt structure is fixed, authors
  do not provide evaluation prompts).

- **README as interface (§4)**: The README documents the install command,
  authoring syntax for both languages, BYOK, minimum Quarto version (>= 1.4),
  and links to the demo book.

## Consequences

- `quarto add` requires the repo to be public (or the author to have access).
  The repo is public, so this is not a constraint.
- The demo book's filter path (`../_extensions/blendtutor/blendtutor.lua`) is
  relative to the repo root, so the demo book must be rendered from within the
  repo. Authors who clone the repo can render it directly; authors who install
  via `quarto add` get the extension but not the demo book (by design — the
  demo book is documentation, not a runtime dependency).
- CI's `quarto add` step clones the repo from GitHub, which adds network
  latency. This is acceptable — it's the real install path, not a mock.
- The `quarto-distribution` job is separate from `quarto-render` to isolate
  install failures from render failures.
