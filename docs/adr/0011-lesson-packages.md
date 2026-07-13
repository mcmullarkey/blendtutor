# ADR-0011: Lesson packages field threaded through runners

- Status: Accepted
- Date: 2026-07-13

## Context

Issue #72 adds an optional `packages: Vec<String>` to the lesson schema so a
lesson author can declare third-party dependencies (e.g. `pandas`, `purrr`).
These packages must cross four boundaries:

1. **Parse boundary** — lesson YAML → `Lesson` struct (`lesson.rs`).
2. **Rust↔JS contract** — `Lesson` → `SiteLesson` JSON (`site/mod.rs`). The
   browser runtime reads `lesson.packages` to call `webR.installPackages` or
   `pyodide.loadPackage`.
3. **Lesson↔runner boundary** — `select_runner` → `PythonRunner::new`
   (`grade.rs`). The local Python runner spawns `uv run --with <pkg>` so the
   submission can `import pandas`.
4. **Core→adapter boundary** — `lesson-runner-core.js` → target adapter
   `run(code, checks, packages)`. The shared core passes packages as a 3rd
   argument; the adapter installs them before evaluating.

The `Runner` trait (`execute(&self, code, checks)`) is the seam every execution
and grading slice depends on (ADR-0005, §3.4). A Python-specific concern (uv
`--with`) must not pollute this language-agnostic trait.

## Options

1. **Add `packages: &[String]` parameter to `Runner::execute`.** Every runner
   receives packages on every call. R discards them silently. This churns the
   trait interface for ALL runners when only Python needs it — violating §3.4
   (the trait is the seam; a language-specific concern does not belong here).
   It also forces every future `impl Runner` to accept and ignore packages.

2. **Packages as constructor state on `PythonRunner`.** The `Runner` trait
   stays unchanged — `execute(&self, code, checks)` keeps its signature.
   `PythonRunner::new(timeout, packages)` stores packages at construction;
   `execute` builds the `uv run --with` command from the stored packages.
   `select_runner(language, packages)` is the lesson→runner boundary where
   packages are injected. `RRunner` is unchanged — no packages field, no
   `--with`. The `Interpreter` struct's `code_args` changes from
   `&'static [&'static str]` to `Vec<String>` to allow runtime-built arg lists
   (Python injects `--with <pkg>` per package; R builds `vec!["--vanilla",
   "-e"]` dynamically but its invocation is byte-identical).

## Decision

Option 2.

- **Schema (§1.1).** `Lesson.packages: Vec<String>` with `#[serde(default)]` —
  mirrors `checks`: "no packages" is an empty vec, not `Option`. Every existing
  lesson stays valid without a `packages` key.
- **Contract shape (§3.2).** `SiteLesson.packages: Vec<String>` is always
  serialized — empty array when absent, never `null` and never omitted. Stable
  contract shape so the JS runtime never handles absence (mirrors the
  `solution: null` precedent from ADR-0008).
- **Trait unchanged (§3.4).** `Runner::execute(&self, code, checks)` signature
  is load-bearing — packages are constructor state on `PythonRunner`, not a
  per-call parameter. A Python-specific concern (uv `--with`) stays out of the
  language-agnostic trait.
- **select_runner boundary (§3.1).** `select_runner(language, packages)` is
  where packages cross from lesson to runner. The `match` is exhaustive: `R`
  ignores packages (`RRunner::default()`), `Python` threads them
  (`PythonRunner::new(timeout, packages.to_vec())`).
- **Interpreter type relaxation.** `Interpreter.code_args` changes from
  `&'static [&'static str]` to `Vec<String>`. This forces both `PYTHON_INTERPRETER`
  and `R_INTERPRETER` from `const` to runtime-built values, but R's invocation
  stays byte-identical (`Rscript --vanilla -e`). The cost is a small `Vec`
  allocation per `execute` call — negligible for the trusted-local model.
- **JS adapter contract.** `runtime.run(code, checks, packages)` — the shared
  core passes `lesson.packages ?? []` as the 3rd argument. Each adapter MUST
  declare the 3rd param (preventing JS silent arg-drop) AND wrap
  `installPackages`/`loadPackage` in the existing try/catch (install failure →
  `{ output: err, ok: false }`, not a crash).

## Consequences

- `Interpreter` can no longer be a `const` — both runners build it at call
  time. R's args are fixed; Python's are derived from stored packages.
- `select_runner` signature change ripples to all callers: `run_lesson`
  passes `&lesson.packages`; grade tests pass `&lesson.packages` or `&[]`.
- `PythonRunner::new` signature change ripples to `runner.rs` integration tests
  and `grade.rs`'s `select_runner` — all updated in this slice.
- `PythonRunner::default()` still exists (30s timeout, empty packages) for
  backward compatibility, but `select_runner` uses `new(timeout, packages)`.
- Future runners (e.g. a hosted backend) that need packages follow the same
  pattern: store at construction, use in `execute`. The trait stays clean.
- AC-3 (R course using purrr/dplyr) and AC-4 (Python course using pandas) can
  now declare `packages:` in their lesson YAML — this slice unblocks them.
