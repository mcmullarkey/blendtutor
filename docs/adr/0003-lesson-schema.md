# ADR-0003: Lesson schema — a typed model behind a validating parse boundary

- Status: Accepted
- Date: 2026-06-06

## Context

Issue #4 ports the R package's lesson YAML (`inst/lessons/add_two_numbers.yaml`)
and its `validate_lesson()` (`R/educator_tools.R`) into the Rust `core`. The R
side parses YAML into an untyped list and validates after the fact: required
fields (`lesson_name`, `exercise.prompt`, `exercise.llm_evaluation_prompt`)
*abort*, while the `{student_code}` placeholder rule only *warns*. Downstream R
code then reaches into the list by string key, so a malformed lesson can travel
far before failing.

We need a representation for the Rust rewrite that the browser build (JSON) and
`--format json` (Slice 5) can both consume, and that the runner (Slices 7–9) can
trust without re-checking. The question is where validation lives and how the
shape is typed.

## Options

1. **Untyped map + a `validate()` pass** (port R as-is) — `HashMap`-shaped
   value, validated by a function that returns a list of warnings/errors.
   Faithful to R, but every consumer re-derives structure by key lookup, illegal
   states stay representable, and "valid" is a runtime property no type carries.
2. **Typed model behind a `Lesson::parse(&str)` boundary** — required fields are
   non-`Option`; `Language` is an enum; the lesson identity is a `LessonId`
   newtype; semantic rules (the `{student_code}` placeholder) run inside `parse`
   and return `Err(ValidationError)`. A constructed `Lesson` is, by construction,
   valid — consumers receive the typed value, never the raw YAML (§1.1–§1.4,
   §1.3.1).

## Decision

Option 2. `core::lesson` owns a typed `Lesson`/`Exercise` model, a `Language { R,
Python }` enum (serialized as `R`/`Python` to match the YAML), and a `LessonId`
newtype. `Lesson::parse(&str) -> Result<Lesson, ValidationError>` is the only
constructor: it deserializes structure (serde) then runs `validate_semantics`
(§5.1 split). File reading is a separate effectful `read_lesson_file` (§2.2);
`parse` is pure and string-tested (§2.3).

Two deltas from the R behavior, both deliberate:

- **`{student_code}` is a hard error, not a warning.** Inserting student code is
  the lesson's whole purpose; a prompt missing the placeholder is broken, so
  `parse` refuses it at the boundary (§1.3.1) rather than warning and proceeding.
- **`LessonId` carries the `lesson_name` value as the v1 identity.** The lesson
  file has no separate id field, and a stable slug id belongs with discovery
  (Slice 6, course dir + manifest). The newtype keeps a lesson's identity from
  being confused with arbitrary strings now (§1.4); promoting it to a distinct
  slug later is a localized change inside `lesson`.

YAML is parsed with **`serde-saphyr`** (pure-Rust, no `unsafe`, line-numbered
errors). `serde_yml` is rejected (RUSTSEC-2025-0068, unsound); `serde_yaml` is
archived.

Recorded forward shape (not realized in this slice): check/test bodies are
code-strings executed by the runner (now native, later webR/Pyodide), and eval
cases live in a sibling `eval_<lesson>.yaml` so lesson files stay
learner-shippable. A future ADR locks those when Slices 7–12 implement them.

## Consequences

- A constructed `Lesson` is valid by type; the runner and the JSON seam consume
  it without re-validating, and a representation change to the YAML stays inside
  `core::lesson` (§3.2).
- The entry points are few — `Lesson::parse` and `read_lesson_file`, returning
  `ValidationError`/`LoadError`. The remaining public names (`Exercise`,
  `Language`, `LessonId`, `ExerciseKind`) are the lesson value's own structure,
  public because they are fields of a `Lesson` callers read and serialize, not
  separate concerns — so the surface sits a little above the §4.3 ~5 soft
  ceiling for a cohesive reason rather than from sprawl. `core::lesson` never
  executes code, calls LLMs, or renders output (§4.1).
- Unknown YAML keys are **rejected** (`#[serde(deny_unknown_fields)]`), so an
  author's typo in an optional field (e.g. `descriptio`) fails `validate`
  instead of silently dropping to `None` (§1.3.1) — the right trade for a
  hand-authored, learner-shippable format. The one key the R example carries,
  `exercise.type`, is therefore modelled as an `ExerciseKind` enum (one variant,
  `function_writing`, matching today's lessons) rather than ignored; new kinds
  are added as the runner supports them (Slice 9), not speculatively.
- Stricter than R: a `{student_code}`-less prompt that the R package merely
  warned about now fails `validate`. This is the intended tightening, called out
  here so the behavior change is not a surprise at cutover (Slice 20).
