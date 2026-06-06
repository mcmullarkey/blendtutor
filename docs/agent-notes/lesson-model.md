---
topic: lesson-model
created: 2026-06-06
slices: [4]
---

How the lesson schema is typed, parsed, and validated (`core::lesson`). See
ADR-0003 for the decision record.

- 2026-06-06 (#4): The lesson schema is a typed model in `crates/core/src/lesson.rs`,
  not an untyped map. `Lesson::parse(&str) -> Result<Lesson, ValidationError>` is
  the only constructor and the single validation boundary: a constructed `Lesson`
  is valid by type, so the runner/JSON-seam consumers never re-validate. Required
  fields (`lesson_name`, `exercise.prompt`, `exercise.llm_evaluation_prompt`) are
  non-`Option` so a serde-default `None` cannot silently pass.
- 2026-06-06 (#4): `parse` is split into two phases (§5.1): serde-saphyr
  *deserialize* (structure — a missing required field yields
  `ValidationError::Parse` whose message names the field, propagated from serde's
  "missing field `…`") then `validate_semantics` (rules structure can't express).
  Today the only semantic rule is the `{student_code}` placeholder.
- 2026-06-06 (#4): The `{student_code}` rule is a **hard error** here, unlike the
  R package's `validate_lesson()` which only *warns*. A prompt lacking the literal
  `{student_code}` returns `Err(ValidationError::MissingStudentCodePlaceholder)`,
  whose Display names both `llm_evaluation_prompt` and `{student_code}`. Tighter
  than R on purpose (ADR-0003).
- 2026-06-06 (#4): Both `Lesson` and `Exercise` carry `#[serde(deny_unknown_fields)]`,
  so an author's typo in an *optional* field (e.g. `descriptio`) fails validation
  instead of dropping to `None` (§1.3.1). Consequence: every YAML key the example
  lessons use must be modelled — that's why `exercise.type` is modelled as the
  `ExerciseKind` enum (one variant, `function_writing`) rather than ignored. Add a
  variant when a new exercise kind appears (Slice 9), don't relax the deny.
- 2026-06-06 (#4): `Language { R, Python }` and `ExerciseKind` are enums, not
  strings (§1.2) — an unknown value is a parse error. `LessonId` is a newtype with
  a **private** inner `String` + `Display`; print `lesson.lesson_name` directly,
  never reach for `.0` (the CLI leak roborev caught). In v1 `LessonId` carries the
  `lesson_name` value as the lesson's identity; a distinct slug id is deferred to
  discovery (Slice 6).
- 2026-06-06 (#4): Effectful/pure split (§2.2) — `read_lesson_file(&Path)` is the
  thin shell that reads the file then calls the pure `parse`. Its `LoadError` keeps
  read failures (`Read`) distinct from validation failures (`Invalid`) so a missing
  file is never reported as a schema problem. The CLI maps `LoadError` into anyhow
  at the edge via `?`; `core` stays anyhow-free.
- 2026-06-06 (#4): Fixtures live under `crates/core/tests/fixtures/lessons/`
  (the schema's home); the cli integration test references the ported
  `add_two_numbers.yaml` cross-crate via
  `concat!(env!("CARGO_MANIFEST_DIR"), "/../core/tests/fixtures/...")`. The
  `validate` success line is `OK: "<name>" is a valid lesson` — CLI tests assert a
  word-bounded `\bOK\b|\bvalid\b` on stdout so a stray "invalid" can't false-pass.
- 2026-06-06 (#4): YAML crate is **`serde-saphyr`** (`serde_saphyr::from_str`);
  do NOT use `serde_yml` (RUSTSEC-2025-0068) or the archived `serde_yaml`. JSON
  round-trip (the browser-build / `--format json` bridge) uses `serde_json` and is
  asserted by value (`assert_eq!` on the full struct), so a lossy/`skip` serializer
  fails the test.
