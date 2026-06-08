---
topic: scaffold
created: 2026-06-07
slices: [14]
---

How `blendtutor init <dir>` builds a ready-to-edit course (`core::scaffold`,
`cli::commands::init`). Ports the intent of the R `create_lesson_package()` to a
plain course directory (no R package). Builds on [[course-discovery]] (the
`blendtutor.toml` shape it must emit) and [[lesson-model]] / [[eval]] (the lesson
and eval schemas its templates must satisfy).

- 2026-06-07 (#14): **Plan vs write split (§5.1, §2).** `scaffold_plan() ->
  Vec<FileSpec>` is pure — it names the five starter files as data with no
  filesystem access, so the file set is unit-asserted directly. `scaffold_course`
  is the lone effectful step. Adding a starter file = one `FileSpec` in the plan;
  the writer never changes (§3.2, templates are data).
- 2026-06-07 (#14): **Templates are embedded via `include_str!` from
  `crates/core/src/scaffold/`.** The `.gitignore` template is stored **dotless**
  in-tree as `scaffold/gitignore` and written to `.gitignore` at scaffold time. A
  real `.gitignore` checked into that source dir would act as an ignore file over
  its sibling templates (hiding them from git / tooling) — the dotless name avoids
  that. Same trick any future dotfile template needs.
- 2026-06-07 (#14): **The templates are validated by the production parsers in a
  unit test** (`Lesson::parse`, `parse_eval_suite`, `Manifest::parse`), and the
  test asserts the manifest's one entry path equals the scaffolded lesson
  filename. This is the contract that makes AC1's "`list` discovers ≥1 lesson"
  hold: the scaffold cannot drift into emitting a course `list` would reject
  without failing a core unit test first. If you change a template, that test is
  the canary.
- 2026-06-07 (#14): **Filename conventions are load-bearing for the spec probe.**
  The lesson file must contain `lesson` and not start with `eval_`
  (`lesson_hello.yaml`); the eval is its `eval_<lesson>` sibling
  (`eval_lesson_hello.yaml`) — matching both the `eval <lesson>` sibling-resolution
  convention and the AC1 probe's `find -path '*lesson*.yaml' ! -name 'eval_*'` /
  `find -name 'eval_*.yaml'` globs. Renaming the lesson means renaming the eval
  sibling and the manifest `path` together.
- 2026-06-07 (#14): **Refuse-on-nonempty is a boundary guard before any write
  (§1.3.1).** `scaffold_course` calls `is_empty_target` and returns
  `ScaffoldError::TargetNotEmpty` *before* `create_dir_all` or any
  `fs::write`, so a refused target is never partially clobbered. Empty set =
  {absent dir, existing-but-empty dir}; only a dir with ≥1 entry is refused. The
  existing-but-empty case is the one `mktemp -d` produces, so it had to be allowed,
  not lumped in with non-empty. A `read_dir` error other than `NotFound` (e.g.
  permissions) propagates as `Write`, never silently read as "empty".
