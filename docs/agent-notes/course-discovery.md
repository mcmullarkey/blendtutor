---
topic: course-discovery
created: 2026-06-06
slices: [6]
---

How a course directory is described and how its lessons are discovered
(`core::course`, `cli::commands::list`, `cli::output`). See ADR-0004 for the
decision record and [[lesson-model]] for the typed lesson `parse` it builds on.

- 2026-06-06 (#6): The course manifest is **`blendtutor.toml` (TOML)**, not YAML —
  a deliberate split: the manifest is *config* (mirrors `Cargo.toml`), the lessons
  are *content* (YAML, ported from R). TOML uses the `toml` crate
  (`toml::from_str`); YAML still uses `serde-saphyr`. Slices 14 (`init`) / 15
  (`new`) / 16–17 (`build`) all read or write this same `blendtutor.toml` shape, so
  it is a cross-slice contract, not a slice-6-local file.
- 2026-06-06 (#6): A manifest is `Manifest { lessons: Vec<ManifestEntry{ id, path }> }`.
  `id` is a **`LessonSlug` newtype** — the *course-scoped* identity, assigned by the
  manifest, distinct from the lesson's own `lesson_name` *title* (which stays a
  `lesson::LessonId`). This is where ADR-0003's foreseen "distinct slug id" landed:
  in `course`, NOT by repurposing `lesson::LessonId`. So the `list` row's `id`
  (slug) and `title` (lesson_name) come from different sources — a lesson file does
  not know which course lists it.
- 2026-06-06 (#6): **Partial failure is represented, not collapsed.**
  `Course::discover() -> Vec<Result<LessonSummary, DiscoveryError>>` — one row per
  manifest entry. A malformed lesson is an `Err` row carrying its slug; it neither
  aborts the scan (which `Result<Vec<_>>` would) nor disappears (the R
  `list_lessons` silent swallow). `DiscoveryError` wraps the lesson's `LoadError`,
  so the read-vs-validation distinction survives into the row. Tests fix both
  fake-passes (abort → good-count wrong; swallow → broken-row count 0).
- 2026-06-06 (#6): `Course::open(dir)` (effectful: reads `dir/blendtutor.toml`) is
  separate from `discover()` (effectful walk) which feeds the **pure** `summarize`.
  A missing/malformed *manifest* is a whole-course `ManifestError` at `open` —
  propagated to `main` as an anyhow error at the CLI edge (exit nonzero), exactly
  like a read error — whereas a bad *lesson* is a row. Two different failure layers.
- 2026-06-06 (#6): The `list` JSON shape is a **flat array of
  `{id, language, title, error}` rows**, every key always present (the absent side
  `null`): a found row has `error:null`; a failed row has `language:null`,
  `title:null` + the message. `language` is **lowercased to the wire form**
  (`r`/`python`) by `cli::output::language_code` (exhaustive match, §1.2), decoupled
  from the `Language` enum's YAML spelling (`R`/`Python`) — same representation/wire
  split as the validate seam ([[output]]).
- 2026-06-06 (#6): Unlike `validate` (which splits stdout=data / stderr=findings),
  **`list` writes the whole document to stdout** in both formats — an error row is
  part of the listing's data ("which lessons exist, including broken ones"), not a
  diagnostic. `render_list` returns a plain `String` (no `Stream`), still pure and
  snapshot-tested; the cli-owned `ListReport`/`LessonRow` is built from core's
  `discover()` so `render_*` is testable without public `LessonSlug`/`DiscoveryError`
  constructors (no core API widening for tests).
- 2026-06-06 (#6): `Manifest::parse` refuses a lesson `path` that is absolute or
  carries `..` (`ManifestError::UnsafePath`), before any file read (§1.3.1) — a
  shared course can't turn `list` into an arbitrary-file read. The check is
  **lexical only**: it does not resolve symlinks. Empty-slug rejection is deferred
  until `new` (#15) actually *produces* slugs; discovery only reads.
- 2026-06-06 (#6): The human listing reduces **each cell to its first line**
  (`cli::output::first_line`) on purpose — a `DiscoveryError`'s Display is the
  multi-line serde-saphyr parser message, which otherwise sprawls the `ERROR` row
  across a dozen lines and breaks the table's alignment (adversarial pass-1 caught
  it; the snapshot had hidden it behind a clean one-line error). JSON keeps the
  **full** message; human is a one-line-per-row summary (run `validate` for the
  full text). Don't "fix" the flattening thinking it truncates data.
- 2026-06-06 (#6): Aside found while greening this slice — `docs.yml`
  (`cargo doc` with `RUSTDOCFLAGS=-D warnings`) had been **red on staging since #4**:
  a public doc comment linked to a private item, and `rustdoc::private_intra_doc_links`
  under `-D warnings` fails the *whole crate* doc build (not just one item). If you
  write `[`thing`]` linking a private fn from public docs, the docs gate breaks —
  use plain text. Slice 6 swept the existing offenders.
