# ADR-0004: Course manifest and lesson discovery

- Status: Accepted
- Date: 2026-06-06

## Context

Issue #6 ports the R package's `list_lessons()` / `invalidate_lesson_cache()`
(`R/lesson_loader.R`, `R/package_discovery.R`) into the Rust `core`. The R side
scans every *installed package* for a lessons directory, builds an
untyped index, caches it, and **silently swallows** a lesson it cannot read — so
a broken lesson simply vanishes from the list and the author never learns why.

The Rust rewrite is a single binary over a course *directory*, so discovery is
filesystem-scoped (no cross-package cache — that is why `invalidate_lesson_cache`
has no port). Two questions: (1) how is a course described — what is the manifest
and where does it live; (2) how is a course with a malformed lesson represented,
so partial failure neither aborts discovery nor disappears. Later slices depend
on the answer: `init` (#14) scaffolds the manifest, `new` (#15) registers a
lesson in it, `build` (#16/#17) reads lesson titles from it.

## Options

**Manifest format**

1. **YAML manifest**, parsed with the same `serde-saphyr` the lessons use. One
   format, one parser. But it conflates *content* (the lesson) with *course
   structure* (config), and the `init` slice already names the file
   `blendtutor.toml`.
2. **TOML `blendtutor.toml`** at the course root: config-shaped, familiar to the
   Rust/CLI audience (mirrors `Cargo.toml`), cleanly separating the course's
   spine from lesson content. Costs one dependency (`toml`).
3. **No manifest — scan every `*.yaml`.** Zero config, but no stable ids, no
   author-controlled ordering, and no way to keep a non-lesson YAML out. This
   implicit discovery is exactly what made the R side fragile.

**Partial failure**

- **A. `Result<Vec<LessonSummary>, E>`** — abort on the first bad lesson. One
  broken file hides every good one.
- **B. `Vec<LessonSummary>`**, dropping the bad ones (R's silent swallow). Good
  lessons list; broken ones vanish without a trace.
- **C. `Vec<Result<LessonSummary, DiscoveryError>>`** — every manifest entry
  yields a row, success or failure. Partial failure is represented, not collapsed
  (§1.1, §1.2).

## Decision

Manifest = **option 2**, partial failure = **option C**.

- `blendtutor.toml` holds an ordered `[[lessons]]` array of `{ id, path }`. `id`
  is a course-scoped slug — a new `LessonSlug` newtype (§1.4) — and `path` is the
  lesson file relative to the manifest. The slug is distinct from the lesson's own
  `lesson_name` *title*: a lesson does not know which course lists it, so the
  course assigns the id (this is where ADR-0003's foreseen "distinct slug id"
  landed — in `course`, not by repurposing `lesson::LessonId`).
- `core::course` owns `Manifest::parse` (pure TOML → typed), `Course::open(dir)`
  (effectful: reads `dir/blendtutor.toml`), and `Course::discover()` (effectful
  walk that reads each lesson file and feeds the **pure** `summarize`, §2.1/§2.2).
- A missing or malformed *manifest* is a whole-course failure: `Course::open`
  returns an error the CLI propagates at its edge, exactly as a read error is
  propagated, never as a lesson finding. A missing or malformed *lesson* is one
  `Err(DiscoveryError)` row carrying its slug — it does not abort the siblings.
- `core::course` does not re-validate lesson semantics: it calls `lesson::parse`
  in one direction (§3.1) and adds only the course slug and the partial-failure
  shape (§4.1).

## Consequences

- A second parser (`toml`) enters the tree. Justified: the manifest is config and
  the lessons are content, and slices 14/15 already assume `blendtutor.toml`.
- `DiscoveryError` wraps the lesson's `LoadError`, so the slice-4 distinction
  between a read failure and a validation failure survives into a discovery row
  rather than being flattened to a bare string at the boundary.
- The JSON `language` spelling is lowercase (`r`/`python`), rendered in the cli
  seam and decoupled from the `Language` enum's YAML spelling (`R`/`Python`) — the
  same representation/wire split ADR-0003 and the output seam (#5) already use.
- `LessonSlug` is an unvalidated newtype today (any non-empty string an author
  writes). Empty-slug rejection waits for an authoring command (`new`, #15) that
  *produces* a slug; discovery only reads what is already on disk.
- Growing the manifest later (course title, per-lesson metadata) is an additive
  change to `Manifest`; `deny_unknown_fields` makes an author's typo'd key fail
  loudly rather than drop silently, matching the lesson model (ADR-0003).
