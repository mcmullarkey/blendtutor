# ADR-0008: Static site build — lesson-JSON contract and the browser runtime seam

- Status: Accepted
- Date: 2026-06-08

(The plan refers to this as "ADR-0005"; that number was already taken by the
runner ADR. The static-site-build ADR is 0008.)

## Context

Issue #16 (Slice 16) adds `blendtutor build --target webr <course>`: an R course
compiles to a static, browser-deployable site where the learner runs code and
checks client-side in webR, with no R install. Later slices add a Pyodide target
(Slice 17), in-browser BYOK LLM feedback (Slice 18), and replace the R-package
workflows. So this slice must establish three things the later ones build on: the
boundary between Rust (author side, assembles the site) and the JS runtime
(learner side, runs code), how a target is selected, and how the site is made
deployable to GitHub Pages — which cannot set the COOP/COEP headers webR needs
for `SharedArrayBuffer`.

## Options

1. **Serialize the internal `Lesson` as the JS contract; generate HTML/JS from
   Rust templates.** Fewer types. But it welds the in-browser runtime to the
   internal schema — every Rust-side schema change (a grading field, a rename)
   ripples into JS, and the client would receive the `llm_evaluation_prompt` it
   has no use for (§3.2 leak). Generating the runner from Rust templates also
   buries real client logic inside Rust string-building (§4.1, §2.1 blur).
2. **A dedicated `SiteLesson` DTO as the contract; ship the runner as static
   asset files.** One extra type and a directory of hand-written assets. The JS
   runtime depends only on the DTO's JSON shape, so a Rust refactor that keeps the
   shape never touches JS (§3.2). The runner HTML/JS are authored as real client
   files; Rust only assembles and injects lesson JSON (§4.1).

## Decision

Option 2.

- **Contract (§3.2).** A `SiteLesson` DTO — `id` (course slug), `title`, `prompt`,
  `code_template`, `checks`, `solution` — is the Rust↔JS boundary, serialized to
  JSON. It is *not* the internal `Lesson`: it drops `llm_evaluation_prompt`
  (server/CLI concern, not shipped to the client) and flattens to what the runner
  needs. A Rust-side change invisible to this JSON shape is invisible to JS.
- **Reference solution (§1.1).** A lesson gains an optional
  `exercise.solution` — the known-correct answer, author-provided. `Option`, not
  required, so every existing lesson stays valid; the site's self-verification and
  the AC2 browser probe consume it. It rides the JSON contract as `solution`.
- **Assets are static templates (§4.1).** `index.html`, `lesson-runner.js`, and
  `coi-serviceworker.js` live under `crates/core/assets/<target>/` and are
  embedded with `include_str!`. They are real client code, not generated logic;
  the build copies them verbatim and adds the per-course lesson JSON. `core::site`
  assembles a site — it does **not** call LLMs or execute code at build time.
- **Pure/effectful split (§2.1, §2.3).** `plan_site(&[(LessonSlug, Lesson)],
  BuildTarget) -> Result<SiteFiles, PlanError>` is pure: it decides which files
  exist and their contents (assets + serialized lessons), with no filesystem
  touch, so the whole site is snapshot-testable. `write_site(&Path, &SiteFiles)`
  is the single effectful step. Loading the course's full lessons is a separate
  effectful `Course::load_lessons` (fail-fast: a site cannot ship a broken
  lesson, unlike `list`'s partial tolerance).
- **Target seam (§1.2, §3.4).** `BuildTarget { Webr, Pyodide }`; each target
  carries the language it serves (Webr→R, Pyodide→Python). `plan_site` **refuses a
  language/target mismatch before emitting anything** (§1.3.1) — an R course built
  for Pyodide errors with `PlanError::LanguageMismatch` and no file is written. A
  second target is a new asset set + match arm, reusing the contract unchanged
  (Slice 17).
- **COOP/COEP shim.** GitHub Pages cannot set response headers, but webR needs
  cross-origin isolation for `SharedArrayBuffer`. The site bundles the
  `coi-serviceworker` shim (a service worker that re-serves responses with the
  COOP/COEP headers) and references it from `index.html`. A present-but-unreferenced
  shim is a defect the AC1 test guards against.
- **Single-language site, v0.** A built site serves one language/target. Mixed
  R+Python sites are out of scope (issue #16); they would be a later composition
  over this same contract, not a change to it.

## Consequences

- The in-browser runner (and Slice 18's BYOK feedback) depend on the `SiteLesson`
  JSON shape, never on `core::lesson` internals; the Pyodide target (Slice 17) is
  an additive `impl` over `plan_site`/`write_site` and the same contract (§3.2,
  §3.4).
- `serde_json` becomes a runtime dependency of `core` (it was dev-only): the build
  serializes lessons at run time, not just in tests.
- The runner JS/HTML cannot be unit-tested from Rust — they are verified in a real
  browser (rodney) against a served `out/`, the AC2 convention. The `solution`
  field ships ahead of Slice 18's feedback layer, a deliberate forward-shape
  commitment so the contract does not churn when that slice lands.
- `solution` is optional, so a lesson without one builds a site whose runner has
  nothing to self-submit; that is acceptable for v0 (the field is an authoring aid
  the browser probe uses), and a future slice may require it per target.
