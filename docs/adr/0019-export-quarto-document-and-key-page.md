# ADR-0019: `export-quarto` document shape and key-page export

- Status: Accepted
- Date: 2026-09-13

## Context

`blendtutor export-quarto <lesson>` prints a bare `::: {.blendtutor}` div
(ADR-0017 distribution, `core::quarto_export`). Authors pasting that snippet
into their own Quarto project hit three silent failures the snippet cannot
warn about:

- No `filters: [mcmullarkey/blendtutor]` → the div renders as inert prose.
- R exercises without `coi: true` → webR never boots (ADR-0015).
- No page carrying `::: {.blendtutor-key}` → authors copy the demo book's
  `api-key.qmd` by hand (the inline no-key form from issue #186 still works,
  but there is no shareable key-management page).

The export must be able to produce a *renderable* page, and a key page, without
breaking the existing snippet contract that docs and tests depend on.

## Options

1. **Documentation only.** Extend README/whole-game with the missing
   front matter. No interface change, but the failures stay silent and every
   author re-derives the YAML by hand.
2. **Always emit front matter.** Renderable by default, but breaks the
   paste-into-an-existing-page workflow (a second YAML header mid-document is
   literal text) and every existing snippet test.
3. **Opt-in shapes on the same command.** `export-quarto <lesson>` keeps the
   snippet; `--document` wraps it with front matter; `--key-page` (mutually
   exclusive with a lesson path, enforced by clap) prints a complete key page.
   Core models the shape as a sum type, not a bool.

## Decision

Option 3.

- **Shape is a type (§1.2).** `core::quarto_export::ExportShape { Snippet,
  Document }`; `export_lesson_to_qmd(&Lesson, ExportShape)` stays pure (§2.1).
- **Document front matter.** `title` (from `lesson_name`),
  `filters: [mcmullarkey/blendtutor]`, and `coi: true` for R lessons only
  (Pyodide needs no isolation, ADR-0015). R documents carry a YAML comment that
  COI does not function in `type: book` projects (README limitation) so the
  author learns it at export time, not after deploy.
- **Key page.** `core::quarto_export::key_page_qmd()` is a pure constant-backed
  function returning a complete page: front matter with the filter, the
  `::: {.blendtutor-key}` mount div, and the storage/HTTP-serving notes
  mirrored from `demo-book/api-key.qmd`. Its file name must match the filter's
  `bt-key-page` default (`api-key.html`), so the CLI help names `api-key.qmd`.
- **CLI boundary (§1.3).** clap `ArgGroup` requires exactly one of `<lesson>`
  or `--key-page`; `--document` requires `<lesson>`. Invalid combinations fail
  at parse time, never inside core.

## Consequences

- The bare snippet output is byte-identical to before for existing callers.
- Project-level filters: a book that already lists the filter in
  `_quarto.yml` should use the snippet (or delete the page-level `filters`),
  since Quarto merges both lists. Documented in the book guide.
- The key-page text now exists in two places (`demo-book/api-key.qmd` and
  `key_page_qmd`). An integration test renders the exported page's mount div
  and asserts the demo copy keeps the same div, so the two cannot drift on the
  part the runtime depends on.
