# ADR-0018: Script-relative asset path resolution via `PANDOC_SCRIPT_FILE`

- Status: Accepted
- Date: 2026-08-02

## Context

`blendtutor.lua` hardcodes asset paths as `_extensions/blendtutor/assets/...`
(`COI_SCRIPT_PATH` / `STYLES_CSS_PATH`). This works for the in-repo layout,
but `quarto add mcmullarkey/blendtutor` installs the extension to
`_extensions/mcmullarkey/blendtutor/` (org/repo path, verified empirically —
see ADR-0017). With a hardcoded prefix the emitted `<link href=...>` /
`<script src=...>` points at `_extensions/blendtutor/assets/styles.css`, which
does not exist in the installed project — the browser 404s (lstat NotFound).

The filter must derive asset URLs from its own install location so the same
code works under in-repo, installed org/repo, and `../`-referenced layouts.

`PANDOC_SCRIPT_FILE` (the path to the executing filter) arrives in two forms,
verified with Quarto 1.10.18:

- **Explicit YAML filter path** — as written, relative to the project root
  (e.g. `../_extensions/blendtutor/blendtutor.lua` from `quarto-fixture/ux.qmd`,
  or `_extensions/mcmullarkey/blendtutor/blendtutor.lua` from a project root).
- **By-name install** (`filters: [blendtutor]` via `_extension.yml`
  `contributes.filters`) — an **absolute** path into the project
  (e.g. `/abs/proj/_extensions/mcmullarkey/blendtutor/blendtutor.lua`).
  On macOS the absolute path may use a different root than `$PWD`
  (`/private/var/...` vs `/var/...` — `/tmp` is a symlink).

Quarto does not rewrite `in-header` hrefs, so an absolute script path must be
converted back to a project-relative URL or the browser 404s.

## Options

1. **Keep hardcoded `_extensions/blendtutor/` constants.** Zero code change,
   but broken for every `quarto add` install (the bug being fixed). Rejected.

2. **Naive dir-extraction of `PANDOC_SCRIPT_FILE`**
   (`script_file:match("^(.*)[/\\]") .. "/assets/"`). Correct for relative
   explicit paths, but emits `/abs/proj/.../assets/styles.css` for by-name
   installs — the file exists on disk (lstat passes) but the browser 404s
   because the href is a filesystem path, not a URL. Rejected (clause 4).

3. **`quarto.utils.relative_to` under Quarto, CWD-strip fallback.**
   Proposed during design, but `quarto.utils.relative_to` does **not** exist
   in Quarto 1.10.18 (verified: `quarto.utils` exposes `resolve_path`,
   `resolve_path_relative_to_document`, etc., but no `relative_to`). A pure
   `$PWD`-prefix strip also fails on macOS `/private/var` vs `/var`. Rejected.

4. **`pandoc.path.directory` + `pandoc.path.is_absolute`.**
   Portable on POSIX, but `pandoc.path` is POSIX-only — a Windows
   `C:\proj\_extensions\...` path returns `directory = "."` and
   `is_absolute = false` (backslash is not a separator for `pandoc.path`).
   Rejected for Windows (code-review requirement).

5. **Own `[/\\]` dir-extraction + `_extensions/` slice for absolute paths.**
   Extract the directory with a Windows-safe `[/\\]` pattern, append
   `/assets/<filename>`, and for absolute results (leading `/` or drive
   letter) slice from the first `_extensions[/\\]` occurrence — which is
   always present for a filter install — falling back to stripping the `$PWD`
   prefix. Normalize backslashes to forward slashes in the emitted href.

## Decision

Option 5 — a pure `resolve_asset_path(script_file, filename)` function in
`blendtutor.lua` (§5 single-responsibility; §2 pure core, effectful emission
stays in `Pandoc()`):

- Directory extraction handles both `/` and `\` separators.
- Absolute paths (by-name install) are converted project-relative by slicing
  from `_extensions/` — robust against macOS `/private/tmp` vs `/tmp`
  root-mismatch — with a `$PWD`-prefix strip as last-resort fallback.
- Emitted hrefs are normalized to `/` (no backslashes).

Behavior contract (§3.2): in-repo → `_extensions/blendtutor/assets/...` when
referenced from a root qmd, `../_extensions/blendtutor/assets/...` from
`quarto-fixture/`-style subdirectories; installed → `_extensions/mcmullarkey/
blendtutor/assets/...`; demo-book/fixture → `../_extensions/blendtutor/
assets/...`. All browser-usable.

## Consequences

- `blendtutor.lua` contains no literal `_extensions/blendtutor/` string — the
  source-grep-zero invariant (spec clause 6) is a regression guard.
- The filter assumes nothing about its install location — it derives from its
  own `PANDOC_SCRIPT_FILE` (§1.5 encode invariant in types).
- `quarto-fixture/` renders (ux.qmd, coi-true.qmd) now emit `../_extensions/`
  hrefs that resolve to the repo-root extension without a symlink.
- **Future work (out of scope, pre-existing):** `coi-serviceworker.js` scope
  defaults to the script's URL directory at runtime; deeply nested install
  paths can leave the page outside the service-worker scope so COI headers are
  never applied. Orthogonal to this ADR — deferred to a future COI AC.
- `docs/adr/0015` line 56's literal path note is superseded by this ADR's
  derived-path contract.
