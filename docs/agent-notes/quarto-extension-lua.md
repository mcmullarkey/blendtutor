---
topic: quarto-extension-lua
created: 2026-08-02
slices: [129]
---

# Quarto extension Lua filter notes

- 2026-08-02 (#129): **`quarto.utils.relative_to` does not exist** in Quarto
  1.10.18. The `quarto.utils` table exposes `resolve_path`,
  `resolve_path_relative_to_document`, `render`, etc. — but no `relative_to`.
  Do not propose it in specs/ADRs without verifying the Quarto version.
- 2026-08-02 (#129): **`pandoc.path` is POSIX-only.** `pandoc.path.directory`
  on a Windows backslash path (`C:\proj\_extensions\x\blendtutor.lua`) returns
  `"."` and `pandoc.path.is_absolute` returns `false`. For Windows-safe path
  handling in filters, do your own `[/\\]` regex dir-extraction
  (`script:match("^(.*)[/\\]")`) and normalize `\` → `/` in emitted hrefs.
- 2026-08-02 (#129): **`PANDOC_SCRIPT_FILE` has two forms.** Explicit YAML
  filter paths arrive as written (relative to the qmd/project root); by-name
  `_extension.yml` installs arrive ABSOLUTE. On macOS the absolute path root
  can differ from `$PWD` (`/private/var/...` vs `/var/...` — `/tmp` is a
  symlink), so a CWD-prefix strip can silently fail; slicing from the
  `_extensions[/\\]` marker is the robust absolute→project-relative
  conversion for filter asset URLs. Quarto does not rewrite `in-header` hrefs.
- 2026-08-02 (#129): **`quarto render` can mutate sibling files.** Rendering
  `demo-book` added `**/*.quarto_ipynb` to `demo-book/.gitignore` — an
  unrelated side effect that must be reverted, not committed.
- 2026-08-04 (#152): **quarto render auto-creates `<dir>/.gitignore`** (contents: `/.quarto/` + `**/*.quarto_ipynb`) in every directory it renders — not just demo-book. demo-standalone/ got one on render. It shows as untracked `?? <dir>/.gitignore` and must be rm'd before commit (root .gitignore already covers `.quarto/`).
- 2026-08-04 (#152): **root `.gitignore` line `/quarto-fixture/*_files/` only matches depth-1.** test_coi_filter.sh renders into `quarto-fixture/coi-book/*_files/` (nested), which escapes the ignore and shows untracked. Cleanup before commit: `rm -rf quarto-fixture/coi-book/*_files/` (or generalize the ignore line).
