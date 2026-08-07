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
- 2026-08-06 (#164): **Pandoc 3.x meta strings are structured, not plain Lua strings.** `doc.meta["k"]` for YAML `k: "false"` arrives as a list of Inlines (`[1] = Str "false"`) — `type(x) == "string"` is FALSE. Any filter opt-out/flag read of a YAML string must normalize via a `meta_string()` helper (handle plain string, `.text`, and `[1].text` forms). The pre-existing `bt-auto-bootstrap` string-form branch was silently dead for this reason; same trap bit `bt-feedback` and `bt-key-page` reads in #164.
- 2026-08-06 (#164): **`printf '%s' "$content" | grep -qF` in test scripts has a SIGPIPE race.** `grep -q` exits as soon as it matches; if the match is early in a large content string, printf is still writing → broken pipe → under `set -o pipefail` the pipeline exits non-zero → the token check false-negatives. Intermittent and token-position-dependent. Use a here-string: `grep -qF "$token" <<< "$content"` (no pipe).
- 2026-08-06 (#164): **`set -uo pipefail` kills shell test scripts during the red phase.** A command-substitution grep for a legitimately-absent token (grep exit 1) aborts the whole script before later clauses run. All `$(... | grep ... || true)` substitutions in BDD test scripts need the `|| true` guard.
- 2026-08-06 (#164): **Concurrent quarto renders of the same fixture tree conflict.** Two render-assert scripts running in parallel (e.g. test_quarto_bootstrap.sh + test_quarto_asset_deployment.sh both render quarto-fixture/mixed-lang.qmd) race on `rm -rf`/write of the shared output — renders fail, HTML goes missing, tests fail spuriously. Run quarto render-assert suites SEQUENTIALLY. A timed-out run can also leave an orphaned `quarto.js render` process holding a lock and pegging CPU — `pkill -f "quarto.js render"` before re-running.
