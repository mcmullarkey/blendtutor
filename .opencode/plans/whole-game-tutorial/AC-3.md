---
ac: 3
depends_on: AC-2
risk: low
status: spec
---

# AC-3 — Document export-quarto as Step 11 in creating-lessons.md

## Executable Spec (resolved)
**Merge decision: Speculator B adopted wholesale** (A's 5 checks subsumed by B's I-series). Key resolver additions: (1) `Depends on: AC-2` — cross-link target ./whole-game.md#quarto-deploy must exist before AC-3 lands (schedule serializes; no file-level conflict); (2) nested-fence guard promoted to HARD predicate clause (≥4 backticks or ~~~ outer fence — 3-backtick truncates under pulldown-cmark, confirmed latent bug at README.md:194-206); (3) verification = `code` + `manual` (tone skim — export-quarto validates lesson first, refusal-arm sentence is tone not pin); (4) probe check 11 uses `-p blendtutor-cli` (crate name, not `blendtutor`).

- **predicate:** `docs/book/src/creating-lessons.md` contains a `## Step 11 —` heading positioned after `## Step 10 —` (Steps 9–10 headings intact → no renumbering breakage); the Step 11 body (a) names the command as `blendtutor export-quarto` with a `.yaml`/`.yml` lesson-path argument (NOT a course dir), (b) shows stdout-redirect guidance to a `.qmd` file (redirect `>` or explicit copy-paste-to-.qmd), (c) shows a fenced-div example whose opening line is byte-identical to real `export_lesson_to_qmd` output for an R lesson — `::: {.blendtutor language="r"}` — with the code fence tagged ` ```r ` (NOT `language="python"` for an R lesson), (d) contains NO `llm_evaluation_prompt` / `gotchas` / `packages` in the example (author-only/out-of-scope fields excluded by the transform), (e) covers the extension install (`quarto add mcmullarkey/blendtutor` or cross-link to README §Quarto Extension) + filter enablement (`filters: [mcmullarkey/blendtutor]` or cross-link) + `quarto render`, (f) does NOT claim `export-quarto` produces a browser site / static site (anti-conflation with `blendtutor build`), (g) carries a cross-link to the whole-game chapter using mdBook's `./whole-game.md` source convention; `mdbook build docs/book` exits 0; the built `docs/book/book/creating-lessons.html` contains the full example (opening `::: {.blendtutor language="r"}` AND the code-template content AND a closing `:::`) as contiguous rendered text — proving no nested-fence collision truncated the example; `cargo run -p blendtutor-cli -- export-quarto <scaffold-lesson>` succeeds (exit 0) and its stdout's first line matches the doc example's opening line.
- **probe:**
```bash
#!/usr/bin/env bash
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"
f=docs/book/src/creating-lessons.md
book=docs/book/book
step11=$(awk '/^## Step 11/{p=1} /^## /&&p&&!/Step 11/{p=0} p' "$f")

# I1 — renumbering integrity: Steps 9,10 intact; Step 11 after Step 10
grep -qF '## Step 9 —' "$f" && grep -qF '## Step 10 —' "$f" && grep -qF '## Step 11 —' "$f"
s10=$(grep -nF '## Step 10 —' "$f" | head -1 | cut -d: -f1)
s11=$(grep -nF '## Step 11 —' "$f" | head -1 | cut -d: -f1)
[ "$s11" -gt "$s10" ]

# I2 — command signature: export-quarto + .yaml lesson path (not course dir)
echo "$step11" | grep -E 'blendtutor export-quarto [^ ]*\.ya?ml'

# I3 — stdout guidance: redirect to .qmd or copy-paste-to-.qmd mention
echo "$step11" | grep -iE '\.qmd|> .*qmd|redirect|copy'

# I4 — example opening line byte-identical to real R output
echo "$step11" | grep -qF '::: {.blendtutor language="r"}'
echo "$step11" | grep -qF '```r'

# I5 — example excludes author-only / out-of-scope fields
! echo "$step11" | grep -qi 'llm_evaluation_prompt'
! echo "$step11" | grep -qi 'gotchas'

# I6 — extension flow: quarto add + filters + quarto render (or README cross-link)
echo "$step11" | grep -qiE 'quarto add mcmullarkey/blendtutor|README.*[Ee]xtension'
echo "$step11" | grep -qiE 'filters: \[mcmullarkey/blendtutor\]|README.*[Ee]xtension'
echo "$step11" | grep -qi 'quarto render'

# I7 — anti-conflation: Step 11 does NOT claim export-quarto builds a browser/static site
! echo "$step11" | grep -qiE 'export-quarto.*(browser site|static site|deploys to)'

# I8 — cross-link to whole-game chapter (mdBook ./whole-game.md convention)
echo "$step11" | grep -qE 'whole-game\.md\b'

# I9 — mdBook build succeeds (nested-fence collision would break/truncate)
mdbook build docs/book

# I10 — built HTML has full example (no fence-collision truncation)
grep -qF '::: {.blendtutor language="r"}' "$book/creating-lessons.html"
grep -qF 'cat("hello' "$book/creating-lessons.html"   # code-template content survived

# I11 — byte-plausible: real export-quarto opening line matches doc example
cargo run -q -p blendtutor-cli -- export-quarto crates/core/src/scaffold/lesson_hello.yaml > /tmp/bt-real-qmd.txt
grep -qF "$(head -1 /tmp/bt-real-qmd.txt)" "$f"

echo "AC-3 OK"
```
- **negative:** Builder adds `## Step 11 — Export to Quarto` with a one-line `blendtutor export-quarto` mention and a FABRICATED fenced-div example copied from memory — `language="python"` for the R scaffold lesson (or a 3-backtick outer fence wrapping 3-backtick inner fences so mdBook truncates the example at the first bare ` ``` `), conflates the output with `blendtutor build` ("export-quarto builds your Quarto site"), omits stdout-redirect guidance, and drops the cross-link — yet a naive `grep export-quarto creating-lessons.md && grep '## Step 11'` passes green.
- **verification:** code (check-docs.sh / mdbook build + grep) + manual (tone skim — succinct, no emojis, exact commands)
- **fixture status:** existing `docs/book/src/creating-lessons.md:405` (Steps 1–10) + NEW Step 11 section; probe is NEW (no existing Step 11 test)
- **rubric anchor:** §4 (Document module responsibility — names what, where, what NOT: export-quarto prints a .qmd snippet, NOT a browser site) + §3 (Cut at the joints — export-quarto ≠ build ≠ eval-report; the doc must not conflate the three outputs)

### Design Intent

`blendtutor export-quarto` is the UNDOCUMENTED bridge from the lesson-YAML authoring world to the Quarto extension authoring world. Authors who follow creating-lessons.md Steps 1–10 produce a browser site via `blendtutor build`; authors who want Quarto-rendered exercises need the fenced-div snippet that `export-quarto` prints to stdout. Step 11 makes that bridge discoverable WHERE USERS LOOK FOR COMMANDS (the creating-lessons walkthrough), not buried in the README extension section. The step covers the full flow — export → fence-div → extension install → quarto render — and cross-links to the whole-game chapter's Quarto deploy section (AC-2) for the deploy story, rather than restating the README's full extension docs. The discoverability rationale (per decomposition): export-quarto is an ALTERNATIVE OUTPUT after Step 10's build, so it slots as Step 11 with NO renumbering of Steps 1–10.

### Technical Context

- **Command semantics** (`crates/cli/src/commands/export_quarto.rs:22-34`): `run(path: &Path)` calls `read_lesson_file(path)` → `Lesson::parse` → `validate_semantics` (so the command VALIDATES the lesson first; an invalid lesson prints to stderr + returns `ExitCode::FAILURE`, distinct from a read error which propagates as `anyhow`). On success, `export_lesson_to_qmd(&lesson)` → `print!("{qmd}")` to STDOUT (no trailing newline beyond what the transform emits). CLI signature: `blendtutor export-quarto <lesson.yaml>` (a lesson PATH, not a course dir — `Commands::ExportQuarto { lesson }` at `main.rs:165`).
- **Transform output** (`crates/core/src/quarto_export.rs:41-99`): opens `::: {.blendtutor language="<r|python>"}\n`, emits prompt as prose, then optional code_template in ` ```<lang> ` fence, optional checks in ` ```{.<lang> .checks} ` fence, optional solution in ` ```{.<lang> .solution} ` fence, optional hints in `::: {.hints}` div, closes `:::\n`. Fence length computed by `fence_for()` (min 3, +1 over longest backtick run in content). EXCLUDES `llm_evaluation_prompt`, `gotchas`, `packages` (author-only / out-of-scope, ADR-0006).
- **Real stdout for scaffold lesson** (`crates/core/src/scaffold/lesson_hello.yaml`, R, no checks/solution/hints):
  ```
  ::: {.blendtutor language="r"}
  Write R code that prints the word "hello" on its own line, using cat().

  ```r
  # Your code here
  cat("hello\n")
  ```
  :::
  ```
  The doc example's opening line MUST be byte-identical to `::: {.blendtutor language="r"}`.
- **Nested-fence collision (CRITICAL sneaky-pass):** The example contains inner ` ```r `...` ``` ` fences. If the doc wraps the example in a 3-backtick outer fence (` ```markdown `), CommonMark/pulldown_cmark closes the outer fence at the first bare ` ``` ` (the inner code block's closing fence) — truncating the example. README lines 194–206 have this latent bug (GitHub renders it loosely; mdBook will not). The doc MUST use a 4+ backtick outer fence (` ````markdown `) or `~~~` tildes, mirroring the `fence_for()` principle the transform itself follows. Probe I9+I10 catch this: `mdbook build` may still exit 0 (no hard failure), but the built HTML loses the code-template content → I10's `grep 'cat("hello'` fails.
- **mdBook link convention** (`docs/book/src/SUMMARY.md`): source links use `./<page>.md` (e.g., creating-lessons.md line 15: `[Example sites](./examples.md)`); built HTML resolves to `./<page>.html`. Cross-link to whole-game chapter → `./whole-game.md` in source (built `./whole-game.html`). Anchor: `./whole-game.md#quarto-deploy` (mdBook auto-slugs headings to kebab-case lowercase).
- **check-docs.sh** (`scripts/check-docs.sh`): runs `mdbook build docs/book` (line 31) — the build is the AC-3 structural gate. No existing `export-quarto` pin (AC-4 adds the `creating-lessons.html contains export-quarto` pin). AC-3 does NOT modify check-docs.sh. AC-3 does NOT touch README (the extension is fully documented at README.md:156–264; Step 11 cross-links rather than restates). No `readme.rs` test affected (AC-3 is docs-only).
- **Step 11 placement:** after `## Step 10 — Build a browser site` (line 362) and before `## Complete example courses` (line 392), keeping "Complete example courses" as the closing section. NO renumbering of Steps 1–10.
- **Expected test migration:** 0 files. AC-3 is docs-only (`docs/book/src/creating-lessons.md`). No Rust/Python test files reference Step 11. The probe surface is `scripts/check-docs.sh` (unchanged by AC-3) + `mdbook build` + grep.

## Dependencies
- Depends on: AC-2 (whole-game.md + Quarto-deploy section must exist for cross-link target; link-correctness ordering, NOT file conflict)
- Blocks: AC-4 (pins built creating-lessons.html contains export-quarto)
- Conflict set: docs/book/src/creating-lessons.md only (AC-2 touches whole-game.md, AC-4 touches check-docs.sh + README — zero file overlap)

## Divergence Log
Predicate = B's 11 assertions wholesale (A subsumed). Nested-fence collision promoted to HARD predicate (built-HTML double-grep catches truncation; mdbook build exit 0 alone insufficient). Cross-link ownership split: AC-3 owns outward link text only; target existence + back-link = AC-2/AC-4 scope. Anchor #quarto-deploy = AC-2-side implementation check (heading must slug). Refusal arm = tone/manual. I11 CI-safe confirmed (offline binary, tracked fixture). README fence latent bug (README.md:194-206) = out of scope (AC-4 append-only territory), surfaced as architecture note. Disagreement = minor.

## Progress
- [ ] Step 11 content in creating-lessons.md — pending

## Decision Log
- 2026-08-10 — Resolver: B adopted; depends_on AC-2 added; fence guard hard; verification code+manual

## Surprises & Discoveries
- (none yet)

## Idempotence & Recovery
- Safe retry: re-run grep probe + mdbook build + cargo run export-quarto
- Rollback: remove Step 11 section from creating-lessons.md