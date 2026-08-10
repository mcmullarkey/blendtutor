---
ac: 2
depends_on: AC-1
risk: medium
status: spec
---

# AC-2 — The whole game chapter (docs/book/src/whole-game.md + SUMMARY.md)

## Executable Spec (resolved)
**Merge decision: Speculator B's 37 clauses adopted wholesale** (A's 7 subsumed). Resolver additions: (1) clause 38 — FIREWORKS_API_KEY prerequisite disclosure (grep; from A's design intent — copy-paste arc dies without the key); (2) clause 5 emoji regex = UNION of B's 6 codepoint blocks + A's \x{FE0F} variation selector + \x{2B00}-\x{2BFF} (catches variation-selector-only sneaky-pass); (3) anchor ownership resolved: AC-2 owns heading "Quarto deploy" (slugs to quarto-deploy), AC-3 owns the back-link; (4) verification = code + manual (tone skim); probe greps source + built HTML, does NOT run the walkthrough commands (behavior pinned by AC-1/3/5 real-binary probes).

- **predicate:** ALL of the following hold (run from repo root after `whole-game.md` + `SUMMARY.md` are written):

  **A. File existence + SUMMARY order + title:**
  1. `docs/book/src/whole-game.md` exists.
  2. `docs/book/src/SUMMARY.md` contains the literal link text `[The whole game](./whole-game.md)`.
  3. In SUMMARY.md, the line containing `whole-game` appears AFTER the `Introduction` line and BEFORE the `Creating Lessons` line.
  4. `whole-game.md`'s first H1 heading is exactly `# The whole game`.

  **B. No emoji (mechanical — perl Unicode codepoint grep):**
  5. `perl -C -ne 'print if /[\x{1F300}-\x{1F5FF}\x{1F600}-\x{1F64F}\x{1F680}-\x{1F6FF}\x{1F900}-\x{1F9FF}\x{2600}-\x{27BF}\x{1F1E6}-\x{1F1FF}\x{FE0F}\x{2B00}-\x{2BFF}]/'` on `whole-game.md` returns zero lines.

  **C. All 8 stages present, in order (init→new→validate→run→eval→eval-report→build→deploy):**
  6. Each of these 8 command strings appears in `whole-game.md`: `blendtutor init`, `blendtutor new lesson --lang r`, `blendtutor validate`, `blendtutor run`, `blendtutor eval` (the eval command, NOT eval-report — matched as `blendtutor eval` followed by a non-hyphen character or end-of-line), `blendtutor eval-report`, `blendtutor build`, `blendtutor export-quarto`.
  7. The first-occurrence line numbers of these 8 commands are strictly monotonically increasing (init < new < validate < run < eval < eval-report < build < export-quarto).

  **D. Scaffold lesson at course ROOT (NOT lessons/):**
  8. `whole-game.md` contains the literal `lesson_hello.yaml` (the scaffold starter at course root).
  9. `whole-game.md` does NOT contain `lessons/lesson_hello.yaml` (scaffold starter is NOT under `lessons/` — that's the `new` command's target, not `init`'s).

  **E. Anti-conflation (export-quarto ≠ build ≠ eval ≠ eval-report):**
  10. `whole-game.md` does NOT contain any string matching `export-quarto.*produces.*site` or `export-quarto.*builds.*site` or `export-quarto.*browser site`.
  11. `whole-game.md` does NOT contain `eval-report.json` (the OLD site-build artifact — the NEW smevals report is `docs/evals/<lesson>/index.html` + `index.json`, a different artifact).

  **F. URL/evidence correctness — /evals/lesson_hello/ NOT /evals/01_seed_data/:**
  12. `whole-game.md` contains the literal `/evals/lesson_hello/` (the scaffold walkthrough's report URL from AC-1).
  13. `whole-game.md` does NOT contain `/evals/01_seed_data/` (the example course's report — wrong course, wrong lesson). Cheapest sneaky-pass: 01_seed_data is the only committed report in the tree.

  **G. Reading-the-report section — REAL report shape (anti-fabrication):**
  14. `whole-game.md` contains a heading (line starting with `#`) matching `reading.*report` (case-insensitive).
  15. `whole-game.md` mentions `index.json` (the summary file with runs/graded/fails/best fields).
  16. `whole-game.md` mentions `grade.yaml` (the per-case grade file with polarity + judge checks).
  17. `whole-game.md` contains the literal `0.8` (the judge-quality threshold — verified against committed grade.yaml).
  18. `whole-game.md` contains all 5 judge metric names verbatim: `verdict_rationale_correctness`, `actionability`, `references_check_results`, `no_solution_leak`, `no_hallucinated_errors` (verified against committed `docs/evals/01_seed_data/.../grade.yaml` — a fabricated description with wrong metric names is the sneaky-pass).
  19. `whole-game.md` mentions `output.txt` (line 1 = `verdict: correct|incorrect`, line 2+ = feedback message).
  20. `whole-game.md` conveys the grade-fail-is-evidence concept: contains the word `evidence` (eval-report exits 0 even when grades fail).

  **H. file:// serve instruction (UX trap):**
  21. `whole-game.md` contains a string matching `http.server` OR `python3 -m http` OR `serve.*over.*http` OR `do not.*file://` OR `file://.*blank` OR `file://.*fail` (smevals SPA index.html uses fetch() which fails on file:// → blank page; MUST tell users to serve over HTTP).

  **I. Both deploy paths:**
  22. `whole-game.md` contains a `build` command with `--target webr` OR `--target pyodide` (Deploy A: standalone static site).
  23. `whole-game.md` contains `quarto add` (Deploy B: Quarto extension install).
  24. `whole-game.md` contains `quarto render` (Deploy B: render the .qmd).

  **J. Cross-links (bidirectional contract):**
  25. `whole-game.md` contains `./creating-lessons.md` (cross-link to the field-reference chapter, NOT a restatement).
  26. `whole-game.md` mentions `README` (cross-link to README's Quarto Extension section).
  27. `whole-game.md` contains a heading (line starting with `#`) matching `[Qq]uarto [Dd]eploy` — mdBook auto-slugs this to `quarto-deploy`, the back-link anchor target for AC-3's cross-link `./whole-game.md#quarto-deploy`. A heading like "Deploying to Quarto" would slug to `deploying-to-quarto` (mismatch → 404).

  **K. Fence collision — if fenced-div example shown, ≥4 backticks:**
  28. IF `whole-game.md` contains `::: {.blendtutor`, THEN it contains at least one line matching `^(`{4,}|~~~+)` (a ≥4-backtick or ~~~ outer fence). A 3-backtick outer fence truncates on the inner ` ```r ` fence — mdBook build exits 0 but the built HTML is missing the example tail.

  **L. Predominantly copy-paste commands (anti-slop proxy):**
  29. `whole-game.md` contains ≥16 lines starting with ` ``` ` (≥8 fenced code blocks — one per stage). <8 fenced blocks = prose-heavy, violating "copy-paste commands".

  **M. mdBook build + built-HTML content integrity (build exit 0 ≠ content correct):**
  30. `mdbook build docs/book` exits 0.
  31. `docs/book/book/whole-game.html` exists.
  32. Built `whole-game.html` contains `evals/lesson_hello` (evidence URL rendered, not just source).
  33. Built `whole-game.html` contains `blendtutor export-quarto`.
  34. Built `whole-game.html` contains `0.8`.
  35. Built `whole-game.html` contains `verdict_rationale_correctness` (catches content swallowed by a render bug).
  36. Built `whole-game.html` contains `creating-lessons.html` (cross-link rendered to built-HTML target, not a dangling `.md` link).
  37. IF `::: {.blendtutor` appears in source, THEN built `whole-game.html` contains `::: {.blendtutor` as rendered text (fenced-div example survived the build).

  **N. FIREWORKS_API_KEY prerequisite disclosure (resolver addition):**
  38. `whole-game.md` mentions `FIREWORKS_API_KEY` (the eval/eval-report stages call the paid judge — the copy-paste arc dies without the key; must disclose the prerequisite, mirroring creating-lessons.md:357-360).

- **probe:**
  ```bash
  #!/usr/bin/env bash
  set -euo pipefail
  cd "$(git rev-parse --show-toplevel)"
  src=docs/book/src/whole-game.md
  sum=docs/book/src/SUMMARY.md
  book_out=docs/book/book

  # === A ===
  test -f "$src"
  grep -qF '[The whole game](./whole-game.md)' "$sum"
  awk '/Introduction/ {i=NR} /\[The whole game\]/ {w=NR} /Creating Lessons/ {c=NR} END {exit !(i>0 && w>i && c>w)}' "$sum"
  grep -qx '# The whole game' "$src"

  # === B ===
  emoji_hits=$(perl -C -ne 'print if /[\x{1F300}-\x{1F5FF}\x{1F600}-\x{1F64F}\x{1F680}-\x{1F6FF}\x{1F900}-\x{1F9FF}\x{2600}-\x{27BF}\x{1F1E6}-\x{1F1FF}\x{FE0F}\x{2B00}-\x{2BFF}]/' "$src" | wc -l)
  [ "$emoji_hits" -eq 0 ]

  # === C ===
  awk '
    /blendtutor init/ && !s1 {s1=NR}
    /blendtutor new lesson/ && !s2 {s2=NR}
    /blendtutor validate/ && !s3 {s3=NR}
    /blendtutor run/ && !/eval-report/ && !s4 {s4=NR}
    /blendtutor eval([^_-]|$)/ && !s5 {s5=NR}
    /blendtutor eval-report/ && !s6 {s6=NR}
    /blendtutor build/ && !s7 {s7=NR}
    /blendtutor export-quarto/ && !s8 {s8=NR}
    END {exit !(s1>0 && s2>s1 && s3>s2 && s4>s3 && s5>s4 && s6>s5 && s7>s6 && s8>s7)}
  ' "$src"

  # === D ===
  grep -qF 'lesson_hello.yaml' "$src"
  ! grep -qF 'lessons/lesson_hello.yaml' "$src"

  # === E ===
  ! grep -qiE 'export-quarto.*(produces|builds).*site|export-quarto.*browser site' "$src"
  ! grep -qi 'eval-report\.json' "$src"

  # === F ===
  grep -qF '/evals/lesson_hello/' "$src"
  ! grep -qF '/evals/01_seed_data/' "$src"

  # === G ===
  grep -qi '^#.*reading.*report' "$src"
  grep -qi 'index\.json' "$src"
  grep -qi 'grade\.yaml' "$src"
  grep -qF '0.8' "$src"
  for m in verdict_rationale_correctness actionability references_check_results no_solution_leak no_hallucinated_errors; do
    grep -qF "$m" "$src"
  done
  grep -qi 'output\.txt' "$src"
  grep -qi 'evidence' "$src"

  # === H ===
  grep -qiE 'http\.server|python3 -m http|serve.*over.*http|do not.*file://|file://.*(blank|fail)' "$src"

  # === I ===
  grep -qE 'build.*--target (webr|pyodide)' "$src"
  grep -qi 'quarto add' "$src"
  grep -qi 'quarto render' "$src"

  # === J ===
  grep -qF './creating-lessons.md' "$src"
  grep -qi 'README' "$src"
  grep -qi '^#.*[Qq]uarto [Dd]eploy' "$src"

  # === K ===
  if grep -qF '::: {.blendtutor' "$src"; then
    grep -qE '^(`{4,}|~~~+)' "$src"
  fi

  # === L ===
  fences=$(grep -cE '^```' "$src")
  [ "$fences" -ge 16 ]

  # === M ===
  mdbook build docs/book
  test -f "$book_out/whole-game.html"
  grep -qF 'evals/lesson_hello' "$book_out/whole-game.html"
  grep -qF 'blendtutor export-quarto' "$book_out/whole-game.html"
  grep -qF '0.8' "$book_out/whole-game.html"
  grep -qi 'verdict_rationale_correctness' "$book_out/whole-game.html"
  grep -qF 'creating-lessons.html' "$book_out/whole-game.html"
  if grep -qF '::: {.blendtutor' "$src"; then
    grep -qF '::: {.blendtutor' "$book_out/whole-game.html"
  fi

  # === N ===
  grep -qi 'FIREWORKS_API_KEY' "$src"

  echo "AC-2 adversarial probe: OK"
  ```

- **negative:** Chapter cites `/evals/01_seed_data/` instead of `/evals/lesson_hello/` (cheapest broken impl — 01_seed_data is the only committed report); fabricated report shape (wrong metric names, wrong threshold — must match committed grade.yaml); missing file:// serve instruction (users open index.html → blank page); 3-backtick fence truncating the quarto fenced-div example (mdBook build exits 0 but built HTML wrong); wrong scaffold path `lessons/lesson_hello.yaml` instead of course-root `lesson_hello.yaml`; emoji present; heading "Deploying to Quarto" slugs to `deploying-to-quarto` instead of `quarto-deploy` → AC-3's back-link 404s; missing FIREWORKS_API_KEY disclosure (copy-paste arc dies without the key).
- **verification:** code (probe — grep chain + mdbook build + built-HTML content greps) + manual (tone skim: "no slop/superfluous text" is subjective — emoji part mechanical, prose-density heuristic via fence-count, genuine slop requires human judgment)
- **fixture status:** `docs/book/src/whole-game.md` (NEW — chapter does not exist); `docs/book/src/SUMMARY.md` (existing:8 lines, modified — insert one line after Introduction). Probe is a one-off bash script (NEW — not a repo test; AC-4 adds the durable check-docs.sh pins).
- **rubric anchor:** §4.1 (document module responsibility — chapter header names what the walkthrough covers, where it lives in the book, what it does NOT cover by cross-linking instead of restating), §3.2 (cut at the joints — cross-links to creating-lessons.md for field reference rather than duplicating; cross-links to README for Quarto extension install rather than restating)

### Design Intent
The whole-game chapter is the R4DS-style "see it all at once" entry point for new users — one continuous walkthrough from `init` to deploy, using the scaffold starter (`lesson_hello.yaml`). Its job is to give a new user a complete mental model of the blendtutor lifecycle in one reading, then cross-link to the field-reference chapters (creating-lessons.md, architecture.md, README) for depth. It is NOT a reference — it does not re-explain every lesson field (that's creating-lessons.md's job) or every deploy header (that's README's job). The "reading the eval report" section is the chapter's unique contribution: no other chapter explains the smevals report shape (index.json, grade.yaml, 5 metrics, 0.8 threshold, file:// gotcha), so this section must describe the REAL committed report, not a fabricated one.

The adversarial lens catches the cheapest broken implementations: wrong evidence URL (01_seed_data instead of lesson_hello); fabricated report shape (invents metric names/thresholds instead of reading committed grade.yaml); missing file:// serve instruction (real UX trap); fence collision (3-backtick outer fence silently truncates); wrong scaffold path (lessons/lesson_hello.yaml instead of course-root).

### Technical Context
**Verified command shapes (from `crates/cli/src/main.rs:31-115`):**
- `blendtutor init <dir>` — positional `dir`. Scaffolds blendtutor.toml, lesson_hello.yaml, eval_lesson_hello.yaml, README.md, .gitignore at course ROOT.
- `blendtutor new lesson --lang r <id>` — nested subcommand `lesson`, `--lang` flag, positional `id`. Writes `lessons/<id>.yaml` (under `lessons/` subdir, NOT course root).
- `blendtutor validate <path>` — positional `path`, optional `--format json`.
- `blendtutor run <lesson> [--code <file>]` — positional `lesson`, optional `--code` (reads from stdin when omitted).
- `blendtutor eval <lesson> [--format json] [--case N]` — positional `lesson`, optional `--format`, `--case`.
- `blendtutor eval-report <lesson>` — positional `lesson`. Output: `docs/evals/<stem>/` where stem = `file_stem(lesson_path)` (verified: `lesson_id_from_path` at `crates/core/src/smevals_gen.rs:352`). For `lesson_hello.yaml` → `docs/evals/lesson_hello/` → Pages URL `/evals/lesson_hello/`.
- `blendtutor build <path> --target <webr|pyodide> -o <out>` — positional `path`, `--target`, `-o`.
- `blendtutor export-quarto <lesson>` — positional `lesson`. Prints fenced-div `.qmd` snippet to stdout. Opening line: `::: {.blendtutor language="r"}` (verified: `crates/core/src/quarto_export.rs:46`).

**Scaffold lesson location (verified: `crates/core/src/scaffold.rs:60-83`):**
- `scaffold_plan()` writes `lesson_hello.yaml` at course ROOT, NOT `lessons/`.
- `add_lesson()` (the `new` command) writes `lessons/<id>.yaml` under `LESSONS_DIR = "lessons"` (line 175).
- Manifest entry for scaffold lesson: `path = "lesson_hello.yaml"` (no `lessons/` prefix).

**Report shape (verified against committed `docs/evals/01_seed_data/`):**
- `index.json`: `{"evals": [{"slug": "...", "runs": N, "graded": N, "fails": N, "best": {...}}]}` — fields: slug, runs, graded, fails, best (config/model/score/runs).
- `grade.yaml` (per case): `outcome: pass|fail`, `score: <float>`, `checks:` array with `checker`, `ok`, `score`, `metrics:` (5 metrics: verdict_rationale_correctness, actionability, references_check_results, no_solution_leak, no_hallucinated_errors — each 0.0–5.0 scale), `notes`. Threshold: 0.8 (case-1 score 0.84 → pass; case-3 score 0.76 → fail — verified).
- `output.txt` (per case): line 1 = `verdict: correct|incorrect`, line 2+ = feedback message.
- `index.html`: smevals SPA — uses `fetch()` which fails on `file://` (CORS) → blank page. Must be served over HTTP (`python3 -m http.server`).

**mdBook conventions (verified):**
- SUMMARY.md links use `./<page>.md` source convention.
- Chapter H1 = `# <Title>` → built HTML → URL `/<slug>.html` (slug = filename stem).
- mdBook auto-slugs headings to kebab-case lowercase: `## Quarto deploy` → id `quarto-deploy`.
- mdBook uses pulldown-cmark; does NOT interpret `:::` as fenced divs (Quarto-only) — `::: {.blendtutor` renders as literal text.
- Fence collision: 3-backtick outer fence around a fenced-div example containing inner ```r fences truncates at the inner fence; build exits 0 but built HTML wrong. ≥4 backticks (or ~~~) needed.

**check-docs.sh (verified: `scripts/check-docs.sh`):**
- Already greps SUMMARY.md for `examples` (line 225) + built HTML for rendered links (lines 231-236).
- AC-4 adds unconditional pins: `grep 'whole-game' SUMMARY.md`, `test -f book/whole-game.html`, `grep 'evals/lesson_hello' book/whole-game.html`, `grep 'export-quarto' book/creating-lessons.html`. Unconditional once AC-1/2/3 land → AC-4 MUST be last.

### Dependencies
- **depends_on:** AC-1 (chapter cites committed report `docs/evals/lesson_hello/` + Pages URL `/evals/lesson_hello/` — hard dependency, AC-1 MUST land first).
- **depended_on_by:** AC-3 (creating-lessons.md Step 11 cross-links to `./whole-game.md#quarto-deploy` — anchor target is AC-2's heading), AC-4 (check-docs.sh pins built whole-game.html for evals/lesson_hello). Both hard downstream: AC-2 MUST land before AC-3 and AC-4.
- **Conflict set:** `docs/book/src/whole-game.md` (NEW), `docs/book/src/SUMMARY.md` (AC-2 is sole modifier). Zero file-level hot conflicts.

## Dependencies
- Depends on: AC-1 (HARD — committed docs/evals/lesson_hello/ report must exist before chapter cites /evals/lesson_hello/ URL; AC-1 MUST land first). Soft forward-ref to AC-3 Step 11 anchor (link may be authored before AC-3 lands).
- Blocks: AC-3 (back-link ./whole-game.md#quarto-deploy targets AC-2's heading), AC-4 (check-docs.sh pins grep built whole-game.html). AC-2 MUST land before both.
- Conflict set: docs/book/src/whole-game.md (NEW, sole owner), docs/book/src/SUMMARY.md (AC-2 sole editor). Zero hot conflicts.
- Risk: medium

## Divergence Log
Predicate depth: B's 37 adopted (A's 7 subsumed — B catches order monotonicity, scaffold-root trap, anti-conflation, real report shape, file:// note, fence collision, built-HTML integrity). Emoji regex: unioned (A's FE0F + 2B00-2BFF added to B's 6 blocks — variation-selector sneaky-pass). FIREWORKS_API_KEY: added as clause 38 (A's design-intent §2 promoted to predicate). Anchor ownership: AC-2 owns heading text (quarto-deploy slug), AC-3 owns back-link. Rubric: §4.1 + §3.2 kept (clauses 25-27 exercise boundary-cut signal). Dependency direction: B correct (AC-3 depends on AC-2's anchor, not vice versa). Disagreement = minor.

## Progress
- [ ] Chapter content + SUMMARY entry — pending

## Decision Log
- 2026-08-10 — Resolver: B adopted + clause 38 (FIREWORKS_API_KEY) + unioned emoji regex; anchor ownership resolved

## Surprises & Discoveries
- (none yet)

## Idempotence & Recovery
- Safe retry: re-run probe (grep chain + mdbook build + built-HTML greps)
- Rollback: remove whole-game.md + SUMMARY entry