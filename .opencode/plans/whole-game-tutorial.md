# Plan: whole-game-tutorial

## Feature Goal

Give new users one continuous end-to-end walkthrough of the blendtutor authoring lifecycle, in the spirit of the R4DS "Whole Game" chapter but package-scale and ruthlessly succinct: scaffold a course → add/edit a lesson → validate → run → eval → generate + read the eval report → deploy as (a) a chapter in a Quarto book via the `blendtutor` Quarto extension and (b) a standalone static site on GitHub Pages. Exact copy-paste commands, minimal prose, no emojis. It lands as a new mdBook chapter (`docs/book/src/whole-game.md` + SUMMARY entry) shipped with the published docs site, and as a side effect documents the currently-undocumented `export-quarto` command in `creating-lessons.md` so it is discoverable outside the walkthrough.

## User Decisions (binding)

- Title: "The whole game" (SUMMARY: Introduction → The whole game → Creating Lessons → ...)
- AC-1: FRESH committed eval report for scaffold lesson_hello (docs/evals/lesson_hello/), not citing 01_seed_data
- AC-3: FULL export-quarto flow documented (creating-lessons.md Step 11)
- AC-4 pins: LOCAL-ONLY enforcement (check-docs.sh; CI docs.yml not touched — decomposition's "fail CI" wording was inaccurate)
- Style: no emojis, no slop, exact commands

## ACs

### AC-1 — Commit real eval-report evidence for scaffold lesson_hello

**Executable Spec (resolved):** Speculator B's spec adopted wholesale (A's structural checks subsumed; B's anti-canned-copy content assertions load-bearing). Verification = `code` (one-off python3+git probe, not a repo test). Rubric anchor = none (evidence-integrity AC).

**predicate:** ALL of the following hold on the committed tree (run from repo root after `git commit`):

**Existence + stem contract:**
1. `docs/evals/lesson_hello/` is a directory (not file, not symlink).
2. `docs/evals/hello/` does NOT exist (manifest id `hello` ≠ file stem `lesson_hello` — wrong-stem sneaky-pass).
3. `docs/evals/lesson_hello/index.json` exists and parses as JSON.
4. `docs/evals/lesson_hello/evals/lesson_hello/eval.json` exists and parses as JSON.

**eval.json is FOR lesson_hello (anti-canned-copy):**
5. `eval.json` → `.eval.name` == `"lesson_hello"` (not `"01_seed_data"`, not `"hello"`).
6. `eval.json` → `.eval.description` contains substring `"hello"` AND `"cat()"` (lesson_hello's exercise prompt — rules out 01_seed_data whose description mentions `survey_data`).
7. `eval.json` → `.eval.tasks` is an array of exactly 2 elements (lesson_hello has 2 cases; 01_seed_data has 4).
8. `eval.json` → `.eval.tasks[0].prompt` == `"cat(\"hello\\n\")"` AND `.eval.tasks[0].expected` == `"correct"`.
9. `eval.json` → `.eval.tasks[1].prompt` == `"cat(\"goodbye\\n\")"` AND `.eval.tasks[1].expected` == `"incorrect"`.
10. `eval.json` → every `.eval.tasks[].lesson` ends with `"/lesson_hello.yaml"` (absolute path baked in is the known AC-5 issue — acceptable; but the FILENAME must be lesson_hello.yaml, not 01_seed_data.yaml).

**Runs are real (anti-stub, anti-stale):**
11. `docs/evals/lesson_hello/evals/lesson_hello/runs/` contains subdirs `case-1` and `case-2`.
12. Each case's deepest timestamped run dir contains `output.txt` whose first line matches `^verdict: (correct|incorrect)$` (the runner contract — a stub would omit this).
13. Each case's `run.yaml` → `task.lesson` ends with `/lesson_hello.yaml` (anti-wrong-lesson).
14. Each case's `run.yaml` → `task.prompt` is `cat("hello\n")` (case-1) or `cat("goodbye\n")` (case-2) (anti-wrong-lesson).
15. `index.json` → `.evals[0].slug` == `"lesson_hello"` AND `.evals[0].runs` == 2 AND `.evals[0].graded` == 2.

**Commit hygiene (anti-sweep, anti-leak):**
16. `git ls-files docs/evals/lesson_hello/` is non-empty (the tree is tracked).
17. `git ls-files | grep -c '\.smevals'` == 0 (ephemeral dir never committed).
18. `git status --porcelain` is empty (clean working tree — no untracked .smevals, no swept 01_seed_data changes).
19. Every path in `git diff --name-only HEAD~1 HEAD` starts with `docs/evals/lesson_hello/` (scoped commit — no unrelated docs/evals/ changes swept in by `git add docs/evals/`).

**probe:**
```bash
# Prerequisite (manual, real-key — builder runs locally, NOT in CI):
# From repo root. Init temp course in /tmp so .smevals can never leak into repo.
cargo run -p blendtutor-cli -- init /tmp/hello-course
FIREWORKS_API_KEY=<real-key> cargo run -p blendtutor-cli -- eval-report /tmp/hello-course/lesson_hello.yaml
# eval-report exits 0 (grade-fail-is-evidence: even all-fail still exits 0 if runs recorded)
git add docs/evals/lesson_hello/
git commit -m "docs(evals): commit real eval-report for lesson_hello"
```
Structural assertions (code — python3 inline script, runnable from repo root after commit): a python3 heredoc asserting all 19 clauses (dir existence, wrong-stem absence, JSON parse, eval.name/description/task-count/prompts/expected, lesson-path filename, runs case-1/case-2 with `^verdict:` first line + run.yaml lesson/prompt, index.json slug/runs/graded, git ls-files non-empty, zero `.smevals` tracked, clean `git status --porcelain`, scoped `git diff --name-only HEAD~1 HEAD`). Full script in AC-1.md.

**negative:** Cheapest broken impl = copy `docs/evals/01_seed_data/` → `docs/evals/lesson_hello/` and commit (passes existence, fails 5-9). Second = create `docs/evals/hello/` using manifest id instead of file stem (breaks URL contract `/evals/lesson_hello/`, assertion 2). Third = `git add docs/evals/` sweeping 01_seed_data changes (assertion 19).

**verification:** code (bash/python structural assertions on committed tree) + manual (real-key run prerequisite — FIREWORKS_API_KEY required for 2 paid judge calls; cannot be faked or CI-automated without the key).

**fixture status:** NEW — `docs/evals/lesson_hello/` (entire committed tree is new; no existing fixture). Scaffold source assets exist at `crates/core/src/scaffold/{lesson_hello.yaml,eval_lesson_hello.yaml,blendtutor.toml}`.

**rubric anchor:** §2.2 (effectful shell — `eval_report::run` is the thin orchestration that produces the artifact; the committed tree IS the effect's observable output) + grade-fail-is-evidence convention (AC-5 smevals-eval-report: non-zero exit + has_runs → proceed to build; do NOT gate merge on judge score).

**Design intent (summary):** produces a committed evidence artifact, not code. Evidence integrity: the committed tree must be provably a REAL fresh `blendtutor eval-report` run against `lesson_hello.yaml`, not a copy, stub, or wrong-lesson report. Adversarial lens: cheapest broken impl that passes "docs/evals/lesson_hello/ exists" = copy 01_seed_data and rename; content assertions make that fake fail loudly.

**Technical context (key):** `crates/cli/src/commands/eval_report.rs:48` — `run(lesson_path)`; canonicalizes path → derives lesson_id from file stem → finds course_root (blendtutor.toml ancestor) → cleans stale .smevals/ → generates eval dir → `uvx smevals==0.2.0 run -g` → `build -o .<stem>.tmp` → `replace_dir` (atomic rename into `docs/evals/<stem>/`). `lesson_id_from_path()` (`smevals_gen.rs:352`) returns file STEM = `lesson_hello` (manifest id `hello` NOT used for output path). `course_root_for()` walks ancestors for blendtutor.toml; `repo_root_for()` walks ancestors for `.git` — course must be inside a git checkout. `clean_stale()` removes `<course>/.smevals/` before regenerating. `replace_dir()` atomic rename — on failure prior committed report survives. grade-fail-is-evidence: non-zero exit + has_runs → proceed; non-zero + no runs = harness failure → error. Committed report carries ABSOLUTE paths (known AC-5 issue) — predicate asserts FILENAME not full path. Judge uses `accounts/fireworks/models/deepseek-v4-flash-0731`; `judge_feedback.py` sends explicit `User-Agent: blendtutor-smevals-judge/0.1` (AC-5 fix — Python-urllib default UA banned by Fireworks Cloudflare edge, error 1010). Judge may score trivial `cat("hello\n")` oddly — EXPECTED, do NOT gate merge on score. Init temp course in `/tmp/hello-course` (outside repo) so `.smevals/` can never leak. **Fail path:** if FIREWORKS_API_KEY unavailable, builder MUST report AC-1 blocked — do NOT fake a report (no stub, no copy).

**Dependencies:** Depends on: none. Blocks: AC-2 (cites path+URL /evals/lesson_hello/), AC-4 (pins built whole-game.html contains evals/lesson_hello). Conflict set: docs/evals/lesson_hello/ only.

### AC-2 — The whole game chapter (docs/book/src/whole-game.md + SUMMARY.md)

**Executable Spec (resolved):** Speculator B's 37 clauses adopted wholesale (A's 7 subsumed). Resolver additions: (1) clause 38 — FIREWORKS_API_KEY prerequisite disclosure (grep); (2) clause 5 emoji regex = UNION of B's 6 codepoint blocks + A's `\x{FE0F}` variation selector + `\x{2B00}-\x{2BFF}`; (3) anchor ownership resolved: AC-2 owns heading "Quarto deploy" (slugs to quarto-deploy), AC-3 owns the back-link; (4) verification = code + manual (tone skim); probe greps source + built HTML, does NOT run the walkthrough commands.

**predicate:** ALL of the following hold (run from repo root after `whole-game.md` + `SUMMARY.md` are written):

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

**probe:** bash script (set -euo pipefail, cd repo root): A — test -f src, grep -qF SUMMARY link, awk Introduction<whole-game<Creating Lessons order, grep -qx '# The whole game'; B — perl emoji grep wc -l == 0; C — awk monotonic first-occurrence of 8 commands; D — grep -qF lesson_hello.yaml, ! grep lessons/lesson_hello.yaml; E — ! grep -qiE conflation, ! grep -qi eval-report.json; F — grep -qF /evals/lesson_hello/, ! grep /evals/01_seed_data/; G — grep -qi '^#.*reading.*report', index.json, grade.yaml, 0.8, 5 metric names loop, output.txt, evidence; H — grep -qiE http.server|python3 -m http|serve.*over.*http|do not.*file://|file://.*(blank|fail); I — grep -qE 'build.*--target (webr|pyodide)', quarto add, quarto render; J — grep -qF ./creating-lessons.md, README, '^#.*[Qq]uarto [Dd]eploy'; K — if ::: {.blendtutor then grep -qE '^(`{4,}|~~~+)'; L — fences=$(grep -cE '^```') >= 16; M — mdbook build docs/book, test -f book_out/whole-game.html, grep built HTML for evals/lesson_hello, blendtutor export-quarto, 0.8, verdict_rationale_correctness, creating-lessons.html, and ::: {.blendtutor if present; N — grep -qi FIREWORKS_API_KEY. Echo "AC-2 adversarial probe: OK".

**negative:** Chapter cites `/evals/01_seed_data/` instead of `/evals/lesson_hello/` (cheapest broken impl — 01_seed_data is the only committed report); fabricated report shape (wrong metric names, wrong threshold — must match committed grade.yaml); missing file:// serve instruction (users open index.html → blank page); 3-backtick fence truncating the quarto fenced-div example (mdBook build exits 0 but built HTML wrong); wrong scaffold path `lessons/lesson_hello.yaml` instead of course-root `lesson_hello.yaml`; emoji present; heading "Deploying to Quarto" slugs to `deploying-to-quarto` instead of `quarto-deploy` → AC-3's back-link 404s; missing FIREWORKS_API_KEY disclosure (copy-paste arc dies without the key).

**verification:** code (probe — grep chain + mdbook build + built-HTML content greps) + manual (tone skim: "no slop/superfluous text" is subjective — emoji part mechanical, prose-density heuristic via fence-count, genuine slop requires human judgment).

**fixture status:** `docs/book/src/whole-game.md` (NEW — chapter does not exist); `docs/book/src/SUMMARY.md` (existing:8 lines, modified — insert one line after Introduction). Probe is a one-off bash script (NEW — not a repo test; AC-4 adds the durable check-docs.sh pins).

**rubric anchor:** §4.1 (document module responsibility — chapter header names what the walkthrough covers, where it lives in the book, what it does NOT cover by cross-linking instead of restating), §3.2 (cut at the joints — cross-links to creating-lessons.md for field reference rather than duplicating; cross-links to README for Quarto extension install rather than restating).

**Design intent (summary):** R4DS-style "see it all at once" entry point — one continuous walkthrough from `init` to deploy using the scaffold starter (`lesson_hello.yaml`). Gives a complete mental model in one reading, then cross-links to field-reference chapters for depth. NOT a reference — does not re-explain every lesson field (creating-lessons.md's job) or every deploy header (README's job). The "reading the eval report" section is the chapter's unique contribution — must describe the REAL committed report, not a fabricated one.

**Technical context (key):** Verified command shapes (main.rs:31-115): `init <dir>`; `new lesson --lang r <id>` (writes `lessons/<id>.yaml` under `lessons/` subdir); `validate <path>`; `run <lesson> [--code <file>]`; `eval <lesson> [--format json] [--case N]`; `eval-report <lesson>` → `docs/evals/<stem>/` (stem = file_stem, `lesson_id_from_path` smevals_gen.rs:352); `build <path> --target <webr|pyodide> -o <out>`; `export-quarto <lesson>` (prints fenced-div `.qmd` to stdout, opening `::: {.blendtutor language="r"}` per quarto_export.rs:46). Scaffold lesson at course ROOT (scaffold.rs:60-83), NOT `lessons/` (add_lesson writes `lessons/<id>.yaml`, LESSONS_DIR="lessons" line 175). Report shape (verified against committed 01_seed_data): index.json `{"evals":[{"slug","runs","graded","fails","best"}]}`; grade.yaml per case `outcome/score/checks[]` with 5 metrics (0.0–5.0), threshold 0.8; output.txt line 1 = `verdict: correct|incorrect`; index.html smevals SPA uses fetch() which fails on file:// → must serve over HTTP. mdBook: SUMMARY links use `./<page>.md`; chapter H1 → URL `/<slug>.html`; auto-slugs headings to kebab-case lowercase (`## Quarto deploy` → `quarto-deploy`); pulldown-cmark does NOT interpret `:::` as fenced divs (renders as literal text); 3-backtick outer fence around fenced-div example truncates at inner fence (build exits 0, built HTML wrong) — ≥4 backticks or ~~~ needed. check-docs.sh already greps SUMMARY for examples (line 225) + built HTML (231-236); AC-4 adds unconditional pins.

**Dependencies:** Depends on: AC-1 (HARD — committed docs/evals/lesson_hello/ report must exist before chapter cites /evals/lesson_hello/ URL; AC-1 MUST land first). Soft forward-ref to AC-3 Step 11 anchor. Blocks: AC-3 (back-link ./whole-game.md#quarto-deploy targets AC-2's heading), AC-4 (check-docs.sh pins grep built whole-game.html). AC-2 MUST land before both. Conflict set: docs/book/src/whole-game.md (NEW, sole owner), docs/book/src/SUMMARY.md (AC-2 sole editor). Zero hot conflicts. Risk: medium.

### AC-3 — Document export-quarto as Step 11 in creating-lessons.md

**Executable Spec (resolved):** Speculator B adopted wholesale (A's 5 checks subsumed by B's I-series). Key resolver additions: (1) `Depends on: AC-2` — cross-link target ./whole-game.md#quarto-deploy must exist before AC-3 lands; (2) nested-fence guard promoted to HARD predicate clause (≥4 backticks or ~~~ outer fence — 3-backtick truncates under pulldown-cmark, confirmed latent bug at README.md:194-206); (3) verification = `code` + `manual` (tone skim — export-quarto validates lesson first, refusal-arm sentence is tone not pin); (4) probe check 11 uses `-p blendtutor-cli` (crate name, not `blendtutor`).

**predicate:** `docs/book/src/creating-lessons.md` contains a `## Step 11 —` heading positioned after `## Step 10 —` (Steps 9–10 headings intact → no renumbering breakage); the Step 11 body (a) names the command as `blendtutor export-quarto` with a `.yaml`/`.yml` lesson-path argument (NOT a course dir), (b) shows stdout-redirect guidance to a `.qmd` file (redirect `>` or explicit copy-paste-to-.qmd), (c) shows a fenced-div example whose opening line is byte-identical to real `export_lesson_to_qmd` output for an R lesson — `::: {.blendtutor language="r"}` — with the code fence tagged ` ```r ` (NOT `language="python"` for an R lesson), (d) contains NO `llm_evaluation_prompt` / `gotchas` / `packages` in the example (author-only/out-of-scope fields excluded by the transform), (e) covers the extension install (`quarto add mcmullarkey/blendtutor` or cross-link to README §Quarto Extension) + filter enablement (`filters: [mcmullarkey/blendtutor]` or cross-link) + `quarto render`, (f) does NOT claim `export-quarto` produces a browser site / static site (anti-conflation with `blendtutor build`), (g) carries a cross-link to the whole-game chapter using mdBook's `./whole-game.md` source convention; `mdbook build docs/book` exits 0; the built `docs/book/book/creating-lessons.html` contains the full example (opening `::: {.blendtutor language="r"}` AND the code-template content AND a closing `:::`) as contiguous rendered text — proving no nested-fence collision truncated the example; `cargo run -p blendtutor-cli -- export-quarto <scaffold-lesson>` succeeds (exit 0) and its stdout's first line matches the doc example's opening line.

**probe:**
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

**negative:** Builder adds `## Step 11 — Export to Quarto` with a one-line `blendtutor export-quarto` mention and a FABRICATED fenced-div example copied from memory — `language="python"` for the R scaffold lesson (or a 3-backtick outer fence wrapping 3-backtick inner fences so mdBook truncates the example at the first bare ` ``` `), conflates the output with `blendtutor build` ("export-quarto builds your Quarto site"), omits stdout-redirect guidance, and drops the cross-link — yet a naive `grep export-quarto creating-lessons.md && grep '## Step 11'` passes green.

**verification:** code (check-docs.sh / mdbook build + grep) + manual (tone skim — succinct, no emojis, exact commands).

**fixture status:** existing `docs/book/src/creating-lessons.md:405` (Steps 1–10) + NEW Step 11 section; probe is NEW (no existing Step 11 test).

**rubric anchor:** §4 (Document module responsibility — names what, where, what NOT: export-quarto prints a .qmd snippet, NOT a browser site) + §3 (Cut at the joints — export-quarto ≠ build ≠ eval-report; the doc must not conflate the three outputs).

**Design intent (summary):** `blendtutor export-quarto` is the UNDOCUMENTED bridge from the lesson-YAML authoring world to the Quarto extension authoring world. Step 11 makes that bridge discoverable WHERE USERS LOOK FOR COMMANDS (the creating-lessons walkthrough), not buried in the README extension section. Covers full flow — export → fence-div → extension install → quarto render — and cross-links to the whole-game chapter's Quarto deploy section (AC-2) for the deploy story, rather than restating the README's full extension docs. export-quarto is an ALTERNATIVE OUTPUT after Step 10's build, so it slots as Step 11 with NO renumbering of Steps 1–10.

**Technical context (key):** Command semantics (`crates/cli/src/commands/export_quarto.rs:22-34`): `run(path)` calls `read_lesson_file` → `Lesson::parse` → `validate_semantics` (VALIDATES lesson first; invalid lesson → stderr + `ExitCode::FAILURE`, distinct from read error which propagates as anyhow). On success `export_lesson_to_qmd(&lesson)` → `print!("{qmd}")` to STDOUT. CLI signature: `blendtutor export-quarto <lesson.yaml>` (a lesson PATH, not a course dir — `Commands::ExportQuarto { lesson }` at main.rs:165). Transform output (`crates/core/src/quarto_export.rs:41-99`): opens `::: {.blendtutor language="<r|python>"}\n`, prompt as prose, optional code_template in ` ```<lang> ` fence, optional checks in ` ```{.<lang> .checks} ` fence, optional solution in ` ```{.<lang> .solution} ` fence, optional hints in `::: {.hints}` div, closes `:::\n`. Fence length by `fence_for()` (min 3, +1 over longest backtick run). EXCLUDES `llm_evaluation_prompt`, `gotchas`, `packages` (author-only / out-of-scope, ADR-0006). Real stdout for scaffold lesson (R, no checks/solution/hints): `::: {.blendtutor language="r"}` / `Write R code that prints the word "hello" on its own line, using cat().` / ` ```r ` / `# Your code here` / `cat("hello\n")` / ` ``` ` / `:::`. Doc example's opening line MUST be byte-identical to `::: {.blendtutor language="r"}`. **Nested-fence collision (CRITICAL sneaky-pass):** example contains inner ` ```r `...` ``` ` fences; a 3-backtick outer fence closes at the first bare ` ``` ` truncating the example. README lines 194–206 have this latent bug (GitHub renders loosely; mdBook will not). MUST use 4+ backtick outer fence (` ````markdown `) or `~~~` tildes. Probe I9+I10 catch this (mdbook build may exit 0, but built HTML loses code-template content → I10's `grep 'cat("hello'` fails). mdBook link convention: source links use `./<page>.md`; built HTML resolves to `./<page>.html`; anchor `./whole-game.md#quarto-deploy` (mdBook auto-slugs headings to kebab-case lowercase). check-docs.sh runs `mdbook build docs/book` (line 31) — the build is the AC-3 structural gate. No existing `export-quarto` pin (AC-4 adds it). AC-3 does NOT modify check-docs.sh, does NOT touch README (extension fully documented at README.md:156–264; Step 11 cross-links rather than restates). No `readme.rs` test affected (docs-only). Step 11 placement: after `## Step 10 — Build a browser site` (line 362) and before `## Complete example courses` (line 392), keeping "Complete example courses" as the closing section. NO renumbering of Steps 1–10. Expected test migration: 0 files.

**Dependencies:** Depends on: AC-2 (whole-game.md + Quarto-deploy section must exist for cross-link target; link-correctness ordering, NOT file conflict). Blocks: AC-4 (pins built creating-lessons.html contains export-quarto). Conflict set: docs/book/src/creating-lessons.md only (AC-2 touches whole-game.md, AC-4 touches check-docs.sh + README — zero file overlap).

### AC-4 — Discoverability pins (README pointer + check-docs.sh pins)

**Executable Spec (resolved):** Speculator B's C1-C10 adopted wholesale (A's 4 pins subsumed). Resolver additions: (1) USER DECISION 2026-08-10 — pins are LOCAL-ONLY enforcement (Option A, decomposition as written): check-docs.sh fails local runs; CI docs.yml does NOT enforce content pins (builds inline); test_docs_pages_artifact.sh Phase 2 skips in CI (no mdbook). Decomposition's "fail CI" wording was inaccurate. Do NOT touch docs.yml or scripts/tests/test_docs_pages_artifact.sh. (2) Path correction: MIRROR_OK check at scripts/tests/test_docs_pages_artifact.sh:304 (B dropped `tests/`). (3) Anti-pattern warning: the evals assemble block at check-docs.sh:95-101 IS guarded (if [ -d docs/evals ]) — AC-4 pins MUST NOT copy that pattern.

**predicate:** ALL of the following hold (run from repo root after AC-1/2/3 land — AC-4 MUST be last):

**C1 — SUMMARY pin (source, established pattern):** `scripts/check-docs.sh` contains an uncommented line matching `grep.*whole-game.*SUMMARY\.md` — pins the SUMMARY entry. Stronger variant: `grep -qF './whole-game.md'` pins the link target, not just the word.

**C2 — Built whole-game.html pin (NOT source — anti-sneaky-pass):** contains an uncommented line matching `grep.*evals/lesson_hello.*book_out/whole-game\.html\|grep.*evals/lesson_hello.*\$book_out/whole-game` — pins the BUILT HTML, NOT the source. A source-grep pin is the cheapest broken implementation (source can be right while built HTML is wrong due to render bug / fence collision).

**C3 — Built creating-lessons.html pin (NOT source):** contains an uncommented line matching `grep.*export-quarto.*book_out/creating-lessons\.html\|grep.*export-quarto.*\$book_out/creating-lessons` — pins BUILT HTML, NOT source (AC-3's speculator-b identified nested-fence collision as a latent render bug; built-HTML pin catches truncation).

**C4 — README pointer pin:** contains an uncommented line matching `grep.*whole-game.*README\.md`. The needle must be `whole-game.html` (built page name), NOT `whole-game.md` (source — dead link from repo-root context).

**C5 — Unconditional (no guard, no `|| true`):** No uncommented line matching `whole-game|export-quarto` also matches `\|\| true`. Pins must NOT be wrapped in `if [ -f ... ]; then ... fi` or suffixed with `|| true`. The existing evals assemble (check-docs.sh:95-101) IS guarded — AC-4's pins must NOT follow that pattern. `set -euo pipefail` (line 15) ensures any unguarded failing grep exits the script.

**C6 — Uncommented (anti-sneaky-pass):** Pin lines are matched by `grep -v '^#' scripts/check-docs.sh` — a commented-out pin doesn't run.

**C7 — Post-build ordering (enforced by set -e):** The pins run AFTER `mdbook build docs/book` (check-docs.sh:31).

**C8 — End-to-end pass (no regression):** `bash scripts/check-docs.sh` exits 0.

**C9 — readme.rs not broken (append-only discipline):** `cargo test -p blendtutor-cli --test readme` exits 0. If the builder REWRITES line 58 (the workflow string) to inline the pointer, any dropped command substring breaks readme.rs — probe catches this.

**C10 — MIRROR_OK count unchanged:** `scripts/tests/test_docs_pages_artifact.sh` line 304 still asserts `MIRROR_OK -eq 11`. AC-4's new pins are NOT docs.yml mirror needles — no count update needed.

**probe:**
```bash
set -e
# C1-C4: Structural — pins exist in check-docs.sh, on BUILT HTML (not source), uncommented
grep -v '^#' scripts/check-docs.sh | grep -q 'whole-game.*SUMMARY\.md'        # C1 SUMMARY pin
grep -v '^#' scripts/check-docs.sh | grep -q 'evals/lesson_hello.*whole-game\.html'  # C2 built HTML (NOT source)
grep -v '^#' scripts/check-docs.sh | grep -q 'export-quarto.*creating-lessons\.html' # C3 built HTML (NOT source)
grep -v '^#' scripts/check-docs.sh | grep -q 'whole-game.*README\.md'       # C4 README pin
# C2/C3 anti-sneaky-pass: pins are NOT on source .md files
! grep -v '^#' scripts/check-docs.sh | grep -q 'src/whole-game\.md.*evals/lesson_hello'
! grep -v '^#' scripts/check-docs.sh | grep -q 'src/creating-lessons\.md.*export-quarto'
# C5: Unconditional — no || true on pin lines
! grep -v '^#' scripts/check-docs.sh | grep -E 'whole-game|export-quarto' | grep -q '|| true'
# C8: End-to-end — check-docs.sh builds book + runs all pins (post-build ordering via set -e)
bash scripts/check-docs.sh
# C9: readme.rs — README append didn't break existing substring needles
cargo test -p blendtutor-cli --test readme
# C10: MIRROR_OK count unchanged
test "$(grep -c 'MIRROR_OK -eq 11' scripts/tests/test_docs_pages_artifact.sh)" -eq 1
```

**Negative probe (unconditionality — run separately, destructive):** remove SUMMARY whole-game entry → `bash scripts/check-docs.sh` must FAIL (proves pins not guarded); restore from /tmp backup.

**negative:** 1. Source-grep instead of built-HTML-grep (MOST LIKELY — source has content, built HTML might not due to render bug; C2/C3 anti-sneaky-pass catches). 2. Guarded pin (`if [ -f ... ]; then ... fi` — skipped when artifact missing; C5 catches `|| true` variant, negative probe catches `if` variant). 3. Commented-out pin (C6 `grep -v '^#'` catches). 4. Wrong README link target (`./docs/book/src/whole-game.md` source instead of deployed URL; C4 needle `whole-game.html` catches). 5. README rewrite of pinned lines (C9 cargo test readme catches). 6. Pin before build (C8 end-to-end via set -e). 7. Breaking existing checks (C8 catches).

**verification:** code.

**fixture status:** `scripts/check-docs.sh:218-236` (existing pin region — append after line 236) · `README.md:56-58` (authoring-workflow section — append one line+link, append-only) · `crates/cli/tests/readme.rs:27-67` (existing — NOT modified, read-only regression check) · `scripts/tests/test_docs_pages_artifact.sh:290-304` (existing — NOT modified, MIRROR_OK count unchanged) · NEW pin lines in check-docs.sh (4 pins).

**rubric anchor:** §1 (Encode invariants in types — pins make a missing/wrong whole-game chapter a check failure, not a silent deploy) + §4 (Document module responsibility — check-docs.sh owns the local mirror contract; pins document what it asserts).

**Design intent (summary):** wires durable regression pins so future changes to AC-1/2/3 artifacts (removing whole-game.md, dropping the evals/lesson_hello citation, deleting Step 11's export-quarto content) fail locally via `bash scripts/check-docs.sh`. Pins are CROSS-AC CONTRACTS: C2 assumes AC-2's whole-game.md cites `/evals/lesson_hello/`; C3 assumes AC-3's creating-lessons.md Step 11 documents `blendtutor export-quarto`. If AC-2/3 don't land the content, the pins fail — that's intended. This is why AC-4 MUST land last: pins are unconditional (no guard), so they fail if any dependency artifact is missing. README pointer is a DISCOVERABILITY wire — a one-line link near the authoring-workflow section (README.md:56-58) pointing to the deployed whole-game chapter, following the existing README link pattern (deployed URLs, e.g., lines 146-149, 293, 316). Append-only discipline: add a new line, never rewrite the pinned workflow string at line 58.

**Technical context (key):** check-docs.sh (238 lines): line 15 `set -euo pipefail`; line 19 `book_out="docs/book/book"`; line 31 `mdbook build docs/book` (pins must run AFTER); lines 95-101 guarded evals assemble (`if [ -d docs/evals ]`) — the ONLY guarded block, AC-4's pins must NOT follow this pattern; lines 218-222 existing README pins; line 225 existing SUMMARY pin; lines 231-236 existing built-HTML pins. **Insertion point:** after line 236, before final `echo "docs: OK"` at line 238. README.md (370 lines): line 56 `## Authoring workflow`; line 58 pinned workflow string (readme.rs needles); lines 146-149, 293, 316 existing deployed-URL links. **Insertion point:** after line 58 (before `### blendtutor init` at line 60). Append-only. readme.rs (68 lines): line 27 lowercased haystack; lines 33-47 9 substring needles; append-only safe (contains-check; removing pinned phrases breaks). AC-4 does NOT touch readme.rs. test_docs_pages_artifact.sh (487 lines): lines 290-301 MIRROR_OK needle list (11); line 304 `MIRROR_OK -eq 11`. AC-4's new pins are NOT mirror needles → count stays 11 → AC-4 does NOT touch this file. docs.yml (CI): line 61 `mdbook build docs/book` (inline, NOT via check-docs.sh); line 137 Pages artifact root. Does NOT grep built HTML for whole-game/evals/lesson_hello/export-quarto. **CI-enforcement note (USER DECISION 2026-08-10):** pins are LOCAL-ONLY (Option A, decomposition as written). check-docs.sh fails local runs; CI docs.yml does NOT enforce content pins (builds inline); test_docs_pages_artifact.sh Phase 2 skips in CI (no mdbook). Do NOT touch docs.yml or scripts/tests/test_docs_pages_artifact.sh. Expected test migration: 0 files.

**Dependencies:** depends_on: AC-1 (produces `docs/evals/lesson_hello/`), AC-2 (produces whole-game.md + SUMMARY entry), AC-3 (produces creating-lessons.md Step 11 with export-quarto) — pins reference all three artifacts. **MUST be last:** pins are unconditional (C5) — if AC-4 lands before AC-2/3, pins fail. Schedule serializes: AC-1 → AC-2 → AC-3 → AC-4. depended_on_by: none.

## Batch Schedule

- **Batch 1 (parallel): AC-1 (committed report), AC-3 (export-quarto Step 11)** — disjoint files, zero overlap. AC-1 needs a real `FIREWORKS_API_KEY` (paid judge calls, ~2 cases — trivial cost) and produces non-deterministic judge scores; that is expected and is precisely the evidence convention.
- **Batch 2 (sequential): AC-2** — depends on AC-1's committed evidence/URL. Also the longest-prose AC, so giving it its own batch keeps review focused.
- **Batch 3 (sequential): AC-4** — pins AC-2/AC-3/AC-1 artifacts; must not be a structural test for things that don't exist yet.

## Open Questions

All resolved via user decisions (title, fresh report, full export-quarto, local-only pins). None remaining.

## Cross-AC Contracts

- **Evidence path/URL:** `docs/evals/lesson_hello/` → `/evals/lesson_hello/` (AC-1 produces, AC-2 cites, AC-4 pins). Lesson-file stem is the URL segment (`lesson_hello.yaml` → `lesson_hello`), same rule as existing `01_seed_data/` report. If AC-1's run produces a different stem, both downstream pins break loudly (that is the intent).
- **Anchor:** AC-2 owns heading "Quarto deploy" (slugs to `quarto-deploy`); AC-3 owns back-link `./whole-game.md#quarto-deploy`.
- **Fence rule:** any fenced-div example wrapped in ≥4-backtick or ~~~ outer fence (3-backtick truncates in mdBook).
- **Command spellings:** `init/new/validate/run/eval/eval-report/build/export-quarto` (verified main.rs — no invented flags).
- **Scaffold lesson at course ROOT** (`lesson_hello.yaml`, NOT `lessons/`).
- **check-docs.sh pins** unconditional + uncommented + on BUILT HTML (`$book_out`) + after mdbook build.
- **README pointer:** deployed URL `https://mcmullarkey.github.io/blendtutor/whole-game.html` (append-only, never rewrite line 58).