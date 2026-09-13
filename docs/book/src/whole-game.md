# The whole game

This chapter walks one course end to end: scaffold it, add a lesson, validate
it, run it, score the grader's polarity, generate an eval report with the LLM
judge, and deploy it two ways — as a static browser site and as a Quarto
document. Every command is copy-paste ready.

To author a course with Claude Code, use the
[`blendtutor-course` skill](https://github.com/mcmullarkey/blendtutor/blob/main/.claude/skills/blendtutor-course/SKILL.md)
that ships in the repo: it writes lessons with checks and solutions, a minimal
eval suite, and the Quarto or site output, and verifies the checks locally.

## Init — scaffold a course

`blendtutor init` creates a course directory with a runnable starter lesson:

```bash
blendtutor init hello-course
```

The starter lesson `lesson_hello.yaml` sits at the course root, so its manifest
path is `lesson_hello.yaml` with no `lessons/` prefix. It is a complete R
lesson — the rest of this walkthrough follows it.

## New lesson — author your own

The scaffold is a starting point. Real lessons live under `lessons/` and are
created with `new lesson`:

```bash
blendtutor new lesson --lang r hello
```

This writes `lessons/hello.yaml`. `--lang r` picks R; `--lang python` picks
Python. Edit the file to change the prompt and code template.

## Validate — check the lesson

Validate a lesson file before running anything:

```bash
blendtutor validate lesson_hello.yaml
```

Validation checks structure and semantics, naming the field and value when
something is wrong. Add `--format json` for machine-readable output.

## Run — see the lesson live

Run the starter lesson locally and read the feedback it produces:

```bash
blendtutor run lesson_hello.yaml
```

`run` executes the lesson and returns the LLM feedback a student would see.
Pipe a submission in, or pass `--code <file>` to grade a submission file.

## Eval — score grading accuracy

`eval` replays each case through the run pipeline and reports how often the
grader's verdict matches the expected one — polarity scoring. This still makes
real paid LLM calls (the student feedback for each case), so set the API key
first and expect spend:

```bash
export FIREWORKS_API_KEY=fw_...
blendtutor eval lesson_hello.yaml
```

The starter lesson ships its `eval_lesson_hello.yaml` sibling with two cases —
a correct submission and an incorrect one. Every lesson added with `new lesson`
gets the same treatment: an `eval_<name>.yaml` sibling scaffolded alongside it.
Use `--case N` for a single case, `--format json` for JSON output.

A mismatched case prints the grader's verbatim feedback on an indented
`grader:` line under its row, and any mismatched run ends with next steps:

```text
case 2: expected correct, got incorrect [mismatch]
  grader: the submission is missing a call to `mean()`

mismatched cases: 2
inspect one: blendtutor eval <lesson> --case N
grading is shaped by the lesson's `llm_evaluation_prompt` and each exercise's reference `solution`
```

To persist the result for `build` to fold into the site's eval-results page,
write the report at the course root:

```bash
blendtutor eval lesson_hello.yaml --write-report
```

This writes `eval-report.json` next to the course's `blendtutor.toml` — found
from the lesson's directory, wherever you run the command from — as the same
full-shape JSON `--format json` prints, regardless of `--format`. Re-runs
overwrite it with a warning. `--case N` and `--write-report` are mutually
exclusive: a single-case report would render as the course-level accuracy.

## Eval report — grade with the LLM judge

`eval-report` drives the pinned smevals runner, which grades each case with
polarity AND a real paid LLM-judge call, then publishes the smevals report into
a browsable site under `docs/evals/<lesson>/`, where `<lesson>` is the stem of
the lesson filename — `lesson_hello.yaml` reports to `docs/evals/lesson_hello/`:

```bash
blendtutor eval-report lesson_hello.yaml
```

This needs the same `FIREWORKS_API_KEY` as `eval`.

If you recorded from a git worktree, scrub the checkout prefix before
committing; `scripts/check-docs.sh` fails any `/Users/` path under `docs/evals/`:

```bash
find docs/evals \( -name 'eval.json' -o -name 'run.yaml' \) -exec \
  perl -pi -e 's{/Users/[^/]*/portfolio/(?:worktree-|blendtutor-)[^/]*/}{}g' {} +
```

## Reading the eval report

The report is static files under `docs/evals/lesson_hello/` — no server
required. The committed starter report shows the shape.

`index.json` is the overview: the eval slug, run counts, `graded`, `fails`, and
`best` — the best score across configs (for the starter's single `default`
config, that's the aggregate accuracy across the suite's cases, 0.76).

Each graded run has a `grade.yaml` under `.../runs/<case>/default/.../grades/`
with an `outcome` (`pass` or `fail`), a `score`, and `checks`. The judge check
scores five metrics from 0.0 to 5.0:
`verdict_rationale_correctness`, `actionability`, `references_check_results`,
`no_solution_leak`, `no_hallucinated_errors`.

A case passes when its score is at least the 0.8 threshold. The starter report
has one of each: case-1 passes at 0.96 with every metric at 4.0 or above, and
case-2 fails at 0.56 — its `no_solution_leak` scored 0.0 because the feedback
gave away the answer string. A failing grade is evidence the case needs
rework, and the metrics say exactly where. `references_check_results` scored
2.0, the only other metric below the 4.0 floor — both are rework targets.

`output.txt` holds the judged feedback: line 1 is `verdict: correct` or
`verdict: incorrect`, and the remaining lines are the feedback text the judge
scored.

Two gotchas. First, `index.html` is a single-page app that fetches the report
data over HTTP, so opening it directly with `file://` shows a blank page. Serve
the directory instead:

```bash
cd docs/evals/lesson_hello && python3 -m http.server
```

Second, the `lesson` field in `eval.json` is repo-relative from the checkout
that recorded the run — treat the report as evidence of a run, not as a
portable path reference.

## Build — a static browser site

`build` turns the course into a fully static site with an in-browser runtime:

```bash
blendtutor build hello-course --target webr -o site
```

`--target` picks the runtime: `webr` for R lessons, `pyodide` for Python. The
`site/` output deploys to GitHub Pages as-is, and the build embeds an
eval-results page when the course has a report.

## Quarto extension

blendtutor also ships as a [Quarto](https://quarto.org) extension for
interactive coding exercises in `.qmd` documents: in-browser editor, instant
checks, solution reveal, and AI feedback, all static HTML. Requires **Quarto >= 1.4**:

```bash
quarto add mcmullarkey/blendtutor
```

This installs to `_extensions/mcmullarkey/blendtutor/` (version 0.2.0).
**Run it from the folder that contains `_quarto.yml`** (or the `.qmd`): Quarto
only discovers `_extensions/` there, so an extension installed one directory up
never loads and exercises render as plain text. Assets deploy alongside the
rendered HTML, so asset resolution is install-path-independent. Commit
`_extensions/` so CI (for example a GitHub Pages workflow) renders the same filter.

### Quick start (zero hand-written bootstrap)

A complete copy-paste document with zero hand-written bootstrap. Filter by name,
`.blendtutor` div, render:

````markdown
---
title: "My exercises"
filters: [mcmullarkey/blendtutor]
---

::: {.blendtutor language="r"}
Write a function `add(a, b)` that returns the sum.

```r
add <- function(a, b) { ___ }
```

```{.r .checks}
stopifnot(add(1, 2) == 3)
```
:::
````

Preview it with `quarto preview` and the exercise is interactive immediately.
Python exercises use the same div with `language="python"`. Enable the filter in
the page front matter or in `_quarto.yml`, not both.

### Auto-bootstrap opt-out

The filter auto-bootstraps by default; to wire up the runtime yourself, set
`bt-auto-bootstrap: false` in the YAML header. To keep it but disable the
auto-mounted AI feedback, set `bt-feedback: false`; see
[BYOK](#byok-bring-your-own-key).

### Export a lesson

`export-quarto` writes the div from a lesson file, carrying its prompt, code
template, checks, solution, hints, gotchas, success criteria, and packages:

```bash
blendtutor export-quarto lesson_hello.yaml                        # snippet to paste into a page
blendtutor export-quarto --document lesson_hello.yaml > page.qmd  # standalone page with front matter
blendtutor export-quarto --key-page > api-key.qmd                 # the API key page
```

Written by hand, the same pieces are optional blocks inside the div: a
`{.r .solution}` code block, `::: {.hints}` and `::: {.gotchas}` bullet divs, a
`::: {.success-criteria}` rubric for AI feedback, and a `packages="dplyr,purrr"`
attribute on the div. `export-quarto` warns on stderr when a lesson has no
checks, solution, or hints.

### Cross-origin isolation (COI)

webR runs faster with `SharedArrayBuffer`, which needs cross-origin isolation
(COOP/COEP). Opt in with `coi: true` (page YAML header) or `coi="true"` (any div);
the filter injects a service-worker shim. Pyodide-only pages do not need COI.

> **Book-mode limitation:** COI does not function in Quarto `type: book` projects: the shim's scope cannot cover the book's `_output/` pages, so webR uses its slower non-isolated channel ([ADR-0015](https://github.com/mcmullarkey/blendtutor/blob/main/docs/adr/0015-opt-in-coi-cross-origin.md)).

### Demo book

A complete demo book with R and Python exercises lives in
[`demo-book/`](https://github.com/mcmullarkey/blendtutor/tree/main/demo-book),
rendered live at <https://mcmullarkey.github.io/blendtutor/demo-book/> (rebuild
locally with `cd demo-book && quarto render`). It is a Quarto `type: book` project, so
COI does not take effect (limitation above). Python exercises are fully interactive
and every page ships a static fallback. R exercises run in the book too, on webR's slower
fallback channel; on the CLI-built [example sites](./examples.md),
R exercises run interactively via webR with isolation.
Over `file://` you get static exercise content only; serve over HTTP:

```bash
cd demo-book/_output && python3 -m http.server 8000
```

## BYOK (Bring Your Own Key)

Browser feedback uses the learner's own API key, with no server-side key.
Feedback is **auto-mounted**: the injected bootstrap imports
`exercise-feedback.js` and calls `mountAllFeedback(registry)` after the runtime
starts. Clicking Get feedback without a stored key shows the key form inline,
and a `--key-page` page manages the key too. The key is shared via
`localStorage`, readable by any JavaScript on the page's origin, so never reuse
a critical key; it is sent only to `api.fireworks.ai`. BYOK is Fireworks-only
(pinned model `accounts/fireworks/models/deepseek-v4-flash-0731`); the CLI
supports other providers (see the [README](https://github.com/mcmullarkey/blendtutor#api-key)).
Serve over HTTP: `file://` breaks `localStorage` sharing and blocks ES modules,
so feedback never mounts. Self-hosted CSP: add
`connect-src https://api.fireworks.ai` (Pages cannot set CSP headers; the shim
covers only COOP/COEP).
