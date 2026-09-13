# Creating Lessons

The command-first guide to authoring a lesson: scaffold a course, author R and Python lessons, validate, grade, and build a deployable site. For a complete end-to-end walkthrough, see [The whole game](./whole-game.md); the shipped reference courses (`examples/write-less-code-r/`, five R lessons, webR; and `examples/write-less-code-python/`, five Python lessons, Pyodide) are the canonical worked examples ([Example sites](./examples.md)).

## Prerequisites

```bash
git clone https://github.com/mcmullarkey/blendtutor.git
cd blendtutor
cargo install --path crates/cli
export FIREWORKS_API_KEY=fw_...
```

`init`, `new`, `validate`, and `build` need no key; only `run` and `eval` call the LLM provider.

## Step 1 — Scaffold a course

```bash
blendtutor init my-stats-course
```

Creates `my-stats-course/` with a `blendtutor.toml` manifest, a starter lesson, a matching eval suite, and a `.gitignore`. Each `[[lessons]]` entry maps a stable slug (`id`) to a lesson `path` relative to the manifest, in the order lessons appear in the built site; `new` appends entries automatically (`Manifest`/`ManifestEntry` in the [API reference](./api/blendtutor_core/index.html) define the fields).

## Step 2 — Add an R lesson

```bash
blendtutor new lesson --lang r seed-data
```

Writes `lessons/seed-data.yaml`; `--lang` selects the runtime target (`r` or `python`). Abridged from `examples/write-less-code-r/01_seed_data.yaml`:

```yaml
lesson_name: "Seed Data"    # shown in the browser site
language: R                 # or Python; selects the runtime
exercise:
  type: "function_writing"
  prompt: |                 # instructions the learner sees
    Create a data frame called `survey_data` with 5 respondents and 6 stress items (stress_1 through stress_6) measured on a 1-5 scale.
  code_template: |          # starter code pre-filled in the editor
    survey_data <- data.frame(respondent_id = 1:5, ...)
  solution: |               # reference solution (eval + site self-check)
    survey_data <- data.frame(respondent_id = 1:5, stress_1 = c(3, ...), ...)
  llm_evaluation_prompt: |  # grading prompt; must contain {student_code}
    ...Student submitted this code: {student_code}...
checks:                     # must evaluate without error, before the LLM verdict
  - "stopifnot(identical(dim(survey_data), c(5L, 7L)))"
  - "stopifnot(identical(survey_data$stress_6, c(1, 2, 3, 4, 5)))"
```

`description`, `example_usage`, and `success_criteria` round out the file; the [`Lesson` and `Exercise` structs](./api/blendtutor_core/index.html) define every field.

## Step 3 — Add a Python lesson

```bash
blendtutor new lesson --lang python tally
```

Same shape with `language: Python`; an optional `packages` list (e.g. `packages: [pandas]`) is loaded by the Pyodide runtime in the browser. Full example: `examples/write-less-code-python/01_seed_data.yaml`.

## Step 4 — Register lessons in the manifest

`new` appends manifest entries automatically; hand-added lesson files need a matching `[[lessons]]` block. `examples/write-less-code-r/blendtutor.toml` shows a complete five-lesson manifest.

## Step 5 — Validate

```bash
blendtutor validate lessons/seed-data.yaml
blendtutor validate lessons/seed-data.yaml --format json
```

Nonzero exit when the lesson is invalid, so it drops cleanly into CI.

## Step 6 — Run a submission

```bash
blendtutor run lessons/seed-data.yaml --code submission.R
echo 'survey_data <- data.frame(respondent_id = 1:5)' | blendtutor run lessons/seed-data.yaml  # --code omitted: reads stdin
```

Executes the `checks`, then asks the LLM for a verdict; the exit code reflects the verdict. `--format json` for a structured report.

## Step 7 — Write an eval suite

Each lesson pairs with a sibling `eval_<name>.yaml` — `new` scaffolds a minimal one-case starter suite next to the lesson (edit it in place; `eval` discovers the suite by the sibling convention), and the starter course's `eval_lesson_hello.yaml` shows the fuller two-case shape — containing sample submissions and expected verdicts (abridged from `examples/write-less-code-r/eval_01_seed_data.yaml`):

```yaml
cases:
  - submission: |-
      survey_data <- data.frame(respondent_id = 1:5, stress_1 = c(3, 4, 5, 2, 1), ...)
    expected: correct
  - submission: |-
      survey_data <- data.frame(respondent_id = 1:5, stress_1 = c(3, 4, 5, 2, 1))
    expected: incorrect  # near-miss: runs cleanly, missing stress_6
```

Include **near-miss** cases — submissions that run cleanly but are subtly wrong; they measure whether the grading prompt catches realistic mistakes, not just syntax errors. The [`EvalSuite` schema](./api/blendtutor_core/index.html) documents the fields.

## Step 8 — Score the grading prompt

```bash
blendtutor eval lessons/seed-data.yaml
```

Replays the eval cases through the run pipeline and reports how often the grader's verdict matches the expected label — regression-test grading accuracy before shipping. `--case N` for one case, `--format json` for structured output. Real paid calls: run against the provider your deployed site will use.

To persist the score for `build` to fold into the site's eval-results page, add `--write-report`: it writes the full-shape `eval-report.json` next to the course's `blendtutor.toml` (found from the lesson's directory, wherever you run from), overwriting a previous report with a warning. `--case N` and `--write-report` are mutually exclusive — a single-case report would render as the course-level accuracy.

## Step 9 — Generate the eval report

```bash
blendtutor eval-report lessons/seed-data.yaml
```

Grades every case with polarity AND a real paid LLM-judge call (driving the pinned [`smevals`](https://pypi.org/project/smevals/) runner via [`uv`](https://docs.astral.sh/uv/), which must be on PATH), producing `.smevals/` (ephemeral, never commit) and `docs/evals/<lesson>/` (the committed static report, published at `/evals/<lesson>/`). Exits 0 as long as the run recorded its cases — a low score is evidence, not a failure. It fails only when a stage produced nothing usable. Same API key as `eval`.

If you recorded from a git worktree, scrub the checkout prefix before committing — `scripts/check-docs.sh` fails any `/Users/` path under `docs/evals/`:

```bash
# find -exec, not docs/evals/**: globstar is absent on macOS bash 3.2 —
# the glob would match nothing and the scrub would silently no-op
find docs/evals \( -name 'eval.json' -o -name 'run.yaml' \) -exec \
  perl -pi -e 's{/Users/[^/]*/portfolio/(?:worktree-|blendtutor-)[^/]*/}{}g' {} +
git add docs/evals
git commit -m "eval report: lessons/seed-data.yaml"
```

## Step 10 — Build a browser site

```bash
blendtutor build my-stats-course --target webr -o site    # R course
blendtutor build my-stats-course --target pyodide -o site # Python course
```

Emits a fully static site — `index.html`, per-lesson JSON, the in-browser runtime, and an embedded eval-results page when the course carries `eval-report.json` — deployable to GitHub Pages as-is. webR needs cross-origin isolation; the build ships a vendored [`coi-serviceworker`](https://github.com/gzuidhof/coi-serviceworker) shim, so no header configuration is required. See the README for deployment details.

## Step 11 — Export a lesson to Quarto

`export-quarto` converts a single lesson YAML into Quarto source on stdout — validating first and refusing invalid lessons. It has three shapes:

```bash
# A fenced-div snippet to paste into an existing page
blendtutor export-quarto lessons/lesson_hello.yaml

# A complete, renderable page (title, filter, and coi for R in the front matter)
blendtutor export-quarto --document lessons/lesson_hello.yaml > hello.qmd

# The API key page learners use to store their Fireworks key
blendtutor export-quarto --key-page > api-key.qmd
```

Every optional field the widget understands is carried over: `code_template`, `checks`, `solution`, `hints`, `gotchas`, `success_criteria` (added to the in-browser feedback prompt), and `packages` (preloaded in webR/Pyodide). A lesson with no checks, solution, or hints still exports, with a warning on stderr: its widget offers only Run and LLM feedback.

To render:

1. Run `quarto add mcmullarkey/blendtutor` **from the folder that contains `_quarto.yml`** (or the `.qmd`, for a standalone page). Quarto only looks for `_extensions/` there; installing one level up leaves the filter undiscoverable and exercises render as plain text.
2. Enable the filter. `--document` and `--key-page` pages declare it themselves; for a snippet, add `filters: [mcmullarkey/blendtutor]` to the page or to `_quarto.yml` — not both, since Quarto merges the lists.
3. R exercises need `coi: true` (webR requires cross-origin isolation). COI does not work in `type: book` projects, so R exercises only run on standalone pages; Python exercises run anywhere.
4. Serve over HTTP (`quarto preview`): `file://` blocks the ES modules and `localStorage` the widget needs.

Requirements and the rendered snippet are covered by the [README §Quarto Extension](https://github.com/mcmullarkey/blendtutor#quarto-extension); for the end-to-end deploy see [whole-game §Quarto deploy](./whole-game.md#quarto-deploy).

## Complete example courses

The reference courses show the full workflow end to end: five lessons each, sibling eval suites, manifests, and committed `eval-report.json`. Browse `examples/write-less-code-{r,python}/`, copy a lesson, and adapt the prompt, checks, and eval cases.
