# Creating Lessons

This page walks through creating an interactive R + Python lesson course from
scratch — from scaffolding the project to building a deployable browser site.
Each step shows the exact command, the file it produces, and what to edit next.

If you want to see a complete, working course before reading further, two
example courses ship in the repository:

- **`examples/write-less-code-r/`** — a five-lesson R course (webR target).
- **`examples/write-less-code-python/`** — a five-lesson Python course
  (Pyodide target), mirroring the R arc.

Both are built and deployed as live browser sites alongside this book (see
[Example sites](./examples.md)).

## Prerequisites

Install the CLI and set an API key for the LLM provider:

```bash
git clone https://github.com/mcmullarkey/blendtutor.git
cd blendtutor
cargo install --path crates/cli
```

```bash
export FIREWORKS_API_KEY=fw_...
```

Authoring commands (`init`, `new`, `validate`, `build`) need no key; only `run`
and `eval` call the provider.

## Step 1 — Scaffold a course

```bash
blendtutor init my-stats-course
```

Creates `my-stats-course/` with a `blendtutor.toml` manifest, a starter lesson
under `lessons/`, a matching eval suite, and a `.gitignore`. The fresh course is
immediately listable and runnable.

The generated manifest looks like this:

```toml
# Your course manifest: an ordered list of lessons, each pairing a stable course
# slug `id` with the lesson file `path` (relative to this manifest). Add lessons
# with `blendtutor new`, or edit this file by hand and `blendtutor list .`.
[[lessons]]
id = "hello"
path = "lesson_hello.yaml"
```

Each `[[lessons]]` entry maps a stable course slug (`id`) to a lesson file
(`path`, relative to the manifest). The order of entries is the order lessons
appear in the built site.

## Step 2 — Add an R lesson

```bash
blendtutor new lesson --lang r seed-data
```

Writes `lessons/seed_data.yaml` (R template) plus a sibling
`eval_seed_data.yaml` for grading cases. The `--lang` flag selects the runtime
the lesson targets (`r` or `python`).

Open the generated lesson file and edit it. Here is the R lesson from
`examples/write-less-code-r/01_seed_data.yaml` — a complete, working example
showing every field:

```yaml
lesson_name: "Seed Data"
language: R
description: "Create the survey_data data frame that anchors the Write Less Code arc"
textbook_reference: "Just Enough Software Engineering - Chapter 8: Write Less Code, Part II"

exercise:
  type: "function_writing"
  prompt: |
    Create a data frame called `survey_data` with 5 respondents and 6 stress
    items (stress_1 through stress_6) measured on a 1-5 scale, plus a
    respondent_id column.
  code_template: |
    survey_data <- data.frame(
      respondent_id = 1:5,
      # Add stress_1 through stress_6 columns here
    )
  solution: |
    survey_data <- data.frame(
      respondent_id = 1:5,
      stress_1 = c(3, 4, 5, 2, 1),
      stress_2 = c(2, 3, 4, 5, 1),
      stress_3 = c(1, 2, 3, 4, 5),
      stress_4 = c(5, 4, 3, 2, 1),
      stress_5 = c(4, 3, 2, 1, 5),
      stress_6 = c(1, 2, 3, 4, 5)
    )
  example_usage: |
    dim(survey_data)  # 5 7
    survey_data$stress_6  # 1 2 3 4 5
  success_criteria: |
    - survey_data has 5 rows and 7 columns
    - stress_6 column contains c(1, 2, 3, 4, 5)
  llm_evaluation_prompt: |
    You are evaluating student code for a software engineering course.

    Exercise: Create a data frame called `survey_data` with 5 respondents and 6
    stress items (stress_1 through stress_6) on a 1-5 scale, plus respondent_id.

    Student submitted this code:
    {student_code}

    Evaluate the code and call the respond_with_feedback function with your assessment.
    Set is_correct to true if the code meets all requirements, false otherwise.
    Provide brief, encouraging feedback (2-3 sentences) in feedback_message.

checks:
  - "stopifnot(identical(dim(survey_data), c(5L, 7L)))"
  - "stopifnot(identical(survey_data$stress_6, c(1, 2, 3, 4, 5)))"
```

### Key fields

- **`lesson_name`** — human-readable title shown in the browser site.
- **`language`** — `R` or `Python`; selects the interpreter at run time.
- **`exercise.prompt`** — the instructions the learner sees.
- **`exercise.code_template`** — starter code pre-filled in the editor.
- **`exercise.solution`** — reference solution (used by `eval` and embedded in
  the site for self-check).
- **`exercise.llm_evaluation_prompt`** — the grading prompt sent to the LLM.
  Must contain the `{student_code}` placeholder; blendtutor substitutes the
  learner's submission at run time.
- **`checks`** — R/Python expressions that must evaluate without error for the
  submission to pass the deterministic layer (runs before the LLM verdict).

## Step 3 — Add a Python lesson

```bash
blendtutor new lesson --lang python tally
```

Writes `lessons/tally.yaml` (Python template) plus `eval_tally.yaml`. Python
lessons can declare a `packages` list so the runner installs them in the
Pyodide/browser environment. Here is the Python lesson from
`examples/write-less-code-python/01_seed_data.yaml`:

```yaml
lesson_name: "Seed Data"
language: Python
description: "Create a pandas DataFrame with survey data including stress columns"
textbook_reference: "Just Enough Software Engineering - Chapter 8: Write Less Code, Part II"

exercise:
  type: "function_writing"
  prompt: |
    Create a pandas DataFrame called `survey_data` with 5 respondents and
    columns `respondent_id`, `stress_1` through `stress_6`. Each stress
    column should contain integer values between 1 and 5.
  code_template: |
    import pandas as pd

    # Create survey_data with 5 respondents and stress_1 through stress_6
  solution: |
    import pandas as pd

    survey_data = pd.DataFrame({
        'respondent_id': [1, 2, 3, 4, 5],
        'stress_1': [3, 4, 2, 5, 3],
        'stress_2': [2, 3, 4, 3, 2],
        'stress_3': [4, 5, 3, 4, 3],
        'stress_4': [3, 2, 4, 5, 4],
        'stress_5': [2, 3, 5, 4, 3],
        'stress_6': [4, 3, 2, 3, 5],
    })
  example_usage: |
    print(survey_data.shape)  # (5, 7)
    print(list(survey_data.columns))  # ['respondent_id', 'stress_1', ..., 'stress_6']
  success_criteria: |
    - DataFrame has 5 rows and 7 columns
    - Contains a `stress_6` column
    - All stress columns contain integers
  llm_evaluation_prompt: |
    You are evaluating student code for a software engineering course.

    Exercise: Create a pandas DataFrame called `survey_data` with 5 respondents
    and columns `respondent_id`, `stress_1` through `stress_6`.

    Student submitted this code:
    {student_code}

    Evaluate the code and call the respond_with_feedback function with your assessment.
    Set is_correct to true if the code meets all requirements, false otherwise.
    Provide brief, encouraging feedback (2-3 sentences) in feedback_message.

checks:
  - "assert len(survey_data) == 5"
  - "assert 'stress_6' in survey_data.columns"
  - "assert survey_data['stress_6'].sum() == 17"

packages:
  - pandas
```

The `packages` field is optional and only meaningful for Python lessons — it
tells the Pyodide runtime which packages to load (e.g. `pandas`).

## Step 4 — Register lessons in the manifest

After adding lessons, make sure each one is listed in `blendtutor.toml`. The
`new` command appends entries automatically, but if you hand-edit lesson files
you may need to update the manifest yourself:

```toml
[[lessons]]
id = "seed-data"
path = "01_seed_data.yaml"

[[lessons]]
id = "tally"
path = "02_tally.yaml"

[[lessons]]
id = "write-a-function"
path = "03_write_a_function.yaml"
```

The `id` is a stable slug used in URLs and eval reports; the `path` is relative
to the manifest file. See `examples/write-less-code-r/blendtutor.toml` and
`examples/write-less-code-python/blendtutor.toml` for complete five-lesson
manifests.

## Step 5 — Validate a lesson

```bash
blendtutor validate lessons/seed_data.yaml
```

Reports missing required fields and common authoring mistakes. Exit code is
nonzero when a lesson is invalid, so it drops cleanly into CI. Add `--format
json` for machine-readable output:

```bash
blendtutor validate lessons/seed_data.yaml --format json
```

## Step 6 — Run a submission

```bash
blendtutor run lessons/seed_data.yaml --code submission.R
```

Runs the submission through the real R interpreter, executes the `checks`, then
asks the LLM for a verdict. `--code <path>` reads a file; omit it to read from
stdin:

```bash
echo 'survey_data <- data.frame(respondent_id=1:5, stress_6=1:5)' | blendtutor run lessons/seed_data.yaml
```

Exit code reflects the verdict (correct / incorrect / error). Add `--format
json` for a structured report.

## Step 7 — Write an eval suite

Each lesson pairs with a sibling `eval_<name>.yaml` that contains sample
submissions and the verdict you expect a good grader to return. The `new`
command creates a starter file; here is the eval suite from
`examples/write-less-code-r/eval_01_seed_data.yaml`:

```yaml
# Eval suite for 01_seed_data — 4 cases: 2 correct, 2 incorrect (1 near-miss).
# The near-miss creates a data frame that runs cleanly but has wrong dimensions.
cases:
  # 1 — correct: full survey_data with 5 rows and 7 columns
  - submission: |-
      survey_data <- data.frame(
        respondent_id = 1:5,
        stress_1 = c(3, 4, 5, 2, 1),
        stress_2 = c(2, 3, 4, 5, 1),
        stress_3 = c(1, 2, 3, 4, 5),
        stress_4 = c(5, 4, 3, 2, 1),
        stress_5 = c(4, 3, 2, 1, 5),
        stress_6 = c(1, 2, 3, 4, 5)
      )
    expected: correct
  # 2 — incorrect (near-miss): runs cleanly but only 6 columns (missing stress_6)
  - submission: |-
      survey_data <- data.frame(
        respondent_id = 1:5,
        stress_1 = c(3, 4, 5, 2, 1),
        stress_2 = c(2, 3, 4, 5, 1),
        stress_3 = c(1, 2, 3, 4, 5),
        stress_4 = c(5, 4, 3, 2, 1),
        stress_5 = c(4, 3, 2, 1, 5)
      )
    expected: incorrect
```

Include **near-miss** cases — submissions that run cleanly but are subtly wrong.
These are the most valuable for measuring whether your grading prompt catches
realistic mistakes, not just syntax errors.

## Step 8 — Score the grading prompt

```bash
blendtutor eval lessons/seed_data.yaml
```

Replays the lesson's `eval_seed_data.yaml` cases through the full run pipeline
and reports how often the grader's verdict matches the expected label. This
lets you measure (and regression-test) grading accuracy before shipping. Because
evals score against whichever provider your API key selects, run them against
the same provider your deployed site will use.

Add `--format json` for machine-readable output:

```bash
blendtutor eval lessons/seed_data.yaml --format json
```

## Step 9 — Build a browser site

Once lessons validate and the eval suite passes, build a static browser site.
Learners edit code in the browser, submit, and get instant AI feedback with no
install required.

For an R course, target webR:

```bash
blendtutor build my-stats-course --target webr -o site
```

For a Python course, target Pyodide:

```bash
blendtutor build my-stats-course --target pyodide -o site
```

Emits a static site to `-o <dir>`: `index.html`, a per-lesson JSON index, the
in-browser runtime, and (if the course carries an `eval-report.json`) an
embedded eval-results page. `--target` picks the WASM runtime — `webr` for R
lessons, `pyodide` for Python.

The built `site/` directory is fully static and deploys to GitHub Pages as-is.
webR needs `SharedArrayBuffer`, which requires cross-origin isolation
(COOP/COEP headers); the build ships a vendored
[`coi-serviceworker`](https://github.com/gzuidhof/coi-serviceworker) shim so
the site works on GitHub Pages without header configuration. See the README for
deployment details.

## Complete example courses

The two reference courses in the repository are the best way to see the full
workflow end-to-end:

- **`examples/write-less-code-r/`** — five R lessons (`01_seed_data.yaml`
  through `05_rule_of_three.yaml`), each with a sibling eval suite, a
  `blendtutor.toml` manifest, and a committed `eval-report.json`.
- **`examples/write-less-code-python/`** — five Python lessons mirroring the R
  arc, declaring `packages: [pandas]`, with eval suites and a committed
  `eval-report.json`.

Browse those directories, copy a lesson file as a starting point, and adapt the
prompt, checks, and eval cases to your own exercises.
