# The whole game

This chapter walks one course end to end: scaffold it, add a lesson, validate
it, run it, grade it with the LLM judge, generate an eval report, and deploy it
two ways — as a static browser site and as a Quarto document. Every command is
copy-paste ready. For the field-by-field anatomy of a lesson file, see
[Creating Lessons](./creating-lessons.md); for the Quarto extension and site
deployment details, see the [README](../../../README.md).

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

## Eval — grade it with the LLM judge

`eval` runs the lesson's cases through the LLM judge. This makes real paid LLM
calls, so set the API key first and expect spend:

```bash
export FIREWORKS_API_KEY=fw_...
blendtutor eval lesson_hello.yaml
```

The starter lesson ships two cases — a correct submission and an incorrect one.
Use `--case N` for a single case, `--format json` for JSON output.

## Eval report — generate the report

`eval-report` aggregates every graded run into a browsable report under
`docs/evals/<lesson>/`, where `<lesson>` is the stem of the lesson filename —
`lesson_hello.yaml` reports to `docs/evals/lesson_hello/`:

```bash
blendtutor eval-report lesson_hello.yaml
```

This needs the same `FIREWORKS_API_KEY` as `eval`.

## Reading the eval report

The report is static files under `docs/evals/lesson_hello/` — no server
required. The committed starter report shows the shape.

`index.json` is the overview: the eval slug, run counts, `graded`, `fails`, and
`best` — the best score across configs (0.76 for the starter).

Each graded run has a `grade.yaml` under `.../runs/<case>/default/.../grades/`
with an `outcome` (`pass` or `fail`), a `score`, and `checks`. The judge check
scores five metrics from 0.0 to 5.0:
`verdict_rationale_correctness`, `actionability`, `references_check_results`,
`no_solution_leak`, `no_hallucinated_errors`.

A case passes when its score is at least the 0.8 threshold. The starter report
has one of each: case-1 passes at 0.96 with every metric at 4.0 or above, and
case-2 fails at 0.56 — its `no_solution_leak` scored 0.0 because the feedback
gave away the answer string. A failing grade is evidence the case needs
rework, and the metrics say exactly where.

`output.txt` holds the judged feedback: line 1 is `verdict: correct` or
`verdict: incorrect`, and the remaining lines are the feedback text the judge
scored.

Two gotchas. First, `index.html` is a single-page app that fetches the report
data over HTTP, so opening it directly with `file://` shows a blank page. Serve
the directory instead:

```bash
cd docs/evals/lesson_hello && python3 -m http.server
```

Second, the `lesson` field in `eval.json` holds the absolute path the eval ran
against — treat the report as evidence of a run, not as a portable path
reference.

## Build — a static browser site

`build` turns the course into a fully static site with an in-browser runtime:

```bash
blendtutor build hello-course --target webr -o site
```

`--target` picks the runtime: `webr` for R lessons, `pyodide` for Python. The
`site/` output deploys to GitHub Pages as-is, and the build embeds an
eval-results page when the course has a report.

## Quarto deploy

`export-quarto` prints the lesson as a Quarto fenced div, ready to paste into a
Quarto document:

```bash
blendtutor export-quarto lesson_hello.yaml
```

````markdown
::: {.blendtutor language="r"}
<!-- the lesson's prompt and code template, rendered from the YAML -->
:::
````

To render exercises, install the blendtutor Quarto extension — see the
[README](../../../README.md) for requirements (Quarto 1.4 or newer) — then
render:

```bash
quarto add mcmullarkey/blendtutor
quarto render
```
