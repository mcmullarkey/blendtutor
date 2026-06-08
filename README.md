# blendtutor

A single-binary CLI for building interactive coding lessons — in R or Python —
with AI-powered feedback. Instructors author a course of exercises and grading
prompts; learners practice in the terminal or in a static browser site, getting
instant, personalized feedback on each submission.

Inspired by [swirl](https://swirlstats.com/) and [learnr](https://rstudio.github.io/learnr/),
and originally designed to complement the
[Just Enough Software Engineering](https://mcmullarkey.github.io/just-enough-software-engineering/)
textbook.

## What is blendtutor?

A course is a directory of lesson YAML files plus a `blendtutor.toml` manifest.
Each lesson describes an exercise prompt, a code template, checks, and an LLM
evaluation prompt. `blendtutor` handles the rest:

- **Instructors** scaffold a course, add lessons, validate them, dry-run the
  grading against sample submissions, score the grading prompt against known
  cases, and build a deployable browser site.
- **Learners** run a lesson, submit code, and get an AI verdict — in the
  terminal locally, or in a browser via [webR](https://docs.r-wasm.org/webr/) /
  [Pyodide](https://pyodide.org/) with no install.

## Installation

`blendtutor` is a Rust binary. Install it from a clone with Cargo:

```bash
git clone https://github.com/mcmullarkey/blendtutor.git
cd blendtutor
cargo install --path crates/cli
```

This puts a `blendtutor` executable on your `PATH` (`~/.cargo/bin`).

## API key

AI feedback calls an LLM provider. Set one of these in your environment:

- `FIREWORKS_API_KEY` — [Fireworks AI](https://fireworks.ai) (the default provider).
- `ANTHROPIC_API_KEY` — [Anthropic](https://www.anthropic.com) (used by built
  browser sites, which are bring-your-own-key / Anthropic-only).

```bash
export FIREWORKS_API_KEY=fw_...
```

Authoring commands (`init`, `new`, `validate`, `build`) need no key; only `run`
and `eval` call the provider.

## Authoring workflow

The instructor loop is **`init → new → validate → run → eval → build`**.

### `blendtutor init` — scaffold a course

```bash
blendtutor init my-course
```

Creates `my-course/` with a `blendtutor.toml` manifest, an example lesson under
`lessons/`, a matching eval suite, and a `.gitignore`. The fresh course is
immediately listable and runnable.

### `blendtutor new` — add a lesson

```bash
blendtutor new lesson --lang r greet         # add lessons/greet.yaml (R)
blendtutor new lesson --lang python tally    # add lessons/tally.yaml (Python)
```

Writes a lesson YAML template plus a sibling `eval_<name>.yaml` for grading
cases. `--lang` selects the runtime the lesson targets (`r` or `python`).

### `blendtutor validate` — check a lesson

```bash
blendtutor validate lessons/greet.yaml
blendtutor validate lessons/greet.yaml --format json   # machine-readable
```

Reports missing required fields and common authoring mistakes. Exit code is
nonzero when a lesson is invalid, so it drops cleanly into CI.

### `blendtutor run` — execute + get feedback

```bash
blendtutor run lessons/greet.yaml --code submission.R
echo 'greet <- function(x) paste("hi", x)' | blendtutor run lessons/greet.yaml
```

Runs a submission through the real interpreter, then asks the LLM for a verdict.
`--code <path>` reads a file; omit it to read the submission from stdin. Add
`--format json` for a structured report. Exit code reflects the verdict
(correct / incorrect / error).

### `blendtutor eval` — score the grading prompt

```bash
blendtutor eval lessons/greet.yaml
blendtutor eval lessons/greet.yaml --format json
```

Replays the lesson's `eval_<name>.yaml` cases through the full run pipeline and
reports how often the grader's verdict matches the expected label — so you can
measure (and regression-test) grading accuracy before shipping. Because evals
score against whichever provider your API key selects, run them against the same
provider your deployed site will use.

### `blendtutor build` — assemble a browser site

```bash
blendtutor build my-course --target webr -o site      # R lessons → webR
blendtutor build my-course --target pyodide -o site   # Python lessons → Pyodide
```

Emits a static site to `-o <dir>`: `index.html`, a per-lesson JSON index, the
in-browser runtime, and (if the course carries an `eval-report.json`) an
embedded eval-results page. `--target` picks the WASM runtime — `webr` for R
lessons, `pyodide` for Python.

## Deploying to GitHub Pages

The built `site/` directory is fully static, so it deploys to **GitHub Pages**
as-is (push it to a `gh-pages` branch or wire it into a Pages workflow).

One caveat: webR needs `SharedArrayBuffer`, which browsers only enable under
**cross-origin isolation** — i.e. `COOP`/`COEP` response headers that GitHub
Pages cannot set. To work around this, the build ships a vendored
[`coi-serviceworker`](https://github.com/gzuidhof/coi-serviceworker) shim that
re-serves the page with the required COOP/COEP headers from a service worker, so
the site is cross-origin isolated on Pages without any header configuration.
(Pyodide-only sites boot on the main thread and do not require this, but the shim
ships for both targets and is harmless when unused.)

## License

MIT License — see the `LICENSE` file for details.
