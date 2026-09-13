---
name: blendtutor-course
description: Author a blendtutor course end to end — scaffold the course, write R or Python lessons with checks, solutions, hints, and success criteria, write a minimal eval suite, verify everything locally, then produce a Quarto snippet/page or a static browser site. Use when someone wants to create or extend a blendtutor course, turn a chapter or tutorial into interactive exercises, write eval cases for a lesson, or publish lessons to Quarto or GitHub Pages.
argument-hint: <source material (chapter file or URL)> [r|python] [quarto|site]
---

# blendtutor course authoring

Turn source material into blendtutor lessons that grade correctly and render well.
One lesson = one exercise = one `lessons/<id>.yaml` plus its `lessons/eval_<id>.yaml`.

## 0. Preconditions

- `blendtutor --version` is **0.2.0 or newer** (older versions drop `packages`,
  `gotchas`, and `success_criteria` from Quarto exports). Install from
  <https://github.com/mcmullarkey/blendtutor/releases>.
- R lessons: `Rscript` on PATH. Python lessons: `uv` on PATH.
- Quarto output: Quarto >= 1.4.
- `run` and `eval` make **paid LLM calls** and need `FIREWORKS_API_KEY` (or
  `ANTHROPIC_API_KEY`) in the environment. Never write a key into a file you commit.

## 1. Gather

Ask only for what you cannot infer:

- **Source material**: read the chapter or tutorial; pick the one skill each exercise practices.
- **Language**: `r` or `python`, per lesson.
- **Output**: Quarto (snippet into an existing book/page, or a standalone page) and/or a static site.
- **Where the course lives**: an existing directory holding `blendtutor.toml`, or a new one.

## 2. Scaffold

```bash
blendtutor init <course-dir>                      # only if no blendtutor.toml exists yet
cd <course-dir>
blendtutor new lesson --lang <r|python> <id>      # writes lessons/<id>.yaml + lessons/eval_<id>.yaml
```

`new` registers the lesson in `blendtutor.toml`. Paths are one argument:
`lessons/<id>.yaml`, never `lessons <id>.yaml`.

## 3. Write the lesson

Replace the scaffold's hello-world content. Keep every field short.

```yaml
lesson_name: "<id>"
language: Python                        # or R
description: "<one line: the skill practiced>"
textbook_reference: "<book - chapter>"  # optional

exercise:
  type: "function_writing"
  prompt: |
    <2-3 sentences. Name every variable or function the checks rely on.>
  code_template: |
    <inline data + comment scaffolding the learner fills in>
  solution: |
    <complete answer; must pass every check>
  hints: |
    - <one bullet per hint; every non-empty line starts with "- ">
  gotchas: |                            # optional: common mistakes, also bullets
    - <a pitfall learners hit>
  success_criteria: |
    - <what a correct answer does, including what checks cannot see>
  llm_evaluation_prompt: |
    You are grading a beginner exercise on <skill>.

    The student submitted this code:
    {student_code}

    <What makes it correct, and what makes it incorrect even if it runs.>
    Call respond_with_feedback with two or three encouraging sentences.

checks:                                 # top level, not inside exercise
  - "assert <expression about the learner's objects>"   # R: "stopifnot(<expression>)"

packages:                               # only if needed
  - pandas
```

Rules that keep lessons working in the browser:

- **Inline the data.** Build a small data frame in `code_template` instead of
  loading a dataset package; webR and Pyodide may not have it. Choose values
  whose correct answer is easy to assert (for example, averages like 48.0 and 39.0).
- **Checks assert results, not source text.** They run after the learner's code
  in the same session and fail if they raise. Any lesson with at least one check
  gets a Check button.
- **No checks for prose-like exercises** (pseudocode, explanations): there is
  nothing to assert. The grader, driven by `success_criteria`, does the work.
- **`success_criteria` reaches the grader** (0.2.0+). Put the lesson's real
  point there, especially what a check cannot verify (structure, style, naming).
- **`hints` and `gotchas` are bullet lists**, or `validate` rejects them.
- **`packages`** names cannot contain quotes, commas, or spaces.

## 4. Write the minimal eval

`lessons/eval_<id>.yaml`, next to the lesson. Four cases, each with a comment:

```yaml
# Eval suite for <id>.yaml: 2 correct, 2 incorrect.
cases:
  # Correct: the canonical solution
  - submission: |-
      <solution>
    expected: correct
  # Correct: different names or methods, same intent
  - submission: |-
      <alternative that still meets every success criterion>
    expected: correct
  # Incorrect: near-miss that passes the checks but misses the lesson's point
  - submission: |-
      <runs cleanly, right output, violates a success criterion>
    expected: incorrect
  # Incorrect: a realistic mistake the checks catch
  - submission: |-
      <wrong result, e.g. reversed sort or missing step>
    expected: incorrect
```

The near-miss case is what measures the grading prompt; do not skip it. For
lessons without checks, make both incorrect cases content mistakes (a missing
step, the wrong operation).

## 5. Verify locally (no paid calls)

```bash
blendtutor validate lessons/<id>.yaml
blendtutor list .
```

Then run the solution and every eval case against the checks. Python:

```bash
uv run --no-project --with pyyaml --with <packages> python - <<'PY'
import contextlib, io, yaml
lesson = yaml.safe_load(open("lessons/<id>.yaml"))
cases = yaml.safe_load(open("lessons/eval_<id>.yaml"))["cases"]
def run(label, code):
    env = {}
    with contextlib.redirect_stdout(io.StringIO()):
        exec(code, env)
    results = []
    for check in lesson.get("checks", []):
        try:
            exec(check, env); results.append("pass")
        except Exception as e:
            results.append(f"fail({type(e).__name__})")
    print(label, results)
run("solution", lesson["exercise"]["solution"])
for i, case in enumerate(cases, 1):
    run(f"case {i} ({case['expected']})", case["submission"])
PY
```

R (pass the lesson path; the eval file is found next to it):

```bash
uv run --no-project --with pyyaml python - lessons/<id>.yaml <<'PY'
import pathlib, subprocess, sys, tempfile, yaml
lesson_path = pathlib.Path(sys.argv[1])
lesson = yaml.safe_load(lesson_path.read_text())
cases = yaml.safe_load(lesson_path.with_name("eval_" + lesson_path.name).read_text())["cases"]
def run(label, code):
    checks = "\n".join(
        f'r <- c(r, tryCatch({{ {c} ; "pass" }}, error = function(e) "fail"))' for c in lesson.get("checks", [])
    )
    script = f"r <- character()\ninvisible(capture.output({{\n{code}\n}}))\n{checks}\ncat(r)\n"
    with tempfile.NamedTemporaryFile("w", suffix=".R", delete=False) as f:
        f.write(script)
    out = subprocess.run(["Rscript", f.name], capture_output=True, text=True)
    pathlib.Path(f.name).unlink(missing_ok=True)
    print(label, out.stdout.strip() or f"submission error: {out.stderr.strip().splitlines()[-1:]}")
run("solution", lesson["exercise"]["solution"])
for i, case in enumerate(cases, 1):
    run(f"case {i} ({case['expected']})", case["submission"])
PY
```

Expect: solution passes; correct cases pass; the near-miss passes (only the
grader can reject it); the checks-catch case fails. Fix the lesson if not.

Only then, **after confirming with the user** (paid calls):

```bash
blendtutor eval lessons/<id>.yaml                  # grader accuracy against expected verdicts
blendtutor eval lessons/<id>.yaml --write-report   # also saves eval-report.json for the site
```

If the grader misjudges a case, tighten `success_criteria` and the
`llm_evaluation_prompt`, then re-run `eval`.

## 6. Publish

### Quarto

```bash
blendtutor export-quarto lessons/<id>.yaml               # snippet: paste into an existing .qmd
blendtutor export-quarto --document lessons/<id>.yaml    # standalone page with front matter
blendtutor export-quarto --key-page > api-key.qmd        # optional API key page
```

Project setup, once:

1. Run `quarto add mcmullarkey/blendtutor` **from the folder that contains
   `_quarto.yml`** (or the standalone `.qmd`). Installed anywhere else, the
   filter never loads and exercises render as plain text.
2. Enable the filter with `filters: [mcmullarkey/blendtutor]` in `_quarto.yml`
   **or** in the page's front matter, not both. `--document` pages already declare it.
3. Commit `_extensions/` so CI renders (for example, a GitHub Pages workflow).
4. Preview with `quarto preview`. Opening HTML via `file://` blocks the widget's JavaScript.

R runs in `type: book` projects without cross-origin isolation, on webR's
slower channel; `coi: true` speeds up standalone pages only.

### Static site

```bash
blendtutor build . --target pyodide -o site   # Python course
blendtutor build . --target webr -o site      # R course
```

One target per build, so keep R and Python lessons in separate courses. The
site deploys to GitHub Pages as-is. `--password` encrypts it; `--embed-key`
(requires `--password`) puts a real API key inside the encrypted site, where
anyone with the password can use it. Mention that risk before suggesting it.

## 7. Report

Tell the user which files you created, the local check results per case, whether
paid evals ran (and their accuracy), and the exact command or snippet for their
chosen output.
