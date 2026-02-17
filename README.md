# blendtutor

A framework for creating interactive R coding lessons with AI-powered feedback — like [learnr](https://rstudio.github.io/learnr/), but for the console with LLM evaluation.

## What is blendtutor?

**blendtutor** lets educators build lesson packages that give students instant, personalized feedback on coding exercises. You write lesson YAML files describing the exercise and how to grade it, and blendtutor handles the interactive session + AI evaluation via [Fireworks AI](https://fireworks.ai).

Inspired by [swirl](https://swirlstats.com/) and originally designed to complement the [Just Enough Software Engineering](https://mcmullarkey.github.io/just-enough-software-engineering/) textbook.

## Installation

```r
# install.packages("pak")
pak::pak("mcmullarkey/blendtutor")
```

Or from a local clone:

```bash
git clone https://github.com/mcmullarkey/blendtutor.git
cd blendtutor
```

```r
devtools::install()
```

## Prerequisites

AI evaluation requires a Fireworks API key:

1. Sign up at [fireworks.ai](https://fireworks.ai)
2. Get your key from [fireworks.ai/api-keys](https://fireworks.ai/api-keys)
3. Add to `.Renviron`:
   ```r
   usethis::edit_r_environ()
   # Add line: FIREWORKS_API_KEY=fw_...
   ```
4. Restart R

## Creating a lesson package

The main use case is scaffolding your own package of lessons:

```r
library(blendtutor)

# Scaffold a new lesson package
create_lesson_package("~/my.lessons", lesson_name = "pseudocode_planning")
```

This creates a standard R package with:

```
my.lessons/
  DESCRIPTION              # blendtutor in Imports
  inst/lessons/            # Lesson YAML files
  evals/                   # Eval scripts for testing grading accuracy
  .claude/skills/          # Claude Code skill for guided lesson authoring
  README.md                # Step-by-step walkthrough
```

### Example lesson package

See [justenougheng](https://github.com/mcmullarkey/justenougheng) for a complete example of a blendtutor lesson package with multiple lessons and evals.

### Writing lessons

Lessons are YAML files in `inst/lessons/`. The key fields:

```yaml
lesson_name: "Writing Pseudocode"
description: "Practice translating requirements into pseudocode"

exercise:
  prompt: |
    Write pseudocode for a function that finds the maximum value in a list.
  llm_evaluation_prompt: |
    You are evaluating student code for a software engineering course.
    Exercise: Write pseudocode for finding the max value in a list.

    Student submitted:
    {student_code}

    Evaluate and call respond_with_feedback with your assessment.
```

The `{student_code}` placeholder is required — blendtutor inserts the student's submission before sending to the LLM.

### Adding more lessons to an existing package

```r
# Add a new lesson YAML template
use_blendtutor_lesson("loop_basics")

# Add a matching eval script
use_blendtutor_evals("loop_basics")
```

### Validating and testing

```r
# Check a lesson for required fields and common issues
validate_lesson("inst/lessons/pseudocode_planning.yaml")

# Run evals to test your grading prompt against known submissions
source("evals/eval_pseudocode_planning.R")
```

### Installing your lesson package

```r
devtools::install()
blendtutor::invalidate_lesson_cache()
blendtutor::list_lessons()
```

If you push to Github, students can install your package using pak, devtools, etc. and all its lessons appear alongside any others.

## Student workflow

Once a lesson package is installed, students interact with it in the R console:

```r
library(blendtutor)

list_lessons()                       # See available lessons across all packages
start_lesson("pseudocode_planning")  # Start a lesson

open_editor()                        # Opens editor with template
# Write code, save, close
submit_code()                        # Get AI feedback

# Iterate: open_editor() -> edit -> submit_code()
```

Code persists between submissions — students refine based on feedback rather than starting over.

### Example session

blendtutor ships with a built-in example lesson:

```r
> start_lesson("add_two_numbers")

============================================================
  Blendtutor: Interactive Coding Lessons
  Lesson: Writing Your First Function
  Reference: Just Enough Software Engineering - Chapter 2
============================================================

Write a function called 'add_two' that takes two numeric
arguments (x and y) and returns their sum.

> submit_code("add_two <- function(x, y) x + y")

Evaluating your code with AI...

FEEDBACK:
------------------------------------------------------------
Excellent work! Your function correctly adds two numbers
using clean, idiomatic R syntax.
------------------------------------------------------------

Congratulations! Lesson complete!
```

## API reference

### Educator functions

| Function | Description |
|---|---|
| `create_lesson_package(path)` | Scaffold a new lesson package |
| `use_blendtutor_lesson(name)` | Add a lesson YAML template |
| `use_blendtutor_evals(name)` | Add an eval script template |
| `validate_lesson(path)` | Check a lesson YAML for issues |

### Student functions

| Function | Description |
|---|---|
| `list_lessons()` | Show all available lessons |
| `start_lesson(name)` | Begin a lesson |
| `open_editor()` | Open code in your editor |
| `submit_code()` | Submit for AI evaluation |
| `reset_lesson()` | Clear current lesson state |

## Cross-package discovery

blendtutor automatically discovers lessons from any installed package that lists `blendtutor` in its `Imports` and has YAML files in `inst/lessons/`. Students see all lessons from all packages in a single `list_lessons()` call. Use `invalidate_lesson_cache()` after installing or removing lesson packages.

## Related projects

- [learnr](https://rstudio.github.io/learnr/) — Interactive tutorials with Shiny
- [swirl](https://swirlstats.com/) — Console-based interactive R learning
- [Fireworks AI](https://fireworks.ai) — Fast inference API
- [Just Enough Software Engineering](https://mcmullarkey.github.io/just-enough-software-engineering/) — Companion textbook

## License

MIT License — see LICENSE file for details.
