# blendtutor course

A starter course scaffolded by `blendtutor init`. Edit the example lesson, add
your own, and share the directory — everything a learner needs lives here.

## Layout

- `blendtutor.toml` — the course manifest: which lessons exist, and in what order.
- `lesson_hello.yaml` — an example lesson. Add more with `blendtutor new`.
- `eval_lesson_hello.yaml` — an example eval suite for the lesson above.
- `.gitignore` — keeps local LLM provider key files out of version control.

## Commands

- `blendtutor list .` — list the lessons in this course.
- `blendtutor validate lesson_hello.yaml` — check a lesson against the schema.
- `blendtutor run lesson_hello.yaml --code submission.R` — grade a submission.
- `blendtutor eval lesson_hello.yaml` — run the lesson's eval suite.

Set your provider key in the environment first (e.g. `FIREWORKS_API_KEY` or
`ANTHROPIC_API_KEY`) so `run` and `eval` can reach the grader.
