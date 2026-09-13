# ADR-0020: Success criteria reach the LLM feedback prompt

- Status: Accepted
- Date: 2026-09-13
- Amends: ADR-0006 (prompt layout), ADR-0008 (SiteLesson contract)

## Context

ADR-0006 replaced each lesson's `llm_evaluation_prompt` with one fixed,
injection-hardened prompt: task, fenced submission, captured output, check
results. The lesson schema still accepts `exercise.success_criteria`, but no
consumer reads it — not `build_prompt`, not the `SiteLesson` JSON, not the
Quarto filter. So a lesson graded by the LLM alone (for example "write
pseudocode as comments", which has no executable checks) is judged against the
prompt text only, and the author's rubric is silently discarded in the CLI,
the static site, and Quarto pages.

## Options

1. **Restore `llm_evaluation_prompt` templates.** Gives authors full control,
   but reopens the prompt-injection surface ADR-0006 closed and ships an
   author-only field to the browser (ADR-0008 §3.2 leak).
2. **Fold criteria into `exercise.prompt` by convention.** No code change, but
   learners then see grading text as the task, and existing lessons stay
   unrubric'd.
3. **Add an optional, neutralized "Success criteria" section to the fixed
   prompt**, carried through every learner-side contract.

## Decision

Option 3.

- **Prompt layout (amends ADR-0006).** When `success_criteria` is present,
  `build_prompt` (Rust) and `buildPrompt` (both JS copies) insert
  `Success criteria:` and the neutralized text immediately after the task
  section. When absent, the prompt is byte-identical to the ADR-0006 layout,
  so existing snapshots and lessons are unaffected.
- **Contracts (amends ADR-0008).** `SiteLesson` gains `success_criteria`
  (always serialized, `null` when absent, mirroring `solution`/`hints`). The
  Quarto payload grows from 9 to 10 keys; authors write criteria in a nested
  `::: {.success-criteria}` div, which `export-quarto` emits from the lesson.
- **Not shipped:** `llm_evaluation_prompt` remains author-only.

## Consequences

- Criteria are visible in page source, like `solution` already is. They are
  grading guidance, not secrets.
- Three prompt builders must stay in lockstep (Rust, `assets/shared/feedback.js`,
  `_extensions/.../exercise-feedback.js`); tests assert the same section label
  and placement in each.
- `scripts/tests/verify_filter_output.py` and the demo-book vendored extension
  copy update with the 10-key contract.
