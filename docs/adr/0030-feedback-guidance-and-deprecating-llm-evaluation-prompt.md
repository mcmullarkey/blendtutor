# ADR-0030: `feedback_guidance` reaches the grader; `llm_evaluation_prompt` is deprecated

- Status: Accepted
- Date: 2026-11-08
- Amends: ADR-0003 (lesson schema), ADR-0006 (prompt layout), ADR-0008 and
  ADR-0020 (site and Quarto contracts)

## Context

While building `eval-compare` (ADR-0029) we found that `build_prompt` (Rust)
and `buildPrompt` (both JS copies) **never read `exercise.llm_evaluation_prompt`**.
Since the fixed prompt layout of ADR-0006, the grader has received a hardcoded
framing, the task, `success_criteria` (ADR-0020), the code, the output, and
the check results. Yet `llm_evaluation_prompt` is **required**, must contain
`{student_code}`, and the `eval` footer tells authors that grading "is shaped
by" it. Authors were maintaining a field that did nothing, and following advice
that could not work.

Meanwhile the dogfood comparison showed that authors need a lever on *how*
feedback is written. The winning candidate used a `success_criteria` bullet
("Feedback guides the learner to the fix without giving a working solution"),
but success criteria describe the code, not the feedback.

## Options

1. **Revive `llm_evaluation_prompt` as a full template.** It honors the field's
   name, but lets each lesson replace the injection-hardened, neutralized prompt
   (ADR-0006) with free text, and every existing value is stale boilerplate
   ("call respond_with_feedback…") written for a prompt that no longer exists.
2. **Keep using `success_criteria` for feedback instructions.** No schema change,
   but it conflates two constructs (what the code must do vs how feedback should
   read), the jingle-jangle problem in the schema itself.
3. **A new optional `feedback_guidance`** section, neutralized and inserted after
   the success criteria in every prompt builder, with `llm_evaluation_prompt`
   made optional and documented as unused.

## Decision

Option 3.

- **Prompt layout.** When `feedback_guidance` is present and non-blank, every
  builder emits `Feedback guidance:` plus the neutralized text after the
  success-criteria section (or the task, when there are no criteria) and before
  the code fence. When it is absent, the prompt is byte-identical to before.
- **Contracts.** `SiteLesson` gains `feedback_guidance` (always serialized,
  `null` when absent). Quarto authors write `::: {.feedback-guidance}`, the Lua
  filter carries it into the widget payload, and `export-quarto` emits it.
  Parity across Rust and both JS copies is required, because `eval` must
  measure the feedback learners actually get.
- **Deprecation.** `llm_evaluation_prompt` becomes `Option<String>`: accepted
  for compatibility, never sent, and when present still checked for
  `{student_code}` so existing error behavior is unchanged. The `eval` footer
  now names the real levers (task text, success criteria, feedback guidance,
  checks), and new lessons are scaffolded with `feedback_guidance`.

## Consequences

- Authors get a lever that works and is measurable with `eval-compare`.
- The constructs stay separate: success criteria (verdict validity) vs feedback
  guidance (leakage, actionability).
- Existing lessons keep validating. Their `llm_evaluation_prompt` text is inert
  and can be deleted at any time.
