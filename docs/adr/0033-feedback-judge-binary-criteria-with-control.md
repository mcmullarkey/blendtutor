# ADR-0033: A feedback judge with binary criteria, rationale first, consensus, and a built-in control

- Status: Accepted
- Date: 2026-11-10

## Context

`eval-compare` shipped `feedback_guidance` on lesson 03 because solution
leakage fell from 13/14 to 0/14 (ADR-0029/0030). Nothing measured whether the
feedback stayed *helpful*: a grader that only says "not quite, keep trying" also
leaks nothing. Two constructs are unmeasured:

- **Diagnosis:** does the feedback name the mistake this submission makes?
- **Actionability:** does it give the learner a concrete next step?

Neither has a deterministic measure, so this needs an LLM judge. A judge is a
new instrument, and its numbers mean nothing until it is validated. The repo's
existing smevals judge (`scripts/smevals/judge_feedback.py`) scores five
dimensions 0–5 in one call and averages them, which bundles constructs and
invites halo effects.

## Options

1. **Reuse the smevals rubric judge.** Already exists, but it is multi-construct,
   Likert-scaled, and has no validity evidence.
2. **One holistic "feedback quality" score.** Simple, but it measures nothing in
   particular.
3. **One binary judge per criterion,** each with its rationale before its
   verdict, sampled k times, and a control that tests the judge on every run.

## Decision

Option 3, as `blendtutor eval <lesson> --judge`.

- **Scope.** Only cases the grader called incorrect are judged; that is where
  feedback must diagnose and guide. Diagnosis needs a misconception tag, and
  actionability applies to every judged case.
- **Separate calls per criterion,** so one impression cannot carry over into the
  other verdict (halo).
- **Binary verdicts** with an explicit construct definition in each prompt,
  anchored to a yes/no question, which raters (human or model) agree on more
  readily than on a scale.
- **Explanation before verdict.** The tool schema lists `explanation` before
  `pass` and the prompt asks for reasoning first, so the verdict is conditioned
  on the reasoning rather than justified afterwards. Schema properties reach the
  model in alphabetical order (schemars over a sorted map), not declaration
  order, so the field is named to sort first, and a unit test pins it. A field
  named `rationale` would have silently asked for the verdict first.
- **Reference-guided diagnosis.** The judge sees the item's misconception label
  and answers "does the feedback name *this*?". The construct is defined by the
  blueprint, not inferred by the judge.
- **Consensus.** Each judgment is the majority of k samples (`--judge-samples`,
  default 3), which reduces random error. Agreement among the k samples is
  reported as the judge's own consistency.
- **Built-in control.** Each tagged item is also judged against a **mismatched**
  misconception (the next blueprint entry). A judge that passes the right label
  95% of the time is uninformative if it also passes wrong labels: the control
  estimates the false-positive rate on every run. Overlapping misconceptions
  mean it need not be zero, but a high rate means the judge is not reading the
  feedback, or the feedback names everything.
- **`--judge-model`** lets the judge differ from the grader, which mitigates
  self-preference when both come from one model family.

## Consequences

- Each judged case costs 3 criteria × k calls.
- The pass rates are **not yet validated against people**. The next slice adds
  human labels (an Artifact labeling page) and reports TPR/TNR per criterion.
  The control is evidence of specificity, not of agreement with humans.
- Actionability can become a guardrail in `eval-compare` once it is validated.
