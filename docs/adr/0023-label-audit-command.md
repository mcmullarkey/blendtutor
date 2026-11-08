# ADR-0023: Label audit: a free `audit` command that checks eval labels against lesson checks

- Status: Accepted
- Date: 2026-11-08

## Context

ADR-0022 classifies the grader's **actual** verdict against the lesson's
checks. That measures the grader. It catches a mislabeled eval item only when
the grader happens to agree with the bad label; when the grader disagrees, the
bad label shows up only as an ordinary `[mismatch]`. The eval's answer key is
itself an instrument, and it needs its own audit.

The audit's logic is the same as ADR-0022's. An `expected` label and a verdict
are both polarity claims about one construct, so `consistency::classify` applies
unchanged. Only two things differ:

- **The instrument under test.** The audit classifies the author's label instead
  of the grader's verdict.
- **The cost.** Running the checks needs R, but no LLM call and no API key, so
  the audit can run on every commit.

## Options

1. **`eval --audit-labels` flag.** Reuses the command, but `eval` means "spend
   money grading". Its exit code is also deliberately 0 on low accuracy, while
   the audit needs to fail on a violation.
2. **Extend `validate`.** Fits "is this instrument well-formed?", but `validate`
   is an instant structural parse with no subprocess, and the audit runs the
   lesson's interpreter once per case.
3. **A new `audit <lesson>` subcommand.** It has its own exit-code contract:
   1 on any violation, 0 otherwise (including discordant, and including "no
   checks", which is not assessable rather than failed).

## Decision

Option 3. `core::eval::audit_labels(lesson, suite)` is the thin effectful shell.
It runs `grade::run_checks` once per case, builds no provider, and returns a
`LabelAuditReport` of `(expected, Consistency)` rows, where each `Consistency`
comes from `classify(&expected, &checks)`. The CLI renders it in label language
("labeled correct but a check failed"), never grader language. JSON mirrors the
eval shape: `{"cases": [{"expected", "consistency"}]}`.

## Consequences

- Mislabeled items are caught for free, before any paid `eval` run, and can
  gate CI.
- One classification function now serves two instruments. If their semantics
  ever diverge, the divergence needs its own type, not a flag on `classify`.
- The report doesn't say *which* check failed yet. Authors open the lesson to
  see. Adding the check detail is deferred.
