# ADR-0031: `eval --repeat N`, run-to-run stability of an evaluation

- Status: Accepted
- Date: 2026-11-08

## Context

The grader is a stochastic LLM, so one `eval` run is one sample of the
measurement. Dogfooding lesson 03 showed it matters unevenly: verdicts were
identical across two runs (31/31), but baseline solution leakage moved from
11/14 to 13/14. Without a repeatability number, an author cannot tell a real
change from run-to-run noise. `eval-compare` (ADR-0029) controls for this by
pairing, but a single report says nothing about it.

## Options

1. **Document "run it twice".** Free, but informal and never reported.
2. **Agreement statistics across runs** (e.g. Fleiss' κ). Standard for raters,
   but at the ceiling we see (almost every case gets the same verdict) κ is
   unstable and hard to read.
3. **A test–retest summary:** cases with the same verdict on every run, with a
   Wilson interval, the flipped cases, and accuracy and leakage per run.

## Decision

Option 3. `blendtutor eval <lesson> --repeat N` (N ≥ 2, enforced by clap)
grades the suite N times and emits a `StabilityReport` instead of a single
report. Its core is a pure fold over N `EvalReport`s. `--repeat` conflicts with
`--case` and `--write-report`, which make no sense across runs.

## Consequences

- Authors see which cases are unstable (items worth rewriting) and how much a
  construct's rate wobbles between runs, before trusting a single run.
- Verdict agreement is the reliability of the *verdict*. Leakage-per-run is
  reported but not summarized; a per-case leakage agreement is a natural
  extension.
- N runs cost N times the grading calls.
