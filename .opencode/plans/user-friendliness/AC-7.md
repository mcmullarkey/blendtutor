---
ac: 7
depends_on: AC-6
risk: low
status: spec
---

### AC-7 — eval terminal interpretation
- Spec mode: **spec-resolved** (single render-only module, closed design space, invariants found by full-test grep below)
- Key files: `crates/cli/src/output.rs` (`render_eval` :378-402, `emit_eval` :412-420, mod tests :422+), `crates/core/src/eval.rs` (`CaseResult::feedback_message()` :283-286 — accessor ALREADY EXISTS, no core change), `crates/cli/src/snapshots/blendtutor__output__tests__eval_human_matches_snapshot.snap`, `crates/cli/tests/eval.rs`, `docs/book/src/whole-game.md` (eval section :57-83)
- Invariants/constraints:
  - `render_eval` is PURE (`&EvalReport -> String`) — footer text derives from case data only; do NOT thread the lesson path / file name into the render (would break the pure seam, §2).
  - JSON arm non-regression: `output.rs:412-416` — `--format json` shape (`cases`, `accuracy`, per-case `feedback_message` already serialized) byte-unchanged; the change is human-format only.
  - Exit-code non-regression: mismatch exits 0 (pinned `tests/eval.rs:279-296`); `eval` reports, never gates.
  - Fixture gap honesty: `CaseResult.score` captures feedback VERBATIM — AC-7 must NOT truncate, summarize, or reword it (user sees the same message the grader produced; first-line-only is acceptable IF the message is single-line; full verbatim is the default).
- Literal grep result (render-token pins — done at plan time per mandate):
  - `"Accuracy:"` + row format `case N: expected X, got Y [match|mismatch]`: `output.rs:383,389-398`, snapshot `.snap` (:5,:7,:9), `output.rs:539-541` test-comment describes fixture (two matches, one mismatch → 2/3).
  - `"[mismatch]"` exactly-one-count assertion: `crates/cli/tests/eval.rs:83` — the next-steps footer MUST NOT contain the literal `"[mismatch]"` (bracketed token) or that count breaks. Footer uses plain words ("did not match" / "mismatched cases: 3").
  - `evidence`: mock provider in `tests/eval.rs` drives alpha/beta/gamma cases (:27-28) with mixed verdicts — mismatched case's feedback string available for assertion.
- **Executable spec**
  - predicate:
    - F1 (mismatch detail): human render shows, immediately under each `[mismatch]` case row, the grader's verbatim `feedback_message` for that case (labeled, e.g. indented `grader: <message>`); `[match]` rows carry no feedback line.
    - F2 (next-steps footer): when ≥1 mismatch, output ends with a guidance footer that (a) names the 1-based numbers of the mismatched cases, (b) points the author at `blendtutor eval <lesson> --case N` to inspect a single case, and (c) names `llm_evaluation_prompt` (and the reference `solution`) as the knobs that shape grading. All-matched run (accuracy 1/1) emits NO footer and NO guidance lines.
    - P1 (row/headline non-regression): headline stays `Accuracy: m/t (p.p%)`; per-case rows stay `case N: expected X, got Y [match|mismatch]`; token set unchanged so `tests/eval.rs` pins (incl. exactly-one `[mismatch]`) stay green.
    - P2 (JSON non-regression): `--format json` output byte-identical to before (existing JSON assertions in `tests/eval.rs` pass unedited).
    - P3 (exit-code non-regression): mismatch still exits 0.
  - probe: `cargo test -p blendtutor-cli` (output.rs unit tests incl. insta snapshot re-review via `cargo insta` + integration `tests/eval.rs`) — snapshot MUST be regenerated+reviewed, not blind-accepted.
  - negative:
    - footer-present-on-full-match arm: all-matched suite emits footer → FAIL (proves footer is mismatch-driven, not unconditional).
    - missing-feedback arm: mismatched case rendered without its feedback message → FAIL (proves the detail is the mismatch's own, not a template).
    - wrong-case-attribution arm: feedback attached to a `[match]` row, or numbering off-by-one vs `case N` rows → FAIL.
    - JSON-drift arm: any byte change in `--format json` output → FAIL (P2).
  - fixture status: EXISTING — `output.rs` mod tests fixture (two matches + one mismatch shape, :539-541) extended with asserted feedback strings; snapshot `.snap` updated via insta review; `tests/eval.rs` mock already returns per-case verdict messages. No new harness.
  - rubric anchor: §2 (render stays pure over `EvalReport`; no lesson path threading), §3.4 (feedback text sourced from `CaseResult::feedback_message()`, the single captured source — never re-derived), §4 (output.rs owns all rendering; no command change), §5.1 (footer builder is one pure helper, not scattered format! calls).
  - assertion-quality: exact-equality (snapshot + row formats) · observable-effect (feedback present for mismatched case) · absence (no footer on full match, no `[mismatch]` literal in footer) · exit-code (P3) — per `docs/test-strategy/assertion-quality.md`.
  - design intent (§2/§3/§4/§5): render_eval gains a pure `feedback line per mismatch` + pure `next_steps_footer(&[case_numbers])`; everything keyed off `EvalReport`'s own data; docs note in whole-game evals section minimal (one short paragraph + example), editing the region AC-6 finalized.

### Progress
- (none yet)

### Decision Log
- (none yet)

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open
