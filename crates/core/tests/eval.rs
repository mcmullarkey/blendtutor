//! Integration tests for the eval model (`core::eval`).
//!
//! AC1: a sibling `eval_<lesson>.yaml` with synthetic submission + expected
//! polarity parses into a typed [`EvalSuite`] — arity, per-index polarity, and
//! the submission↔verdict positional binding are all pinned against the ported
//! fireworks-vitals ground truth. The expected polarity is the dedicated
//! `ExpectedVerdict` sum type (no stringly field), so an illegal verdict is a
//! parse error rather than data travelling downstream (ADR-0007).

use std::path::Path;

use blendtutor_core::eval::{ExpectedVerdict, parse_eval_suite};

/// Read a fixture from `tests/fixtures/evals/` by name.
fn eval_fixture(name: &str) -> String {
    let path = Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/evals/"
    ))
    .join(name);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("fixture {} should be readable: {e}", path.display()))
}

#[test]
fn parses_ten_fireworks_vitals_cases_into_typed_suite() {
    let yaml = eval_fixture("eval_fireworks_vitals.yaml");
    let suite = parse_eval_suite(&yaml).expect("the ported fireworks-vitals fixture is valid");

    assert_eq!(suite.cases.len(), 10, "all ten ported cases parse");

    // Per-index polarity pins both variants — so a constant or all-default impl
    // cannot pass — and the document order: idx 9 is the prompt-injection case,
    // which must stay last.
    assert_eq!(suite.cases[0].expected, ExpectedVerdict::Correct);
    assert_eq!(suite.cases[1].expected, ExpectedVerdict::Incorrect);
    assert_eq!(suite.cases[9].expected, ExpectedVerdict::Incorrect);

    // Submission↔verdict are bound positionally: case 0 carries its own
    // commented pseudocode, not case 1's uncommented text.
    assert!(
        suite.cases[0].submission.contains("# Read in a csv"),
        "case 0 carries its own submission, got: {:?}",
        suite.cases[0].submission
    );
}

#[test]
fn rejects_unknown_verdict_naming_the_offending_case() {
    let yaml = eval_fixture("eval_fireworks_vitals_bad_verdict.yaml");
    let err = parse_eval_suite(&yaml)
        .expect_err("an unparseable expected verdict must be rejected, not coerced to a default");
    let msg = err.to_string();

    // The error quotes the bad token rather than failing generically — and
    // proves it reached idx 2 (the corrupted case), not an earlier unrelated
    // error.
    assert!(
        msg.contains("maybe"),
        "error should quote the unknown verdict token, got: {msg}"
    );
    // …and names the offending case so the author can find it in the file,
    // rather than emitting a message that omits which case failed.
    assert!(
        msg.contains("case 2"),
        "error should name the offending case (index 2), got: {msg}"
    );
}
