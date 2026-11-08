//! Solution leakage: does the grader's feedback hand the learner a working fix?
//! (ADR-0028)
//!
//! Feedback **leaks** when one of its fenced code blocks, appended to the
//! learner's submission, makes the lesson's checks pass when the submission
//! alone did not. That is measured by execution, not by a judge.
//!
//! The pure core is here: [`code_blocks`] extracts fenced blocks, [`assessable`]
//! decides whether a case can be measured, and [`decide`] turns the check results
//! of each attempted fix into a [`Leakage`]. [`measure`] is the thin effectful
//! step that runs the attempts. This module does not judge whether a *partial*
//! hint gives too much away; it measures complete fixes only.

use serde::{Serialize, Serializer};

use super::{ExpectedVerdict, wilson};
use crate::grade::{CheckOutcome, run_checks, select_runner};
use crate::lesson::Lesson;

/// Whether an assessable case's feedback handed over a working fix.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Leakage {
    /// A code block from the feedback, appended to the submission, passes every
    /// check.
    Leaked,
    /// No code block fixes the submission (or the feedback has no code).
    Withheld,
}

impl Leakage {
    /// The wire token.
    pub const fn token(self) -> &'static str {
        match self {
            Leakage::Leaked => "leaked",
            Leakage::Withheld => "withheld",
        }
    }
}

impl Serialize for Leakage {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.token())
    }
}

/// Every fenced code block (```` ``` ````, with or without a language tag) in
/// `message`, in order. An unclosed fence contributes nothing.
pub fn code_blocks(message: &str) -> Vec<String> {
    let mut blocks = Vec::new();
    let mut open: Option<Vec<&str>> = None;
    for line in message.lines() {
        let fence = line.trim_start().starts_with("```");
        match (open.as_mut(), fence) {
            (None, true) => open = Some(Vec::new()),
            (Some(body), false) => body.push(line),
            (Some(_), true) => blocks.push(open.take().unwrap_or_default().join("\n")),
            (None, false) => {}
        }
    }
    blocks
}

/// Every snippet of `message` that might be runnable code, in order and without
/// duplicates: fenced blocks, inline code spans, and blank-line-separated
/// paragraphs outside fences. Graders do not always fence their code. Dogfooding
/// found full solutions in inline spans and bare lines, which a fences-only
/// reading missed (ADR-0028 amendment). Prose candidates are harmless: they
/// fail to parse, so their attempt never passes a check.
pub fn candidates(message: &str) -> Vec<String> {
    let mut found: Vec<String> = Vec::new();
    let mut push = |snippet: &str| {
        let snippet = snippet.trim();
        if !snippet.is_empty() && !found.iter().any(|seen| seen == snippet) {
            found.push(snippet.to_string());
        }
    };
    for block in code_blocks(message) {
        push(&block);
    }
    let mut in_fence = false;
    let mut paragraph: Vec<&str> = Vec::new();
    let mut prose = String::new();
    for line in message.lines() {
        if line.trim_start().starts_with("```") {
            in_fence = !in_fence;
            push(&paragraph.join("\n"));
            paragraph.clear();
            continue;
        }
        if in_fence {
            continue;
        }
        prose.push_str(line);
        prose.push('\n');
        if line.trim().is_empty() {
            push(&paragraph.join("\n"));
            paragraph.clear();
        } else {
            paragraph.push(line);
        }
    }
    push(&paragraph.join("\n"));
    for (index, span) in prose.split('`').enumerate() {
        if index % 2 == 1 {
            push(span);
        }
    }
    found
}

/// Whether leakage can be measured for a case: it is labeled incorrect, the
/// lesson has checks, and the submission alone fails at least one of them
/// (a crash, `NotRun`, leaves nothing for appended code to fix).
pub fn assessable(expected: ExpectedVerdict, checks: &[CheckOutcome]) -> bool {
    expected == ExpectedVerdict::Incorrect
        && checks
            .iter()
            .any(|c| matches!(c, CheckOutcome::Fail { .. }))
        && !checks
            .iter()
            .any(|c| matches!(c, CheckOutcome::NotRun { .. }))
}

/// Leaked when any attempt (one per code block) passes every check.
pub fn decide(attempts: &[Vec<CheckOutcome>]) -> Leakage {
    let fixes = |outcomes: &Vec<CheckOutcome>| {
        !outcomes.is_empty() && outcomes.iter().all(|c| *c == CheckOutcome::Pass)
    };
    if attempts.iter().any(fixes) {
        Leakage::Leaked
    } else {
        Leakage::Withheld
    }
}

/// Run each of `message`'s code blocks appended to `submission` against the
/// lesson's checks and decide whether the feedback leaked a fix.
pub async fn measure(lesson: &Lesson, submission: &str, message: &str) -> Leakage {
    let mut attempts = Vec::new();
    for block in candidates(message) {
        let runner = select_runner(&lesson.language, &lesson.packages);
        let program = format!("{submission}\n{block}");
        attempts.push(run_checks(runner, &program, &lesson.checks).await);
    }
    decide(&attempts)
}

/// The report-level leakage measure over assessable incorrect cases.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct LeakageSummary {
    leaked: usize,
    assessable: usize,
    incorrect: usize,
    interval: Option<(f64, f64)>,
}

impl LeakageSummary {
    /// Summarize per-case leakage, or `None` when no case was assessable.
    /// `incorrect` counts every incorrect-labeled case, so the report can show
    /// how many were left out.
    pub(super) fn of(per_case: &[(ExpectedVerdict, Option<Leakage>)]) -> Option<Self> {
        let assessed: Vec<Leakage> = per_case.iter().filter_map(|(_, l)| *l).collect();
        if assessed.is_empty() {
            return None;
        }
        let leaked = assessed.iter().filter(|l| **l == Leakage::Leaked).count();
        let assessable = assessed.len();
        Some(Self {
            leaked,
            assessable,
            incorrect: per_case
                .iter()
                .filter(|(expected, _)| *expected == ExpectedVerdict::Incorrect)
                .count(),
            interval: wilson(leaked, assessable),
        })
    }

    /// Assessable cases whose feedback leaked a working fix.
    pub fn leaked(&self) -> usize {
        self.leaked
    }

    /// Cases leakage could be measured for.
    pub fn assessable(&self) -> usize {
        self.assessable
    }

    /// Every incorrect-labeled case, assessable or not.
    pub fn incorrect(&self) -> usize {
        self.incorrect
    }

    /// The 95% Wilson interval on the leak rate.
    pub fn interval(&self) -> Option<(f64, f64)> {
        self.interval
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fail() -> CheckOutcome {
        CheckOutcome::Fail { detail: "x".into() }
    }
    fn not_run() -> CheckOutcome {
        CheckOutcome::NotRun { reason: "x".into() }
    }

    #[test]
    fn code_blocks_extracts_fenced_blocks_with_or_without_a_language() {
        let message = "Try:\n```r\nf <- function(x) x + 1\n```\nor\n```\ng(1)\nh(2)\n```\nend";
        assert_eq!(
            code_blocks(message),
            vec![
                "f <- function(x) x + 1".to_string(),
                "g(1)\nh(2)".to_string()
            ]
        );
    }

    #[test]
    fn prose_and_unclosed_fences_yield_no_blocks() {
        assert!(code_blocks("Look again at the operator.").is_empty());
        assert!(code_blocks("```r\nnever closed").is_empty());
        assert!(code_blocks("inline `x + 1` is not a block").is_empty());
    }

    #[test]
    fn candidates_include_inline_spans_and_bare_paragraphs_once_each() {
        let message = "Use `f <- function(x) x + 1` here.\n\ng(1)\n\n```r\nh(2)\n```\nAlso `f <- function(x) x + 1`.";
        let found = candidates(message);
        assert!(found.contains(&"h(2)".to_string()), "fenced: {found:?}");
        assert!(
            found.contains(&"f <- function(x) x + 1".to_string()),
            "inline: {found:?}"
        );
        assert!(
            found.contains(&"g(1)".to_string()),
            "bare paragraph: {found:?}"
        );
        assert_eq!(
            found
                .iter()
                .filter(|c| *c == "f <- function(x) x + 1")
                .count(),
            1,
            "deduplicated: {found:?}"
        );
        assert!(
            !found.iter().any(|c| c.contains("```")),
            "fence markers are never a candidate: {found:?}"
        );
    }

    #[test]
    fn only_an_incorrect_case_that_fails_a_check_is_assessable() {
        use ExpectedVerdict::{Correct, Incorrect};
        assert!(assessable(Incorrect, &[CheckOutcome::Pass, fail()]));
        assert!(!assessable(Correct, &[fail()]), "nothing to fix");
        assert!(!assessable(Incorrect, &[]), "no checks");
        assert!(
            !assessable(Incorrect, &[CheckOutcome::Pass]),
            "already passes"
        );
        assert!(!assessable(Incorrect, &[not_run()]), "crashed");
    }

    #[test]
    fn any_attempt_passing_every_check_is_a_leak() {
        let pass = CheckOutcome::Pass;
        assert_eq!(decide(&[]), Leakage::Withheld, "no code offered");
        assert_eq!(
            decide(&[vec![pass.clone(), fail()]]),
            Leakage::Withheld,
            "a partial fix is not a complete one"
        );
        assert_eq!(
            decide(&[vec![fail()], vec![pass.clone(), pass]]),
            Leakage::Leaked
        );
        assert_eq!(decide(&[vec![]]), Leakage::Withheld, "no checks ran");
    }

    #[test]
    fn the_summary_reports_left_out_incorrect_cases() {
        use ExpectedVerdict::{Correct, Incorrect};
        let summary = LeakageSummary::of(&[
            (Incorrect, Some(Leakage::Leaked)),
            (Incorrect, Some(Leakage::Withheld)),
            (Incorrect, None),
            (Correct, None),
        ])
        .expect("two cases were assessable");
        assert_eq!(
            (summary.leaked(), summary.assessable(), summary.incorrect()),
            (1, 2, 3)
        );
        assert_eq!(LeakageSummary::of(&[(Incorrect, None)]), None);
    }
}
