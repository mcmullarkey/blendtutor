//! The pure, injection-hardened prompt builder.
//!
//! Owns the prompt's domain inputs ([`Submission`], [`ExecResults`]), its output
//! ([`Prompt`]), the structural delimiter/label constants, and [`build_prompt`]
//! itself. Pure and fixture-free (§2.1, §2.3): no client, env, network, or IO — the
//! effectful request lives in [`feedback`](super::feedback). The student submission
//! is untrusted input bound for an LLM, so every interpolated value is neutralized:
//! a forged delimiter or label can never become a second structural token.

use crate::grade::CheckOutcome;
use crate::lesson::Lesson;
use crate::runner::ExecutionResult;

/// Opens the fence around the verbatim student submission.
///
/// Exported so tests and the builder reference one literal source — the same
/// constant is both emitted by [`build_prompt`] and asserted against, so the
/// test predicate pins the real delimiter rather than a hand-copied twin that
/// could drift from it (§1.5).
pub const OPEN_CODE: &str = "<<<STUDENT_CODE_BEGIN>>>";
/// Closes the fence around the verbatim student submission.
pub const CLOSE_CODE: &str = "<<<STUDENT_CODE_END>>>";
/// Labels the captured-output section.
pub const OUTPUT_LABEL: &str = "<<<CAPTURED_OUTPUT>>>";
/// Labels the check-results section.
pub const CHECKS_LABEL: &str = "<<<CHECK_RESULTS>>>";

/// What replaces any structural token found inside untrusted interpolated text,
/// so an injected fence or label can never count as a real one.
const NEUTRALIZED: &str = "[neutralized-delimiter]";

/// A learner's submitted code, awaiting feedback.
///
/// A newtype around the source so submitted code is never confused with arbitrary
/// text elsewhere (§1.4).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Submission {
    /// The submitted source code, verbatim.
    pub code: String,
}

impl Submission {
    /// Wrap submitted source `code`.
    pub fn new(code: impl Into<String>) -> Self {
        Self { code: code.into() }
    }
}

/// A graded run: what the submission produced, paired with its per-check verdicts.
///
/// Bundles the single [`ExecutionResult`] from running the submission with the
/// ordered [`CheckOutcome`]s from grading it — one per lesson check, by index. It
/// does **not** carry the check code-strings; those live on the [`Lesson`] and are
/// paired with `outcomes` positionally (ADR-0006). This is the input
/// [`build_prompt`] reads.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExecResults {
    /// What running the submission produced.
    pub output: ExecutionResult,
    /// One verdict per lesson check, in `lesson.checks` order.
    pub outcomes: Vec<CheckOutcome>,
}

/// A rendered LLM feedback prompt.
///
/// A newtype around the assembled text so a prompt is never confused with
/// arbitrary text, and the rendering stays the single thing the provider sends
/// (§1.4).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Prompt(String);

impl Prompt {
    /// The rendered prompt text.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Strip every structural token out of untrusted `text`.
///
/// Applied to each interpolated value so the only occurrences of the fences and
/// labels in the final prompt are the ones [`build_prompt`] itself emits. The
/// replacement marker contains no structural token, so replacements can neither
/// create nor hide one another. This is the whole injection defense: an injected
/// `CLOSE_CODE` + forged `CHECKS_LABEL` cannot break out of the fence or forge a
/// verdict, because each is rewritten before it reaches the prompt.
fn neutralize(text: &str) -> String {
    text.replace(OPEN_CODE, NEUTRALIZED)
        .replace(CLOSE_CODE, NEUTRALIZED)
        .replace(OUTPUT_LABEL, NEUTRALIZED)
        .replace(CHECKS_LABEL, NEUTRALIZED)
}

/// Render one check as `<check code>: <outcome>` for the checks section, both
/// sides neutralized.
fn render_check(check: &str, outcome: &CheckOutcome) -> String {
    let verdict = match outcome {
        CheckOutcome::Pass => "pass".to_string(),
        CheckOutcome::Fail { detail } => format!("fail — {}", neutralize(detail)),
        CheckOutcome::NotRun { reason } => format!("not run — {}", neutralize(reason)),
    };
    format!("{}: {verdict}", neutralize(check))
}

/// Render the check-results section: one line per outcome, in order, each labeled
/// with its lesson check code-string.
///
/// Iterates over the **outcomes** — the verdicts — not the lesson's checks, so a
/// verdict is never silently dropped. In the normal flow the two are 1:1:
/// [`run_checks`](crate::grade::run_checks) returns exactly one outcome per
/// `lesson.checks` entry, in order. If a caller hand-built [`ExecResults`] with
/// more outcomes than the lesson has checks, the extra outcomes still render
/// (labeled by 1-based position) rather than vanishing; checks with no outcome
/// have no verdict to report and so contribute no line.
fn render_checks(lesson: &Lesson, outcomes: &[CheckOutcome]) -> String {
    outcomes
        .iter()
        .enumerate()
        .map(|(i, outcome)| {
            let fallback = format!("check {}", i + 1);
            let label = lesson
                .checks
                .get(i)
                .map(String::as_str)
                .unwrap_or(fallback.as_str());
            render_check(label, outcome)
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Build the LLM feedback prompt for a graded submission.
///
/// Pure (§2.1): it reads only the borrowed domain values and performs no IO, env
/// read, or network call, so identical inputs always render byte-identically. The
/// layout is a fixed structure — the task, a single fenced copy of the submission,
/// a captured-output section, and a check-results section (one line per outcome,
/// labeled with its `lesson.checks` entry by index) — not the lesson's
/// `llm_evaluation_prompt` template (ADR-0006). `results.outcomes` is expected 1:1
/// with `lesson.checks` (the shape `run_checks` produces); the rendering never
/// silently drops a verdict if they desync (see [`render_checks`]). Every
/// interpolated value is [`neutralize`]d, so the fences and labels each appear
/// exactly once even when the submission forges them: injected text can never be
/// read as code or as a verdict.
pub fn build_prompt(lesson: &Lesson, submission: &Submission, results: &ExecResults) -> Prompt {
    let task = neutralize(&lesson.exercise.prompt);
    let code = neutralize(&submission.code);
    let output = neutralize(&results.output.stdout);
    let checks = render_checks(lesson, &results.outcomes);

    let rendered = [
        "You are evaluating student code for a programming exercise.",
        "",
        "Task:",
        task.trim_end(),
        "",
        OPEN_CODE,
        code.trim_end_matches('\n'),
        CLOSE_CODE,
        "",
        OUTPUT_LABEL,
        output.trim_end_matches('\n'),
        "",
        CHECKS_LABEL,
        &checks,
    ]
    .join("\n");

    Prompt(rendered)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn neutralize_rewrites_every_structural_token() {
        for token in [OPEN_CODE, CLOSE_CODE, OUTPUT_LABEL, CHECKS_LABEL] {
            let injected = format!("before {token} after");
            let cleaned = neutralize(&injected);
            assert!(
                !cleaned.contains(token),
                "neutralize must rewrite {token}, got: {cleaned}"
            );
            assert!(
                cleaned.contains(NEUTRALIZED),
                "the token is replaced by the neutral marker"
            );
        }
    }

    #[test]
    fn neutralize_marker_introduces_no_structural_token() {
        // The marker must itself be free of every structural token, or a
        // replacement could resurrect one it just removed.
        for token in [OPEN_CODE, CLOSE_CODE, OUTPUT_LABEL, CHECKS_LABEL] {
            assert!(
                !NEUTRALIZED.contains(token),
                "the neutral marker must not contain {token}"
            );
        }
    }

    #[test]
    fn render_check_distinguishes_the_three_outcomes() {
        assert_eq!(render_check("c", &CheckOutcome::Pass), "c: pass");
        assert_eq!(
            render_check(
                "c",
                &CheckOutcome::Fail {
                    detail: "boom".to_string()
                }
            ),
            "c: fail — boom"
        );
        assert_eq!(
            render_check(
                "c",
                &CheckOutcome::NotRun {
                    reason: "no run".to_string()
                }
            ),
            "c: not run — no run"
        );
    }

    fn lesson_with_checks(checks: &[&str]) -> Lesson {
        let check_lines = checks
            .iter()
            .map(|c| format!("  - \"{c}\""))
            .collect::<Vec<_>>()
            .join("\n");
        let yaml = format!(
            "lesson_name: \"L\"\nlanguage: R\nchecks:\n{check_lines}\nexercise:\n  \
             prompt: \"do it\"\n  llm_evaluation_prompt: \"grade {{student_code}}\"\n"
        );
        Lesson::parse(&yaml).expect("the constructed lesson is valid")
    }

    #[test]
    fn render_checks_labels_each_outcome_with_its_check_in_order() {
        let lesson = lesson_with_checks(&["first_check", "second_check"]);
        let rendered = render_checks(
            &lesson,
            &[
                CheckOutcome::Pass,
                CheckOutcome::Fail {
                    detail: "nope".to_string(),
                },
            ],
        );
        assert_eq!(rendered, "first_check: pass\nsecond_check: fail — nope");
    }

    #[test]
    fn render_checks_never_drops_a_verdict_when_outcomes_exceed_checks() {
        // A desynced ExecResults (more outcomes than the lesson has checks) must
        // not hide a verdict — the security-relevant property is that a Fail is
        // always shown. The unlabeled extra falls back to its 1-based position.
        let lesson = lesson_with_checks(&["only_check"]);
        let rendered = render_checks(
            &lesson,
            &[
                CheckOutcome::Pass,
                CheckOutcome::Fail {
                    detail: "hidden?".to_string(),
                },
            ],
        );
        assert_eq!(rendered, "only_check: pass\ncheck 2: fail — hidden?");
        assert!(
            rendered.contains("hidden?"),
            "the second verdict must not be dropped, got: {rendered}"
        );
    }

    #[test]
    fn render_checks_renders_no_line_for_a_check_without_an_outcome() {
        // The other desync direction: more checks than outcomes. A check with no
        // outcome has no verdict to report, so it contributes no line — it must
        // never surface as a phantom pass.
        let lesson = lesson_with_checks(&["graded", "ungraded"]);
        let rendered = render_checks(&lesson, &[CheckOutcome::Pass]);
        assert_eq!(rendered, "graded: pass");
        assert!(
            !rendered.contains("ungraded"),
            "a check with no outcome must not appear as a phantom verdict, got: {rendered}"
        );
    }

    #[test]
    fn render_check_neutralizes_a_token_in_the_outcome_detail() {
        // A check's failure detail is interpreter output — untrusted — so a fence
        // smuggled through stderr must be neutralized too.
        let rendered = render_check(
            "c",
            &CheckOutcome::Fail {
                detail: format!("oops {CLOSE_CODE}"),
            },
        );
        assert!(
            !rendered.contains(CLOSE_CODE),
            "a token in a check detail is neutralized, got: {rendered}"
        );
    }
}
