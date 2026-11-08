//! What the lesson's checks make of a suggested misconception's example
//! (ADR-0032).
//!
//! [`SuggestionCheck`] classifies the check outcomes of one example submission:
//! caught (some check fails), not caught (all pass, so only the grader can
//! catch it), does not run (unusable as an item), or unchecked (the lesson has
//! no checks). [`check_example`] is the thin effectful step that runs it. It
//! does not decide whether a suggestion belongs in the blueprint; a person does.

use serde::{Serialize, Serializer};

use crate::grade::{CheckOutcome, run_checks, select_runner};
use crate::lesson::Lesson;

/// How the lesson's checks respond to a suggested misconception's example.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SuggestionCheck {
    /// At least one check fails: the checks already detect this mistake.
    Caught,
    /// Every check passes: only the grader can catch this mistake.
    NotCaught,
    /// The example crashes before any check runs: fix or drop it.
    DoesNotRun,
    /// The lesson has no checks.
    Unchecked,
}

impl SuggestionCheck {
    /// Classify an example's check outcomes. `NotRun` takes precedence, since a
    /// crashing example shows no misconception at all.
    pub fn of(outcomes: &[CheckOutcome]) -> Self {
        if outcomes.is_empty() {
            SuggestionCheck::Unchecked
        } else if outcomes
            .iter()
            .any(|o| matches!(o, CheckOutcome::NotRun { .. }))
        {
            SuggestionCheck::DoesNotRun
        } else if outcomes
            .iter()
            .any(|o| matches!(o, CheckOutcome::Fail { .. }))
        {
            SuggestionCheck::Caught
        } else {
            SuggestionCheck::NotCaught
        }
    }

    /// The wire token.
    pub const fn token(self) -> &'static str {
        match self {
            SuggestionCheck::Caught => "caught",
            SuggestionCheck::NotCaught => "not_caught",
            SuggestionCheck::DoesNotRun => "does_not_run",
            SuggestionCheck::Unchecked => "unchecked",
        }
    }
}

impl Serialize for SuggestionCheck {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.token())
    }
}

/// Run `example` against the lesson's checks and classify the result.
pub async fn check_example(lesson: &Lesson, example: &str) -> SuggestionCheck {
    let runner = select_runner(&lesson.language, &lesson.packages);
    SuggestionCheck::of(&run_checks(runner, example, &lesson.checks).await)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classifies_each_check_response() {
        let fail = CheckOutcome::Fail {
            detail: String::new(),
        };
        let not_run = CheckOutcome::NotRun {
            reason: String::new(),
        };
        assert_eq!(SuggestionCheck::of(&[]), SuggestionCheck::Unchecked);
        assert_eq!(
            SuggestionCheck::of(&[CheckOutcome::Pass, fail.clone()]),
            SuggestionCheck::Caught
        );
        assert_eq!(
            SuggestionCheck::of(&[CheckOutcome::Pass]),
            SuggestionCheck::NotCaught
        );
        assert_eq!(
            SuggestionCheck::of(&[fail, not_run]),
            SuggestionCheck::DoesNotRun
        );
    }
}
