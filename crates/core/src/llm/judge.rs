//! The feedback judge's prompt and request (ADR-0033).
//!
//! One binary criterion per call, so one impression cannot carry over into
//! another verdict. Each prompt states its construct as a yes/no question, and
//! the tool schema orders `rationale` before `pass`, so the verdict is
//! conditioned on the reasoning. [`build_judge_prompt`] is pure;
//! [`request_judgment`] sends it through the shared `extract` path. Sampling,
//! consensus, and the control live in `eval::judge`.

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use super::feedback::{FeedbackError, extract};
use super::grader::Grader;

/// What a judge call is asked to decide.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Criterion<'a> {
    /// Does the feedback name this specific misconception?
    Diagnosis {
        /// The blueprint label the feedback is judged against.
        misconception: &'a str,
    },
    /// Does the feedback give the learner a concrete next step?
    Actionability {
        /// A candidate definition to judge by instead of
        /// [`ACTIONABILITY_DEFINITION`] (prompt optimization, ADR-0036).
        definition: Option<&'a str>,
    },
}

/// The built-in actionability construct, shown to the judge and to human
/// labelers alike.
pub const ACTIONABILITY_DEFINITION: &str = "Pass only if a learner could act on it right away: it \
     says what to change, look at, or try next in this code. Fail if it only says the answer \
     is wrong, only encourages, or stays so general it would fit any submission. Handing over \
     a complete working solution does not count as a next step.";

/// The judge's answer: its explanation, then its verdict.
///
/// The model sees the JSON schema's properties in **alphabetical** order (schemars
/// over serde_json's sorted map), not declaration order. The field is named
/// `explanation` so it sorts, and is asked for, before `pass`. A test pins this:
/// renaming it to something that sorts after `pass` would silently ask for the
/// verdict first.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, JsonSchema)]
pub struct JudgeVerdict {
    /// Why the feedback does or does not meet the criterion, written first.
    pub explanation: String,
    /// Whether the feedback meets the criterion.
    pub pass: bool,
}

/// Build the judge prompt for one criterion. `task` is the exercise, the
/// `submission` is what the learner wrote, and `feedback` is the grader's
/// message being judged.
pub fn build_judge_prompt(
    criterion: Criterion<'_>,
    task: &str,
    submission: &str,
    feedback: &str,
) -> String {
    let (marker, question, definition) = match criterion {
        Criterion::Diagnosis { misconception } => (
            "CRITERION: diagnosis",
            format!("Does the feedback identify this specific mistake: \"{misconception}\"?"),
            "Pass only if the feedback names this mistake or clearly describes it, so the \
             learner would understand that this is what went wrong. Fail if it names a \
             different mistake, stays generic (\"not quite\", \"check your logic\"), or \
             lists many possible problems without singling this one out."
                .to_string(),
        ),
        Criterion::Actionability { definition } => (
            "CRITERION: actionability",
            "Does the feedback give the learner a concrete next step?".to_string(),
            definition
                .map(str::trim)
                .unwrap_or(ACTIONABILITY_DEFINITION)
                .to_string(),
        ),
    };
    [
        "You are judging the feedback an automated tutor gave a learner on a programming \
         exercise. Judge only the one criterion below."
            .to_string(),
        String::new(),
        marker.to_string(),
        question,
        definition,
        String::new(),
        "Exercise:".to_string(),
        task.trim_end().to_string(),
        String::new(),
        "Learner's submission:".to_string(),
        submission.trim_end().to_string(),
        String::new(),
        "Feedback to judge:".to_string(),
        feedback.trim_end().to_string(),
        String::new(),
        "First write your explanation, citing the feedback's own words, then give pass as \
         true or false."
            .to_string(),
    ]
    .join("\n")
}

/// Ask `judge` for one verdict on one criterion.
pub async fn request_judgment(
    judge: &Grader,
    prompt: &str,
    base_url_override: Option<&str>,
) -> Result<JudgeVerdict, FeedbackError> {
    extract(judge, prompt, base_url_override).await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_diagnosis_prompt_defines_the_construct_against_the_given_label() {
        let p = build_judge_prompt(
            Criterion::Diagnosis {
                misconception: "returns x instead of x + y",
            },
            "Write add_two.",
            "add_two <- function(x, y) x",
            "FB you return x",
        );
        for part in [
            "CRITERION: diagnosis",
            "\"returns x instead of x + y\"",
            "stays generic",
            "Write add_two.",
            "add_two <- function(x, y) x",
            "FB you return x",
            "First write your explanation",
        ] {
            assert!(p.contains(part), "missing {part:?}:\n{p}");
        }
        assert!(!p.contains("CRITERION: actionability"));
    }

    #[test]
    fn an_actionability_prompt_excludes_solution_dumps_from_next_steps() {
        let p = build_judge_prompt(Criterion::Actionability { definition: None }, "t", "s", "f");
        assert!(p.contains("CRITERION: actionability"));
        assert!(p.contains("complete working solution does not count"));
        assert!(!p.contains("CRITERION: diagnosis"));
    }

    #[test]
    fn a_candidate_actionability_definition_replaces_the_built_in_one() {
        let p = build_judge_prompt(
            Criterion::Actionability {
                definition: Some("  Guide, do not tell.\n"),
            },
            "t",
            "s",
            "f",
        );
        assert!(p.contains("\nGuide, do not tell.\n"), "{p}");
        assert!(!p.contains(ACTIONABILITY_DEFINITION));
    }

    #[test]
    fn the_verdict_schema_asks_for_the_explanation_before_the_verdict() {
        // Properties reach the model sorted alphabetically, whatever the
        // declaration order; the explanation must still come first.
        let schema = serde_json::to_string(&schemars::schema_for!(JudgeVerdict)).unwrap();
        let properties = &schema[schema.find("\"properties\"").unwrap()..];
        let (e, p) = (
            properties.find("\"explanation\"").unwrap(),
            properties.find("\"pass\"").unwrap(),
        );
        assert!(
            e < p,
            "explanation must precede pass in the schema: {schema}"
        );
    }
}
