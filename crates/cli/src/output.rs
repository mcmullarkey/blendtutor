//! Rendering of typed command results to a terminal.
//!
//! This module is the renderer seam (§3.2, §3.4): each command computes a typed
//! result value and hands it here to be written out. Nothing in command logic
//! knows about output formats or terminals, so adding a format touches only this
//! module. It renders results — it never computes them (§4.2).

use std::fmt;
use std::io::{self, Write};
use std::process::ExitCode;

use clap::ValueEnum;
use serde::Serialize;

/// How a command's result is rendered for the user.
///
/// Modelling the choice as an enum rather than a string makes dispatch
/// exhaustive (§1.2): a new format is a new variant the compiler forces every
/// renderer to handle.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, ValueEnum)]
pub enum OutputFormat {
    /// Human-readable text (the default).
    #[default]
    Human,
    /// Stable, machine-readable JSON.
    Json,
}

impl fmt::Display for OutputFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Defer to the clap value name so the `--format` default stays in lockstep
        // with the parsed spelling; a renamed variant cannot drift the default.
        self.to_possible_value()
            .expect("every OutputFormat variant has a clap value name")
            .get_name()
            .fmt(f)
    }
}

/// The outcome of validating one lesson — a pure value the `validate` command
/// computes and then hands to [`emit_validate`]. Separating the result from its
/// rendering keeps a "half-rendered" state unrepresentable (§1.1): the command
/// cannot print before the outcome is decided.
pub struct ValidateReport {
    outcome: Outcome,
}

/// A validation outcome carries exactly the data its variant needs — a valid
/// lesson its name, an invalid one the finding that sank it — so neither can be
/// constructed without it (§1.1, §1.2). `validate` reports the first problem it
/// hits, so an invalid outcome carries exactly one finding; the JSON view still
/// renders it inside a `findings` array for a stable, forward-compatible shape.
enum Outcome {
    Valid { lesson_name: String },
    Invalid { finding: String },
}

impl ValidateReport {
    /// The lesson parsed and satisfied every rule.
    pub fn valid(lesson_name: String) -> Self {
        Self {
            outcome: Outcome::Valid { lesson_name },
        }
    }

    /// The lesson failed validation; `finding` states the problem. A mandatory
    /// finding makes an "invalid with nothing to say" outcome unrepresentable.
    pub fn invalid(finding: String) -> Self {
        Self {
            outcome: Outcome::Invalid { finding },
        }
    }

    /// The process exit code for this outcome, computed once from the result and
    /// independent of the render format (§3.4): valid succeeds, invalid fails.
    pub fn exit_code(&self) -> ExitCode {
        match self.outcome {
            Outcome::Valid { .. } => ExitCode::SUCCESS,
            Outcome::Invalid { .. } => ExitCode::FAILURE,
        }
    }
}

/// The machine-readable view of a [`ValidateReport`]: a documented, stable shape
/// of a string `status` and a `findings` array, plus the lesson name when known.
#[derive(Serialize)]
struct ValidateDocument<'a> {
    status: &'a str,
    #[serde(skip_serializing_if = "Option::is_none")]
    lesson_name: Option<&'a str>,
    findings: &'a [String],
}

impl<'a> ValidateDocument<'a> {
    fn of(report: &'a ValidateReport) -> Self {
        const NONE: &[String] = &[];
        match &report.outcome {
            Outcome::Valid { lesson_name } => Self {
                status: "valid",
                lesson_name: Some(lesson_name),
                findings: NONE,
            },
            Outcome::Invalid { finding } => Self {
                status: "invalid",
                lesson_name: None,
                findings: std::slice::from_ref(finding),
            },
        }
    }
}

/// Which standard stream a rendered result belongs on.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Stream {
    Out,
    Err,
}

/// A rendered result: the text to write and the stream it belongs on. Returning
/// this (rather than writing) keeps rendering pure and snapshot-testable (§2.1);
/// the single write is the caller's only effect.
struct Rendered {
    text: String,
    stream: Stream,
}

/// Render a [`ValidateReport`] in `format` without performing any I/O (§2.1).
///
/// JSON is machine output and always goes to stdout. Human output sends the
/// success line to stdout and validation findings to stderr, keeping diagnostics
/// out of the command's data stream.
fn render_validate(report: &ValidateReport, format: OutputFormat) -> Rendered {
    match format {
        OutputFormat::Json => Rendered {
            text: serde_json::to_string(&ValidateDocument::of(report))
                .expect("ValidateDocument serializes infallibly"),
            stream: Stream::Out,
        },
        OutputFormat::Human => match &report.outcome {
            Outcome::Valid { lesson_name } => Rendered {
                text: format!("OK: \"{lesson_name}\" is a valid lesson"),
                stream: Stream::Out,
            },
            Outcome::Invalid { finding } => Rendered {
                text: finding.clone(),
                stream: Stream::Err,
            },
        },
    }
}

/// Render `report` in `format` and write it to the appropriate standard stream —
/// the single place validation output reaches the terminal (§2.4, §5.1).
pub fn emit_validate(report: &ValidateReport, format: OutputFormat) -> io::Result<()> {
    let Rendered { text, stream } = render_validate(report, format);
    match stream {
        Stream::Out => writeln!(io::stdout(), "{text}"),
        Stream::Err => writeln!(io::stderr(), "{text}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Pin the human rendering of a valid lesson so any drift in the success
    /// line — wording, spacing, quoting — fails loudly (AC2). The snapshot is the
    /// committed source of truth for the default format.
    #[test]
    fn validate_human_matches_snapshot() {
        let report = ValidateReport::valid("Writing Your First Function".to_string());

        let rendered = render_validate(&report, OutputFormat::Human);

        // The success line is data, so it belongs on stdout.
        assert_eq!(rendered.stream, Stream::Out);
        insta::assert_snapshot!(rendered.text);
    }

    /// Pin the human rendering of an *invalid* lesson — the twin of the valid
    /// render (§3.2): the finding text, on stderr. `validate` produces exactly
    /// one finding, so this pins the real shape, not a synthetic multi-line one.
    #[test]
    fn validate_human_invalid_matches_snapshot() {
        let report =
            ValidateReport::invalid("exercise.llm_evaluation_prompt is required".to_string());

        let rendered = render_validate(&report, OutputFormat::Human);

        // The finding is a diagnostic, so it belongs on stderr.
        assert_eq!(rendered.stream, Stream::Err);
        insta::assert_snapshot!(rendered.text);
    }
}
