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

use blendtutor_core::course::{DiscoveryError, LessonSummary};
use blendtutor_core::lesson::Language;

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

/// One row of a lesson listing as the cli renders it: a lesson that was
/// discovered, or one that failed to load. Modelling the two as a sum type keeps
/// a row from being half-found, half-failed (§1.1).
enum LessonRow {
    Found {
        id: String,
        language: Language,
        title: String,
    },
    Failed {
        id: String,
        error: String,
    },
}

impl LessonRow {
    /// The row's course slug, shown in every format and used to size the human
    /// table's first column.
    fn id(&self) -> &str {
        match self {
            LessonRow::Found { id, .. } | LessonRow::Failed { id, .. } => id,
        }
    }
}

/// The outcome of discovering a course's lessons: one [`LessonRow`] per manifest
/// entry, in manifest order. The pure value the `list` command computes and then
/// hands to [`emit_list`].
pub struct ListReport {
    rows: Vec<LessonRow>,
}

impl ListReport {
    /// Build a report from the rows `core` discovered: each `Ok` lesson becomes a
    /// found row, each `Err` a failed row carrying its slug and message. Order is
    /// preserved so the listing follows the manifest. The command shapes this
    /// data; the renderer only formats it (§5.1).
    pub fn from_discovery(rows: Vec<Result<LessonSummary, DiscoveryError>>) -> Self {
        let rows = rows
            .into_iter()
            .map(|row| match row {
                Ok(summary) => LessonRow::Found {
                    id: summary.id.to_string(),
                    language: summary.language,
                    title: summary.title,
                },
                Err(error) => LessonRow::Failed {
                    id: error.id().to_string(),
                    error: error.to_string(),
                },
            })
            .collect();
        Self { rows }
    }
}

/// The lowercase, machine-readable spelling of a lesson language for the JSON and
/// human listings. An exhaustive match (§1.2): a new `Language` variant forces a
/// spelling here rather than defaulting silently. Lowercase is the wire form,
/// decoupled from the enum's YAML spelling (`R`/`Python`).
fn language_code(language: &Language) -> &'static str {
    match language {
        Language::R => "r",
        Language::Python => "python",
    }
}

/// The machine-readable view of one lesson row: a stable, documented shape with
/// `id` always present, `language`/`title` for a found lesson, `error` for a
/// failed one. Every key is always emitted (the absent side is `null`) so
/// consumers can rely on the shape.
#[derive(Serialize)]
struct LessonRowView<'a> {
    id: &'a str,
    language: Option<&'a str>,
    title: Option<&'a str>,
    error: Option<&'a str>,
}

impl<'a> LessonRowView<'a> {
    fn of(row: &'a LessonRow) -> Self {
        match row {
            LessonRow::Found {
                id,
                language,
                title,
            } => Self {
                id,
                language: Some(language_code(language)),
                title: Some(title),
                error: None,
            },
            LessonRow::Failed { id, error } => Self {
                id,
                language: None,
                title: None,
                error: Some(error),
            },
        }
    }
}

/// Render a [`ListReport`] in `format` without performing any I/O (§2.1).
///
/// Both formats are a single document on stdout: JSON is a stable array of row
/// objects; human is an aligned table. Unlike `validate`, the listing has no
/// stderr split — an error row is part of the listing's data, not a diagnostic.
fn render_list(report: &ListReport, format: OutputFormat) -> String {
    match format {
        OutputFormat::Json => {
            let view: Vec<LessonRowView> = report.rows.iter().map(LessonRowView::of).collect();
            serde_json::to_string(&view).expect("lesson rows serialize infallibly")
        }
        OutputFormat::Human => render_list_human(report),
    }
}

/// The human listing: one aligned `id  language  title` line per lesson, with a
/// failed lesson shown as an `ERROR` row carrying its message. An empty course
/// says so rather than printing nothing.
fn render_list_human(report: &ListReport) -> String {
    if report.rows.is_empty() {
        return "No lessons found.".to_string();
    }
    let id_width = report
        .rows
        .iter()
        .map(|row| row.id().len())
        .max()
        .unwrap_or(0);
    report
        .rows
        .iter()
        .map(|row| match row {
            LessonRow::Found {
                id,
                language,
                title,
            } => format!("{id:<id_width$}  {:<7}  {title}", language_code(language)),
            LessonRow::Failed { id, error } => format!("{id:<id_width$}  {:<7}  {error}", "ERROR"),
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Render `report` in `format` and write it to stdout — the single place the
/// listing reaches the terminal (§2.4, §5.1).
pub fn emit_list(report: &ListReport, format: OutputFormat) -> io::Result<()> {
    writeln!(io::stdout(), "{}", render_list(report, format))
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

    /// A report fixture: two discovered lessons (one R, one Python) and one that
    /// failed to load — the AC2 shape, used to pin both the human and JSON views.
    fn mixed_report() -> ListReport {
        ListReport {
            rows: vec![
                LessonRow::Found {
                    id: "add-two".to_string(),
                    language: Language::R,
                    title: "Add Two Numbers".to_string(),
                },
                LessonRow::Found {
                    id: "greet".to_string(),
                    language: Language::Python,
                    title: "Greet Someone".to_string(),
                },
                LessonRow::Failed {
                    id: "broken".to_string(),
                    error: "invalid lesson: missing field `language`".to_string(),
                },
            ],
        }
    }

    /// Pin the human listing — aligned `id language title` rows plus the `ERROR`
    /// row — so any drift in spacing or wording fails loudly.
    #[test]
    fn list_human_matches_snapshot() {
        insta::assert_snapshot!(render_list(&mixed_report(), OutputFormat::Human));
    }

    /// The JSON view is a stable array: a found row carries `language`/`title`
    /// with `error: null`; a failed row carries `error` with the other fields
    /// null. Both keys always present so consumers can rely on the shape.
    #[test]
    fn list_json_separates_found_rows_from_failed_rows() {
        let json = render_list(&mixed_report(), OutputFormat::Json);
        let rows: serde_json::Value = serde_json::from_str(&json).expect("list json parses");

        // The array preserves manifest order and length — one row per entry.
        assert_eq!(rows.as_array().expect("json is an array").len(), 3);

        assert_eq!(rows[0]["id"], "add-two");
        assert_eq!(rows[0]["language"], "r");
        assert_eq!(rows[0]["title"], "Add Two Numbers");
        assert!(rows[0]["error"].is_null(), "a found row has no error");

        // The second found row carries the *other* language — proof both
        // languages are emitted, not one default repeated.
        assert_eq!(rows[1]["id"], "greet");
        assert_eq!(rows[1]["language"], "python");
        assert_eq!(rows[1]["title"], "Greet Someone");
        assert!(rows[1]["error"].is_null(), "a found row has no error");

        assert_eq!(rows[2]["id"], "broken");
        assert!(
            rows[2]["language"].is_null(),
            "a failed row has no language"
        );
        assert!(rows[2]["title"].is_null(), "a failed row has no title");
        assert_eq!(rows[2]["error"], "invalid lesson: missing field `language`");
    }

    /// An empty course is a real, if unusual, state: human says so in words and
    /// JSON is an empty array, not blank output. Pinning both keeps the wording
    /// and the `[]` contract from drifting silently.
    #[test]
    fn list_renders_an_empty_course_in_both_formats() {
        let empty = ListReport { rows: vec![] };
        assert_eq!(
            render_list(&empty, OutputFormat::Human),
            "No lessons found."
        );
        assert_eq!(render_list(&empty, OutputFormat::Json), "[]");
    }
}
