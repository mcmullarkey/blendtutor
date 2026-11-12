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
use blendtutor_core::eval::{
    AuditWarning, CaseResult, CompareReport, Conclusion, Consistency, CoverageRow,
    CriterionAgreement, CriterionRate, Decision, DetectionRow, Discordance, EvalReport,
    ItemsReport, JudgeAgreement, JudgeSummary, LabelAudit, LabelAuditReport, LeakageSummary,
    MisconceptionCoverage, PairedChange, Rate, SourceRow, StabilityReport, SuggestionCheck,
    Violation,
};
use blendtutor_core::lesson::Language;
use blendtutor_core::llm::Verdict;
use blendtutor_core::run::RunReport;

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

/// The first line of a possibly multi-line value. A lesson title (a YAML block
/// scalar) or a discovery error (the multi-line parser message) would otherwise
/// embed newlines into a table row and break its alignment; the full text is
/// kept in the JSON view for machine consumers.
fn first_line(text: &str) -> &str {
    text.lines().next().unwrap_or("")
}

/// The human listing: one aligned `id  language  title` line per lesson, with a
/// failed lesson shown as an `ERROR` row carrying its message. Each cell is
/// reduced to its first line so a multi-line title or error cannot break the
/// table. An empty course says so rather than printing nothing.
fn render_list_human(report: &ListReport) -> String {
    if report.rows.is_empty() {
        return "No lessons found.".to_string();
    }
    let id_width = report
        .rows
        .iter()
        .map(|row| first_line(row.id()).len())
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
            } => format!(
                "{:<id_width$}  {:<7}  {}",
                first_line(id),
                language_code(language),
                first_line(title)
            ),
            LessonRow::Failed { id, error } => format!(
                "{:<id_width$}  {:<7}  {}",
                first_line(id),
                "ERROR",
                first_line(error)
            ),
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Render `report` in `format` and write it to stdout — the single place the
/// listing reaches the terminal (§2.4, §5.1).
pub fn emit_list(report: &ListReport, format: OutputFormat) -> io::Result<()> {
    writeln!(io::stdout(), "{}", render_list(report, format))
}

/// The human rendering of a [`RunReport`]: a verdict headline (`Correct!` /
/// `Incorrect.`) followed by the learner-facing feedback.
///
/// Pure (§2.1): it reads the report and returns text, performing no I/O. The
/// verdict word is matched off the typed [`Verdict`], so it cannot drift from the
/// graded result. "Incorrect" deliberately starts the line so a `\bcorrect\b`
/// word-boundary match cannot mistake an incorrect verdict for a pass.
fn render_run(report: &RunReport) -> String {
    match report.verdict() {
        Verdict::Correct { message } => format!("Correct!\n\n{message}"),
        Verdict::Incorrect { message } => format!("Incorrect.\n\n{message}"),
    }
}

/// Render `report` in `format` and write it to stdout — the single place a run
/// result reaches the terminal (§2.4, §5.1).
///
/// JSON is the report's canonical machine document (defined in `core`); human is
/// the verdict + feedback. Both are the command's data, so both go to stdout —
/// diagnostics (a load or provider failure) are errors that reach stderr via
/// `main`, never mixed into this stream.
pub fn emit_run(report: &RunReport, format: OutputFormat) -> io::Result<()> {
    let text = match format {
        OutputFormat::Json => {
            serde_json::to_string(report).expect("a RunReport serializes to JSON infallibly")
        }
        OutputFormat::Human => render_run(report),
    };
    writeln!(io::stdout(), "{text}")
}

/// The human rendering of an [`EvalReport`]: an accuracy headline, one row per
/// case showing the expected and actual polarity and whether they matched, the
/// grader's verbatim feedback under each mismatched row, and — when at least
/// one case mismatched — a next-steps footer.
///
/// Pure (§2.1): it reads the report and returns text, performing no I/O. The
/// match marker is derived from the typed `matched` flag, so it cannot drift from
/// the score; accuracy is shown as both the exact `matched/total` fraction and a
/// rounded percentage. Each polarity word is the report's own canonical token, so
/// the rendering cannot drift from the accepted set. The footer is derived from
/// the case data alone — the lesson path is never threaded in, so the render
/// stays a pure function of the report (§2).
fn render_eval(report: &EvalReport) -> String {
    let total = report.cases().len();
    let matched = report.cases().iter().filter(|case| case.matched()).count();
    let mut lines = vec![
        format!(
            "Accuracy: {matched}/{total} ({percent:.1}%)",
            percent = report.accuracy() * 100.0
        ),
        consistency_headline(report),
        String::new(),
    ];
    for (position, case) in report.cases().iter().enumerate() {
        let marker = if case.matched() {
            "[match]"
        } else {
            "[mismatch]"
        };
        lines.push(format!(
            "case {number}: expected {expected}, got {actual} {marker}",
            number = position + 1,
            expected = case.expected().token(),
            actual = case.actual().token(),
        ));
        if let Some(callout) = consistency_line(case) {
            lines.push(callout);
        }
        if let Some(feedback) = feedback_line(case) {
            lines.push(feedback);
        }
    }
    if !report.by_misconception().is_empty() {
        lines.push(String::new());
        lines.push("Grader detection by misconception (95% interval):".to_string());
        lines.extend(report.by_misconception().iter().map(detection_line));
    }
    if !report.by_source().is_empty() {
        lines.push(String::new());
        lines.push("Grader accuracy by item source (95% interval):".to_string());
        lines.extend(report.by_source().iter().map(source_line));
    }
    if let Some(summary) = report.leakage() {
        lines.push(String::new());
        lines.push(leakage_line(summary));
    }
    if let Some(judge) = report.judge() {
        lines.push(String::new());
        lines.extend(judge_lines(judge, "cases graded incorrect"));
    }
    if let Some(footer) = next_steps_footer(report) {
        lines.push(String::new());
        lines.push(footer);
    }
    lines.join("\n")
}

/// Whose polarity claim is being compared with the lesson's checks: the
/// grader's verdict (`eval`, ADR-0022) or the eval author's label (`audit`,
/// ADR-0023). A closed enum so the call-out wording cannot name some other
/// instrument by accident.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Claimant {
    Grader,
    Label,
}

impl Claimant {
    const fn verb(self) -> &'static str {
        match self {
            Claimant::Grader => "graded",
            Claimant::Label => "labeled",
        }
    }
}

/// A consistency headline titled `title`: violation and discordance counts over
/// the cases that had checks, or an explicit "not assessable" when none did, so
/// a lesson with no checks never reads as agreeing with checks it does not have.
fn consistency_summary(title: &str, items: &[Consistency]) -> String {
    let checked = items
        .iter()
        .filter(|c| **c != Consistency::Unchecked)
        .count();
    if checked == 0 {
        return format!("{title}: not assessable (lesson has no checks)");
    }
    let violations = items
        .iter()
        .filter(|c| matches!(c, Consistency::Violated(_)))
        .count();
    let discordant = items
        .iter()
        .filter(|c| matches!(c, Consistency::Discordant(_)))
        .count();
    let noun = if violations == 1 {
        "violation"
    } else {
        "violations"
    };
    format!(
        "{title}: {violations} {noun}, {discordant} discordant ({checked}/{total} cases checked)",
        total = items.len()
    )
}

/// The description of a claim that disagrees with its checks, worded for
/// `claimant`, or `None` when it agrees or the lesson has no checks.
fn disagreement(consistency: Consistency, claimant: Claimant) -> Option<String> {
    let verb = claimant.verb();
    let text = match consistency {
        Consistency::Unchecked | Consistency::Consistent => return None,
        Consistency::Violated(Violation::CorrectDespiteFailedCheck) => {
            format!("violation — {verb} correct but a check failed")
        }
        Consistency::Violated(Violation::CorrectDespiteNotRun) => {
            format!("violation — {verb} correct but the submission never ran")
        }
        Consistency::Discordant(Discordance::IncorrectDespitePassingChecks) => {
            format!("discordant — {verb} incorrect but every check passed")
        }
    };
    Some(text)
}

/// The eval report's `Consistency:` headline (ADR-0022).
fn consistency_headline(report: &EvalReport) -> String {
    let items: Vec<Consistency> = report.cases().iter().map(CaseResult::consistency).collect();
    consistency_summary("Consistency", &items)
}

/// The indented call-out under an eval case whose verdict disagrees with its
/// checks.
fn consistency_line(case: &CaseResult) -> Option<String> {
    disagreement(case.consistency(), Claimant::Grader).map(|text| format!("  consistency: {text}"))
}

/// The indented `grader:` line carrying one case's verbatim feedback — present
/// only for a mismatched case (§3.4): the grader's own words, never reworded or
/// truncated, and never attached to a `[match]` row where the polarity did not
/// surprise the author. A multi-line message keeps every character verbatim;
/// its continuation lines are indented to align under the first feedback line
/// (the prefix is [`GRADER_PREFIX_WIDTH`] columns) so the message cannot break
/// the row alignment.
const GRADER_PREFIX: &str = "  grader: ";

/// The width of the [`GRADER_PREFIX`] label: continuation lines of a multi-line
/// grader message are indented this far so they align under the feedback text.
const GRADER_PREFIX_WIDTH: usize = GRADER_PREFIX.len();

fn feedback_line(case: &CaseResult) -> Option<String> {
    if case.matched() {
        return None;
    }
    let continuation = format!("\n{:width$}", "", width = GRADER_PREFIX_WIDTH);
    Some(format!(
        "{GRADER_PREFIX}{}",
        case.feedback_message().replace('\n', &continuation)
    ))
}

/// The next-steps footer for a report with at least one mismatched case, or
/// `None` for a full match — the guidance is mismatch-driven, not unconditional
/// (§5.1: one pure helper, not scattered `format!` calls).
///
/// It names the 1-based numbers of the mismatched cases, points at re-running a
/// single case with `--case N`, and names the knobs that shape grading — the
/// lesson's task text, `success_criteria`, `feedback_guidance`, and `checks`, the
/// inputs that actually reach the grader (ADR-0030).
/// It speaks in plain words ("mismatched"), never the bracketed `[mismatch]`
/// token, which the integration tests count exactly.
fn next_steps_footer(report: &EvalReport) -> Option<String> {
    let mismatched: Vec<usize> = report
        .cases()
        .iter()
        .enumerate()
        .filter(|(_, case)| !case.matched())
        .map(|(position, _)| position + 1)
        .collect();
    if mismatched.is_empty() {
        return None;
    }
    let numbers = mismatched
        .iter()
        .map(|number| number.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    Some(format!(
        "mismatched cases: {numbers}\n\
         inspect one: blendtutor eval <lesson> --case N\n\
         grading is shaped by the lesson's task text, `success_criteria`, `feedback_guidance`, and `checks`"
    ))
}

/// Render `report` in `format` and write it to stdout — the single place an eval
/// result reaches the terminal (§2.4, §5.1).
///
/// JSON is the report's canonical machine document (defined in `core`), the
/// artifact a built site embeds without re-scoring; human is the accuracy
/// headline plus per-case rows. Both are the command's data, so both go to
/// stdout — a load or pipeline failure is an error that reaches stderr via
/// `main`, never mixed into this stream.
pub fn emit_eval(report: &EvalReport, format: OutputFormat) -> io::Result<()> {
    let text = match format {
        OutputFormat::Json => {
            serde_json::to_string(report).expect("an EvalReport serializes to JSON infallibly")
        }
        OutputFormat::Human => render_eval(report),
    };
    writeln!(io::stdout(), "{text}")
}

/// The human rendering of a [`LabelAuditReport`] (ADR-0023): a `Label audit:`
/// headline, then one row per label that disagrees with its checks, worded about
/// the label and never about a grader. Pure (§2.1).
fn render_audit(report: &LabelAuditReport) -> String {
    let items: Vec<Consistency> = report.cases().iter().map(LabelAudit::consistency).collect();
    let mut lines = vec![consistency_summary("Label audit", &items)];
    let flagged: Vec<String> = report
        .cases()
        .iter()
        .enumerate()
        .flat_map(|(position, case)| {
            let number = position + 1;
            let disagrees = disagreement(case.consistency(), Claimant::Label)
                .map(|text| format!("case {number}: {text}"));
            let warns = case
                .warning()
                .map(|warning| format!("case {number}: {}", warning_text(warning, case)));
            disagrees.into_iter().chain(warns)
        })
        .collect();
    if !flagged.is_empty() {
        lines.push(String::new());
        lines.extend(flagged);
    }
    if !report.coverage().is_empty() {
        lines.push(String::new());
        lines.push("Misconception coverage by checks:".to_string());
        lines.extend(report.coverage().iter().map(coverage_line));
    }
    lines.join("\n")
}

/// The human text for an audit warning on `case` (ADR-0026 amendment).
fn warning_text(warning: AuditWarning, case: &LabelAudit) -> String {
    match warning {
        AuditWarning::TaggedItemNeverRan => format!(
            "warning — tagged {id} but the submission never ran, so it can't show the misconception",
            id = case
                .misconception()
                .map(|id| id.as_str())
                .unwrap_or("a misconception"),
        ),
    }
}

/// The feedback judge section (ADR-0033): each criterion's pass rate with its
/// interval, the mismatched-label control right under diagnosis so the two are
/// read together, and how often the k samples agreed. `scope` names what was
/// judged, e.g. "cases graded incorrect".
fn judge_lines(judge: &JudgeSummary, scope: &str) -> Vec<String> {
    let rate = |r: &CriterionRate| {
        let range = r
            .interval()
            .map(|(lo, hi)| format!(" ({:.0}%–{:.0}%)", lo * 100.0, hi * 100.0))
            .unwrap_or_default();
        format!("{}/{}{range}", r.passed(), r.judged())
    };
    vec![
        format!(
            "Feedback judge (majority of {} samples, on {scope}):",
            judge.samples()
        ),
        format!(
            "  Diagnosis names the item's misconception: {}",
            rate(judge.diagnosis())
        ),
        format!(
            "    control, against a mismatched misconception: {}",
            rate(judge.diagnosis_control())
        ),
        format!(
            "  Gives an actionable next step: {}",
            rate(judge.actionability())
        ),
        format!(
            "  Judge consistency: {}/{} judgments unanimous across samples",
            judge.unanimous(),
            judge.judgments()
        ),
    ]
}

/// The solution-leakage line (ADR-0028): the leak rate over assessable incorrect
/// cases with its interval, plus how many incorrect cases were left out, so a
/// rate over a small, selected subset is never shown bare.
fn leakage_line(summary: &LeakageSummary) -> String {
    let (leaked, assessable) = (summary.leaked(), summary.assessable());
    let range = summary
        .interval()
        .map(|(lo, hi)| format!(" ({:.0}%–{:.0}%)", lo * 100.0, hi * 100.0))
        .unwrap_or_default();
    let left_out = summary.incorrect() - assessable;
    let tail = if left_out == 0 {
        String::new()
    } else {
        format!(
            "; {left_out} of {incorrect} incorrect cases not assessable",
            incorrect = summary.incorrect()
        )
    };
    format!(
        "Solution leakage: {leaked}/{assessable} assessable incorrect cases leaked a working fix{range}{tail}"
    )
}

/// One item source's grader-accuracy row (ADR-0026), or "no items" when the
/// suite has none from that source.
fn source_line(row: &SourceRow) -> String {
    let head = format!("  {}", row.source().token());
    match row.interval() {
        None => format!("{head}: no items"),
        Some((lo, hi)) => format!(
            "{head}: {matched}/{items} matched ({lo:.0}%–{hi:.0}%)",
            matched = row.matched(),
            items = row.items(),
            lo = lo * 100.0,
            hi = hi * 100.0,
        ),
    }
}

/// One misconception's grader-detection row (ADR-0025): caught/items with the
/// Wilson interval as whole percents, or "no items" for a gap in the bank.
fn detection_line(row: &DetectionRow) -> String {
    let head = format!("  {id} {label}", id = row.id(), label = row.label());
    match row.interval() {
        None => format!("{head}: no items"),
        Some((lo, hi)) => format!(
            "{head}: {caught}/{items} caught ({lo:.0}%–{hi:.0}%)",
            caught = row.caught(),
            items = row.items(),
            lo = lo * 100.0,
            hi = hi * 100.0,
        ),
    }
}

/// One misconception's coverage row (ADR-0024): what the checks caught, and
/// for a gap, what it means for the author.
fn coverage_line(row: &CoverageRow) -> String {
    let head = format!("  {id} {label}", id = row.id(), label = row.label());
    let (caught, items) = (row.caught(), row.items());
    match row.status() {
        MisconceptionCoverage::NoItems => format!("{head}: no items — add some to the eval suite"),
        MisconceptionCoverage::Covered => format!("{head}: {caught}/{items} caught"),
        MisconceptionCoverage::Partial => {
            format!("{head}: {caught}/{items} caught — partial, some items slip past the checks")
        }
        MisconceptionCoverage::Uncovered => {
            format!("{head}: {caught}/{items} caught — uncovered, only the grader can catch it")
        }
    }
}

/// Render a label audit in `format` and write it to stdout: the single place an
/// audit reaches the terminal (§2.4).
pub fn emit_audit(report: &LabelAuditReport, format: OutputFormat) -> io::Result<()> {
    let text = match format {
        OutputFormat::Json => {
            serde_json::to_string(report).expect("a LabelAuditReport serializes to JSON infallibly")
        }
        OutputFormat::Human => render_audit(report),
    };
    writeln!(io::stdout(), "{text}")
}

/// The human rendering of a [`StabilityReport`] (ADR-0031): the share of cases
/// with the same verdict on every run, with its interval and the flipped cases,
/// then accuracy (and leakage, when assessable) per run. Pure (§2.1).
fn render_stability(report: &StabilityReport) -> String {
    let range = report
        .interval()
        .map(|(lo, hi)| format!(" ({:.0}%–{:.0}%)", lo * 100.0, hi * 100.0))
        .unwrap_or_default();
    let flipped = if report.flipped_cases().is_empty() {
        "none".to_string()
    } else {
        report
            .flipped_cases()
            .iter()
            .map(|n| format!("case {n}"))
            .collect::<Vec<_>>()
            .join(", ")
    };
    let percents = |rates: &[f64]| {
        rates
            .iter()
            .map(|r| format!("{:.0}%", r * 100.0))
            .collect::<Vec<_>>()
            .join(", ")
    };
    let mut lines = vec![
        format!(
            "Stability over {runs} runs: same verdict on {stable}/{cases} cases{range}; flipped: {flipped}",
            runs = report.runs(),
            stable = report.stable(),
            cases = report.cases(),
        ),
        format!("Accuracy per run: {}", percents(report.accuracy_per_run())),
    ];
    if !report.leakage_per_run().is_empty() {
        lines.push(format!(
            "Solution leakage per run: {}",
            percents(report.leakage_per_run())
        ));
    }
    lines.join("\n")
}

/// The human rendering of misconception suggestions (ADR-0032): one numbered
/// line per suggestion saying what the lesson's checks make of its example,
/// followed by the example, indented. Pure (§2.1).
fn render_suggestions(suggestions: &[crate::commands::suggest::CheckedSuggestion]) -> String {
    if suggestions.is_empty() {
        return "No new misconceptions suggested.".to_string();
    }
    let mut lines = vec!["Suggested misconceptions not yet in the blueprint:".to_string()];
    for (position, s) in suggestions.iter().enumerate() {
        let verdict = match s.checks {
            SuggestionCheck::Caught => "caught by the checks",
            SuggestionCheck::NotCaught => "not caught by the checks (only the grader can catch it)",
            SuggestionCheck::DoesNotRun => "example does not run (fix or drop it)",
            SuggestionCheck::Unchecked => "lesson has no checks",
        };
        lines.push(String::new());
        lines.push(format!("{}. {}: {verdict}", position + 1, s.label));
        lines.extend(s.example_submission.lines().map(|l| format!("     {l}")));
    }
    lines.join("\n")
}

/// Render misconception suggestions in `format` and write them to stdout.
pub fn emit_suggestions(
    suggestions: &[crate::commands::suggest::CheckedSuggestion],
    format: OutputFormat,
) -> io::Result<()> {
    let text = match format {
        OutputFormat::Json => serde_json::json!({ "suggestions": suggestions }).to_string(),
        OutputFormat::Human => render_suggestions(suggestions),
    };
    writeln!(io::stdout(), "{text}")
}

/// Render a stability report in `format` and write it to stdout (§2.4).
pub fn emit_stability(report: &StabilityReport, format: OutputFormat) -> io::Result<()> {
    let text = match format {
        OutputFormat::Json => {
            serde_json::to_string(report).expect("a StabilityReport serializes to JSON infallibly")
        }
        OutputFormat::Human => render_stability(report),
    };
    writeln!(io::stdout(), "{text}")
}

/// The human rendering of a [`CompareReport`] (ADR-0029): the paired setup and
/// margin, one line per construct with counts, change, and interval in points,
/// and the decision with its reasons. Pure (§2.1).
fn render_compare(report: &CompareReport) -> String {
    let margin = report.margin().value() * 100.0;
    let mut lines = vec![
        format!(
            "Paired comparison on {n} cases (margin {margin:.0} points)",
            n = report.paired_cases()
        ),
        String::new(),
        change_line("Accuracy", report.accuracy()),
    ];
    if let Some(leakage) = report.leakage() {
        lines.push(change_line("Solution leakage", leakage));
    }
    lines.push(String::new());
    lines.push(match report.decision() {
        Decision::Ship => "Decision: ship".to_string(),
        Decision::Hold => format!("Decision: hold ({})", report.reasons().join("; ")),
    });
    lines.join("\n")
}

/// One construct's paired change: per-arm counts, the change and its 95%
/// interval in whole points, and the conclusion in words.
fn change_line(name: &str, change: &PairedChange) -> String {
    let (lo, hi) = change.interval();
    let conclusion = match change.conclusion() {
        Conclusion::NonInferior => "non-inferior",
        Conclusion::NotShownNonInferior => "not shown non-inferior",
        Conclusion::Improved => "improved",
        Conclusion::Worse => "worse",
        Conclusion::NotShownImproved => "not shown improved",
    };
    format!(
        "{name}: baseline {b}/{n}, candidate {c}/{n}, change {delta:+.0} points \
         (95% CI {lo:+.0} to {hi:+.0}): {conclusion}",
        b = change.baseline(),
        c = change.candidate(),
        n = change.n(),
        delta = change.change() * 100.0,
        lo = lo * 100.0,
        hi = hi * 100.0,
    )
}

/// Render a comparison in `format` and write it to stdout (§2.4).
pub fn emit_compare(report: &CompareReport, format: OutputFormat) -> io::Result<()> {
    let text = match format {
        OutputFormat::Json => {
            serde_json::to_string(report).expect("a CompareReport serializes to JSON infallibly")
        }
        OutputFormat::Human => render_compare(report),
    };
    writeln!(io::stdout(), "{text}")
}

/// Emit a `judge-feedback` report (ADR-0034): JSON is the full report with
/// every item's judgment by id; human output is the judge summary alone.
pub fn emit_items(report: &ItemsReport, format: OutputFormat) -> io::Result<()> {
    let text = match format {
        OutputFormat::Json => {
            serde_json::to_string(report).expect("an ItemsReport serializes to JSON infallibly")
        }
        OutputFormat::Human => match report.judge() {
            Some(judge) => judge_lines(judge, &format!("{} items", report.len())).join("\n"),
            None => "No items to judge.".to_string(),
        },
    };
    writeln!(io::stdout(), "{text}")
}

/// Emit a `judge-agreement` report (ADR-0035).
pub fn emit_agreement(report: &JudgeAgreement, format: OutputFormat) -> io::Result<()> {
    let text = match format {
        OutputFormat::Json => {
            serde_json::to_string(report).expect("a JudgeAgreement serializes to JSON infallibly")
        }
        OutputFormat::Human => render_agreement(report),
    };
    writeln!(io::stdout(), "{text}")
}

/// The human rendering of judge-vs-human agreement: TPR and TNR side by side
/// per criterion (a yes-sayer reads as TNR 0), the disagreeing items to read,
/// and any ids the join dropped. Pure (§2.1).
fn render_agreement(report: &JudgeAgreement) -> String {
    let rate = |r: &Rate| {
        let range = r
            .interval()
            .map(|(lo, hi)| format!(" ({:.0}%–{:.0}%)", lo * 100.0, hi * 100.0))
            .unwrap_or_default();
        format!("{}/{}{range}", r.hits(), r.of())
    };
    let line = |name: &str, c: &CriterionAgreement| {
        format!(
            "  {name} ({} pairs): TPR {}, TNR {}",
            c.pairs(),
            rate(c.tpr()),
            rate(c.tnr())
        )
    };
    let plural =
        |n: usize, one: &str, many: &str| format!("{n} {}", if n == 1 { one } else { many });
    let mut lines = vec![
        "Judge vs human labels:".to_string(),
        line("Diagnosis", report.diagnosis()),
        line("Actionability", report.actionability()),
    ];
    let disagreements = report.disagreements();
    if disagreements.is_empty() {
        lines.push("  Disagreements: none".to_string());
    } else {
        let listed: Vec<String> = disagreements
            .iter()
            .map(|d| format!("{} {}", d.id(), d.criterion()))
            .collect();
        lines.push(format!(
            "  Disagreements: {} ({})",
            disagreements.len(),
            listed.join(", ")
        ));
    }
    let (unlabeled, unjudged) = (report.unlabeled().len(), report.unjudged().len());
    if unlabeled + unjudged > 0 {
        lines.push(format!(
            "  Unpaired: {} without a label, {} without a judgment",
            plural(unlabeled, "judged item", "judged items"),
            plural(unjudged, "label", "labels"),
        ));
    }
    lines.join("\n")
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
                    // A realistic, multi-line parser message: the human table must
                    // flatten it to one line while JSON keeps it whole.
                    error: "invalid lesson: missing field `language`\n --> <input>:5:1\n  | ^"
                        .to_string(),
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
        // JSON keeps the full multi-line message — machine consumers get every
        // line, unlike the human table which shows only the first.
        assert_eq!(
            rows[2]["error"],
            "invalid lesson: missing field `language`\n --> <input>:5:1\n  | ^"
        );
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

    /// Pin the human rendering of an eval report — the accuracy headline, the
    /// 1-based per-case rows with their `[match]`/`[mismatch]` markers, each
    /// mismatched case's verbatim `grader:` feedback line, and the next-steps
    /// footer — so any drift in wording, numbering, or the fraction/percentage
    /// fails loudly. The fixture is the AC1 shape (two matches, one mismatch →
    /// 2/3) with a distinct feedback message per case, so a feedback line under
    /// the wrong row cannot pass.
    #[test]
    fn eval_human_matches_snapshot() {
        use blendtutor_core::eval::{CaseResult, EvalReport, ExpectedVerdict};

        let correct = Verdict::Correct {
            message: "alpha checks out".to_string(),
        };
        let incorrect = Verdict::Incorrect {
            message: "beta is wrong".to_string(),
        };
        let flipped = Verdict::Incorrect {
            message: "gamma polarity flipped".to_string(),
        };
        let report = EvalReport::new(
            &[],
            vec![
                CaseResult::score(ExpectedVerdict::Correct.into(), &correct, &[]),
                CaseResult::score(ExpectedVerdict::Incorrect.into(), &incorrect, &[]),
                CaseResult::score(ExpectedVerdict::Correct.into(), &flipped, &[]),
            ],
        );

        insta::assert_snapshot!(render_eval(&report));
    }

    /// F1 — a mismatched case's row is immediately followed by the grader's
    /// verbatim feedback on an indented `grader:` line, and `[match]` rows carry
    /// no feedback line. The three distinct messages make attribution
    /// provable: only the mismatch's own message may appear (negative
    /// missing-feedback and wrong-case-attribution arms).
    #[test]
    fn eval_human_shows_grader_feedback_only_under_mismatch_rows() {
        use blendtutor_core::eval::{CaseResult, EvalReport, ExpectedVerdict};

        let correct = Verdict::Correct {
            message: "alpha checks out".to_string(),
        };
        let incorrect = Verdict::Incorrect {
            message: "beta is wrong".to_string(),
        };
        let flipped = Verdict::Incorrect {
            message: "gamma polarity flipped".to_string(),
        };
        let report = EvalReport::new(
            &[],
            vec![
                CaseResult::score(ExpectedVerdict::Correct.into(), &correct, &[]),
                CaseResult::score(ExpectedVerdict::Incorrect.into(), &incorrect, &[]),
                CaseResult::score(ExpectedVerdict::Correct.into(), &flipped, &[]),
            ],
        );

        let rendered = render_eval(&report);

        assert!(
            rendered.contains("  grader: gamma polarity flipped"),
            "the mismatched case's verbatim feedback must appear, indented and labeled, got: {rendered:?}"
        );
        assert!(
            !rendered.contains("grader: alpha checks out"),
            "a [match] row carries no feedback line, got: {rendered:?}"
        );
        assert!(
            !rendered.contains("grader: beta is wrong"),
            "a [match] row carries no feedback line, got: {rendered:?}"
        );
        // Immediately under its row: the feedback line is the very next line
        // after the mismatch row it belongs to.
        let mismatch_row_position = rendered
            .lines()
            .position(|line| line.ends_with("[mismatch]"))
            .expect("a mismatch row is rendered");
        assert_eq!(
            rendered.lines().nth(mismatch_row_position + 1),
            Some("  grader: gamma polarity flipped"),
            "the grader feedback line must come immediately after its mismatch row"
        );
    }

    /// F2 — a run with at least one mismatch ends with a next-steps footer that
    /// names the 1-based mismatched case numbers, points at
    /// `blendtutor eval <lesson> --case N`, and names the inputs that reach the
    /// grader (`success_criteria`, `feedback_guidance`), never the unused
    /// `llm_evaluation_prompt` or `solution` (ADR-0030). The footer must not
    /// contain the bracketed `[mismatch]` token — `tests/eval.rs` counts
    /// `[mismatch]` occurrences exactly, and the footer speaks in plain words.
    #[test]
    fn eval_human_mismatch_run_ends_with_next_steps_footer() {
        use blendtutor_core::eval::{CaseResult, EvalReport, ExpectedVerdict};

        let correct = Verdict::Correct {
            message: "alpha checks out".to_string(),
        };
        let incorrect = Verdict::Incorrect {
            message: "beta is wrong".to_string(),
        };
        let flipped = Verdict::Incorrect {
            message: "gamma polarity flipped".to_string(),
        };
        let report = EvalReport::new(
            &[],
            vec![
                CaseResult::score(ExpectedVerdict::Correct.into(), &correct, &[]),
                CaseResult::score(ExpectedVerdict::Incorrect.into(), &incorrect, &[]),
                CaseResult::score(ExpectedVerdict::Correct.into(), &flipped, &[]),
            ],
        );

        let rendered = render_eval(&report);

        assert!(
            rendered.contains("mismatched cases: 3"),
            "the footer must name the 1-based mismatched case numbers, got: {rendered:?}"
        );
        assert!(
            rendered.contains("blendtutor eval <lesson> --case N"),
            "the footer must point at re-running a single case, got: {rendered:?}"
        );
        assert!(
            rendered.contains("success_criteria") && rendered.contains("feedback_guidance"),
            "the footer must name the inputs that reach the grader, got: {rendered:?}"
        );
        assert!(
            !rendered.contains("llm_evaluation_prompt") && !rendered.contains("solution"),
            "the footer must not name inputs the grader never sees, got: {rendered:?}"
        );
        assert!(
            !rendered.contains("[mismatch] --") && rendered.matches("[mismatch]").count() == 1,
            "the footer must not add bracketed [mismatch]-like tokens, got: {rendered:?}"
        );
        // The footer is the tail: the render ends with the footer's last line.
        assert!(
            rendered.ends_with("`feedback_guidance`, and `checks`"),
            "the footer must be the end of the output, got: {rendered:?}"
        );
    }

    /// F2 — several mismatches are listed together: the footer names every
    /// mismatched 1-based case number joined with `", "` (cases 1 and 3 here),
    /// so a multi-mismatch run points the learner at all of them, not just the
    /// first. The `1, 3` literal is a deliberate contract pin on the
    /// comma-joined numbering the AC's plural "numbers" requires.
    #[test]
    fn eval_human_footer_lists_every_mismatched_case_number() {
        use blendtutor_core::eval::{CaseResult, EvalReport, ExpectedVerdict};

        let wrong_alpha = Verdict::Incorrect {
            message: "alpha is off".to_string(),
        };
        let wrong_beta = Verdict::Incorrect {
            message: "beta is off".to_string(),
        };
        let wrong_gamma = Verdict::Incorrect {
            message: "gamma is off".to_string(),
        };
        let report = EvalReport::new(
            &[],
            vec![
                // Case 1: expected correct, got incorrect → mismatch.
                CaseResult::score(ExpectedVerdict::Correct.into(), &wrong_alpha, &[]),
                // Case 2: expected incorrect, got incorrect → match.
                CaseResult::score(ExpectedVerdict::Incorrect.into(), &wrong_beta, &[]),
                // Case 3: expected correct, got incorrect → mismatch.
                CaseResult::score(ExpectedVerdict::Correct.into(), &wrong_gamma, &[]),
            ],
        );

        let rendered = render_eval(&report);

        assert!(
            rendered.contains("mismatched cases: 1, 3"),
            "the footer must name every mismatched case, comma-joined, got: {rendered:?}"
        );
    }

    /// F2 negative — an all-matched run emits NO footer and NO guidance lines:
    /// the footer is mismatch-driven, not unconditional.
    #[test]
    fn eval_human_full_match_emits_no_footer() {
        use blendtutor_core::eval::{CaseResult, EvalReport, ExpectedVerdict};

        let correct = Verdict::Correct {
            message: "alpha checks out".to_string(),
        };
        let incorrect = Verdict::Incorrect {
            message: "beta is wrong".to_string(),
        };
        let report = EvalReport::new(
            &[],
            vec![
                CaseResult::score(ExpectedVerdict::Correct.into(), &correct, &[]),
                CaseResult::score(ExpectedVerdict::Incorrect.into(), &incorrect, &[]),
            ],
        );

        let rendered = render_eval(&report);

        assert!(
            !rendered.contains("mismatched cases"),
            "a full match emits no footer, got: {rendered:?}"
        );
        assert!(
            !rendered.contains("--case"),
            "a full match emits no guidance lines, got: {rendered:?}"
        );
        assert!(
            !rendered.contains("feedback_guidance"),
            "a full match emits no guidance lines, got: {rendered:?}"
        );
        assert!(
            !rendered.contains("grader:"),
            "a full match emits no feedback lines, got: {rendered:?}"
        );
    }

    /// A multi-line grader message keeps every character verbatim but has each
    /// continuation line indented to align under the first feedback line (the
    /// `grader:` prefix width), so a multi-line message cannot break the row
    /// alignment. The expected block is derived from the same prefix-width
    /// constant the renderer uses (derive-from-source), so the pin survives a
    /// legitimate prefix rewording while still failing on any lost indent or
    /// truncated content.
    #[test]
    fn eval_human_multiline_feedback_indents_continuation_lines() {
        use blendtutor_core::eval::{CaseResult, EvalReport, ExpectedVerdict};

        let correct = Verdict::Correct {
            message: "alpha checks out".to_string(),
        };
        let incorrect = Verdict::Incorrect {
            message: "beta is wrong".to_string(),
        };
        let multiline = Verdict::Incorrect {
            message: "gamma polarity flipped\nthe prompt admits both polarities\ncheck the reference solution"
                .to_string(),
        };
        let report = EvalReport::new(
            &[],
            vec![
                CaseResult::score(ExpectedVerdict::Correct.into(), &correct, &[]),
                CaseResult::score(ExpectedVerdict::Incorrect.into(), &incorrect, &[]),
                CaseResult::score(ExpectedVerdict::Correct.into(), &multiline, &[]),
            ],
        );

        let rendered = render_eval(&report);

        let indent = " ".repeat(GRADER_PREFIX_WIDTH);
        let expected_block = format!(
            "  grader: gamma polarity flipped\n{indent}the prompt admits both polarities\n{indent}check the reference solution"
        );
        assert!(
            rendered.contains(&expected_block),
            "multi-line feedback must stay verbatim with continuation lines aligned under the first, got: {rendered:?}"
        );
    }

    /// P2 — the JSON document is byte-identical to the pre-change shape: the
    /// per-case keys (`expected`, `actual`, `matched`, `feedback_message`) and
    /// the top-level (`cases`, `accuracy`) in declaration order, with the
    /// verbatim messages. The accuracy literal is serde_json's rendering of
    /// 2/3 — a deliberate contract pin on the wire shape machine consumers
    /// embed without re-scoring.
    #[test]
    fn eval_json_shape_is_byte_stable() {
        use blendtutor_core::eval::{CaseResult, EvalReport, ExpectedVerdict};

        let correct = Verdict::Correct {
            message: "alpha checks out".to_string(),
        };
        let incorrect = Verdict::Incorrect {
            message: "beta is wrong".to_string(),
        };
        let flipped = Verdict::Incorrect {
            message: "gamma polarity flipped".to_string(),
        };
        let report = EvalReport::new(
            &[],
            vec![
                CaseResult::score(ExpectedVerdict::Correct.into(), &correct, &[]),
                CaseResult::score(ExpectedVerdict::Incorrect.into(), &incorrect, &[]),
                CaseResult::score(ExpectedVerdict::Correct.into(), &flipped, &[]),
            ],
        );

        let json = serde_json::to_string(&report).expect("an EvalReport serializes infallibly");

        let expected = concat!(
            r#"{"cases":[{"expected":"correct","actual":"correct","matched":true,"feedback_message":"alpha checks out","consistency":{"status":"unchecked"}},"#,
            r#"{"expected":"incorrect","actual":"incorrect","matched":true,"feedback_message":"beta is wrong","consistency":{"status":"unchecked"}},"#,
            r#"{"expected":"correct","actual":"incorrect","matched":false,"feedback_message":"gamma polarity flipped","consistency":{"status":"unchecked"}}],"#,
            r#""accuracy":0.6666666666666666}"#,
        );
        assert_eq!(json, expected, "the JSON wire shape must not drift");
    }
}
