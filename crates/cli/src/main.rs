//! `blendtutor` command-line entry point.
//!
//! A thin shell: parse arguments and delegate to `blendtutor-core`. No domain
//! logic lives here.

use std::path::PathBuf;
use std::process::ExitCode;

use clap::{ArgGroup, Parser, Subcommand};

use blendtutor_core::eval::Margin;
use blendtutor_core::lesson::Language;
use blendtutor_core::llm::ModelId;
use blendtutor_core::quarto_export::ExportShape;
use blendtutor_core::site::BuildTarget;

mod commands;
mod output;

use commands::export_quarto::ExportRequest;
use output::OutputFormat;

/// Author and run interactive R and Python coding lessons with LLM feedback.
#[derive(Parser)]
#[command(name = "blendtutor", version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

/// The blendtutor subcommands. Each delegates to a domain entry point in `core`;
/// modelling the command set as a sum type keeps dispatch exhaustive rather than
/// matching on raw strings.
#[derive(Subcommand)]
enum Commands {
    /// Scaffold a new course directory.
    Init {
        /// Path to the course directory to create (must be empty or new).
        dir: PathBuf,
    },
    /// Create a new lesson from a template.
    New {
        /// What to create (currently only `lesson`).
        #[command(subcommand)]
        target: NewTarget,
    },
    /// Validate a lesson file against the schema.
    Validate {
        /// Path to the lesson YAML file.
        path: PathBuf,
        /// Output format: `human` (default) or `json`.
        #[arg(long, value_enum, default_value_t = OutputFormat::Human)]
        format: OutputFormat,
    },
    /// List the lessons discoverable in a course.
    List {
        /// Path to the course directory (the one holding `blendtutor.toml`).
        path: PathBuf,
        /// Output format: `human` (default) or `json`.
        #[arg(long, value_enum, default_value_t = OutputFormat::Human)]
        format: OutputFormat,
    },
    /// Run a lesson: execute a submission, grade it, and get LLM feedback.
    Run {
        /// Path to the lesson YAML file.
        lesson: PathBuf,
        /// Path to the student's submission. Read from stdin when omitted.
        #[arg(long)]
        code: Option<PathBuf>,
        /// Output format: `human` (default) or `json`.
        #[arg(long, value_enum, default_value_t = OutputFormat::Human)]
        format: OutputFormat,
        /// Grade with this provider model id instead of the default.
        #[arg(long)]
        model: Option<ModelId>,
    },
    /// Run feedback-quality evals for a lesson.
    Eval {
        /// Path to the lesson YAML file; its sibling `eval_<lesson>.yaml`
        /// supplies the eval cases.
        lesson: PathBuf,
        /// Output format: `human` (default) or `json`.
        #[arg(long, value_enum, default_value_t = OutputFormat::Human)]
        format: OutputFormat,
        /// Score only the 1-based case `N` instead of the whole suite.
        #[arg(long)]
        case: Option<usize>,
        /// Also write the full-shape report as `eval-report.json` at the
        /// course root (the nearest `blendtutor.toml` ancestor) — the durable
        /// artifact `build` folds into the site's eval-results page.
        #[arg(long)]
        write_report: bool,
        /// Grade with this provider model id instead of the default.
        #[arg(long)]
        model: Option<ModelId>,
        /// Grade the suite N times (N >= 2) and report run-to-run stability
        /// instead of a single run.
        #[arg(long, value_parser = clap::value_parser!(u32).range(2..), conflicts_with_all = ["case", "write_report"])]
        repeat: Option<u32>,
    },
    /// Check each eval case's expected label against the lesson's checks
    /// (no LLM call, no API key). Exits 1 if any label contradicts them.
    Audit {
        /// Path to the lesson YAML file; its sibling `eval_<lesson>.yaml`
        /// supplies the labels to audit.
        lesson: PathBuf,
        /// Output format: `human` (default) or `json`.
        #[arg(long, value_enum, default_value_t = OutputFormat::Human)]
        format: OutputFormat,
    },
    /// Compare two lessons' grading prompts on the baseline's eval suite (paired)
    /// and decide: ship (exit 0) only if accuracy is non-inferior and solution
    /// leakage improved; otherwise hold (exit 3).
    EvalCompare {
        /// The current lesson; its sibling `eval_<lesson>.yaml` supplies the cases.
        baseline: PathBuf,
        /// The same lesson with the grading prompt to evaluate.
        candidate: PathBuf,
        /// Output format: `human` (default) or `json`.
        #[arg(long, value_enum, default_value_t = OutputFormat::Human)]
        format: OutputFormat,
        /// Non-inferiority margin for accuracy, as a proportion (pre-register it).
        #[arg(long, default_value = "0.05")]
        margin: Margin,
        /// Grade both arms with this provider model id instead of the default.
        #[arg(long)]
        model: Option<ModelId>,
    },
    /// Suggest misconceptions the lesson's eval blueprint does not cover yet, each
    /// with an example submission checked against the lesson's checks.
    SuggestMisconceptions {
        /// Path to the lesson YAML file; its sibling eval suite's blueprint (if any)
        /// lists the misconceptions already covered.
        lesson: PathBuf,
        /// Output format: `human` (default) or `json`.
        #[arg(long, value_enum, default_value_t = OutputFormat::Human)]
        format: OutputFormat,
        /// Ask this provider model id instead of the default.
        #[arg(long)]
        model: Option<ModelId>,
    },
    /// Generate a static smevals eval report for a lesson.
    EvalReport {
        /// Path to the lesson YAML file; its sibling `eval_<lesson>.yaml`
        /// supplies the eval cases. The report is published to
        /// `docs/evals/<lesson>/` under the repo root.
        lesson: PathBuf,
    },
    /// Build a browser-deployable lesson site from a course.
    Build {
        /// Path to the course directory (the one holding `blendtutor.toml`).
        path: PathBuf,
        /// Browser runtime to target: `webr` (R lessons) or `pyodide` (Python).
        #[arg(long, value_parser = commands::build::parse_target)]
        target: BuildTarget,
        /// Output directory for the generated site.
        #[arg(short = 'o', long)]
        out: PathBuf,
        /// Password for site encryption (site is unencrypted when omitted).
        #[arg(long)]
        password: Option<String>,
        /// Embed an API key in the encrypted payload (requires --password).
        /// Format: `provider:key` (e.g. `fireworks:fw_xxx`, `anthropic:sk-ant-xxx`).
        /// The key is encrypted into the site payload and pre-loaded into the
        /// learner's sessionStorage at decrypt time, so the key-entry prompt
        /// is skipped.
        #[arg(long)]
        embed_key: Option<String>,
    },
    /// Export a lesson YAML file to a Quarto `.qmd` fenced-div snippet, a
    /// complete page (`--document`), or print the API key page (`--key-page`).
    #[command(group(ArgGroup::new("source").required(true).args(["lesson", "key_page"])))]
    ExportQuarto {
        /// Path to the lesson YAML file.
        lesson: Option<PathBuf>,
        /// Prefix the exercise with front matter (title, blendtutor filter, and
        /// `coi: true` for R) so the page renders on its own.
        #[arg(long, requires = "lesson", conflicts_with = "key_page")]
        document: bool,
        /// Print a complete API key page; save it as `api-key.qmd`.
        #[arg(long)]
        key_page: bool,
    },
}

/// What `blendtutor new` creates. A nested subcommand rather than a positional
/// string keeps the set of creatable things a checked sum type (§1.2) and lets it
/// grow — e.g. a future `new eval` — without reparsing free text.
#[derive(Subcommand)]
enum NewTarget {
    /// Add a new lesson from a language template.
    Lesson {
        /// The lesson's language: `r` or `python`.
        #[arg(long, value_parser = commands::new::parse_language)]
        lang: Language,
        /// The lesson's course id; also its file stem under `lessons/`.
        id: String,
    },
}

/// Turn the `export-quarto` flags into a request. clap's `source` group
/// guarantees a missing lesson path means `--key-page` was given.
fn export_request(lesson: Option<PathBuf>, document: bool) -> ExportRequest {
    match lesson {
        Some(path) => ExportRequest::Lesson {
            path,
            shape: if document {
                ExportShape::Document
            } else {
                ExportShape::Snippet
            },
        },
        None => ExportRequest::KeyPage,
    }
}

fn main() -> anyhow::Result<ExitCode> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Init { dir } => commands::init::run(&dir),
        Commands::Validate { path, format } => commands::validate::run(&path, format),
        Commands::List { path, format } => commands::list::run(&path, format),
        Commands::Run {
            lesson,
            code,
            format,
            model,
        } => commands::run::run(&lesson, code.as_deref(), format, model),
        Commands::Eval {
            lesson,
            format,
            case,
            write_report,
            model,
            repeat,
        } => commands::eval::run(&lesson, format, case, write_report, model, repeat),
        Commands::EvalReport { lesson } => commands::eval_report::run(&lesson),
        Commands::SuggestMisconceptions {
            lesson,
            format,
            model,
        } => commands::suggest::run(&lesson, format, model),
        Commands::EvalCompare {
            baseline,
            candidate,
            format,
            margin,
            model,
        } => commands::eval_compare::run(&baseline, &candidate, format, margin, model),
        Commands::Audit { lesson, format } => commands::audit::run(&lesson, format),
        Commands::New { target } => match target {
            NewTarget::Lesson { lang, id } => commands::new::run(lang, &id),
        },
        Commands::Build {
            path,
            target,
            out,
            password,
            embed_key,
        } => commands::build::run(
            &path,
            target,
            &out,
            password.as_deref(),
            embed_key.as_deref(),
        ),
        Commands::ExportQuarto {
            lesson, document, ..
        } => commands::export_quarto::run(export_request(lesson, document)),
    }
}
