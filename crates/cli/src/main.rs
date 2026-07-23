//! `blendtutor` command-line entry point.
//!
//! A thin shell: parse arguments and delegate to `blendtutor-core`. No domain
//! logic lives here.

use std::path::PathBuf;
use std::process::ExitCode;

use clap::{Parser, Subcommand};

use blendtutor_core::lesson::Language;
use blendtutor_core::site::BuildTarget;

mod commands;
mod output;

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
    },
    /// Run feedback-quality evals for a lesson.
    Eval {
        /// Path to the lesson YAML file; its sibling `eval_<lesson>.yaml`
        /// supplies the eval cases.
        lesson: PathBuf,
        /// Output format: `human` (default) or `json`.
        #[arg(long, value_enum, default_value_t = OutputFormat::Human)]
        format: OutputFormat,
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
    /// Export a lesson YAML file to a Quarto `.qmd` fenced-div snippet.
    ExportQuarto {
        /// Path to the lesson YAML file.
        lesson: PathBuf,
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
        } => commands::run::run(&lesson, code.as_deref(), format),
        Commands::Eval { lesson, format } => commands::eval::run(&lesson, format),
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
        Commands::ExportQuarto { lesson } => commands::export_quarto::run(&lesson),
    }
}
