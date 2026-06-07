//! `blendtutor` command-line entry point.
//!
//! A thin shell: parse arguments and delegate to `blendtutor-core`. No domain
//! logic lives here.

use std::path::PathBuf;
use std::process::ExitCode;

use clap::{Parser, Subcommand};

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
    Init,
    /// Create a new lesson from a template.
    New,
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
    /// Run a lesson interactively with LLM feedback.
    Run,
    /// Run feedback-quality evals for a lesson.
    Eval,
    /// Build a browser-deployable lesson site.
    Build,
}

impl Commands {
    /// The subcommand's canonical name, for user-facing messages. The match is
    /// exhaustive, so a new variant cannot silently skip getting a name.
    const fn name(&self) -> &'static str {
        match self {
            Commands::Init => "init",
            Commands::New => "new",
            Commands::Validate { .. } => "validate",
            Commands::List { .. } => "list",
            Commands::Run => "run",
            Commands::Eval => "eval",
            Commands::Build => "build",
        }
    }
}

fn main() -> anyhow::Result<ExitCode> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Validate { path, format } => commands::validate::run(&path, format),
        Commands::List { path, format } => commands::list::run(&path, format),
        other => Err(blendtutor_core::NotYetImplemented::new(other.name()).into()),
    }
}
