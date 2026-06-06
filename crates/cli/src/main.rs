//! `blendtutor` command-line entry point.
//!
//! A thin shell: parse arguments and delegate to `blendtutor-core`. No domain
//! logic lives here.

use clap::{Parser, Subcommand};

/// Author and run interactive R and Python coding lessons with LLM feedback.
#[derive(Parser)]
#[command(name = "blendtutor")]
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
    Validate,
    /// List the lessons discoverable in a course.
    List,
    /// Run a lesson interactively with LLM feedback.
    Run,
    /// Run feedback-quality evals for a lesson.
    Eval,
    /// Build a browser-deployable lesson site.
    Build,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let command = match cli.command {
        Commands::Init => "init",
        Commands::New => "new",
        Commands::Validate => "validate",
        Commands::List => "list",
        Commands::Run => "run",
        Commands::Eval => "eval",
        Commands::Build => "build",
    };
    Err(blendtutor_core::NotYetImplemented::new(command).into())
}
