//! `blendtutor` command-line entry point.
//!
//! A thin shell: parse arguments and delegate. No domain logic lives here.

use clap::Parser;

/// Author and run interactive R and Python coding lessons with LLM feedback.
#[derive(Parser)]
#[command(name = "blendtutor")]
struct Cli {}

fn main() {
    let _cli = Cli::parse();
}
