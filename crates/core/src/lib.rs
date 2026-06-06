//! # blendtutor-core
//!
//! Domain logic and adapters for blendtutor: the lesson model, language runners
//! (R and Python), grading, and LLM providers — built up slice by slice.
//!
//! This is the reusable core a future Tauri GUI can call directly, with no
//! process boundary. It deliberately does **not** parse command-line arguments
//! or render terminal output; that is the responsibility of the `blendtutor-cli`
//! crate, which depends on this one (the dependency only ever points cli → core).

use std::error::Error;
use std::fmt;

/// Error for a command that is planned but not yet implemented in the current slice.
///
/// The walking skeleton wires every subcommand through `core` so the cli → core
/// boundary is real from the start; each command returns this until its slice
/// lands. Keeping it a typed error (rather than `anyhow`) lets `core` stay free
/// of the error-reporting crate, which belongs at the CLI edge.
#[derive(Debug)]
pub struct NotYetImplemented {
    command: &'static str,
}

impl NotYetImplemented {
    /// Build the error for the named `command` (e.g. `"validate"`).
    pub const fn new(command: &'static str) -> Self {
        Self { command }
    }
}

impl fmt::Display for NotYetImplemented {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "`{}` is not yet implemented", self.command)
    }
}

impl Error for NotYetImplemented {}
