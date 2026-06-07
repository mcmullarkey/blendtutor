//! Subcommand handlers.
//!
//! Each module turns parsed arguments into a call to `blendtutor-core` and
//! renders the result for the terminal. No domain logic lives here — that is
//! `core`'s responsibility (the dependency only ever points cli → core).

pub mod list;
pub mod run;
pub mod validate;
