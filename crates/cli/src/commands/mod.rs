//! Subcommand handlers.
//!
//! Each module turns parsed arguments into a call to `blendtutor-core` and
//! renders the result for the terminal. No domain logic lives here — that is
//! `core`'s responsibility (the dependency only ever points cli → core).

pub mod eval;
pub mod init;
pub mod list;
pub mod new;
pub mod run;
pub mod validate;

/// The environment variable that overrides the provider's base URL — the test
/// seam pointing the rig client at a stub (ADR-0006). Shared by every command
/// that drives the provider pipeline (`run`, `eval`); unset in production.
pub(crate) const PROVIDER_URL_VAR: &str = "BLENDTUTOR_PROVIDER_URL";
