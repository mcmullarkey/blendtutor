//! Subcommand handlers.
//!
//! Each module turns parsed arguments into a call to `blendtutor-core` and
//! renders the result for the terminal. No domain logic lives here — that is
//! `core`'s responsibility (the dependency only ever points cli → core).

use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};

pub mod build;
pub mod eval;
pub mod eval_report;
pub mod export_quarto;
pub mod init;
pub mod list;
pub mod new;
pub mod run;
pub mod validate;

/// The environment variable that overrides the provider's base URL — the test
/// seam pointing the rig client at a stub (ADR-0006). Shared by every command
/// that drives the provider pipeline (`run`, `eval`); unset in production.
pub(crate) const PROVIDER_URL_VAR: &str = "BLENDTUTOR_PROVIDER_URL";

/// The eval suite that sits beside `lesson_path`: the lesson's file name prefixed
/// with `eval_`, so `lessons/foo.yaml` pairs with `lessons/eval_foo.yaml`. This is
/// the instructor-only sibling convention — the suite is authored next to its
/// lesson and is never bundled into a built site.
///
/// Shared by `eval` and `eval-report` (single source, so the two commands can
/// never derive the sibling path differently).
pub(crate) fn sibling_suite_path(lesson_path: &Path) -> PathBuf {
    let file_name = lesson_path.file_name().unwrap_or_else(|| OsStr::new(""));
    let mut suite_name = OsString::from("eval_");
    suite_name.push(file_name);
    lesson_path.with_file_name(suite_name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sibling_suite_prefixes_the_lesson_file_name_with_eval() {
        assert_eq!(
            sibling_suite_path(Path::new("courses/intro/lesson_one.yaml")),
            PathBuf::from("courses/intro/eval_lesson_one.yaml")
        );
    }

    #[test]
    fn sibling_suite_keeps_a_bare_file_name_in_the_current_directory() {
        assert_eq!(
            sibling_suite_path(Path::new("lesson_one.yaml")),
            PathBuf::from("eval_lesson_one.yaml")
        );
    }

    #[test]
    fn sibling_suite_of_a_path_without_a_file_name_is_the_prefix_alone() {
        // A degenerate path (a bare root) has no file name to pair with; the
        // result is the `eval_` prefix alone, so the subsequent read fails with a
        // path-named error rather than silently scoring nothing.
        assert_eq!(sibling_suite_path(Path::new("/")), PathBuf::from("/eval_"));
    }
}
