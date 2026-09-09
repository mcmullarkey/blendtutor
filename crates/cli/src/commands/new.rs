//! `blendtutor new lesson --lang <r|python> <id>` — add a language-appropriate
//! lesson to the current course, scaffold its `eval_<id>.yaml` grading suite,
//! and register the lesson in the manifest.
//!
//! A thin shell over [`blendtutor_core::scaffold::add_lesson`]: parse the `--lang`
//! flag into the core [`Language`] at the boundary (§1.2, §1.3), then hand the
//! course (the current directory) and id to core. The decision of *what* a lesson
//! and its grading suite contain, and *how* the lesson is registered, lives in
//! `core` (§4.1); this command only names the target and frames the outcome.

use std::path::Path;
use std::process::ExitCode;

use blendtutor_core::lesson::Language;
use blendtutor_core::scaffold::add_lesson;

use crate::commands::sibling_suite_path;

/// Parse the `--lang` flag into a core [`Language`], rejecting any other value at
/// the CLI boundary (§1.3) so an unknown language never travels downstream as
/// data. The accepted spellings are the lowercase wire forms `r` and `python`,
/// matching how `list` renders a lesson's language.
pub fn parse_language(value: &str) -> Result<Language, String> {
    match value {
        "r" => Ok(Language::R),
        "python" => Ok(Language::Python),
        other => Err(format!(
            "unknown language {other:?}; expected `r` or `python`"
        )),
    }
}

/// Add a `language` lesson `id` to the course in the current directory.
///
/// Delegates to [`add_lesson`]; an invalid id, an existing lesson, or a write
/// failure propagates to `main` as an error with a nonzero exit, so the command
/// never half-reports success.
pub fn run(language: Language, id: &str) -> anyhow::Result<ExitCode> {
    let path = add_lesson(Path::new("."), language, id)?;
    let eval_path = sibling_suite_path(&path);
    println!(
        "Added {id} lesson at {} + eval sibling at {}",
        path.display(),
        eval_path.display()
    );
    Ok(ExitCode::SUCCESS)
}
