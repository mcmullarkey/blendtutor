//! The webR build target: assemble an R-lesson site that boots webR in the
//! browser (ADR-0008).
//!
//! This module owns only the webR-specific client assets — the page shell and the
//! thin runner adapter, embedded at compile time and copied verbatim (§4.1:
//! assembly, not codegen). The cross-target scaffolding (the shared runner core,
//! the COOP/COEP shim, and the per-lesson JSON contract) lives in
//! [`super::assemble`], so a second target adds an impl + assets without
//! re-deriving the shared layout (§4.2).

use crate::course::LessonSlug;
use crate::lesson::Lesson;

use super::SiteFiles;

/// The page shell — boots the shim, the webR runtime, and the runner.
const INDEX_HTML: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/assets/webr/index.html"
));
/// The webR runtime adapter: boots webR and runs a submission + checks in R atop
/// the shared runner core.
const LESSON_RUNNER_JS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/assets/webr/lesson-runner.js"
));

/// Assemble the webR site for `lessons`. Pure: the caller has already verified
/// every lesson is R (§1.3.1), so this only contributes the webR shell + runner
/// to the shared [`super::assemble`] scaffolding.
pub(super) fn plan(lessons: &[(LessonSlug, Lesson)]) -> SiteFiles {
    super::assemble(INDEX_HTML, LESSON_RUNNER_JS, lessons)
}
