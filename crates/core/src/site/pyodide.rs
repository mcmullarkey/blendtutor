//! The Pyodide build target: assemble a Python-lesson site that boots Pyodide in
//! the browser (ADR-0008).
//!
//! Mirrors [`super::webr`]: it owns only the Python-specific client assets — the
//! page shell (which loads the Pyodide runtime) and the thin runner adapter
//! (which boots Pyodide and grades in Python). The cross-target scaffolding — the
//! shared runner core, the COOP/COEP shim, and the per-lesson JSON contract —
//! comes from [`super::assemble`] unchanged, which is what proves the
//! [`super::BuildTarget`] seam: a second target adds an impl + assets, reusing the
//! shared layout and the lesson-JSON contract (§3.2, §3.4, §4.2).

use crate::course::LessonSlug;
use crate::lesson::Lesson;

use super::{SiteFiles, TargetAssets};

/// The page shell — boots the shim, loads the Pyodide runtime, and the runner.
const INDEX_HTML: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/assets/pyodide/index.html"
));
/// The Pyodide runtime adapter: boots Pyodide and runs a submission + checks in
/// Python atop the shared runner core.
const LESSON_RUNNER_JS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/assets/pyodide/lesson-runner.js"
));

/// Assemble the Pyodide site for `lessons`. Pure: the caller has already verified
/// every lesson is Python (§1.3.1), so this only contributes the Pyodide shell +
/// runner to the shared [`super::assemble`] scaffolding.
pub(super) fn plan(lessons: &[(LessonSlug, Lesson)]) -> SiteFiles {
    super::assemble(
        TargetAssets {
            index_html: INDEX_HTML,
            lesson_runner_js: LESSON_RUNNER_JS,
        },
        lessons,
    )
}
