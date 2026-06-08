//! The webR build target: assemble an R-lesson site that boots webR in the
//! browser (ADR-0008).
//!
//! The page shell, the in-browser runner, and the COOP/COEP service-worker shim
//! are static client assets embedded at compile time and copied verbatim into
//! every built site (§4.1: assembly, not codegen). The only generated content is
//! the per-lesson JSON contract. The shared assembly lives here for now; Slice 17
//! factors the cross-target scaffolding out when the Pyodide target lands.

use serde::Serialize;

use crate::course::LessonSlug;
use crate::lesson::Lesson;

use super::{SiteFile, SiteFiles, SiteLesson};

/// The page shell — boots the shim and the runner, holds the lesson UI.
const INDEX_HTML: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/assets/webr/index.html"
));
/// The in-browser runner: registers the shim, boots webR, runs submissions+checks.
const LESSON_RUNNER_JS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/assets/webr/lesson-runner.js"
));
/// The COOP/COEP service-worker shim, so SharedArrayBuffer (webR) works on Pages.
const COI_SERVICEWORKER_JS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/assets/webr/coi-serviceworker.js"
));

/// Assemble the webR site for `lessons`. Pure: the caller has already verified
/// every lesson is R (§1.3.1), so this only projects each lesson to the JSON
/// contract and lays the files out in deterministic order.
pub(super) fn plan(lessons: &[(LessonSlug, Lesson)]) -> SiteFiles {
    let mut files = vec![
        asset("index.html", INDEX_HTML),
        asset("lesson-runner.js", LESSON_RUNNER_JS),
        asset("coi-serviceworker.js", COI_SERVICEWORKER_JS),
    ];

    // Per-lesson JSON is keyed by position, not slug, so an author's slug can
    // never reach the filesystem as a path (a shared course could carry a `..`
    // slug). The slug rides inside the JSON as `id`; `lessons.json` is the ordered
    // slug index the runner enumerates.
    let mut slugs = Vec::new();
    for (index, (slug, lesson)) in lessons.iter().enumerate() {
        let site_lesson = SiteLesson::from_lesson(slug, lesson);
        files.push(SiteFile {
            path: format!("lessons/{index}.json").into(),
            contents: to_json(&site_lesson),
        });
        slugs.push(site_lesson.id);
    }
    files.push(SiteFile {
        path: "lessons.json".into(),
        contents: to_json(&slugs),
    });

    SiteFiles { files }
}

/// A static asset file copied verbatim into the site.
fn asset(path: &str, contents: &str) -> SiteFile {
    SiteFile {
        path: path.into(),
        contents: contents.to_string(),
    }
}

/// Serialize a contract value to pretty JSON. Infallible for the plain-data
/// contract types, so a failure is a programmer error, not a runtime condition.
fn to_json<T: Serialize>(value: &T) -> String {
    serde_json::to_string_pretty(value).expect("the site contract serializes infallibly")
}
