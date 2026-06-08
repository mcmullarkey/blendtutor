//! Static-site assembly: turn a course into a browser-deployable lesson site.
//!
//! `core::site` owns assembly *only* (ADR-0008, §4.1): it decides which files a
//! site contains and their contents, and commits them to disk. It does **not**
//! call LLMs or execute learner code — that happens in the browser at learner
//! time, via the runtime the emitted assets boot (webR / Pyodide).
//!
//! The split (§2.1, §2.3): [`plan_site`] is pure — given the loaded lessons and a
//! [`BuildTarget`] it returns the [`SiteFiles`] (relative paths + contents), with
//! no filesystem touch, so a whole site is snapshot-testable. [`write_site`] is
//! the single effectful step. The [`SiteLesson`] JSON is the contract between Rust
//! (author side) and the JS runtime (learner side): a Rust-side change that keeps
//! the JSON shape leaves the runtime untouched (§3.2).

mod webr;

use std::error::Error;
use std::fmt;
use std::path::{Path, PathBuf};

use serde::Serialize;

use crate::course::LessonSlug;
use crate::lesson::{Language, Lesson};

/// Which browser runtime a built site targets, and so which language it serves.
///
/// An enum, not a string (§1.2): an unknown target is rejected at the CLI parse
/// boundary rather than carried as data. Each target serves exactly one language
/// (§3.4); the build refuses a course whose lessons do not match it (§1.3.1).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuildTarget {
    /// webR — runs R in the browser, for R lessons.
    Webr,
    /// Pyodide — runs Python in the browser, for Python lessons.
    Pyodide,
}

impl BuildTarget {
    /// The language this target serves: webR runs R, Pyodide runs Python.
    pub fn language(self) -> Language {
        match self {
            BuildTarget::Webr => Language::R,
            BuildTarget::Pyodide => Language::Python,
        }
    }
}

impl fmt::Display for BuildTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            BuildTarget::Webr => "webr",
            BuildTarget::Pyodide => "pyodide",
        })
    }
}

/// One lesson as the in-browser runner consumes it: the Rust↔JS JSON contract
/// (ADR-0008, §3.2).
///
/// Deliberately *not* the internal [`Lesson`]: it carries only what the runtime
/// needs and drops `llm_evaluation_prompt` (a server/CLI concern never shipped to
/// the client). A Rust-side change invisible to this shape is invisible to JS.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SiteLesson {
    /// The lesson's course-scoped slug (its manifest id).
    pub id: String,
    /// The lesson's human-readable title (its `lesson_name`).
    pub title: String,
    /// What the learner is asked to do.
    pub prompt: String,
    /// Optional starter code for the editor.
    pub code_template: Option<String>,
    /// The check code-strings, run against a submission in the browser to grade it.
    pub checks: Vec<String>,
    /// The author's known-correct answer, for the runner to self-verify.
    pub solution: Option<String>,
}

impl SiteLesson {
    /// Derive the contract row from a discovered lesson and its course slug.
    ///
    /// Pure (§2.2): a projection of the already-validated [`Lesson`] that keeps
    /// only the fields the browser needs. Dropping `llm_evaluation_prompt` here is
    /// what decouples the JS runtime from the internal schema (§3.2).
    fn from_lesson(slug: &LessonSlug, lesson: &Lesson) -> SiteLesson {
        SiteLesson {
            id: slug.to_string(),
            title: lesson.lesson_name.to_string(),
            prompt: lesson.exercise.prompt.clone(),
            code_template: lesson.exercise.code_template.clone(),
            checks: lesson.checks.clone(),
            solution: lesson.exercise.solution.clone(),
        }
    }
}

/// One file in a planned site: a path relative to the output root and its
/// contents — the unit [`write_site`] commits to disk.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SiteFile {
    /// Path relative to the output directory (e.g. `index.html`,
    /// `lessons/0.json`).
    pub path: PathBuf,
    /// The file's full contents.
    pub contents: String,
}

/// A fully planned site: every file it comprises, in a deterministic order.
///
/// Produced by the pure [`plan_site`] and consumed by the effectful
/// [`write_site`], so the whole site can be asserted without touching a
/// filesystem (§2.3).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SiteFiles {
    files: Vec<SiteFile>,
}

impl SiteFiles {
    /// The planned files, in deterministic order.
    pub fn files(&self) -> &[SiteFile] {
        &self.files
    }
}

/// Why a site could not be planned.
#[derive(Debug)]
pub enum PlanError {
    /// A lesson's language does not match the target's (e.g. an R lesson built for
    /// the Pyodide/Python target). Refused before any file is produced (§1.3.1) so
    /// a broken, mixed-language site never ships.
    LanguageMismatch {
        /// The offending lesson's course slug.
        lesson: LessonSlug,
        /// The language that lesson is authored in.
        lesson_language: Language,
        /// The target the build was asked for.
        target: BuildTarget,
    },
    /// The target is recognized but its assembler is not built yet — the Pyodide
    /// target arrives in Slice 17. Carried so a valid-but-unbuilt target fails
    /// cleanly rather than panicking.
    TargetUnsupported {
        /// The not-yet-built target.
        target: BuildTarget,
    },
}

impl fmt::Display for PlanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PlanError::LanguageMismatch {
                lesson,
                lesson_language,
                target,
            } => write!(
                f,
                "lesson {lesson} is written in {lesson_language:?}, which does not match \
                 the {target} target's language ({:?})",
                target.language()
            ),
            PlanError::TargetUnsupported { target } => {
                write!(f, "the {target} build target is not implemented yet")
            }
        }
    }
}

impl Error for PlanError {}

/// Plan the static site for `lessons`, targeting `target`.
///
/// Pure (§2.1): it decides which files exist and their contents — embedded assets
/// plus the per-lesson JSON contract — with no filesystem access, so the result is
/// snapshot-testable. Refuses a language/target mismatch before producing anything
/// (§1.3.1): every lesson must be in the target's language, or the whole build
/// fails with [`PlanError::LanguageMismatch`] and no [`SiteFiles`] are returned.
pub fn plan_site(
    lessons: &[(LessonSlug, Lesson)],
    target: BuildTarget,
) -> Result<SiteFiles, PlanError> {
    for (slug, lesson) in lessons {
        if lesson.language != target.language() {
            return Err(PlanError::LanguageMismatch {
                lesson: slug.clone(),
                lesson_language: lesson.language.clone(),
                target,
            });
        }
    }
    match target {
        BuildTarget::Webr => Ok(webr::plan(lessons)),
        BuildTarget::Pyodide => Err(PlanError::TargetUnsupported { target }),
    }
}

/// Write a planned site to `out_dir`, creating parent directories as needed.
///
/// The single effectful step (§2.3): everything about *what* the site contains was
/// already decided by the pure [`plan_site`]; this only commits it to disk. Called
/// only after `plan_site` succeeds, so a refused build (a language mismatch) never
/// reaches here and never creates `out_dir`.
pub fn write_site(out_dir: &Path, site: &SiteFiles) -> std::io::Result<()> {
    for file in &site.files {
        let dest = out_dir.join(&file.path);
        if let Some(parent) = dest.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&dest, &file.contents)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::course::Course;
    use serde_json::Value;

    /// Load the all-R fixture course's lessons in full (slug + parsed lesson).
    fn r_course() -> Vec<(LessonSlug, Lesson)> {
        Course::open(Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/r-course"
        )))
        .expect("r-course opens")
        .load_lessons()
        .expect("r-course lessons load")
    }

    /// The Python-only subset of `course_basic` (its `greet` lesson) — a matching
    /// course for the Pyodide target, used to reach the not-yet-built arm without
    /// a dedicated Python fixture (that lands with Slice 17).
    fn python_only() -> Vec<(LessonSlug, Lesson)> {
        Course::open(Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/course_basic"
        )))
        .expect("course_basic opens")
        .load_lessons()
        .expect("course_basic lessons load")
        .into_iter()
        .filter(|(_, lesson)| lesson.language == Language::Python)
        .collect()
    }

    /// Find a planned file by its relative path, or panic listing what is present.
    fn file<'a>(site: &'a SiteFiles, path: &str) -> &'a SiteFile {
        site.files()
            .iter()
            .find(|f| f.path == Path::new(path))
            .unwrap_or_else(|| {
                let present: Vec<_> = site.files().iter().map(|f| f.path.clone()).collect();
                panic!("planned site is missing {path}; present: {present:?}")
            })
    }

    #[test]
    fn build_target_language_maps_each_runtime_to_its_language() {
        assert_eq!(BuildTarget::Webr.language(), Language::R);
        assert_eq!(BuildTarget::Pyodide.language(), Language::Python);
    }

    #[test]
    fn build_target_display_names_each_runtime() {
        assert_eq!(BuildTarget::Webr.to_string(), "webr");
        assert_eq!(BuildTarget::Pyodide.to_string(), "pyodide");
    }

    #[test]
    fn site_lesson_from_lesson_projects_each_contract_field() {
        let lessons = r_course();
        let (slug, lesson) = &lessons[0];
        let site_lesson = SiteLesson::from_lesson(slug, lesson);

        // Every field is pinned so a producer mutation that swaps two sources is
        // caught — and so the dropped `llm_evaluation_prompt` stays dropped.
        assert_eq!(site_lesson.id, "add-two");
        assert_eq!(site_lesson.title, "Add Two Numbers");
        assert_eq!(site_lesson.prompt, lesson.exercise.prompt);
        assert_eq!(site_lesson.code_template, lesson.exercise.code_template);
        assert_eq!(site_lesson.checks, lesson.checks);
        assert_eq!(site_lesson.solution, lesson.exercise.solution);
        assert!(
            site_lesson.solution.as_deref().unwrap().contains("x + y"),
            "the add-two solution rides the contract verbatim"
        );
    }

    #[test]
    fn plan_site_webr_assembles_the_shell_runner_shim_and_one_json_per_lesson() {
        let site =
            plan_site(&r_course(), BuildTarget::Webr).expect("an all-R course plans for webr");

        // The page shell, the runner, and the COOP/COEP shim all land.
        assert!(
            file(&site, "index.html")
                .contents
                .contains("coi-serviceworker.js")
        );
        assert!(
            file(&site, "lesson-runner.js")
                .contents
                .to_lowercase()
                .contains("webr"),
            "the runner boots webR"
        );
        // The shim file exists (its referencing from index.html is asserted above).
        let _ = file(&site, "coi-serviceworker.js");

        // One JSON per manifest entry — count matches the course, neither dropped
        // nor duplicated.
        let per_lesson = site
            .files()
            .iter()
            .filter(|f| f.path.starts_with("lessons/"))
            .count();
        assert_eq!(per_lesson, 2, "two lessons -> two per-lesson JSON files");
        let _ = file(&site, "lessons/0.json");
        let _ = file(&site, "lessons/1.json");
    }

    #[test]
    fn plan_site_serializes_each_lesson_to_the_browser_contract() {
        let site = plan_site(&r_course(), BuildTarget::Webr).expect("plans");

        // The per-lesson JSON carries exactly the contract fields the runner reads.
        let first: Value = serde_json::from_str(&file(&site, "lessons/0.json").contents)
            .expect("the per-lesson JSON parses");
        assert_eq!(first["id"], "add-two");
        assert_eq!(first["title"], "Add Two Numbers");
        assert!(first["checks"].as_array().unwrap().len() == 2);
        assert!(first["solution"].as_str().unwrap().contains("x + y"));

        // The lessons index is the ordered slug list the runner enumerates — by
        // index, so a lesson slug never becomes a filesystem path.
        let index: Value =
            serde_json::from_str(&file(&site, "lessons.json").contents).expect("index parses");
        assert_eq!(index, serde_json::json!(["add-two", "square"]));
    }

    #[test]
    fn plan_site_refuses_an_r_course_built_for_the_pyodide_target() {
        // Symmetric twin of the happy path: a language/target mismatch is refused
        // (§1.3.1) and returns no SiteFiles, so write_site is never reached.
        let err = plan_site(&r_course(), BuildTarget::Pyodide)
            .expect_err("an R course cannot be built for the Python target");
        match err {
            PlanError::LanguageMismatch {
                lesson_language,
                target,
                ..
            } => {
                assert_eq!(lesson_language, Language::R);
                assert_eq!(target, BuildTarget::Pyodide);
            }
            other => panic!("expected a language mismatch, got {other:?}"),
        }
    }

    #[test]
    fn plan_site_pyodide_target_is_not_built_yet() {
        // A matching (Python) course for Pyodide passes the language check but the
        // assembler is not built until Slice 17.
        let err = plan_site(&python_only(), BuildTarget::Pyodide)
            .expect_err("the pyodide assembler is not implemented in this slice");
        assert!(
            matches!(
                err,
                PlanError::TargetUnsupported {
                    target: BuildTarget::Pyodide
                }
            ),
            "expected TargetUnsupported(Pyodide), got {err:?}"
        );
    }

    #[test]
    fn write_site_writes_every_planned_file_to_disk() {
        let site = plan_site(&r_course(), BuildTarget::Webr).expect("plans");
        let tmp = tempfile::tempdir().unwrap();
        write_site(tmp.path(), &site).expect("the site writes");

        // The top-level shell and a nested per-lesson file both land — the latter
        // proves the parent directory is created.
        assert!(tmp.path().join("index.html").is_file());
        let lesson_json = std::fs::read_to_string(tmp.path().join("lessons/0.json"))
            .expect("the nested lesson JSON is written");
        let parsed: Value = serde_json::from_str(&lesson_json).expect("it parses");
        assert_eq!(parsed["id"], "add-two");
    }

    #[test]
    fn plan_error_display_distinguishes_its_variants() {
        let mismatch = plan_site(&r_course(), BuildTarget::Pyodide).unwrap_err();
        let text = mismatch.to_string().to_lowercase();
        assert!(
            text.contains("does not match") && text.contains("language"),
            "a mismatch names the language/target clash, got: {text}"
        );

        let unsupported = plan_site(&python_only(), BuildTarget::Pyodide).unwrap_err();
        assert!(
            unsupported.to_string().contains("not implemented"),
            "an unbuilt target says so, got: {unsupported}"
        );

        // Both are std::error::Errors with no nested source.
        let as_error: &dyn Error = &mismatch;
        assert!(as_error.source().is_none());
    }
}
