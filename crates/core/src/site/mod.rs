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

mod pyodide;
mod webr;

use std::error::Error;
use std::fmt;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

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

/// Whether a built site's lessons carry eval validation, and at what accuracy.
///
/// A *represented* state (§1.2), never a missing-file inference: the effectful
/// shell decides whether a report is present and hands the pure assembly an
/// explicit value, so the eval-results page can always distinguish "validated at
/// N%" from "not validated". A missing report can never masquerade as a real 0%.
#[derive(Debug, Clone, PartialEq)]
pub enum EvalSummary {
    /// Evals were run; `accuracy` is the figure the Slice-13 report recorded,
    /// carried through as-is (§3.2) and only formatted — never recomputed here.
    Validated {
        /// The suite's accuracy as recorded, a fraction in `[0.0, 1.0]`.
        accuracy: f64,
    },
    /// No eval report accompanied the course — the page says so explicitly.
    NotValidated,
}

impl EvalSummary {
    /// The `data-eval-status` body attribute the *validated* page carries. The
    /// single source for the marker, so the page and any probe asserting it cannot
    /// drift; mutually exclusive — as a substring — with
    /// [`Self::NOT_VALIDATED_MARKER`], so asserting one present and the other
    /// absent pins the state unambiguously.
    pub const VALIDATED_MARKER: &'static str = r#"data-eval-status="validated""#;
    /// The `data-eval-status` body attribute the *not-validated* page carries — the
    /// observable form of the [`EvalSummary::NotValidated`] state (§1.2).
    pub const NOT_VALIDATED_MARKER: &'static str = r#"data-eval-status="not-validated""#;
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
        }
    }
}

impl Error for PlanError {}

/// A course's bundled eval report could not be read as the Slice-13 JSON artifact.
///
/// Distinct from an *absent* report (which the CLI shell maps to
/// [`EvalSummary::NotValidated`] before parsing): a report that is present but
/// malformed fails the build loudly rather than silently dropping to
/// not-validated, so a corrupt artifact can never quietly unvalidate a course.
#[derive(Debug)]
pub struct EvalReportError(serde_json::Error);

impl fmt::Display for EvalReportError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "eval report is not valid Slice-13 JSON: {}", self.0)
    }
}

impl Error for EvalReportError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(&self.0)
    }
}

/// The in-browser runner core every target's `lesson-runner.js` imports: the
/// cross-target scaffolding (lesson loading, rendering, pass/fail reporting) that
/// a target's thin adapter sits atop. Shared, not duplicated per target (§4.2).
const LESSON_RUNNER_CORE_JS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/assets/shared/lesson-runner-core.js"
));
/// The COOP/COEP service-worker shim, so cross-origin isolation (and so
/// SharedArrayBuffer) works on GitHub Pages. Vendored once and shared by every
/// target — the isolation it provides is runtime-agnostic.
const COI_SERVICEWORKER_JS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/assets/shared/coi-serviceworker.js"
));
/// The BYOK feedback backend: the `FeedbackBackend` JS contract + its
/// `byok-anthropic` impl (Slice 18). Shared by every target — the Anthropic
/// browser call is identical whether the lesson runs in webR or Pyodide, so it is
/// assembled once here rather than forked per target (§4.2). The seam a future
/// backend (WebLLM) plugs into with no change to lesson rendering or execution.
const FEEDBACK_JS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/assets/shared/feedback.js"
));

/// A build target's own client assets: its page shell and its runner adapter.
///
/// A named-field struct, not two positional `&str`s (§1.4): the shell and the
/// runner are both HTML/JS strings of the same type, so a caller could silently
/// transpose them — injecting runner JS where the page shell belongs — with no
/// compile error. Naming the fields makes that illegal state unrepresentable at
/// the one seam every target extends through.
pub(super) struct TargetAssets<'a> {
    /// The target's `index.html` page shell.
    pub index_html: &'a str,
    /// The target's `lesson-runner.js` runtime adapter.
    pub lesson_runner_js: &'a str,
}

/// Assemble a site from one target's [`TargetAssets`] and the loaded lessons.
///
/// The shared scaffolding both targets reuse (§4.2): a target supplies only its
/// own shell and runner; the runner core, the COOP/COEP shim, and the per-lesson
/// JSON contract — keyed by position, never by slug, so an author's slug can never
/// reach the filesystem as a path — are identical across targets and laid out here
/// in deterministic order. The slug rides inside the JSON as `id`; `lessons.json`
/// is the ordered slug index the runner enumerates.
fn assemble(assets: TargetAssets<'_>, lessons: &[(LessonSlug, Lesson)]) -> SiteFiles {
    let mut files = vec![
        asset("index.html", assets.index_html),
        asset("lesson-runner.js", assets.lesson_runner_js),
        asset("lesson-runner-core.js", LESSON_RUNNER_CORE_JS),
        asset("coi-serviceworker.js", COI_SERVICEWORKER_JS),
        asset("feedback.js", FEEDBACK_JS),
    ];

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

/// Fold a Slice-13 eval report's JSON into the page model, taking its accuracy
/// as-is (§3.2) — the figure the `eval` command recorded, never recomputed here.
///
/// Pure (§2.3): the effectful read of the artifact file happens at the CLI edge,
/// which hands this the bytes. An *absent* report is the shell's concern (it maps
/// to [`EvalSummary::NotValidated`] without calling this); a *present* report that
/// does not parse is an [`EvalReportError`], so the build fails loudly rather than
/// silently treating a corrupt report as unvalidated.
pub fn eval_summary_from_report_json(json: &str) -> Result<EvalSummary, EvalReportError> {
    /// The slice of the report contract the page consumes: its aggregate accuracy.
    /// Per-case results are intentionally ignored — the page shows the headline
    /// figure, and reading only `accuracy` keeps the build from depending on the
    /// full case shape.
    #[derive(Deserialize)]
    struct ReportArtifact {
        accuracy: f64,
    }

    let artifact: ReportArtifact = serde_json::from_str(json).map_err(EvalReportError)?;
    Ok(EvalSummary::Validated {
        accuracy: artifact.accuracy,
    })
}

/// Render the standalone eval-results page for a site's [`EvalSummary`].
///
/// §5.1: the single function mapping the validation state to its page. Each state
/// renders its own `data-eval-status` marker and human copy, so a learner — and
/// the build's probe — can always tell a validated course (with its accuracy)
/// from an unvalidated one (§1.2); a missing report never renders as a real 0%.
fn eval_results_html(summary: &EvalSummary) -> String {
    let (status, body) = match summary {
        EvalSummary::Validated { accuracy } => (
            EvalSummary::VALIDATED_MARKER,
            format!(
                "These lessons were validated against an eval suite at \
                 <strong>{}% accuracy</strong>.",
                accuracy_percent(*accuracy)
            ),
        ),
        EvalSummary::NotValidated => (
            EvalSummary::NOT_VALIDATED_MARKER,
            "These lessons have <strong>not been validated</strong> — no eval \
             report accompanied this course."
                .to_string(),
        ),
    };
    format!(
        "<!doctype html>\n\
         <html lang=\"en\">\n\
         <head>\n\
         <meta charset=\"utf-8\">\n\
         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
         <title>Eval results</title>\n\
         </head>\n\
         <body {status}>\n\
         <h1>Eval results</h1>\n\
         <p>{body}</p>\n\
         </body>\n\
         </html>\n"
    )
}

/// The accuracy as a whole-number percentage for display. Reads the report's
/// figure as-is (§3.2) and only formats it — no re-scoring.
fn accuracy_percent(accuracy: f64) -> i64 {
    (accuracy * 100.0).round() as i64
}

/// Plan the static site for `lessons`, targeting `target`, folding in the eval
/// validation `eval` carries.
///
/// Pure (§2.1): it decides which files exist and their contents — embedded assets
/// plus the per-lesson JSON contract — with no filesystem access, so the result is
/// snapshot-testable. Refuses a language/target mismatch before producing anything
/// (§1.3.1): every lesson must be in the target's language, or the whole build
/// fails with [`PlanError::LanguageMismatch`] and no [`SiteFiles`] are returned.
/// Past that guard the build dispatches on the [`BuildTarget`] seam (§3.4) — each
/// target contributing only its own shell + runner atop the shared `assemble`.
///
/// The eval-results page is target-independent, so it is folded in here once
/// (§4.1) rather than per target: whichever runtime a site serves, the same
/// [`EvalSummary`] renders the same page — its accuracy, or an explicit
/// not-validated state — so "validation travels with the lessons".
pub fn plan_site(
    lessons: &[(LessonSlug, Lesson)],
    target: BuildTarget,
    eval: &EvalSummary,
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
    let mut site = match target {
        BuildTarget::Webr => webr::plan(lessons),
        BuildTarget::Pyodide => pyodide::plan(lessons),
    };
    site.files.push(SiteFile {
        path: "eval-results.html".into(),
        contents: eval_results_html(eval),
    });
    Ok(site)
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

    /// The all-Python `python-course` fixture's lessons in full (slug + parsed
    /// lesson) — a matching course for the Pyodide target, mirroring `r_course`.
    fn python_course() -> Vec<(LessonSlug, Lesson)> {
        Course::open(Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/python-course"
        )))
        .expect("python-course opens")
        .load_lessons()
        .expect("python-course lessons load")
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

    /// Plan a site with no eval validation — the default for the tests about
    /// lesson assembly, not the eval-results page. Eval folding is exercised by the
    /// dedicated tests below with an explicit [`EvalSummary`].
    fn plan(lessons: &[(LessonSlug, Lesson)], target: BuildTarget) -> Result<SiteFiles, PlanError> {
        plan_site(lessons, target, &EvalSummary::NotValidated)
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
        let site = plan(&r_course(), BuildTarget::Webr).expect("an all-R course plans for webr");

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
        // The shared runner core and the shim file both land (the shim's
        // referencing from index.html is asserted above).
        let _ = file(&site, "lesson-runner-core.js");
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
        let site = plan(&r_course(), BuildTarget::Webr).expect("plans");

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
    fn plan_site_serializes_a_missing_solution_as_json_null_not_dropped() {
        // solution is optional (ADR-0008): a lesson without one must still build,
        // with the field present-but-null so the contract shape stays stable for the
        // runner — never silently dropped (which a later `skip_serializing_if` would
        // do, breaking `lessons[i].solution`). course_basic's R lesson carries none.
        let r_lessons: Vec<(LessonSlug, Lesson)> = Course::open(Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/course_basic"
        )))
        .expect("course_basic opens")
        .load_lessons()
        .expect("course_basic lessons load")
        .into_iter()
        .filter(|(_, lesson)| lesson.language == Language::R)
        .collect();
        assert!(!r_lessons.is_empty(), "course_basic has an R lesson");

        let site =
            plan(&r_lessons, BuildTarget::Webr).expect("a solution-less R course still plans");
        let lesson: Value =
            serde_json::from_str(&file(&site, "lessons/0.json").contents).expect("parses");
        assert!(
            lesson.get("solution").is_some() && lesson["solution"].is_null(),
            "a missing solution serializes as null, never dropped: {lesson}"
        );
    }

    #[test]
    fn plan_site_refuses_an_r_course_built_for_the_pyodide_target() {
        // Symmetric twin of the happy path: a language/target mismatch is refused
        // (§1.3.1) and returns no SiteFiles, so write_site is never reached.
        let err = plan(&r_course(), BuildTarget::Pyodide)
            .expect_err("an R course cannot be built for the Python target");
        // PlanError has a single variant today, so a catch-all arm would be
        // unreachable; assert the shape with `matches!`. A future variant keeps
        // this honest — the R/Pyodide fields are pinned, not wildcarded.
        assert!(
            matches!(
                &err,
                PlanError::LanguageMismatch {
                    lesson_language: Language::R,
                    target: BuildTarget::Pyodide,
                    ..
                }
            ),
            "expected an R-vs-Pyodide language mismatch, got {err:?}"
        );
    }

    #[test]
    fn plan_site_pyodide_assembles_the_shell_runner_core_shim_and_one_json_per_lesson() {
        let site = plan(&python_course(), BuildTarget::Pyodide)
            .expect("an all-Python course plans for pyodide");

        // The shell boots the Pyodide runtime and references the COOP/COEP shim;
        // the runner boots Pyodide — not webR — so this is genuinely the Python
        // target and not a verbatim copy of the webR assets.
        let index = &file(&site, "index.html").contents;
        assert!(index.contains("coi-serviceworker.js"));
        assert!(
            index.to_lowercase().contains("pyodide"),
            "index.html boots the Pyodide runtime"
        );
        assert!(
            file(&site, "lesson-runner.js")
                .contents
                .to_lowercase()
                .contains("pyodide"),
            "the runner boots Pyodide"
        );
        // The shared runner core and the shim both land (referencing asserted above).
        let _ = file(&site, "lesson-runner-core.js");
        let _ = file(&site, "coi-serviceworker.js");

        // One JSON per manifest entry — the same per-lesson contract the webR
        // target produces, carried across the seam unchanged (§3.2).
        let per_lesson = site
            .files()
            .iter()
            .filter(|f| f.path.starts_with("lessons/"))
            .count();
        assert_eq!(per_lesson, 1, "one lesson -> one per-lesson JSON file");
        let first: Value = serde_json::from_str(&file(&site, "lessons/0.json").contents)
            .expect("the per-lesson JSON parses");
        assert_eq!(first["id"], "add-two");
        assert_eq!(first["title"], "Add Two Numbers");
        let index_json: Value =
            serde_json::from_str(&file(&site, "lessons.json").contents).expect("index parses");
        assert_eq!(index_json, serde_json::json!(["add-two"]));
    }

    #[test]
    fn plan_site_carries_the_same_shared_core_and_shim_across_both_targets() {
        // The seam's payoff (§3.2, §4.2): the shared runner core and the COOP/COEP
        // shim are byte-identical whichever target produced them — only the shell
        // and runner glue differ. A divergence here means a target forked the
        // shared scaffolding instead of reusing it.
        let webr = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let pyodide = plan(&python_course(), BuildTarget::Pyodide).expect("plans");
        for shared in ["lesson-runner-core.js", "coi-serviceworker.js"] {
            assert_eq!(
                file(&webr, shared).contents,
                file(&pyodide, shared).contents,
                "{shared} must be identical across targets"
            );
        }
        // ...while the per-target shell genuinely differs (R vs Python boot).
        assert_ne!(
            file(&webr, "index.html").contents,
            file(&pyodide, "index.html").contents,
            "each target carries its own shell"
        );
    }

    #[test]
    fn plan_site_assembles_the_shared_byok_feedback_backend() {
        // Slice 18: the FeedbackBackend contract + its byok-anthropic impl ship as a
        // single shared feedback.js (§4.2) — the Anthropic BYOK call is identical for
        // R and Python lessons — referenced by each target's shell. A per-target fork
        // or a shell that never loads it both fail here.
        let webr = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let pyodide = plan(&python_course(), BuildTarget::Pyodide).expect("plans");

        for site in [&webr, &pyodide] {
            let _ = file(site, "feedback.js");
            assert!(
                file(site, "index.html").contents.contains("feedback.js"),
                "each shell must load feedback.js (the seam is dead if unreferenced)"
            );
        }
        assert_eq!(
            file(&webr, "feedback.js").contents,
            file(&pyodide, "feedback.js").contents,
            "feedback.js is shared — byte-identical across targets, not forked"
        );
    }

    #[test]
    fn write_site_writes_every_planned_file_to_disk() {
        let site = plan(&r_course(), BuildTarget::Webr).expect("plans");
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
    fn plan_error_language_mismatch_displays_the_clash_with_no_nested_source() {
        let mismatch = plan(&r_course(), BuildTarget::Pyodide).unwrap_err();
        let text = mismatch.to_string().to_lowercase();
        assert!(
            text.contains("does not match") && text.contains("language"),
            "a mismatch names the language/target clash, got: {text}"
        );

        // A std::error::Error with no nested source.
        let as_error: &dyn Error = &mismatch;
        assert!(as_error.source().is_none());
    }

    #[test]
    fn eval_results_html_validated_renders_the_accuracy_and_validated_marker() {
        // The validated page shows the report's accuracy as a percentage and
        // carries the validated marker, never the not-validated one (§1.2). Two
        // different accuracies render two different figures, so the percentage is
        // derived from the summary, not a constant.
        let two_thirds = eval_results_html(&EvalSummary::Validated {
            accuracy: 0.6666666666666666,
        });
        assert!(
            two_thirds.contains("67%"),
            "2/3 should render 67%: {two_thirds}"
        );
        assert!(two_thirds.contains(EvalSummary::VALIDATED_MARKER));
        assert!(!two_thirds.contains(EvalSummary::NOT_VALIDATED_MARKER));

        let half = eval_results_html(&EvalSummary::Validated { accuracy: 0.5 });
        assert!(
            half.contains("50%"),
            "the figure tracks the summary, not a hardcoded constant: {half}"
        );
    }

    #[test]
    fn eval_results_html_not_validated_renders_an_explicit_marker() {
        // The not-validated page is explicit (§1.2): it carries the not-validated
        // marker and plain human copy, and is *not* the validated state — so a
        // missing report is never mistaken for a real result.
        let page = eval_results_html(&EvalSummary::NotValidated);
        assert!(page.contains(EvalSummary::NOT_VALIDATED_MARKER));
        assert!(
            page.to_lowercase().contains("not been validated"),
            "the page states plainly the lessons were not validated: {page}"
        );
        assert!(!page.contains(EvalSummary::VALIDATED_MARKER));
    }

    #[test]
    fn eval_summary_from_report_json_takes_the_accuracy_as_is() {
        // §3.2: the build consumes the Slice-13 artifact as-is — the accuracy is
        // read straight from the JSON, never recomputed from the cases. A report
        // whose recorded accuracy (0.5) contradicts its single all-matched case
        // (which would re-derive 1.0) proves it: the summary reflects the recorded
        // figure, not a re-scored one.
        let json = r#"{"cases":[{"expected":"correct","actual":"correct","matched":true}],"accuracy":0.5}"#;
        let summary = eval_summary_from_report_json(json).expect("a well-formed report parses");
        assert_eq!(summary, EvalSummary::Validated { accuracy: 0.5 });
    }

    #[test]
    fn eval_summary_from_report_json_rejects_a_malformed_report() {
        // A present-but-unreadable report is a loud error, not a silent drop to
        // not-validated — a corrupt artifact must never quietly unvalidate a course.
        assert!(eval_summary_from_report_json("{ not json").is_err());
        // Missing the accuracy field is equally unreadable for the page's purpose.
        assert!(eval_summary_from_report_json(r#"{"cases":[]}"#).is_err());
    }

    #[test]
    fn plan_site_folds_the_eval_results_page_for_each_state() {
        // The eval-results page rides every built site (§4.1), in whichever state
        // the EvalSummary carries — the same plan_site path handles both states.
        let validated = plan_site(
            &r_course(),
            BuildTarget::Webr,
            &EvalSummary::Validated {
                accuracy: 0.6666666666666666,
            },
        )
        .expect("plans");
        let validated_page = file(&validated, "eval-results.html").contents.clone();
        assert!(validated_page.contains("67%"));
        assert!(validated_page.contains(EvalSummary::VALIDATED_MARKER));
        assert!(!validated_page.contains(EvalSummary::NOT_VALIDATED_MARKER));

        let unvalidated =
            plan_site(&r_course(), BuildTarget::Webr, &EvalSummary::NotValidated).expect("plans");
        assert!(
            file(&unvalidated, "eval-results.html")
                .contents
                .contains(EvalSummary::NOT_VALIDATED_MARKER)
        );

        // Target-independent: the same summary renders the same page whichever
        // runtime the site serves (§4.1) — the page is not forked per target.
        let pyodide = plan_site(
            &python_course(),
            BuildTarget::Pyodide,
            &EvalSummary::Validated {
                accuracy: 0.6666666666666666,
            },
        )
        .expect("plans");
        assert_eq!(
            file(&pyodide, "eval-results.html").contents,
            validated_page,
            "the eval-results page is identical across targets for the same summary"
        );
    }
}
