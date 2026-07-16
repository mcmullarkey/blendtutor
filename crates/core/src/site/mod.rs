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

use crate::course::{LessonSlug, SiteConfig};
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
    /// Third-party packages the lesson's code depends on. Always serialized as
    /// an array — empty when the lesson declares none, never `null` or omitted
    /// — so the JS runtime contract shape stays stable (ADR-0011, mirroring the
    /// `solution: null` precedent from ADR-0008).
    pub packages: Vec<String>,
    /// The author's known-correct answer, for the runner to self-verify.
    pub solution: Option<String>,
    /// Optional learner-facing hints, rendered as an expandable `<details>` panel
    /// in the browser. Always serialized (null when absent, never dropped via
    /// `skip_serializing_if`) so the contract shape stays stable — mirroring the
    /// `solution: null` precedent from ADR-0008.
    pub hints: Option<String>,
    /// Optional learner-facing gotchas (common pitfalls), rendered as an
    /// expandable `<details>` panel in the browser. Always serialized (null
    /// when absent, never dropped via `skip_serializing_if`) so the contract
    /// shape stays stable — mirroring the `solution: null` and `hints: null`
    /// precedents from ADR-0008.
    pub gotchas: Option<String>,
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
            packages: lesson.packages.clone(),
            solution: lesson.exercise.solution.clone(),
            hints: lesson.exercise.hints.clone(),
            gotchas: lesson.exercise.gotchas.clone(),
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
/// unreadable fails the build loudly rather than silently dropping to
/// not-validated, so a corrupt artifact can never quietly unvalidate a course.
#[derive(Debug)]
pub enum EvalReportError {
    /// The report is not valid JSON, or lacks the expected `accuracy` shape.
    Malformed(serde_json::Error),
    /// The report parsed but its accuracy lies outside the representable
    /// `[0.0, 1.0]` (§1.3.1): a hand-edited or corrupt figure that would otherwise
    /// render nonsense like `200%`. Rejected at the read boundary, not carried.
    AccuracyOutOfRange {
        /// The out-of-range figure the report recorded.
        accuracy: f64,
    },
}

impl fmt::Display for EvalReportError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalReportError::Malformed(err) => {
                write!(f, "eval report is not valid Slice-13 JSON: {err}")
            }
            EvalReportError::AccuracyOutOfRange { accuracy } => write!(
                f,
                "eval report accuracy {accuracy} is outside the expected range [0.0, 1.0]"
            ),
        }
    }
}

impl Error for EvalReportError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            EvalReportError::Malformed(err) => Some(err),
            EvalReportError::AccuracyOutOfRange { .. } => None,
        }
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
/// The shared design-token stylesheet: the `--bt-` custom-property system and
/// semantic region rules embedded at compile time and loaded by both targets'
/// page shells via `<link rel="stylesheet" href="styles.css">`. Shared, not
/// duplicated per target (§4.2): a single source of truth for the cross-target
/// visual vocabulary, so a token-value change touches one `:root` declaration
/// and both targets pick it up without touching Rust or JS.
const STYLES_CSS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/assets/shared/styles.css"
));
/// The vendored CodeMirror 6 ESM bundle: a pre-built (one-time esbuild authoring,
/// NOT build-time codegen — ADR-0008) bundle of the CM6 core (`EditorView`) plus
/// the R and Python language packs and the UX-polish extensions (`lineNumbers`,
/// `highlightActiveLine`, `bracketMatching`, `indentWithTab`). Shared by every
/// target — the editor is runtime-agnostic, so the bundle is assembled once here
/// rather than forked per target (§4.2). Main-thread-only (no web workers) to
/// avoid COOP/COEP conflicts; the seam AC-2/AC-3 consume read-only.
const CODEMIRROR_JS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/assets/shared/codemirror.js"
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

/// Render the `config.js` contract file: the `window.__btConfig` global the
/// feedback.js rate limiter reads. Pure (§2.2): a direct projection of the
/// [`SiteConfig`] into the JS contract shape — the Rust→JS boundary for
/// site-level configuration (§3.2), alongside the per-lesson JSON contract.
fn config_js(site_config: &SiteConfig) -> String {
    format!(
        "window.__btConfig = {{ maxFeedbackPerSession: {} }};",
        site_config.max_feedback_per_session
    )
}

/// Assemble a site from one target's [`TargetAssets`] and the loaded lessons.
///
/// The shared scaffolding both targets reuse (§4.2): a target supplies only its
/// own shell and runner; the runner core, the COOP/COEP shim, the `config.js`
/// contract, and the per-lesson JSON contract — keyed by position, never by
/// slug, so an author's slug can never reach the filesystem as a path — are
/// identical across targets and laid out here in deterministic order. The slug
/// rides inside the JSON as `id`; `lessons.json` is the ordered slug index the
/// runner enumerates. `config.js` sits immediately before `feedback.js` so the
/// `window.__btConfig` global is available when the rate limiter reads it.
fn assemble(
    assets: TargetAssets<'_>,
    lessons: &[(LessonSlug, Lesson)],
    site_config: &SiteConfig,
) -> SiteFiles {
    let config_contents = config_js(site_config);
    let mut files = vec![
        asset("index.html", assets.index_html),
        asset("lesson-runner.js", assets.lesson_runner_js),
        asset("lesson-runner-core.js", LESSON_RUNNER_CORE_JS),
        asset("coi-serviceworker.js", COI_SERVICEWORKER_JS),
        asset("config.js", &config_contents),
        asset("feedback.js", FEEDBACK_JS),
        asset("styles.css", STYLES_CSS),
        asset("codemirror.js", CODEMIRROR_JS),
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

    let artifact: ReportArtifact =
        serde_json::from_str(json).map_err(EvalReportError::Malformed)?;
    // A genuine Slice-13 report's accuracy is matched/total, always in [0.0, 1.0].
    // Anything else is a corrupt or hand-edited artifact — reject it at the read
    // boundary (§1.3.1) so a nonsense figure (`200%`, `-50%`, NaN) never renders.
    if !(0.0..=1.0).contains(&artifact.accuracy) {
        return Err(EvalReportError::AccuracyOutOfRange {
            accuracy: artifact.accuracy,
        });
    }
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
    site_config: &SiteConfig,
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
        BuildTarget::Webr => webr::plan(lessons, site_config),
        BuildTarget::Pyodide => pyodide::plan(lessons, site_config),
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
    /// dedicated tests below with an explicit [`EvalSummary`]. Uses the default
    /// [`SiteConfig`] (max_feedback_per_session = 20) — site-config behavior is
    /// exercised by the dedicated `feedback_rate_limit_*` tests below.
    fn plan(lessons: &[(LessonSlug, Lesson)], target: BuildTarget) -> Result<SiteFiles, PlanError> {
        plan_site(
            lessons,
            target,
            &EvalSummary::NotValidated,
            &SiteConfig::default(),
        )
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
        assert_eq!(site_lesson.packages, lesson.packages);
        assert_eq!(site_lesson.solution, lesson.exercise.solution);
        assert!(
            site_lesson.solution.as_deref().unwrap().contains("x + y"),
            "the add-two solution rides the contract verbatim"
        );
        assert_eq!(
            site_lesson.hints, lesson.exercise.hints,
            "hints ride the contract verbatim from exercise.hints"
        );
        assert_eq!(
            site_lesson.gotchas, lesson.exercise.gotchas,
            "gotchas ride the contract verbatim from exercise.gotchas"
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
        assert!(
            first["hints"].as_str().unwrap().contains("assignment"),
            "the add-two hints ride the contract JSON verbatim"
        );

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
    fn plan_site_serializes_hints_when_present() {
        // hints is optional: a lesson that carries one must serialize the text
        // verbatim into the per-lesson JSON so the browser runner can render it
        // in an expandable <details>. r-course's add-two lesson carries hints.
        let site = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let first: Value = serde_json::from_str(&file(&site, "lessons/0.json").contents)
            .expect("the per-lesson JSON parses");
        assert!(
            first["hints"].is_string(),
            "a lesson with hints serializes them as a JSON string, got: {first}"
        );
        assert!(
            first["hints"].as_str().unwrap().contains("assignment"),
            "the hints text rides the contract verbatim: {first}"
        );
    }

    #[test]
    fn plan_site_serializes_a_missing_hints_as_json_null_not_dropped() {
        // hints is optional: a lesson without one must still build, with the
        // field present-but-null so the contract shape stays stable for the
        // runner — never silently dropped (which a `skip_serializing_if` would
        // do, breaking `lessons[i].hints`). course_basic's R lesson carries none.
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

        let site = plan(&r_lessons, BuildTarget::Webr).expect("a hints-less R course still plans");
        let lesson: Value =
            serde_json::from_str(&file(&site, "lessons/0.json").contents).expect("parses");
        assert!(
            lesson.get("hints").is_some() && lesson["hints"].is_null(),
            "a missing hints serializes as null, never dropped: {lesson}"
        );
    }

    #[test]
    fn plan_site_serializes_gotchas_when_present() {
        // gotchas is optional: a lesson that carries one must serialize the text
        // verbatim into the per-lesson JSON so the browser runner can render it.
        // We parse a lesson with bullet-formatted gotchas and pair it with a slug
        // from the r-course fixture (the slug is just an identifier).
        use crate::lesson::Lesson;
        let yaml = r#"
lesson_name: "Gotcha Lesson"
language: R
exercise:
  prompt: "Write add_two(x, y)."
  gotchas: |
    - R uses '<-' for assignment, not '='.
    - Functions return their last expression automatically.
  llm_evaluation_prompt: "Grade this: {student_code}"
"#;
        let lesson = Lesson::parse(yaml).expect("a lesson with gotchas should parse");
        let slug = r_course()[0].0.clone();
        let lessons = [(slug, lesson)];
        let site = plan(&lessons, BuildTarget::Webr).expect("plans");
        let first: Value = serde_json::from_str(&file(&site, "lessons/0.json").contents)
            .expect("the per-lesson JSON parses");
        assert!(
            first["gotchas"].is_string(),
            "a lesson with gotchas serializes them as a JSON string, got: {first}"
        );
        assert!(
            first["gotchas"].as_str().unwrap().contains("'<-'"),
            "the gotchas text rides the contract verbatim: {first}"
        );
    }

    #[test]
    fn plan_site_serializes_a_missing_gotchas_as_json_null_not_dropped() {
        // gotchas is optional: a lesson without one must still build, with the
        // field present-but-null so the contract shape stays stable for the
        // runner — never silently dropped (which a `skip_serializing_if` would
        // do, breaking `lessons[i].gotchas`). course_basic's R lesson carries
        // none. Mirrors the solution: null and hints: null precedents.
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
            plan(&r_lessons, BuildTarget::Webr).expect("a gotchas-less R course still plans");
        let lesson: Value =
            serde_json::from_str(&file(&site, "lessons/0.json").contents).expect("parses");
        assert!(
            lesson.get("gotchas").is_some() && lesson["gotchas"].is_null(),
            "a missing gotchas serializes as null, never dropped: {lesson}"
        );
    }

    #[test]
    fn plan_site_serializes_packages_as_array_even_when_empty() {
        // ADR-0011: packages must always serialize as an array — empty when the
        // lesson declares none, never null or absent — so the JS runtime
        // contract shape stays stable (mirrors the solution: null precedent).
        // The r-course fixture lessons have no `packages` key.
        let site = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let lesson: Value =
            serde_json::from_str(&file(&site, "lessons/0.json").contents).expect("parses");
        assert!(
            lesson["packages"].is_array(),
            "packages must be an array, not null/absent: {lesson}"
        );
        assert!(
            lesson["packages"].as_array().unwrap().is_empty(),
            "a lesson without packages must emit an empty array: {lesson}"
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
        for shared in [
            "lesson-runner-core.js",
            "coi-serviceworker.js",
            "styles.css",
            "codemirror.js",
        ] {
            assert_eq!(
                file(&webr, shared).contents,
                file(&pyodide, shared).contents,
                "{shared} must be identical across targets"
            );
        }
        // ...while the per-target shell genuinely differs (R vs Python boot) —
        // but only by the 3 known diffs (title, boot text, CDN script).
        assert_ne!(
            file(&webr, "index.html").contents,
            file(&pyodide, "index.html").contents,
            "each target carries its own shell"
        );
    }

    #[test]
    fn plan_site_styles_css_is_present_for_both_targets() {
        // AC-1 predicate 1: styles.css lands as a planned file for both targets,
        // not just one — a target that forgot to include it would miss here.
        let webr = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let pyodide = plan(&python_course(), BuildTarget::Pyodide).expect("plans");
        let _ = file(&webr, "styles.css");
        let _ = file(&pyodide, "styles.css");
    }

    #[test]
    fn plan_site_shells_have_link_and_no_inline_style() {
        // AC-1 predicate 2: both shells reference the external stylesheet and
        // have zero inline <style> blocks. A shell that keeps the old inline
        // <style> AND adds the <link> would be caught here.
        for (target, course) in [
            (BuildTarget::Webr, r_course()),
            (BuildTarget::Pyodide, python_course()),
        ] {
            let site = plan(&course, target).expect("plans");
            let html = &file(&site, "index.html").contents;
            // Accept both self-closing <link ... /> and HTML5 <link ... >
            let link_self_closing = html.contains(r#"<link rel="stylesheet" href="styles.css" />"#);
            let link_html5 = html.contains(r#"<link rel="stylesheet" href="styles.css">"#);
            assert!(
                link_self_closing || link_html5,
                "{target} index.html must link styles.css"
            );
            assert!(
                !html.contains("<style>"),
                "{target} index.html must not contain inline <style>"
            );
        }
    }

    #[test]
    fn plan_site_styles_css_declares_tokens_and_uses_them() {
        // AC-1 predicate 3: styles.css declares --bt- custom properties in :root
        // and has >=4 rules referencing var(--bt-). This proves tokens are used,
        // not declared-dead.
        let site = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let css = &file(&site, "styles.css").contents;
        assert!(css.contains(":root"), "styles.css must have a :root block");
        assert!(
            css.contains("--bt-"),
            "styles.css must declare --bt- custom properties"
        );
        let var_refs: Vec<&str> = css
            .lines()
            .filter(|line| line.contains("var(--bt-"))
            .collect();
        assert!(
            var_refs.len() >= 4,
            "styles.css must have >= 4 rules referencing var(--bt-), got {}: {:?}",
            var_refs.len(),
            var_refs
        );
    }

    #[test]
    fn plan_site_workspace_styles_use_tokens_and_status_renders_as_pill() {
        // AC-2: workspace CSS contract — selectors present, token usage floor,
        // no hardcoded hex, badge/pill via data-status attribute selectors (not
        // class selectors), every AC-2-added token consumed outside :root.
        let site = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let css = &file(&site, "styles.css").contents;

        // 1. Workspace selectors present
        for selector in [
            ".lesson-picker",
            "#lesson-title",
            "#lesson-prompt",
            "#submission",
            ".controls",
            "#run",
            "#submit",
            "#lesson-status",
            "#output",
        ] {
            assert!(
                css.contains(selector),
                "styles.css must contain selector `{selector}`"
            );
        }

        // 2. ≥6 var(--bt-) references within workspace rules (exclude :root).
        // Split on the workspace section marker to isolate workspace rules.
        let workspace_marker = "/* === workspace === */";
        assert!(
            css.contains(workspace_marker),
            "styles.css must have a workspace section marker"
        );
        let after_marker = css
            .split(workspace_marker)
            .nth(1)
            .expect("workspace section exists");
        // Only count lines in the workspace section that contain var(--bt-)
        let workspace_var_refs: Vec<&str> = after_marker
            .lines()
            .filter(|line| line.contains("var(--bt-"))
            .collect();
        assert!(
            workspace_var_refs.len() >= 6,
            "styles.css workspace section must have >= 6 var(--bt-) references, got {}: {:?}",
            workspace_var_refs.len(),
            workspace_var_refs
        );

        // 3. No hardcoded hex in workspace rules — exclude the dark-mode @media block
        //    (which carries hex token overrides by design).
        // Match exactly 3, 6, or 8 hex digits after # (full color or shorthand,
        // with optional alpha). Avoids matching non-color hex patterns like
        // "#feedback" in comments (7 chars, but {6} and {3} won't match 7).
        let before_dark_mode = after_marker
            .split("/* ── Dark mode ")
            .next()
            .unwrap_or(after_marker);
        let hex_re =
            regex_lite::Regex::new(r"#[0-9a-fA-F]{6}(?:[0-9a-fA-F]{2})?\b|#[0-9a-fA-F]{3}\b")
                .unwrap();
        if let Some(hit) = hex_re.find(before_dark_mode) {
            panic!(
                "workspace rules must not contain hardcoded hex literals, found `{}`",
                hit.as_str()
            );
        }

        // 4. Exactly 5 #lesson-status[data-status="..."] rules
        //    (4 status values + 1 dark-mode idle override in @media block)
        let status_selector_re =
            regex_lite::Regex::new(r#"#lesson-status\[data-status="[^"]+"\]"#).unwrap();
        let status_matches: Vec<_> = status_selector_re.find_iter(css).collect();
        assert_eq!(
            status_matches.len(),
            5,
            "expected exactly 5 `#lesson-status[data-status=\"...\"]` rules \
             (4 status values + 1 dark-mode idle override), got {}: {:?}",
            status_matches.len(),
            status_matches
        );

        // 5. No .status-* class selectors
        let status_class_re = regex_lite::Regex::new(r"\.status-(idle|running|pass|fail)").unwrap();
        assert!(
            !status_class_re.is_match(css),
            "styles.css must not contain .status-* class selectors"
        );

        // 6. Every AC-2-added token used >= 1 outside :root.
        // The 9 tokens from the AC-2 token table — some may already be declared
        // by AC-1 as forward-declarations. We check each declared in :root has
        // a var(--bt-...) consumer outside :root.
        let ac2_tokens = [
            "--bt-color-status-idle",
            "--bt-color-border",
            "--bt-color-brand-hover",
            "--bt-space-xs",
            "--bt-space-sm",
            "--bt-space-md",
            "--bt-space-lg",
            "--bt-shadow-sm",
            "--bt-radius-pill",
        ];
        // Parse the :root block to find which tokens AC-2's section adds.
        // We use the workspace marker to split: tokens in :root but consumed
        // by workspace rules are AC-2's responsibility.
        let root_block: &str = &css[..css.find(":root").unwrap_or(0)];
        let _root_block = root_block; // suppress unused warning in red phase
        let outside_root = &css[css.find("}").map(|i| i + 1).unwrap_or(0)..];

        for token in &ac2_tokens {
            let token_declared = css.contains(&format!("{token}:"));
            let token_consumed = outside_root.contains(&format!("var({token}"));
            if token_declared {
                assert!(
                    token_consumed,
                    "AC-2-added token {token} is declared in :root but never used via var() \
                     outside :root (dead token)"
                );
            }
        }
    }

    #[test]
    fn plan_site_shells_contain_semantic_regions() {
        // AC-1 predicate 4: both shells have the required semantic layout regions
        // with the expected structure.
        for (target, course) in [
            (BuildTarget::Webr, r_course()),
            (BuildTarget::Pyodide, python_course()),
        ] {
            let site = plan(&course, target).expect("plans");
            let html = &file(&site, "index.html").contents;

            // Exactly one of each semantic region
            let header_count = html.matches(r#"<header class="site-header">"#).count();
            let main_count = html.matches(r#"<main class="workspace">"#).count();
            let footer_count = html.matches(r#"<footer class="site-footer">"#).count();
            assert_eq!(
                header_count, 1,
                "{target}: expected 1 <header class=\"site-header\">"
            );
            assert_eq!(
                main_count, 1,
                "{target}: expected 1 <main class=\"workspace\">"
            );
            assert_eq!(
                footer_count, 1,
                "{target}: expected 1 <footer class=\"site-footer\">"
            );

            // Header contains <h1>blendtutor</h1>
            assert!(
                html.contains("<h1>blendtutor</h1>"),
                "{target}: header must contain <h1>blendtutor</h1>"
            );

            // All 6 data-test hooks present
            for hook in [
                "lesson-select",
                "submission",
                "run",
                "lesson-status",
                "output",
                "feedback",
            ] {
                let attr = format!(r#"data-test="{}""#, hook);
                assert!(
                    html.contains(&attr),
                    "{target}: missing data-test=\"{hook}\""
                );
            }

            // submit carries data-action="submit"
            assert!(
                html.contains(r#"data-action="submit""#),
                "{target}: submit must carry data-action=\"submit\""
            );

            // lesson-status has data-status="idle"
            assert!(
                html.contains(r#"data-status="idle""#),
                "{target}: lesson-status must have data-status=\"idle\""
            );

            // All JS-required IDs present
            for id in [
                "boot-status",
                "lesson-title",
                "lesson-prompt",
                "submission",
                "lesson-select",
                "output",
                "run",
                "feedback",
                "submit",
                "lesson-status",
            ] {
                let attr = format!(r#"id="{}""#, id);
                assert!(html.contains(&attr), "{target}: missing id=\"{id}\"");
            }

            // AC-2 clause 2: the submission mount is a <div> (CM6 parent), NOT a
            // <textarea>. A shell that kept the old textarea fails here; a shell
            // with both fails here too (exactly one div, zero textarea).
            assert!(
                html.contains(r#"<div id="submission""#),
                "{target}: submission mount must be <div id=\"submission\"> (CM6 parent)"
            );
            assert!(
                !html.contains(r#"<textarea id="submission""#),
                "{target}: submission mount must NOT be a <textarea> (replaced by CM6 editor)"
            );
        }
    }

    #[test]
    fn plan_site_shells_have_correct_head_load_order() {
        // AC-1 predicate 5: head load order preserved — coi-serviceworker.js
        // before styles.css link, and the link is before the body-end module scripts.
        for (target, course) in [
            (BuildTarget::Webr, r_course()),
            (BuildTarget::Pyodide, python_course()),
        ] {
            let site = plan(&course, target).expect("plans");
            let html = &file(&site, "index.html").contents;

            let coi_pos = html
                .find(r#"src="coi-serviceworker.js""#)
                .expect("coi-serviceworker.js must be referenced");
            let link_pos = html
                .find(r#"href="styles.css""#)
                .expect("styles.css link must be present");
            assert!(
                coi_pos < link_pos,
                "{target}: coi-serviceworker.js must appear before styles.css link"
            );

            // Module scripts come after the stylesheet link (in body-end)
            let link_close_pos = html[link_pos..]
                .find('>')
                .map(|p| link_pos + p)
                .expect("link tag closes");
            let runner_pos = html
                .find(r#"src="lesson-runner.js""#)
                .expect("lesson-runner.js must be referenced");
            assert!(
                link_close_pos < runner_pos,
                "{target}: styles.css link must appear before lesson-runner.js script"
            );
        }
    }

    #[test]
    fn plan_site_shells_differ_only_by_three_known_diffs() {
        // AC-1 predicate 6: after normalizing (stripping) the 3 known diffs,
        // the two shells are byte-identical. This catches a 4th unintended
        // divergence between targets.
        let webr = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let pyodide = plan(&python_course(), BuildTarget::Pyodide).expect("plans");

        let webr_html = &file(&webr, "index.html").contents;
        let pyodide_html = &file(&pyodide, "index.html").contents;

        // Normalize: strip <title> contents, #boot-status text, and pyodide CDN
        // script block (comment + script tag, only present in pyodide shell).
        let normalize = |html: &str| -> String {
            let mut s = html.to_string();
            // Strip <title> content (keep the tags)
            s = s.replace(
                "<title>blendtutor — interactive R lessons</title>",
                "<title></title>",
            );
            s = s.replace(
                "<title>blendtutor — interactive Python lessons</title>",
                "<title></title>",
            );
            // Strip boot-status text
            s = s.replace("Booting webR…", "");
            s = s.replace("Booting Pyodide…", "");
            // Strip the pyodide CDN comment + script block (only present in pyodide),
            // including the indentation whitespace and newline before the comment.
            let pyodide_block_start = "<!--\n      The Pyodide runtime";
            if let Some(start) = s.find(pyodide_block_start) {
                // Back up to the preceding newline to drop the indentation too
                let preceding = s[..start].rfind('\n').map(|i| i + 1).unwrap_or(start);
                // Find the closing </script> after the block start
                if let Some(end) = s[start..].find("</script>") {
                    let block_end = start + end + "</script>".len();
                    // Strip the trailing newline after the script tag
                    let after = &s[block_end..];
                    let strip_newline = after.strip_prefix('\n').unwrap_or(after);
                    s = format!("{}{}", &s[..preceding], strip_newline);
                }
            }
            s
        };

        assert_eq!(
            normalize(webr_html),
            normalize(pyodide_html),
            "after normalizing 3 known diffs (title, boot text, CDN script), \
             shells must be byte-identical"
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

        // The inclusive bounds render their whole-number percent — 0% is a real
        // (validated, all-wrong) result, distinct from the not-validated state.
        assert!(eval_results_html(&EvalSummary::Validated { accuracy: 0.0 }).contains("0%"));
        let perfect = eval_results_html(&EvalSummary::Validated { accuracy: 1.0 });
        assert!(
            perfect.contains("100%"),
            "1.0 should render 100%: {perfect}"
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
        assert!(matches!(
            eval_summary_from_report_json("{ not json"),
            Err(EvalReportError::Malformed(_))
        ));
        // Missing the accuracy field is equally unreadable for the page's purpose.
        assert!(matches!(
            eval_summary_from_report_json(r#"{"cases":[]}"#),
            Err(EvalReportError::Malformed(_))
        ));
    }

    #[test]
    fn eval_summary_from_report_json_rejects_an_out_of_range_accuracy() {
        // §1.3.1: a real report's accuracy is matched/total, always in [0.0, 1.0].
        // A figure outside that is a corrupt/hand-edited artifact and is rejected at
        // the read boundary, so the page never renders nonsense like 200% or -50%.
        // (JSON has no NaN/Inf literal, so only finite out-of-range is reachable
        // from an artifact; the guard rejects NaN/Inf too, defensively.)
        for bogus in ["2.0", "-0.5", "1.0001"] {
            let json = format!(r#"{{"cases":[],"accuracy":{bogus}}}"#);
            let result = eval_summary_from_report_json(&json);
            assert!(
                matches!(result, Err(EvalReportError::AccuracyOutOfRange { .. })),
                "accuracy {bogus} must be rejected as out of range, got {result:?}"
            );
        }
        // The inclusive bounds 0.0 and 1.0 are valid — a perfect or zero score.
        for ok in ["0.0", "1.0"] {
            let json = format!(r#"{{"cases":[],"accuracy":{ok}}}"#);
            assert!(
                eval_summary_from_report_json(&json).is_ok(),
                "accuracy {ok} is on the inclusive boundary and must be accepted"
            );
        }
    }

    #[test]
    fn eval_report_error_displays_each_variant_and_chains_its_source() {
        // The malformed variant names the JSON failure and preserves the underlying
        // serde error as its source, so a build failure is diagnosable.
        let malformed = eval_summary_from_report_json("{ not json").unwrap_err();
        assert!(
            malformed.to_string().contains("not valid Slice-13 JSON"),
            "malformed Display names the JSON failure, got: {malformed}"
        );
        let as_error: &dyn Error = &malformed;
        assert!(
            as_error.source().is_some(),
            "a malformed report chains its serde source for diagnosis"
        );

        // The out-of-range variant names the offending figure and the bound, and is
        // self-contained — a validation failure, no nested cause.
        let out_of_range =
            eval_summary_from_report_json(r#"{"cases":[],"accuracy":2.0}"#).unwrap_err();
        let text = out_of_range.to_string();
        assert!(
            text.contains('2') && text.contains("range"),
            "out-of-range Display names the bad figure and the range, got: {text}"
        );
        let as_error: &dyn Error = &out_of_range;
        assert!(as_error.source().is_none());
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
            &SiteConfig::default(),
        )
        .expect("plans");
        let validated_page = file(&validated, "eval-results.html").contents.clone();
        assert!(validated_page.contains("67%"));
        assert!(validated_page.contains(EvalSummary::VALIDATED_MARKER));
        assert!(!validated_page.contains(EvalSummary::NOT_VALIDATED_MARKER));

        let unvalidated = plan_site(
            &r_course(),
            BuildTarget::Webr,
            &EvalSummary::NotValidated,
            &SiteConfig::default(),
        )
        .expect("plans");
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
            &SiteConfig::default(),
        )
        .expect("plans");
        assert_eq!(
            file(&pyodide, "eval-results.html").contents,
            validated_page,
            "the eval-results page is identical across targets for the same summary"
        );
    }

    #[test]
    fn plan_site_emits_vendored_codemirror_bundle() {
        // AC-1 (code-editor): a pre-built CodeMirror 6 ESM bundle is vendored as a
        // committed static asset at assets/shared/codemirror.js, embedded at compile
        // time via include_str! (CODEMIRROR_JS const) and emitted to built sites for
        // BOTH targets through the shared `assemble` step — not forked per target.
        //
        // 8 clauses pin the invariant (§1.5 — predicates, not coincident shape):
        //   1. codemirror.js present in SiteFiles for both targets
        //   2. byte-identical across targets (shared, not forked — §4.2)
        //   3. contents.len() > 10_000 (catches empty/comment-only stub)
        //   4. contains EditorView (CM6 core export — catches wrong-library/UMD)
        //   5. contains evidence of lang-r + lang-python + lineNumbers +
        //      highlightActiveLine + bracketMatching + indentWithTab
        //   6. no `new Worker(` (COOP/COEP isolation — no web workers)
        //   7. deterministic order: after styles.css AND before any lessons/ file
        //      (full positional invariant, not weak "after styles.css" proxy)
        //   8. CODEMIRROR_JS const compiles — proved by the test running
        //      (include_str! refuses a missing asset at compile time — §1.3.1)
        let webr = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let pyodide = plan(&python_course(), BuildTarget::Pyodide).expect("plans");

        // Clause 1: codemirror.js lands as a planned file for both targets — a
        // target that forgot to include it (or embedded it in webr.rs/pyodide.rs
        // instead of shared mod.rs) misses here.
        let webr_cm = file(&webr, "codemirror.js").contents.clone();
        let pyodide_cm = file(&pyodide, "codemirror.js").contents.clone();

        // Clause 2: byte-identical across targets — the bundle is shared, not
        // forked per target (§4.2). A divergence means a target forked the bundle.
        assert_eq!(
            webr_cm, pyodide_cm,
            "codemirror.js must be byte-identical across targets (shared, not forked)"
        );

        // Clause 3: the bundle is a real CM6 build, not an empty/comment-only stub.
        // CM6 core + lang-r + lang-python is ~150-250KB minified; a 23-byte stub
        // fails here.
        assert!(
            webr_cm.len() > 10_000,
            "codemirror.js must be a real bundle (>10_000 bytes), got {} bytes",
            webr_cm.len()
        );

        // Clause 4: EditorView — the CM6 core export. A wrong-library or UMD-global
        // bundle lacking the ESM EditorView export fails here.
        assert!(
            webr_cm.contains("EditorView"),
            "codemirror.js must contain EditorView (CM6 core ESM export)"
        );

        // Clause 5: evidence of both language packs AND the UX-polish exports.
        // - `rLanguage` is the R language descriptor name (lang-r evidence —
        //   distinctive, preserved by esbuild; the bare export name `r` is too
        //   short to grep reliably).
        // - `python` is the lang-python export name (11 occurrences in the bundle).
        // - lineNumbers, highlightActiveLine, bracketMatching, indentWithTab are
        //   the UX-polish exports AC-3 consumes (AC-1 owns the complete export set).
        // - keymap is the keymap facet from @codemirror/view — AC-3 composes
        //   indentWithTab via keymap.of([indentWithTab]) so Tab stays in the
        //   editor (not browser focus traversal). Re-vendored by AC-3.
        // - syntaxHighlighting + defaultHighlightStyle are the token-styling
        //   exports from @codemirror/language — without these the editor renders
        //   text with no `.tok-*` classes (the builder-vision-probe regression:
        //   language support parses, highlight style colors).
        // - HighlightStyle + tags are the custom-style exports: HighlightStyle
        //   (from @codemirror/language) lets the runner define a HighlightStyle
        //   with deterministic `.tok-*` class names, and tags (from
        //   @lezer/highlight) supplies the tag constants the style maps. Without
        //   these the runner cannot override the default's opaque `ͼa` classes.
        // A core-only bundle missing the language packs, or a bundle missing the
        // UX/highlight exports, fails here.
        for needle in [
            "rLanguage",
            "python",
            "lineNumbers",
            "highlightActiveLine",
            "bracketMatching",
            "indentWithTab",
            "keymap",
            "syntaxHighlighting",
            "defaultHighlightStyle",
            "HighlightStyle",
            "tags",
        ] {
            assert!(
                webr_cm.contains(needle),
                "codemirror.js must contain `{needle}` (language pack or UX export evidence)"
            );
        }

        // Clause 6: no web workers — CM6 must be main-thread-only to avoid COOP/COEP
        // conflicts. A bundler-emitted `new Worker(` (e.g. a future WASM-parser
        // extension) fails here.
        assert!(
            !webr_cm.contains("new Worker("),
            "codemirror.js must not contain `new Worker(` (COOP/COEP isolation)"
        );

        // Clause 7: full positional invariant — codemirror.js sits after styles.css
        // AND before any lessons/ file in the deterministic files vector. This is
        // the sufficient condition (not the weak "after styles.css" proxy): a rule
        // appended after codemirror.js but before lessons/ would still pass the weak
        // proxy but break the intended order. Checking both bounds pins the full
        // invariant.
        let webr_files = webr.files();
        let pos = |name: &str| -> usize {
            webr_files
                .iter()
                .position(|f| f.path == Path::new(name))
                .unwrap_or_else(|| panic!("{name} must be in the planned site"))
        };
        let cm_pos = pos("codemirror.js");
        let styles_pos = pos("styles.css");
        let first_lesson_pos = webr_files
            .iter()
            .position(|f| f.path.starts_with("lessons/"))
            .expect("at least one lessons/ file exists");
        assert!(
            cm_pos > styles_pos,
            "codemirror.js (index {cm_pos}) must come after styles.css (index {styles_pos})"
        );
        assert!(
            cm_pos < first_lesson_pos,
            "codemirror.js (index {cm_pos}) must come before the first lessons/ file \
             (index {first_lesson_pos})"
        );

        // Clause 8: CODEMIRROR_JS const compiles — proved by the test running at
        // all. include_str! refuses a missing asset at compile time (§1.3.1), so
        // reaching this assertion means the const resolved. No runtime check needed;
        // the test's existence is the proof.
    }

    #[test]
    fn plan_site_shells_load_codemirror_as_import() {
        // AC-2 (code-editor): the lesson runner core wires a CodeMirror 6 editor
        // into the submission mount via a STATIC ESM import from the vendored
        // codemirror.js bundle (AC-1), reads the doc back through a
        // `getSubmission()` contract (not `.value`), never touches `innerHTML`
        // on the editor DOM, and each target adapter declares its `language`
        // as a dedicated field (not a string match on `runtime.name`).
        //
        // 6 clauses pin the build-time invariant (§1.5):
        //   3. data-test="submission" + id="submission" preserved on the div;
        //      the runSubmission contract on window.__bt is unchanged.
        //   4. lesson-runner-core.js has a static `import { EditorView ... }`
        //      from "./codemirror.js" (not a dynamic import, not a global).
        //   5. feedback.js reads the submission via `getSubmission()`, never
        //      `.value` on a submission element (the old textarea proxy).
        //   6. lesson-runner-core.js never uses `innerHTML` on the editor DOM
        //      (untrusted code_template must never be parsed as HTML).
        //   7. both lesson-runner.js adapters pass a `language:` field (closed
        //      set: "r" | "python"), not a runtime.name string match.
        // (Clause 1 — codemirror.js in site output — is pinned by
        //  plan_site_emits_vendored_codemirror_bundle; clause 2 — div not
        //  textarea — is pinned in plan_site_shells_contain_semantic_regions.)
        let webr = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let pyodide = plan(&python_course(), BuildTarget::Pyodide).expect("plans");

        // Clause 3: the rodney test contract survives the textarea→div swap. The
        // data-test hook and id are still on the submission mount, and the
        // window.__bt.runSubmission seam is still exported by the runner core.
        for (target, site) in [(BuildTarget::Webr, &webr), (BuildTarget::Pyodide, &pyodide)] {
            let html = &file(site, "index.html").contents;
            assert!(
                html.contains(r#"data-test="submission""#),
                "{target}: data-test=\"submission\" hook must be preserved on the div"
            );
            assert!(
                html.contains(r#"id="submission""#),
                "{target}: id=\"submission\" must be preserved on the div"
            );
        }
        let core = &file(&webr, "lesson-runner-core.js").contents;
        assert!(
            core.contains("runSubmission"),
            "lesson-runner-core.js must still export the runSubmission contract"
        );
        assert!(
            core.contains("window.__bt"),
            "lesson-runner-core.js must still expose window.__bt"
        );

        // Clause 4: static ESM import of EditorView from the vendored bundle. A
        // dynamic import (`import("./codemirror.js")`) or a UMD global fails here
        // — the static import is what makes the editor a build-time dependency.
        assert!(
            core.contains("import { EditorView"),
            "lesson-runner-core.js must statically import EditorView from codemirror.js"
        );
        assert!(
            core.contains("\"./codemirror.js\""),
            "lesson-runner-core.js must import from \"./codemirror.js\""
        );

        // Clause 5: feedback.js reads the submission through the getSubmission()
        // contract, never the old `.value` textarea proxy. A feedback.js that
        // still reads `submissionEl.value` or `getElementById(\"submission\").value`
        // fails here — the div has no `.value`, so that path returns undefined.
        let feedback = &file(&webr, "feedback.js").contents;
        assert!(
            feedback.contains("getSubmission"),
            "feedback.js must read the submission via getSubmission()"
        );
        assert!(
            !feedback.contains("submissionEl.value"),
            "feedback.js must NOT read submissionEl.value (the div has no .value)"
        );
        assert!(
            !feedback.contains("getElementById(\"submission\").value"),
            "feedback.js must NOT read .value on the submission element"
        );

        // Clause 6: no innerHTML on the editor DOM. The code_template is untrusted
        // lesson content; parsing it as HTML would be an injection vector (the
        // same threat model that keeps lesson titles off innerHTML). The runner
        // core must use EditorView.dispatch / textContent, never innerHTML.
        assert!(
            !core.contains("innerHTML"),
            "lesson-runner-core.js must never use innerHTML (untrusted code_template)"
        );

        // Clause 7: each target adapter declares a `language:` field (closed set),
        // not a string match on runtime.name. A webr adapter with `language: \"r\"`
        // and a pyodide adapter with `language: \"python\"` pass; an adapter that
        // omits the field (relying on runtime.name matching) fails here.
        let webr_runner = &file(&webr, "lesson-runner.js").contents;
        let pyodide_runner = &file(&pyodide, "lesson-runner.js").contents;
        assert!(
            webr_runner.contains("language: \"r\""),
            "webr/lesson-runner.js must declare language: \"r\""
        );
        assert!(
            pyodide_runner.contains("language: \"python\""),
            "pyodide/lesson-runner.js must declare language: \"python\""
        );
    }

    #[test]
    fn plan_site_cm6_editor_ux_extensions_configured() {
        // AC-3 (code-editor): the CM6 editor is configured with standard
        // code-editor UX — line numbers, bracket matching, smart tab handling,
        // active line highlighting, spellcheck off. This test pins the 5
        // build-time-checkable clauses (1, 5, 8, 10, 11) of the executable spec
        // by parsing the assembled SiteFiles content. The rodney browser probes
        // (clauses 2/3/4/6/7/9/12) are run by @builder-vision-probe — they need
        // a live browser + CM6 boot to check computed styles.
        //
        // 8 clauses pin the build-time invariant (§1.5 — predicates, not
        // coincident shape):
        //   1. lineNumbers() wired into the editor extensions
        //   2. highlightActiveLine() wired
        //   3. bracketMatching() wired
        //   4. indentWithTab in a keymap.of([...]) composition (Tab stays in
        //      editor, not browser focus traversal — sneaky-pass #3)
        //   5. spellcheck explicitly set to "false" (not absent — absent
        //      inherits true on contenteditable, sneaky-pass #5)
        //   6. spellcheck never set to "true"
        //   7. .cm-matchingBracket CSS rule has a non-transparent visual property
        //      (background/outline/box-shadow/border-color — sneaky-pass #2:
        //      bracketMatching() adds the class but no CSS = no visual)
        //   8. .cm-gutters not hidden via display:none/visibility:hidden
        //      (sneaky-pass #1: gutter exists but zero pixels)
        let webr = plan(&r_course(), BuildTarget::Webr).expect("plans");

        let core = &file(&webr, "lesson-runner-core.js").contents;
        let css = &file(&webr, "styles.css").contents;

        // Clause 1: lineNumbers() — the gutter extension is wired. A config
        // that imports lineNumbers but forgets to add it to the extensions
        // array fails here.
        assert!(
            core.contains("lineNumbers()"),
            "lesson-runner-core.js must wire lineNumbers() into the editor extensions"
        );

        // Clause 2: highlightActiveLine() — active-line highlighting wired.
        assert!(
            core.contains("highlightActiveLine()"),
            "lesson-runner-core.js must wire highlightActiveLine() into the editor extensions"
        );

        // Clause 3: bracketMatching() — bracket-match highlighting wired.
        assert!(
            core.contains("bracketMatching()"),
            "lesson-runner-core.js must wire bracketMatching() into the editor extensions"
        );

        // Clause 4: indentWithTab in a keymap.of([...]) composition. Importing
        // indentWithTab alone is insufficient — it must be passed to keymap.of
        // so the Tab key is intercepted by the editor (sneaky-pass #3: imported
        // but not composed, Tab falls through to browser focus traversal).
        assert!(
            core.contains("indentWithTab"),
            "lesson-runner-core.js must reference indentWithTab"
        );
        assert!(
            core.contains("keymap.of("),
            "lesson-runner-core.js must compose indentWithTab via keymap.of([...])"
        );

        // Clause 5: spellcheck explicitly set to "false". The contenteditable
        // .cm-content inherits spellcheck=true by browser default; an absent
        // attribute is NOT sufficient (sneaky-pass #5). The explicit "false"
        // string must appear alongside spellcheck.
        assert!(
            core.contains("spellcheck"),
            "lesson-runner-core.js must set spellcheck on the editor content"
        );
        assert!(
            core.contains("\"false\""),
            "lesson-runner-core.js must set spellcheck to \"false\" (explicit, not absent)"
        );

        // Clause 6: spellcheck never set to "true". A config that accidentally
        // enables spellcheck fails here.
        assert!(
            !core.contains("spellcheck: \"true\""),
            "lesson-runner-core.js must NOT set spellcheck to \"true\""
        );
        assert!(
            !core.contains("spellcheck:'true'"),
            "lesson-runner-core.js must NOT set spellcheck to 'true'"
        );

        // Clause 7: .cm-matchingBracket CSS rule has a non-transparent visual
        // property. bracketMatching() adds the cm-matchingBracket class to the
        // DOM, but without a CSS rule the class is invisible (sneaky-pass #2).
        // The rule must set background/outline/box-shadow/border-color to a
        // non-transparent value. We check the rule exists AND contains one of
        // the visual properties (a bare `.cm-matchingBracket {}` empty rule fails).
        assert!(
            css.contains(".cm-matchingBracket"),
            "styles.css must contain a .cm-matchingBracket rule"
        );
        // Extract the .cm-matchingBracket block and verify it has a visual
        // property. A transparent-only rule (e.g. background: transparent) is
        // insufficient — the bracket match must be VISIBLE.
        let bm_pos = css.find(".cm-matchingBracket").expect("bracket-match rule");
        let after_bm = &css[bm_pos..];
        let brace = after_bm
            .find('{')
            .expect(".cm-matchingBracket must be followed by a declaration block");
        let body_start = bm_pos + brace + 1;
        let body_slice = &css[body_start..];
        let close = body_slice
            .find('}')
            .expect(".cm-matchingBracket block must close");
        let bm_body = &css[body_start..body_start + close];
        assert!(
            bm_body.contains("background")
                || bm_body.contains("outline")
                || bm_body.contains("box-shadow")
                || bm_body.contains("border-color"),
            ".cm-matchingBracket must have a visual property (background/outline/box-shadow/border-color)"
        );
        assert!(
            !bm_body.contains("transparent"),
            ".cm-matchingBracket visual property must NOT be transparent"
        );

        // Clause 8: .cm-gutters not hidden. A rule that sets display:none or
        // visibility:hidden on .cm-gutters makes the gutter zero-pixels
        // (sneaky-pass #1: gutter exists in DOM but is invisible).
        assert!(
            !css.contains(".cm-gutters") || !gutter_hidden(css),
            "styles.css must NOT hide .cm-gutters via display:none or visibility:hidden"
        );
    }

    #[test]
    fn plan_site_runner_core_sets_cursor_color_via_cm6_theme() {
        // v4 cursor fix: CSS-only overrides in styles.css (!important +
        // caret-color, PRs #93/#94) did not work in the live browser despite
        // correct CSS on the deployed site. CM6's injected base-theme styles
        // were winning the cascade. The fix moves cursor styling into a CM6
        // EditorView.theme() extension, which injects styles via CM6's own
        // style-module system at a higher precedence than the base theme.
        //
        // 7 clauses pin the fix:
        //   1. EditorView.theme() is called to create a theme extension
        //   2. The theme targets .cm-cursor with borderLeftColor
        //   3. The borderLeftColor uses var(--bt-color-cursor) (adapts to
        //      light/dark via the CSS variable defined in styles.css)
        //   4. The theme sets borderLeftWidth: 2px (wider than CM6 default)
        //   5. The theme sets marginLeft: -1px (centers the wider cursor)
        //   6. The theme targets .cm-content with caretColor (native caret)
        //   7. The theme extension is added to the editorExtensions array
        let webr = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let core = &file(&webr, "lesson-runner-core.js").contents;

        // Clause 1: EditorView.theme() is called — creates a CM6 theme
        // extension rather than relying on external CSS.
        assert!(
            core.contains("EditorView.theme("),
            "lesson-runner-core.js must call EditorView.theme() to create a \
             cursor theme extension (CSS-only overrides did not work in live \
             browser — CM6 base-theme styles won the cascade)"
        );

        // Clause 2: the theme targets .cm-cursor with borderLeftColor.
        // CM6's base theme sets borderLeft: "1.2px solid black" — we must
        // override the borderLeftColor to a light value for dark mode.
        assert!(
            core.contains("borderLeftColor"),
            "lesson-runner-core.js cursor theme must set borderLeftColor on \
             .cm-cursor (CM6 default is black — invisible on dark background)"
        );

        // Clause 3: borderLeftColor uses var(--bt-color-cursor) so the cursor
        // adapts to light/dark mode via the CSS variable defined in
        // styles.css (light: #1a1a1a, dark: #ffffff). The fallback #ffffff
        // ensures visibility if the variable is undefined.
        assert!(
            core.contains("var(--bt-color-cursor, #ffffff)"),
            "lesson-runner-core.js cursor theme must use \
             var(--bt-color-cursor, #ffffff) for borderLeftColor \
             (adapts to light/dark via CSS variable, fallback ensures \
             visibility)"
        );

        // Clause 4: cursor width is 2px (wider than CM6 default 1.2px).
        assert!(
            core.contains("borderLeftWidth") && core.contains("\"2px\""),
            "lesson-runner-core.js cursor theme must set borderLeftWidth: \
             \"2px\" (CM6 default 1.2px is too faint — widened for \
             accessibility)"
        );

        // Clause 5: the theme sets marginLeft to center the wider cursor on
        // the insertion point. CM6's default cursor is 1.2px with
        // marginLeft: -0.6px; at 2px width the cursor must shift to -1px
        // to stay centered.
        assert!(
            core.contains("marginLeft"),
            "lesson-runner-core.js cursor theme must set marginLeft for centering"
        );

        // Clause 6: the theme targets .cm-content with caretColor. The
        // native contenteditable caret renders as faint gray in dark mode;
        // setting caretColor makes it match the drawn cursor.
        assert!(
            core.contains("caretColor"),
            "lesson-runner-core.js cursor theme must set caretColor on \
             .cm-content (native contenteditable caret is faint gray \
             without it)"
        );

        // Clause 7: the theme extension is added to the editorExtensions
        // array. A theme defined but not wired into the extensions array
        // would have no effect (sneaky-pass: defined but not composed).
        assert!(
            core.contains("cursorTheme"),
            "lesson-runner-core.js must add cursorTheme to the \
             editorExtensions array (a theme defined but not wired has no \
             effect)"
        );
    }

    #[test]
    fn plan_site_runner_core_renders_hints_as_expandable_details() {
        // The lesson runner core must render `lesson.hints` as an expandable
        // <details> element when the hints field is non-null and non-empty,
        // and remove it when switching to a lesson without hints.
        //
        // Build-time-checkable invariants (the rodney browser probe verifies the
        // live DOM behavior):
        //   1. References `lesson.hints` — the field is consumed, not ignored.
        //   2. Creates a <details> element with id="lesson-hints".
        //   3. Creates a <summary> child (the expandable toggle).
        //   4. Sets hints text via textContent — NEVER parsed as HTML (the same
        //      threat model that keeps lesson titles off innerHTML; untrusted
        //      lesson content must never be parsed as HTML).
        //   5. Inserts the details after #lesson-prompt (the prompt element).
        //   6. Removes the element when hints is null/empty (lesson switch).
        let webr = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let core = &file(&webr, "lesson-runner-core.js").contents;

        // Clause 1: the hints field is read from the lesson object.
        assert!(
            core.contains("lesson.hints"),
            "lesson-runner-core.js must reference lesson.hints"
        );

        // Clause 2: a <details> element with id="lesson-hints" is created.
        assert!(
            core.contains(r#""details""#),
            "lesson-runner-core.js must create a <details> element"
        );
        assert!(
            core.contains(r#""lesson-hints""#),
            "lesson-runner-core.js must set id=\"lesson-hints\" on the details"
        );

        // Clause 3: a <summary> child is created.
        assert!(
            core.contains(r#""summary""#),
            "lesson-runner-core.js must create a <summary> element"
        );

        // Clause 4: hints text is set via textContent. The existing
        // `!core.contains("innerHTML")` invariant (plan_site_shells_load_codemirror_as_import
        // clause 6) already guarantees no innerHTML is used anywhere — so the
        // hints text MUST go through textContent. We assert textContent appears
        // in the hints-rendering path by checking it is used at all (the existing
        // invariant covers the negative case).
        assert!(
            core.contains("textContent"),
            "lesson-runner-core.js must use textContent for hints text"
        );

        // Clause 5: the details is inserted after the prompt element. The runner
        // core holds a `promptEl` reference (document.getElementById("lesson-prompt"));
        // the hints details must be inserted relative to it.
        assert!(
            core.contains("promptEl"),
            "lesson-runner-core.js must reference promptEl for hints insertion"
        );

        // Clause 6: the element is removed when hints is absent. A renderLesson
        // that only creates but never removes would leave a stale hints panel
        // from a previous lesson visible after switching.
        assert!(
            core.contains(".remove()"),
            "lesson-runner-core.js must remove the hints element when absent"
        );
    }

    #[test]
    fn plan_site_styles_css_has_hints_panel_rules() {
        // The #lesson-hints <details> panel must be styled via var(--bt-*)
        // tokens, with a pointer cursor on the <summary> toggle. Dark mode is
        // handled by the existing :root token overrides in the @media block —
        // no hardcoded hex literals in the hints rules.
        let webr = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let css = &file(&webr, "styles.css").contents;

        // Clause 1: #lesson-hints selector exists.
        assert!(
            css.contains("#lesson-hints"),
            "styles.css must contain a #lesson-hints rule"
        );

        // Clause 2: the summary toggle has cursor: pointer.
        let hints_pos = css
            .find("#lesson-hints")
            .expect("#lesson-hints rule exists");
        let after_hints = &css[hints_pos..];
        assert!(
            after_hints.contains("cursor: pointer") || after_hints.contains("cursor:pointer"),
            "styles.css must set cursor: pointer on the hints summary toggle"
        );

        // Clause 3: the hints rules use var(--bt-*) tokens (no hardcoded hex).
        // Extract the #lesson-hints rule block and check for token usage.
        let brace = after_hints
            .find('{')
            .expect("#lesson-hints must have an opening brace");
        let body_start = hints_pos + brace + 1;
        let body_slice = &css[body_start..];
        let close = body_slice
            .find('}')
            .expect("#lesson-hints block must close");
        let hints_body = &css[body_start..body_start + close];
        assert!(
            hints_body.contains("var(--bt-"),
            "#lesson-hints must use var(--bt-*) tokens, got: {hints_body}"
        );
    }

    #[test]
    fn plan_site_runner_core_splits_hints_and_gotchas() {
        // AC-2 (issue #88): Split hints and gotchas into separate expandable UI
        // sections. The lesson runner core must have TWO separate functions —
        // renderHints and renderGotchas — each creating its own <details> with a
        // unique ID, parsing bullet lines into <ul><li>, using textContent (never
        // innerHTML), and independently removing stale DOM. renderLesson must
        // call both. The combined "Hints & Gotchas" panel is gone.
        let webr = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let core = &file(&webr, "lesson-runner-core.js").contents;

        // Clause 1: renderGotchas function exists (new — the split).
        assert!(
            core.contains("function renderGotchas("),
            "lesson-runner-core.js must define a renderGotchas function"
        );

        // Clause 2: renderHints function still exists (now hints-only).
        assert!(
            core.contains("function renderHints("),
            "lesson-runner-core.js must define a renderHints function"
        );

        // Clause 3: lesson.gotchas is referenced (the field is consumed).
        assert!(
            core.contains("lesson.gotchas"),
            "lesson-runner-core.js must reference lesson.gotchas"
        );

        // Clause 4: unique ID "lesson-gotchas" for the gotchas details.
        assert!(
            core.contains(r#""lesson-gotchas""#),
            "lesson-runner-core.js must set id=\"lesson-gotchas\" on the gotchas details"
        );

        // Clause 5: the combined "Hints & Gotchas" label is gone.
        assert!(
            !core.contains("Hints & Gotchas"),
            "lesson-runner-core.js must NOT use the combined 'Hints & Gotchas' label"
        );

        // Clause 6: bullet parsing creates <ul> and <li> elements.
        assert!(
            core.contains(r#""ul""#),
            "lesson-runner-core.js must create <ul> elements for bullet parsing"
        );
        assert!(
            core.contains(r#""li""#),
            "lesson-runner-core.js must create <li> elements for bullet parsing"
        );

        // Clause 7: bullet marker detection uses startsWith (for "- " or "* ").
        assert!(
            core.contains("startsWith"),
            "lesson-runner-core.js must use startsWith for bullet marker detection"
        );

        // Clause 8: textContent is used for <li> text (not innerHTML).
        // The existing !innerHTML invariant (plan_site_shells_load_codemirror_as_import
        // clause 6) covers the negative case.
        assert!(
            core.contains("textContent"),
            "lesson-runner-core.js must use textContent for bullet text"
        );

        // Clause 9: each function independently removes stale DOM (>= 2 .remove()).
        let remove_count = core.matches(".remove()").count();
        assert!(
            remove_count >= 2,
            "lesson-runner-core.js must have >= 2 .remove() calls (one per section), got {remove_count}"
        );

        // Clause 10: renderLesson calls both renderHints and renderGotchas.
        assert!(
            core.contains("renderHints(lesson.hints)"),
            "renderLesson must call renderHints(lesson.hints)"
        );
        assert!(
            core.contains("renderGotchas(lesson.gotchas)"),
            "renderLesson must call renderGotchas(lesson.gotchas)"
        );
    }

    #[test]
    fn plan_site_styles_css_has_gotchas_panel_rules() {
        // AC-2 (issue #88): The #lesson-gotchas <details> panel must be styled
        // via var(--bt-*) tokens, parallel to #lesson-hints. Dark mode is
        // handled by the existing :root token overrides in the @media block —
        // no hardcoded hex literals in the gotchas rules.
        let webr = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let css = &file(&webr, "styles.css").contents;

        // Clause 1: #lesson-gotchas selector exists.
        assert!(
            css.contains("#lesson-gotchas"),
            "styles.css must contain a #lesson-gotchas rule"
        );

        // Clause 2: the gotchas summary toggle has cursor: pointer.
        let gotchas_pos = css
            .find("#lesson-gotchas")
            .expect("#lesson-gotchas rule exists");
        let after_gotchas = &css[gotchas_pos..];
        assert!(
            after_gotchas.contains("cursor: pointer") || after_gotchas.contains("cursor:pointer"),
            "styles.css must set cursor: pointer on the gotchas summary toggle"
        );

        // Clause 3: the gotchas rules use var(--bt-*) tokens (no hardcoded hex).
        let brace = after_gotchas
            .find('{')
            .expect("#lesson-gotchas must have an opening brace");
        let body_start = gotchas_pos + brace + 1;
        let body_slice = &css[body_start..];
        let close = body_slice
            .find('}')
            .expect("#lesson-gotchas block must close");
        let gotchas_body = &css[body_start..body_start + close];
        assert!(
            gotchas_body.contains("var(--bt-"),
            "#lesson-gotchas must use var(--bt-*) tokens, got: {gotchas_body}"
        );
    }

    /// Detect whether a `.cm-gutters` rule sets display:none or visibility:hidden.
    fn gutter_hidden(css: &str) -> bool {
        let mut rest = css;
        while let Some(pos) = rest.find(".cm-gutters") {
            let after = &rest[pos..];
            let Some(brace) = after.find('{') else {
                rest = &rest[pos + ".cm-gutters".len()..];
                continue;
            };
            let body_start = pos + brace + 1;
            let body_slice = &rest[body_start..];
            let Some(close) = body_slice.find('}') else {
                rest = &rest[pos + ".cm-gutters".len()..];
                continue;
            };
            let body = &rest[body_start..body_start + close];
            if body.contains("display: none") || body.contains("visibility: hidden") {
                return true;
            }
            rest = &rest[body_start + close..];
        }
        false
    }

    // --- AC-4: client-side rate limiting — config.js emission (predicates 4-5) --

    #[test]
    fn feedback_rate_limit_plan_site_emits_config_js() {
        // Predicate 4: plan_site emits config.js with "maxFeedbackPerSession": 5.
        // The config.js file is the Rust→JS contract for site-level configuration
        // (§3.2): it carries window.__btConfig = { maxFeedbackPerSession: N },
        // which feedback.js reads to enforce the per-session rate limit.
        let site_config = SiteConfig {
            max_feedback_per_session: 5,
        };
        let site = plan_site(
            &r_course(),
            BuildTarget::Webr,
            &EvalSummary::NotValidated,
            &site_config,
        )
        .expect("plans");
        let config = file(&site, "config.js");
        assert!(
            config.contents.contains("window.__btConfig"),
            "config.js must set window.__btConfig; got: {}",
            config.contents
        );
        assert!(
            config.contents.contains("maxFeedbackPerSession"),
            "config.js must contain maxFeedbackPerSession; got: {}",
            config.contents
        );
        assert!(
            config.contents.contains("maxFeedbackPerSession: 5"),
            "config.js must carry the configured max (5); got: {}",
            config.contents
        );

        // The default SiteConfig (max=20) emits 20 — the default the feedback
        // rate limiter reads when no [site] section is present.
        let default_site = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let default_config = file(&default_site, "config.js");
        assert!(
            default_config
                .contents
                .contains("maxFeedbackPerSession: 20"),
            "config.js must carry the default max (20); got: {}",
            default_config.contents
        );

        // config.js is byte-identical across targets (shared contract, §4.2).
        let pyodide = plan_site(
            &python_course(),
            BuildTarget::Pyodide,
            &EvalSummary::NotValidated,
            &site_config,
        )
        .expect("plans");
        assert_eq!(
            file(&site, "config.js").contents,
            file(&pyodide, "config.js").contents,
            "config.js must be byte-identical across targets for the same SiteConfig"
        );
    }

    #[test]
    fn feedback_rate_limit_shells_load_config_before_feedback() {
        // Predicate 5: both targets' index.html load config.js BEFORE feedback.js.
        // config.js sets window.__btConfig synchronously (classic script, not a
        // deferred module) so the global is available when feedback.js (a deferred
        // module) reads it. A shell that loads feedback.js first would leave
        // __btConfig undefined at rate-limit check time.
        let webr = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let pyodide = plan(&python_course(), BuildTarget::Pyodide).expect("plans");
        for (target, site) in [(BuildTarget::Webr, &webr), (BuildTarget::Pyodide, &pyodide)] {
            let html = &file(&site, "index.html").contents;
            let config_pos = html
                .find(r#"src="config.js""#)
                .unwrap_or_else(|| panic!("{target}: index.html must reference config.js"));
            let feedback_pos = html
                .find(r#"src="feedback.js""#)
                .unwrap_or_else(|| panic!("{target}: index.html must reference feedback.js"));
            assert!(
                config_pos < feedback_pos,
                "{target}: config.js must load before feedback.js (so __btConfig is set \
                 before the rate limiter reads it)"
            );
        }
    }

    // --- AC-4: client-side rate limiting — feedback.js source scan (6,7) -------

    #[test]
    fn feedback_rate_limit_feedback_js_has_rate_limiting() {
        // Predicate 6: feedback.js contains the rate-limiting patterns. No JS
        // harness exists, so this is a static source scan of the emitted feedback.js
        // (verification: code). 6 sub-clauses pin the invariant (§1.5):
        //   1. "bt_feedback_count" sessionStorage key
        //   2. window.__btConfig.maxFeedbackPerSession read
        //   3. limit-reached message string
        //   4. counter increment AFTER try/catch (failed requests count)
        //   5. textContent for limit message (NOT innerHTML — XSS defense)
        //   6. parseInt guard on stored counter
        let webr = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let feedback = &file(&webr, "feedback.js").contents;

        // Clause 1: the bt_feedback_count sessionStorage key.
        assert!(
            feedback.contains("bt_feedback_count"),
            "feedback.js must use the bt_feedback_count sessionStorage key; feedback={feedback}"
        );

        // Clause 2: window.__btConfig.maxFeedbackPerSession read.
        assert!(
            feedback.contains("__btConfig"),
            "feedback.js must read window.__btConfig; feedback={feedback}"
        );
        assert!(
            feedback.contains("maxFeedbackPerSession"),
            "feedback.js must read maxFeedbackPerSession from __btConfig; feedback={feedback}"
        );

        // Clause 3: a limit-reached message string (the user-facing copy).
        assert!(
            feedback.to_lowercase().contains("limit"),
            "feedback.js must contain a limit-reached message; feedback={feedback}"
        );

        // Clause 5: textContent for the limit message (NOT innerHTML). The
        // existing !innerHTML invariant (plan_site_shells_load_codemirror_as_import
        // clause 6) already guarantees no innerHTML anywhere in lesson-runner-core.js;
        // here we pin that feedback.js uses textContent for the limit message.
        assert!(
            feedback.contains("textContent"),
            "feedback.js must use textContent for the limit message; feedback={feedback}"
        );

        // Clause 6: parseInt guard on the stored counter. Without parseInt, a
        // corrupt or missing sessionStorage value yields a string comparison
        // (or NaN), silently disabling limiting.
        assert!(
            feedback.contains("parseInt"),
            "feedback.js must use parseInt to guard the stored counter; feedback={feedback}"
        );

        // Clause 4: counter increment AFTER try/catch (failed requests count).
        // The increment call must NOT be inside the try block (which would skip
        // it on error) and must appear after the catch block's renderError call
        // (the last statement in the catch). We scope the search to handleSubmit
        // to avoid matching the try/catch in listModels.
        let handle_submit_pos = feedback
            .find("async function handleSubmit")
            .expect("feedback.js must define handleSubmit");
        let handle_submit_body = &feedback[handle_submit_pos..];

        let try_pos = handle_submit_body
            .find("try {")
            .expect("handleSubmit must have a try block");
        let catch_pos = handle_submit_body
            .find("} catch (error) {")
            .expect("handleSubmit must have a catch block");
        let increment_pos = handle_submit_body
            .find("incrementFeedbackCount()")
            .expect("feedback.js must call incrementFeedbackCount in handleSubmit");
        let render_error_pos = handle_submit_body
            .find("renderError(container, error)")
            .expect("handleSubmit must call renderError in the catch block");

        // The increment must NOT be inside the try block (between try { and } catch).
        assert!(
            !(increment_pos > try_pos && increment_pos < catch_pos),
            "incrementFeedbackCount must NOT be inside the try block \
             (failed requests count — the negative case: increment only inside try \
             skips failures); feedback={feedback}"
        );
        // The increment must be after renderError (the last statement in the catch),
        // so it is reached on both success and failure paths.
        assert!(
            increment_pos > render_error_pos,
            "incrementFeedbackCount must be called AFTER the try/catch block \
             (after renderError in the catch — both paths reach it); feedback={feedback}"
        );
    }

    #[test]
    fn feedback_rate_limit_feedback_js_does_not_increment_around_list_models() {
        // Predicate 7: feedback.js does NOT increment the counter around
        // listModels (model discovery is not feedback — it doesn't count).
        // We verify the increment call does not appear in the listModels or
        // renderModelPicker function bodies.
        let webr = plan(&r_course(), BuildTarget::Webr).expect("plans");
        let feedback = &file(&webr, "feedback.js").contents;

        // Check listModels: find its body (from the function definition to the
        // next function definition) and assert no increment call within.
        let listmodels_start = feedback
            .find("async function listModels")
            .expect("feedback.js must define listModels");
        let after_listmodels = &feedback[listmodels_start..];
        let listmodels_end = after_listmodels[1..]
            .find("\nfunction ")
            .or_else(|| after_listmodels[1..].find("\nasync function "))
            .map(|p| p + 1)
            .unwrap_or(after_listmodels.len());
        let listmodels_body = &after_listmodels[..listmodels_end];
        assert!(
            !listmodels_body.contains("incrementFeedbackCount()"),
            "listModels must NOT call incrementFeedbackCount \
             (model discovery doesn't count); feedback={feedback}"
        );

        // Check renderModelPicker: the function that calls listModels. The
        // increment must not appear here either — the guard and increment live
        // in handleSubmit, after the model picker phase.
        let picker_start = feedback
            .find("async function renderModelPicker")
            .expect("feedback.js must define renderModelPicker");
        let after_picker = &feedback[picker_start..];
        let picker_end = after_picker[1..]
            .find("\nfunction ")
            .or_else(|| after_picker[1..].find("\nasync function "))
            .map(|p| p + 1)
            .unwrap_or(after_picker.len());
        let picker_body = &after_picker[..picker_end];
        assert!(
            !picker_body.contains("incrementFeedbackCount()"),
            "renderModelPicker must NOT call incrementFeedbackCount \
             (model discovery doesn't count); feedback={feedback}"
        );
    }
}
