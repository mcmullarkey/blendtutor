//! Course discovery: a course directory's manifest and the lessons it lists.
//!
//! Holds the [`Manifest`] (the parsed `blendtutor.toml`), the [`Course`] that
//! owns a course directory, and [`Course::discover`] — the effectful walk that
//! reads each listed lesson and summarizes it. A discovered entry is a
//! `Result<LessonSummary, DiscoveryError>` so a malformed lesson is reported as
//! an error row, never aborting the scan nor silently dropped (ADR-0004). This
//! module does not validate lesson semantics beyond what [`crate::lesson`]
//! already does (§4.1); it adds only the course-scoped slug and the
//! partial-failure shape.

use std::error::Error;
use std::fmt;
use std::path::{Path, PathBuf};

use serde::Deserialize;

use crate::lesson::{Language, Lesson, LoadError, read_lesson_file};

/// The manifest file at the root of every course directory.
const MANIFEST_FILENAME: &str = "blendtutor.toml";

/// A lesson's course-scoped identity: the `id` an author gives an entry in
/// `blendtutor.toml`.
///
/// A newtype over `String` (§1.4) so a course slug is never confused with the
/// lesson's own `lesson_name` title or with arbitrary text. The slug is assigned
/// by the course, not the lesson (ADR-0004): a lesson file does not know which
/// course lists it.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct LessonSlug(String);

impl fmt::Display for LessonSlug {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

/// One lesson's entry in a [`Manifest`]: its course slug and the file that holds
/// it, relative to the manifest.
///
/// Unknown keys are rejected (§1.3.1) so an author's typo in `blendtutor.toml`
/// fails loudly rather than dropping silently, matching the lesson model.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ManifestEntry {
    /// The lesson's course-scoped id.
    pub id: LessonSlug,
    /// The lesson file, relative to the course directory.
    pub path: PathBuf,
}

/// The default maximum feedback requests per browser session when the `[site]`
/// section is absent or omits `max_feedback_per_session`. Lifted to a named
/// function so both the serde default and [`SiteConfig::default`] share one
/// source (§5.1) — a future tuning change touches one place.
const fn default_max_feedback() -> u32 {
    20
}

/// Site-level configuration from the optional `[site]` section of
/// `blendtutor.toml`.
///
/// Unknown keys are rejected (§1.3.1) so an author's typo in `[site]` fails
/// loudly rather than dropping silently, matching the lesson and manifest
/// models. The `max_feedback_per_session` field defaults to 20 when the `[site]`
/// section is present but omits it (via `#[serde(default = "default_max_feedback")]`);
/// when the entire `[site]` section is absent, [`Manifest::site`] is `None` and
/// the caller applies [`SiteConfig::default`] (which also yields 20).
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SiteConfig {
    /// The maximum number of feedback requests a learner may make per browser
    /// session. Defaults to 20. A value of 0 disables feedback entirely.
    #[serde(default = "default_max_feedback")]
    pub max_feedback_per_session: u32,
}

impl Default for SiteConfig {
    fn default() -> Self {
        SiteConfig {
            max_feedback_per_session: default_max_feedback(),
        }
    }
}

/// A course manifest: the ordered list of lessons a course contains, parsed from
/// `blendtutor.toml`.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Manifest {
    /// The lessons this course lists, in author order.
    pub lessons: Vec<ManifestEntry>,
    /// Site-level configuration from the optional `[site]` section. `None` when
    /// the section is absent; the caller applies [`SiteConfig::default`] (max=20).
    #[serde(default)]
    pub site: Option<SiteConfig>,
}

impl Manifest {
    /// Parse a manifest from a `blendtutor.toml` document.
    ///
    /// The pure parse boundary (§2.1): the caller reads the file, this turns the
    /// text into the typed model. A malformed document or a typo'd key yields
    /// [`ManifestError::Parse`]; a lesson path that escapes the course directory
    /// yields [`ManifestError::UnsafePath`]. Both are caught here, before any
    /// lesson file is read (§1.3.1), so a parsed `Manifest` carries only relative,
    /// non-`..` paths. The check is lexical — it does not resolve symlinks, so a
    /// symlink inside the course could still point elsewhere.
    pub fn parse(toml: &str) -> Result<Manifest, ManifestError> {
        let manifest: Manifest =
            toml::from_str(toml).map_err(|e| ManifestError::Parse(e.to_string()))?;
        manifest.validate_paths()?;
        Ok(manifest)
    }

    /// Reject any entry whose path would reach outside the course directory.
    ///
    /// A manifest is author-written but a course may be shared, so an absolute
    /// path or a `..` component is refused at the boundary rather than handed to
    /// [`Course::discover`] as a read of an arbitrary file (§1.3.1).
    fn validate_paths(&self) -> Result<(), ManifestError> {
        for entry in &self.lessons {
            if escapes_course_dir(&entry.path) {
                return Err(ManifestError::UnsafePath {
                    id: entry.id.clone(),
                    path: entry.path.clone(),
                });
            }
        }
        Ok(())
    }
}

/// Whether `path` is lexically outside the course directory: an absolute path,
/// or one carrying a `..` component. A lexical check only — it does not resolve
/// symlinks, so a symlink within the course is not followed.
fn escapes_course_dir(path: &Path) -> bool {
    path.is_absolute()
        || path
            .components()
            .any(|component| matches!(component, std::path::Component::ParentDir))
}

/// Why a `blendtutor.toml` could not be turned into a [`Manifest`].
///
/// Read failures stay distinct from parse failures (mirroring
/// [`LoadError`]) so a missing manifest is never
/// reported as a malformed one.
#[derive(Debug)]
pub enum ManifestError {
    /// The manifest file could not be read (missing, permissions).
    Read(std::io::Error),
    /// The file was read but is not a valid manifest (bad TOML, a missing or
    /// typo'd key). Carries the parser message.
    Parse(String),
    /// An entry's `path` would resolve outside the course directory (it is
    /// absolute or climbs through `..`), so it is refused before any file is read.
    UnsafePath {
        /// The slug of the offending entry.
        id: LessonSlug,
        /// The rejected path, as written in the manifest.
        path: PathBuf,
    },
}

impl fmt::Display for ManifestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ManifestError::Read(e) => write!(f, "could not read course manifest: {e}"),
            ManifestError::Parse(msg) => write!(f, "invalid course manifest: {msg}"),
            ManifestError::UnsafePath { id, path } => write!(
                f,
                "lesson {id:?} path {path:?} escapes the course directory; \
                 lesson paths must be relative and stay within the course",
            ),
        }
    }
}

impl Error for ManifestError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ManifestError::Read(e) => Some(e),
            ManifestError::Parse(_) | ManifestError::UnsafePath { .. } => None,
        }
    }
}

/// A course directory: its root and parsed manifest.
///
/// Construct one through [`Course::open`]; a value of this type has, by
/// construction, a manifest that parsed.
#[derive(Debug, Clone)]
pub struct Course {
    root: PathBuf,
    manifest: Manifest,
}

impl Course {
    /// Open the course rooted at `dir` by reading and parsing its
    /// `blendtutor.toml`.
    ///
    /// The effectful shell (§2.2) over the pure [`Manifest::parse`]. A missing or
    /// malformed manifest is a whole-course failure (ADR-0004) — distinct from a
    /// single bad lesson, which [`discover`](Course::discover) reports as a row.
    pub fn open(dir: &Path) -> Result<Course, ManifestError> {
        let text =
            std::fs::read_to_string(dir.join(MANIFEST_FILENAME)).map_err(ManifestError::Read)?;
        let manifest = Manifest::parse(&text)?;
        Ok(Course {
            root: dir.to_path_buf(),
            manifest,
        })
    }

    /// Discover every lesson the manifest lists, one row per entry.
    ///
    /// Each entry yields `Ok(LessonSummary)` if its lesson loads and validates,
    /// or `Err(DiscoveryError)` carrying the entry's slug if it does not — so a
    /// malformed lesson neither aborts the scan nor disappears (ADR-0004, §1.2).
    /// The per-lesson read is effectful; the `summarize` it feeds is pure
    /// (§2.1, §2.2), depending on [`crate::lesson`] in one direction (§3.1).
    pub fn discover(&self) -> Vec<Result<LessonSummary, DiscoveryError>> {
        self.manifest
            .lessons
            .iter()
            .map(|entry| {
                read_lesson_file(&self.root.join(&entry.path))
                    .map(|lesson| summarize(entry.id.clone(), &lesson))
                    .map_err(|source| DiscoveryError {
                        id: entry.id.clone(),
                        source,
                    })
            })
            .collect()
    }

    /// Load every lesson the manifest lists, in full and in author order.
    ///
    /// The effectful loader the static-site build needs (ADR-0008). Unlike
    /// [`discover`](Course::discover) — which tolerates a partial failure by
    /// returning one row per entry — this short-circuits on the first lesson that
    /// fails to load: a site cannot be built from a course with a broken lesson,
    /// so the whole build fails rather than emitting a site that silently drops a
    /// page. Each entry pairs the manifest slug with its parsed [`Lesson`].
    pub fn load_lessons(&self) -> Result<Vec<(LessonSlug, Lesson)>, DiscoveryError> {
        self.manifest
            .lessons
            .iter()
            .map(|entry| {
                read_lesson_file(&self.root.join(&entry.path))
                    .map(|lesson| (entry.id.clone(), lesson))
                    .map_err(|source| DiscoveryError {
                        id: entry.id.clone(),
                        source,
                    })
            })
            .collect()
    }

    /// The site-level configuration from the manifest's `[site]` section, if
    /// present. Returns `None` when the course has no `[site]` section; the
    /// caller applies [`SiteConfig::default`] (max=20) in that case.
    pub fn site_config(&self) -> Option<&SiteConfig> {
        self.manifest.site.as_ref()
    }
}

/// One discovered lesson, as listed: its course slug, language, and title.
///
/// The title is the lesson's own `lesson_name`; the slug is the manifest id. They
/// are distinct fields so the listing can show both, and a missing title can
/// never quietly default to the id (ADR-0004).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LessonSummary {
    /// The lesson's course-scoped id, from the manifest.
    pub id: LessonSlug,
    /// The language the lesson is authored in.
    pub language: Language,
    /// The lesson's human-readable title (its `lesson_name`).
    pub title: String,
}

/// Summarize a loaded lesson under its course slug.
///
/// Pure (§2.2): it derives the list row from the already-validated lesson and the
/// manifest-assigned id. It reads `lesson` and never the other way round (§3.1).
fn summarize(id: LessonSlug, lesson: &Lesson) -> LessonSummary {
    LessonSummary {
        id,
        language: lesson.language.clone(),
        title: lesson.lesson_name.to_string(),
    }
}

/// Why one manifest entry could not be discovered: its slug and the underlying
/// load failure.
///
/// Carrying the slug lets the listing report a broken lesson as a row that still
/// names which lesson broke; wrapping the [`LoadError`] preserves the
/// read-vs-validation distinction rather than flattening it to a bare string.
#[derive(Debug)]
pub struct DiscoveryError {
    id: LessonSlug,
    source: LoadError,
}

impl DiscoveryError {
    /// The course slug of the entry that failed to load.
    pub fn id(&self) -> &LessonSlug {
        &self.id
    }
}

impl fmt::Display for DiscoveryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // The slug is rendered by the listing alongside the message; here we
        // surface just the cause so callers compose their own framing.
        self.source.fmt(f)
    }
}

impl Error for DiscoveryError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(&self.source)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The course fixture with a manifest and two valid lessons, one R and one
    /// Python. Lives under this crate's fixtures (the schema's home).
    fn course_basic() -> &'static Path {
        Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/course_basic"
        ))
    }

    #[test]
    fn manifest_parse_reads_each_entrys_id_and_path() {
        let manifest = Manifest::parse(
            r#"
[[lessons]]
id = "add-two"
path = "add_two.yaml"

[[lessons]]
id = "greet"
path = "greet.yaml"
"#,
        )
        .expect("a well-formed manifest should parse");

        assert_eq!(manifest.lessons.len(), 2);
        assert_eq!(manifest.lessons[0].id, LessonSlug("add-two".to_string()));
        assert_eq!(manifest.lessons[0].path, PathBuf::from("add_two.yaml"));
        assert_eq!(manifest.lessons[1].id, LessonSlug("greet".to_string()));
    }

    #[test]
    fn manifest_parse_rejects_unknown_key_so_author_typos_surface() {
        let err = Manifest::parse(
            r#"
[[lessons]]
id = "add-two"
pathh = "add_two.yaml"
"#,
        )
        .expect_err("a typo'd key must not be silently dropped");
        assert!(
            matches!(err, ManifestError::Parse(_)),
            "a malformed manifest is a parse error, got: {err:?}"
        );
    }

    #[test]
    fn manifest_parse_rejects_a_path_escaping_the_course_directory() {
        for escaping in ["../secrets.yaml", "/etc/passwd", "nested/../../up.yaml"] {
            let err = Manifest::parse(&format!("[[lessons]]\nid = \"x\"\npath = {escaping:?}\n"))
                .expect_err("a path leaving the course directory must be refused");
            assert!(
                matches!(err, ManifestError::UnsafePath { .. }),
                "an escaping path should be UnsafePath, got: {err:?} for {escaping}"
            );
        }
    }

    #[test]
    fn discover_summarizes_each_lesson_with_its_slug_language_and_title() {
        let course = Course::open(course_basic()).expect("course_basic should open");
        let rows = course.discover();

        assert_eq!(rows.len(), 2, "course_basic lists two lessons");

        let summary = |slug: &str| -> LessonSummary {
            rows.iter()
                .filter_map(|r| r.as_ref().ok())
                .find(|s| s.id == LessonSlug(slug.to_string()))
                .unwrap_or_else(|| panic!("an Ok row with slug {slug:?} should be discovered"))
                .clone()
        };

        // The slug comes from the manifest; language and title come from the
        // parsed lesson — distinct sources, so a "title == id" collapse is caught.
        let add_two = summary("add-two");
        assert_eq!(add_two.language, Language::R);
        assert_eq!(add_two.title, "Add Two Numbers");

        let greet = summary("greet");
        assert_eq!(greet.language, Language::Python);
        assert_eq!(greet.title, "Greet Someone");
    }

    #[test]
    fn discover_reports_a_malformed_lesson_as_an_error_row_keeping_the_good_ones() {
        let course = Course::open(Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/course_partial"
        )))
        .expect("course_partial should open");
        let rows = course.discover();

        // Three manifest entries -> three rows. Partial failure is represented as
        // a Vec<Result>: it is neither flattened to Result<Vec> (which would abort
        // the whole scan on the first bad lesson) nor filtered down to the good
        // ones (the R-style silent swallow).
        assert_eq!(rows.len(), 3, "one row per manifest entry");
        assert_eq!(
            rows.iter().filter(|row| row.is_ok()).count(),
            2,
            "the two valid lessons still discover"
        );

        let errors: Vec<&DiscoveryError> =
            rows.iter().filter_map(|row| row.as_ref().err()).collect();
        assert_eq!(errors.len(), 1, "exactly the one malformed lesson errors");
        assert_eq!(
            errors[0].id().to_string(),
            "broken",
            "the error row carries the manifest slug, not the unparseable lesson's name"
        );
    }

    #[test]
    fn open_missing_manifest_is_a_read_error_not_a_parse_error() {
        let err = Course::open(Path::new("/no/such/course"))
            .expect_err("a directory without a manifest cannot open");
        assert!(
            matches!(err, ManifestError::Read(_)),
            "a missing manifest is a read error, got: {err:?}"
        );
    }

    #[test]
    fn manifest_error_display_and_source_distinguish_each_variant() {
        use std::io::{Error as IoError, ErrorKind};

        // Read names itself a read failure and exposes the io::Error as its source.
        let read = ManifestError::Read(IoError::new(ErrorKind::NotFound, "nope"));
        assert!(
            read.to_string().contains("could not read course manifest"),
            "Read should label itself, got: {read}"
        );
        assert!(
            std::error::Error::source(&read).is_some(),
            "Read should expose the io::Error as its source"
        );

        // Parse carries the parser message and has no nested source.
        let parse = ManifestError::Parse("bad toml".to_string());
        let parse_msg = parse.to_string();
        assert!(
            parse_msg.contains("invalid course manifest") && parse_msg.contains("bad toml"),
            "Parse should frame and carry the message, got: {parse_msg}"
        );
        assert!(std::error::Error::source(&parse).is_none());

        // UnsafePath says the path escapes and has no nested source.
        let unsafe_path = ManifestError::UnsafePath {
            id: LessonSlug("x".to_string()),
            path: PathBuf::from("../escape.yaml"),
        };
        assert!(
            unsafe_path.to_string().contains("escapes"),
            "UnsafePath should say the path escapes, got: {unsafe_path}"
        );
        assert!(std::error::Error::source(&unsafe_path).is_none());
    }

    #[test]
    fn discovery_error_display_and_source_surface_the_underlying_load_failure() {
        let course = Course::open(Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/course_partial"
        )))
        .expect("course_partial should open");
        let rows = course.discover();
        let err = rows
            .iter()
            .filter_map(|row| row.as_ref().err())
            .next()
            .expect("the malformed lesson produces an error");

        // Display surfaces the underlying load failure rather than an empty string.
        assert!(
            err.to_string().contains("language"),
            "Display should surface the missing-field cause, got: {err}"
        );
        // The LoadError is exposed as the source, preserving the error chain.
        assert!(
            std::error::Error::source(err).is_some(),
            "DiscoveryError should expose its LoadError as the source"
        );
    }

    #[test]
    fn load_lessons_returns_every_lesson_in_full_and_in_author_order() {
        let course = Course::open(course_basic()).expect("course_basic should open");
        let lessons = course
            .load_lessons()
            .expect("every lesson in course_basic loads");

        // Full lessons paired with their manifest slug, in author order — not the
        // summaries `discover` returns. The order assertion pins manifest order so
        // a later reader (the site build) emits pages deterministically.
        let pairs: Vec<(String, Language)> = lessons
            .iter()
            .map(|(slug, lesson)| (slug.to_string(), lesson.language.clone()))
            .collect();
        assert_eq!(
            pairs,
            vec![
                ("add-two".to_string(), Language::R),
                ("greet".to_string(), Language::Python),
            ],
        );
    }

    #[test]
    fn load_lessons_fails_on_the_first_broken_lesson_rather_than_dropping_it() {
        let course = Course::open(Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/course_partial"
        )))
        .expect("course_partial should open");

        // Unlike `discover`, a single broken lesson fails the whole load — a site
        // must never be built from an incomplete course.
        let err = course
            .load_lessons()
            .expect_err("a course with a malformed lesson cannot be loaded in full");
        assert_eq!(
            err.id().to_string(),
            "broken",
            "the failure names the manifest slug of the lesson that broke"
        );
    }

    // --- AC-4: client-side rate limiting — SiteConfig parse (predicates 1-3) ----

    #[test]
    fn feedback_rate_limit_manifest_parses_max_from_site_section() {
        // Predicate 1: Manifest::parse on [site] max_feedback_per_session = 5
        // yields manifest.site.max_feedback_per_session == 5.
        let manifest = Manifest::parse(
            r#"
[[lessons]]
id = "add-two"
path = "add_two.yaml"

[site]
max_feedback_per_session = 5
"#,
        )
        .expect("a manifest with [site] max=5 should parse");
        assert_eq!(
            manifest
                .site
                .as_ref()
                .expect("site config present when [site] is given")
                .max_feedback_per_session,
            5,
        );
    }

    #[test]
    fn feedback_rate_limit_manifest_defaults_to_20_without_site_section() {
        // Predicate 2: Manifest::parse without [site] yields default 20.
        // The absent [site] section makes manifest.site None; the caller applies
        // SiteConfig::default() which carries max=20 (#[serde(default)]).
        let manifest = Manifest::parse(
            r#"
[[lessons]]
id = "add-two"
path = "add_two.yaml"
"#,
        )
        .expect("a manifest without [site] should parse");
        assert!(
            manifest.site.is_none(),
            "absent [site] yields None; the caller applies SiteConfig::default()"
        );
        // The default the caller applies carries max=20.
        assert_eq!(SiteConfig::default().max_feedback_per_session, 20);
    }

    #[test]
    fn feedback_rate_limit_manifest_rejects_negative_max() {
        // Predicate 3: Manifest::parse on max_feedback_per_session = -1 is Err
        // (u32 rejects negatives at the parse boundary — §1.3.1).
        let err = Manifest::parse(
            r#"
[[lessons]]
id = "add-two"
path = "add_two.yaml"

[site]
max_feedback_per_session = -1
"#,
        )
        .expect_err("a negative max must be rejected (u32 rejects negatives)");
        assert!(
            matches!(err, ManifestError::Parse(_)),
            "a negative max is a parse error, got: {err:?}"
        );
    }

    // --- Course::site_config accessor — kills mutants on the method itself -----

    #[test]
    fn site_config_returns_the_manifests_site_section_when_present() {
        // The accessor must return the actual [site] config, not None and not a
        // default — a non-default max (5) pins both: None fails is_some, default
        // (max=20) fails the value check.
        let manifest = Manifest::parse(
            r#"
[[lessons]]
id = "add-two"
path = "add_two.yaml"

[site]
max_feedback_per_session = 5
"#,
        )
        .expect("a manifest with [site] max=5 should parse");
        let course = Course {
            root: PathBuf::from("."),
            manifest,
        };
        let config = course
            .site_config()
            .expect("site_config must return Some when [site] is present");
        assert_eq!(config.max_feedback_per_session, 5);
    }

    #[test]
    fn site_config_returns_none_when_no_site_section() {
        // Without [site], the accessor returns None — the caller applies
        // SiteConfig::default() (max=20). Returning a leaked default here would
        // break the None contract the build relies on.
        let manifest = Manifest::parse(
            r#"
[[lessons]]
id = "add-two"
path = "add_two.yaml"
"#,
        )
        .expect("a manifest without [site] should parse");
        let course = Course {
            root: PathBuf::from("."),
            manifest,
        };
        assert!(
            course.site_config().is_none(),
            "site_config must return None when [site] is absent"
        );
    }
}
