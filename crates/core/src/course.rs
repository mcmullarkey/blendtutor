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

/// A course manifest: the ordered list of lessons a course contains, parsed from
/// `blendtutor.toml`.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Manifest {
    /// The lessons this course lists, in author order.
    pub lessons: Vec<ManifestEntry>,
}

impl Manifest {
    /// Parse a manifest from a `blendtutor.toml` document.
    ///
    /// The pure parse boundary (§2.1): the caller reads the file, this turns the
    /// text into the typed model. A malformed document or a typo'd key yields
    /// [`ManifestError::Parse`] carrying the parser message.
    pub fn parse(toml: &str) -> Result<Manifest, ManifestError> {
        toml::from_str(toml).map_err(|e| ManifestError::Parse(e.to_string()))
    }
}

/// Why a `blendtutor.toml` could not be turned into a [`Manifest`].
///
/// Read failures stay distinct from parse failures (mirroring
/// [`LoadError`](crate::lesson::LoadError)) so a missing manifest is never
/// reported as a malformed one.
#[derive(Debug)]
pub enum ManifestError {
    /// The manifest file could not be read (missing, permissions).
    Read(std::io::Error),
    /// The file was read but is not a valid manifest (bad TOML, a missing or
    /// typo'd key). Carries the parser message.
    Parse(String),
}

impl fmt::Display for ManifestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ManifestError::Read(e) => write!(f, "could not read course manifest: {e}"),
            ManifestError::Parse(msg) => write!(f, "invalid course manifest: {msg}"),
        }
    }
}

impl Error for ManifestError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ManifestError::Read(e) => Some(e),
            ManifestError::Parse(_) => None,
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
    /// The per-lesson read is effectful; the [`summarize`] it feeds is pure
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
    fn open_missing_manifest_is_a_read_error_not_a_parse_error() {
        let err = Course::open(Path::new("/no/such/course"))
            .expect_err("a directory without a manifest cannot open");
        assert!(
            matches!(err, ManifestError::Read(_)),
            "a missing manifest is a read error, got: {err:?}"
        );
    }
}
