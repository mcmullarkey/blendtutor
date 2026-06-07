//! Course scaffolding: the templates a new course starts from and the plan that
//! writes them.
//!
//! Holds the embedded starter templates (`include_str!`), the pure
//! [`scaffold_plan`] that names the files a fresh course contains (testable with
//! no filesystem, §2.3), and [`scaffold_course`] — the single effectful step
//! that refuses a non-empty target before any write (§1.3.1) then writes that
//! plan into it. This module owns *what a new course contains*; it does not parse
//! CLI flags or decide where the course lives (§4.1). The templates are data the
//! writer consumes, so changing a template never changes the writer (§3.2).

use std::error::Error;
use std::fmt;
use std::path::{Path, PathBuf};

/// One file a scaffold writes: its path relative to the course directory and the
/// exact bytes to write there.
///
/// A plain data record (§3.2): the [plan](scaffold_plan) is pure data the
/// effectful writer consumes, so a template edit is invisible to the writer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileSpec {
    /// Where the file goes, relative to the course directory.
    pub path: PathBuf,
    /// The file's contents, verbatim.
    pub contents: &'static str,
}

/// The course manifest every scaffold writes.
const MANIFEST_TEMPLATE: &str = include_str!("scaffold/blendtutor.toml");
/// The example lesson every scaffold writes.
const LESSON_TEMPLATE: &str = include_str!("scaffold/lesson_hello.yaml");
/// The example eval suite paired with the lesson.
const EVAL_TEMPLATE: &str = include_str!("scaffold/eval_lesson_hello.yaml");
/// The course README.
const README_TEMPLATE: &str = include_str!("scaffold/README.md");
/// The key-ignoring `.gitignore`. Stored dotless in-tree (`scaffold/gitignore`)
/// so it does not act as a real ignore file over its sibling templates; it is
/// written to `.gitignore` in the [plan](scaffold_plan).
const GITIGNORE_TEMPLATE: &str = include_str!("scaffold/gitignore");

/// The manifest filename a scaffolded course writes and `list` later reads.
const MANIFEST_FILENAME: &str = "blendtutor.toml";
/// The example lesson's filename; the manifest's one entry points here.
const LESSON_FILENAME: &str = "lesson_hello.yaml";
/// The example eval suite's filename, the `eval_<lesson>` sibling of the lesson.
const EVAL_FILENAME: &str = "eval_lesson_hello.yaml";

/// Compute the set of files a fresh course scaffold writes.
///
/// Pure (§2.1, §2.2): it returns the plan as data with no filesystem access, so
/// the file set — and that each template is internally valid — is asserted
/// directly in a unit test. [`scaffold_course`] is the effectful step that
/// writes it (§5.1, plan vs write split).
pub fn scaffold_plan() -> Vec<FileSpec> {
    vec![
        FileSpec {
            path: PathBuf::from(MANIFEST_FILENAME),
            contents: MANIFEST_TEMPLATE,
        },
        FileSpec {
            path: PathBuf::from(LESSON_FILENAME),
            contents: LESSON_TEMPLATE,
        },
        FileSpec {
            path: PathBuf::from(EVAL_FILENAME),
            contents: EVAL_TEMPLATE,
        },
        FileSpec {
            path: PathBuf::from("README.md"),
            contents: README_TEMPLATE,
        },
        FileSpec {
            path: PathBuf::from(".gitignore"),
            contents: GITIGNORE_TEMPLATE,
        },
    ]
}

/// Why a course could not be scaffolded into a target directory.
#[derive(Debug)]
pub enum ScaffoldError {
    /// The target directory already holds files, so the scaffold is refused
    /// before any write (§1.3.1) rather than overwriting an existing course.
    TargetNotEmpty(PathBuf),
    /// A filesystem operation failed while writing the scaffold (creating the
    /// directory or writing a file).
    Write(std::io::Error),
}

impl fmt::Display for ScaffoldError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScaffoldError::TargetNotEmpty(path) => write!(
                f,
                "target directory {path:?} is not empty; init refuses to overwrite \
                 an existing course — choose an empty or new directory",
            ),
            ScaffoldError::Write(e) => write!(f, "could not write course scaffold: {e}"),
        }
    }
}

impl Error for ScaffoldError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ScaffoldError::TargetNotEmpty(_) => None,
            ScaffoldError::Write(e) => Some(e),
        }
    }
}

/// Scaffold a fresh course into `dir`, writing every file in the
/// [plan](scaffold_plan).
///
/// The effectful shell (§2.2) over the pure plan. The refuse-on-nonempty guard
/// fires first (§1.3.1): a directory that already holds files is rejected as
/// [`ScaffoldError::TargetNotEmpty`] *before any write*, so a refused target is
/// left exactly as it was — never partially clobbered. An absent or empty target
/// is then created if missing and each [`FileSpec`] written verbatim.
pub fn scaffold_course(dir: &Path) -> Result<(), ScaffoldError> {
    if !is_empty_target(dir)? {
        return Err(ScaffoldError::TargetNotEmpty(dir.to_path_buf()));
    }
    std::fs::create_dir_all(dir).map_err(ScaffoldError::Write)?;
    for spec in scaffold_plan() {
        std::fs::write(dir.join(&spec.path), spec.contents).map_err(ScaffoldError::Write)?;
    }
    Ok(())
}

/// Whether `dir` is a safe scaffold target: it does not yet exist, or exists and
/// holds no entries.
///
/// The boundary check behind the guard (§1.3.1). A `NotFound` directory is an
/// empty target (it will be created); a directory with any entry is not. A read
/// failure other than `NotFound` (e.g. a permission error) propagates as a write
/// error rather than being read as "empty", so the guard never green-lights a
/// target it could not actually inspect.
fn is_empty_target(dir: &Path) -> Result<bool, ScaffoldError> {
    match std::fs::read_dir(dir) {
        Ok(mut entries) => Ok(entries.next().is_none()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(true),
        Err(e) => Err(ScaffoldError::Write(e)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::course::Manifest;
    use crate::eval::parse_eval_suite;
    use crate::lesson::Lesson;

    /// The planned file at `path`, or a panic naming the missing path.
    fn planned(path: &str) -> FileSpec {
        scaffold_plan()
            .into_iter()
            .find(|spec| spec.path == Path::new(path))
            .unwrap_or_else(|| panic!("the plan should include {path:?}"))
    }

    #[test]
    fn plan_lists_the_starter_files_as_pure_data() {
        // Pure: asserted as data with no directory to write into (§2.3). The set
        // is exactly what `list` needs (manifest + lesson) plus the authoring
        // extras (eval, README, gitignore) — pinned so a dropped file is caught
        // here, not only end to end.
        let mut paths: Vec<String> = scaffold_plan()
            .iter()
            .map(|spec| spec.path.to_string_lossy().into_owned())
            .collect();
        paths.sort();
        assert_eq!(
            paths,
            vec![
                ".gitignore",
                "README.md",
                "blendtutor.toml",
                "eval_lesson_hello.yaml",
                "lesson_hello.yaml",
            ]
        );
    }

    #[test]
    fn the_scaffolded_lesson_eval_and_manifest_parse_with_production_parsers() {
        // The templates are validated by the very parsers production uses, so the
        // scaffolded course is internally consistent: `list` resolves the manifest
        // entry to a real, parseable lesson rather than a dangling path. This is
        // what makes the AC1 "list discovers >=1 lesson" guarantee hold.
        Lesson::parse(planned(LESSON_FILENAME).contents)
            .expect("the example lesson must satisfy the lesson schema");
        parse_eval_suite(planned(EVAL_FILENAME).contents)
            .expect("the example eval suite must satisfy the eval schema");
        let manifest = Manifest::parse(planned(MANIFEST_FILENAME).contents)
            .expect("the example manifest must parse");

        assert_eq!(manifest.lessons.len(), 1, "one example lesson is listed");
        assert_eq!(
            manifest.lessons[0].path,
            Path::new(LESSON_FILENAME),
            "the manifest entry points at the scaffolded lesson file"
        );
    }

    #[test]
    fn scaffold_course_writes_every_planned_file_verbatim() {
        let dir = tempfile::tempdir().unwrap();
        scaffold_course(dir.path()).expect("scaffolding an empty dir succeeds");

        for spec in scaffold_plan() {
            let written = dir.path().join(&spec.path);
            assert_eq!(
                std::fs::read_to_string(&written)
                    .unwrap_or_else(|e| panic!("{:?} should have been written: {e}", spec.path)),
                spec.contents,
                "{:?} should be written verbatim from its template",
                spec.path
            );
        }
    }

    #[test]
    fn scaffold_course_creates_the_target_directory_when_absent() {
        let parent = tempfile::tempdir().unwrap();
        let course = parent.path().join("new_course");
        scaffold_course(&course).expect("a not-yet-existing target is created and written");
        assert!(
            course.join(MANIFEST_FILENAME).is_file(),
            "the manifest lands inside the freshly-created course directory"
        );
    }

    #[test]
    fn scaffold_course_refuses_a_nonempty_target_before_writing_anything() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("sentinel.txt"), "KEEP").unwrap();

        let err = scaffold_course(dir.path())
            .expect_err("a directory that already holds files must be refused");
        assert!(
            matches!(err, ScaffoldError::TargetNotEmpty(_)),
            "a non-empty target is a TargetNotEmpty refusal, got {err:?}"
        );

        // The guard fires before any write: only the seeded file remains, and it
        // is untouched. No plan file leaked into the directory.
        let mut names: Vec<String> = std::fs::read_dir(dir.path())
            .unwrap()
            .map(|entry| entry.unwrap().file_name().to_string_lossy().into_owned())
            .collect();
        names.sort();
        assert_eq!(names, vec!["sentinel.txt".to_string()]);
        assert_eq!(
            std::fs::read_to_string(dir.path().join("sentinel.txt")).unwrap(),
            "KEEP"
        );
    }

    #[test]
    fn scaffold_course_accepts_an_existing_empty_directory() {
        // The boundary case the AC2 probe drives: `mktemp -d` yields a directory
        // that exists and is empty. That is a valid target, distinct from a
        // non-empty one.
        let dir = tempfile::tempdir().unwrap();
        scaffold_course(dir.path()).expect("an existing but empty directory is a valid target");
        assert!(dir.path().join(MANIFEST_FILENAME).is_file());
    }

    #[test]
    fn is_empty_target_surfaces_a_non_notfound_error_rather_than_reading_it_as_empty() {
        // A path that is an existing *file* cannot be read as a directory:
        // `read_dir` fails with a non-NotFound error. The guard must propagate
        // that, never collapse it to "empty" — otherwise an uninspectable target
        // would be green-lit. Distinguishes the NotFound arm (empty) from every
        // other read failure (an error).
        let file = tempfile::NamedTempFile::new().unwrap();
        let result = is_empty_target(file.path());
        assert!(
            matches!(result, Err(ScaffoldError::Write(_))),
            "a non-directory target is a Write error, not an empty target, got {result:?}"
        );
    }

    #[test]
    fn scaffold_error_display_and_source_distinguish_each_variant() {
        use std::io::{Error as IoError, ErrorKind};

        // TargetNotEmpty names the refusal, names the path, and has no nested
        // source.
        let refused = ScaffoldError::TargetNotEmpty(PathBuf::from("/some/course"));
        let refused_msg = refused.to_string();
        assert!(
            refused_msg.contains("not empty") && refused_msg.contains("/some/course"),
            "TargetNotEmpty should explain the refusal and name the path, got: {refused_msg}"
        );
        assert!(std::error::Error::source(&refused).is_none());

        // Write labels itself a write failure and exposes the io::Error as its
        // source, preserving the error chain.
        let write = ScaffoldError::Write(IoError::new(ErrorKind::PermissionDenied, "denied"));
        let write_msg = write.to_string();
        assert!(
            write_msg.contains("could not write course scaffold") && write_msg.contains("denied"),
            "Write should frame and carry the message, got: {write_msg}"
        );
        assert!(std::error::Error::source(&write).is_some());
    }
}
