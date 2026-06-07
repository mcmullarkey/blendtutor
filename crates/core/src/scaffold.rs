//! Course scaffolding: the templates a new course starts from and the plan that
//! writes them.
//!
//! Holds the embedded starter templates (`include_str!`), the pure
//! [`scaffold_plan`] that names the files a fresh course contains (testable with
//! no filesystem, §2.3), and [`scaffold_course`] — the single effectful step
//! that writes that plan into a target directory. This module owns *what a new
//! course contains*; it does not parse CLI flags or decide where the course
//! lives (§4.1). The templates are data the writer consumes, so changing a
//! template never changes the writer (§3.2).

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
    /// A filesystem operation failed while writing the scaffold (creating the
    /// directory or writing a file).
    Write(std::io::Error),
}

impl fmt::Display for ScaffoldError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScaffoldError::Write(e) => write!(f, "could not write course scaffold: {e}"),
        }
    }
}

impl Error for ScaffoldError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ScaffoldError::Write(e) => Some(e),
        }
    }
}

/// Scaffold a fresh course into `dir`, writing every file in the
/// [plan](scaffold_plan).
///
/// The effectful shell (§2.2) over the pure plan: it creates `dir` if missing,
/// then writes each [`FileSpec`] verbatim. A missing target is created; an
/// existing one is written into.
pub fn scaffold_course(dir: &Path) -> Result<(), ScaffoldError> {
    std::fs::create_dir_all(dir).map_err(ScaffoldError::Write)?;
    for spec in scaffold_plan() {
        std::fs::write(dir.join(&spec.path), spec.contents).map_err(ScaffoldError::Write)?;
    }
    Ok(())
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
}
