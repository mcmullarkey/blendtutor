//! Course scaffolding: the templates a new course starts from and the plan that
//! writes them.
//!
//! Holds the embedded starter templates (`include_str!`), the pure
//! [`scaffold_plan`] that names the files a fresh course contains (testable with
//! no filesystem, §2.3), and [`scaffold_course`] — the single effectful step
//! that refuses a non-empty target before any write (§1.3.1) then writes that
//! plan into it. It also grows an existing course one lesson at a time: the pure
//! [`lesson_template`] selects a language-appropriate starter (§2.1) and the
//! effectful [`add_lesson`] writes it and registers it in the manifest. This
//! module owns *what a course's content is*; it does not parse CLI flags or decide
//! where the course lives (§4.1). The templates are data the writer consumes, so
//! changing a template never changes the writer (§3.2).

use std::error::Error;
use std::fmt;
use std::path::{Path, PathBuf};

use crate::lesson::Language;

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
/// The boundary check behind the guard (§1.3.1). A directory with any entry is
/// not empty. A read failure other than `NotFound` (e.g. a permission error)
/// propagates as a write error rather than being read as "empty", so the guard
/// never green-lights a target it could not actually inspect.
///
/// `read_dir` reports `NotFound` for two different paths: one that truly does not
/// exist (a creatable, absent target) and a *broken symlink* (the link exists but
/// its target does not). An `lstat` (`symlink_metadata`, which does not follow the
/// link) tells them apart: if the path itself exists it is an existing object the
/// scaffold must refuse, not silently try to create over — which would otherwise
/// fail later with a cryptic "File exists".
fn is_empty_target(dir: &Path) -> Result<bool, ScaffoldError> {
    match std::fs::read_dir(dir) {
        Ok(mut entries) => Ok(entries.next().is_none()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            // `read_dir` reports NotFound both for a truly-absent path and for a
            // broken symlink (the link exists, its target does not). A
            // non-following lstat tells them apart. This lstat runs *only after*
            // read_dir already returned NotFound on the same path, so its only
            // reachable outcomes are NotFound (the path is genuinely absent — a
            // creatable, empty target) and Ok (the path exists as its own inode,
            // e.g. a broken symlink — an existing object to refuse). A
            // non-NotFound lstat error (e.g. a permission failure) cannot arise on
            // a path read_dir just reported NotFound — that would have surfaced as
            // the outer non-NotFound arm — so collapsing every lstat error to
            // "absent" green-lights no un-inspectable target.
            Ok(std::fs::symlink_metadata(dir).is_err())
        }
        Err(e) => Err(ScaffoldError::Write(e)),
    }
}

/// The subdirectory new lessons are written into, relative to the course root.
/// The `init` example lesson sits at the course root; lessons added later live
/// under here so a growing course keeps its lesson files in one place.
const LESSONS_DIR: &str = "lessons";

/// The exercise body of an R starter lesson: a valid `exercise` block whose
/// `llm_evaluation_prompt` carries the required `{student_code}` placeholder.
const R_EXERCISE: &str = r#"exercise:
  type: "function_writing"
  prompt: |
    Write R code that prints the word "hello" on its own line, using cat().
  code_template: |
    # Your code here
    cat("hello\n")
  example_usage: |
    cat("hello\n")  # prints: hello
  success_criteria: |
    - Prints exactly the word "hello"
    - Uses cat()
  llm_evaluation_prompt: |
    You are grading a beginner R exercise: print the word "hello" with cat().

    The student submitted this code:
    {student_code}

    Decide whether it prints "hello" and call respond_with_feedback with your
    assessment. Set is_correct true when the requirement is met, and give two or
    three sentences of encouraging feedback.
"#;

/// The exercise body of a Python starter lesson: the `print()` twin of
/// [`R_EXERCISE`], likewise carrying the `{student_code}` placeholder.
const PYTHON_EXERCISE: &str = r#"exercise:
  type: "function_writing"
  prompt: |
    Write Python code that prints the word "hello" on its own line, using print().
  code_template: |
    # Your code here
    print("hello")
  example_usage: |
    print("hello")  # prints: hello
  success_criteria: |
    - Prints exactly the word "hello"
    - Uses print()
  llm_evaluation_prompt: |
    You are grading a beginner Python exercise: print the word "hello" with print().

    The student submitted this code:
    {student_code}

    Decide whether it prints "hello" and call respond_with_feedback with your
    assessment. Set is_correct true when the requirement is met, and give two or
    three sentences of encouraging feedback.
"#;

/// Render a starter lesson for `language` under the slug `id`.
///
/// Pure (§2.1): it picks the language-appropriate exercise body and frames it
/// with the lesson's `lesson_name`, `language`, and a description — returning the
/// YAML as data with no filesystem access, so the result is asserted directly
/// against the production parser. The `language` choice drives the template, so a
/// `--lang python` lesson never declares `language: R` (§1.2). The caller is
/// responsible for `id` being a safe slug; [`add_lesson`] guards it.
pub fn lesson_template(language: Language, id: &str) -> String {
    let (lang, exercise) = match language {
        Language::R => ("R", R_EXERCISE),
        Language::Python => ("Python", PYTHON_EXERCISE),
    };
    format!(
        "# A lesson scaffolded by `blendtutor new lesson`. Edit it, then check your\n\
         # changes with `blendtutor validate {LESSONS_DIR}/{id}.yaml`.\n\
         lesson_name: \"{id}\"\n\
         language: {lang}\n\
         description: \"A starter {lang} lesson — replace with your own exercise\"\n\
         {exercise}"
    )
}

/// Why a lesson could not be added to a course.
///
/// The three refusals are kept distinct from a genuine write failure so the cli
/// can frame each: an [`InvalidId`](AddLessonError::InvalidId), a
/// [`NotACourse`](AddLessonError::NotACourse), or an
/// [`AlreadyExists`](AddLessonError::AlreadyExists) is a user-correctable refusal
/// caught at the boundary *before any write*, whereas a
/// [`Write`](AddLessonError::Write) is an underlying I/O fault.
#[derive(Debug)]
pub enum AddLessonError {
    /// The lesson id is not a safe slug (empty, or containing a path separator,
    /// `..`, whitespace, or punctuation), so it is refused before any write
    /// (§1.3.1) rather than escaping the course's `lessons/` directory or
    /// breaking the manifest's TOML. Carries the rejected id.
    InvalidId(String),
    /// The target directory is not a course: it has no `blendtutor.toml` to
    /// register the lesson in, so the add is refused before any write (§1.3.1)
    /// rather than leaving an orphan lesson in a non-course directory. Carries the
    /// directory. (Run `blendtutor init` first, or `cd` into a course.)
    NotACourse(PathBuf),
    /// A lesson already exists at the target path, so the command refuses to
    /// overwrite it (no clobber), before any write. Carries the course-relative
    /// path that is already taken.
    AlreadyExists(PathBuf),
    /// A filesystem operation failed while writing the lesson file or registering
    /// it in the manifest.
    Write(std::io::Error),
}

impl fmt::Display for AddLessonError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AddLessonError::InvalidId(id) => write!(
                f,
                "lesson id {id:?} is not a valid slug; use letters, digits, \
                 hyphens, and underscores only",
            ),
            AddLessonError::NotACourse(dir) => write!(
                f,
                "{dir:?} is not a blendtutor course (no blendtutor.toml); run \
                 `blendtutor init` or cd into a course before adding a lesson",
            ),
            AddLessonError::AlreadyExists(path) => write!(
                f,
                "a lesson already exists at {path:?}; new lesson refuses to \
                 overwrite it — choose a different id",
            ),
            AddLessonError::Write(e) => write!(f, "could not write the new lesson: {e}"),
        }
    }
}

impl Error for AddLessonError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            AddLessonError::InvalidId(_)
            | AddLessonError::NotACourse(_)
            | AddLessonError::AlreadyExists(_) => None,
            AddLessonError::Write(e) => Some(e),
        }
    }
}

/// Whether `id` is a safe lesson slug: non-empty and built only from ASCII
/// letters, digits, hyphens, and underscores.
///
/// The boundary check behind [`add_lesson`]'s guard (§1.3.1). The id is used both
/// as a filename stem and, verbatim, inside the manifest's TOML, so excluding path
/// separators, `.`, whitespace, and quotes keeps a new lesson from escaping
/// `lessons/` or corrupting `blendtutor.toml`.
fn is_valid_slug(id: &str) -> bool {
    !id.is_empty()
        && id
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
}

/// Add a `language` lesson `id` to the course rooted at `dir`: write
/// `lessons/<id>.yaml` from [`lesson_template`] and register it in the manifest.
///
/// The effectful shell (§2.2) over the pure template selection. Three guards fire
/// before any write (§1.3.1): an unsafe slug is refused as
/// [`AddLessonError::InvalidId`]; a directory with no `blendtutor.toml` is refused
/// as [`AddLessonError::NotACourse`] — its manifest is *opened* up front, so a
/// non-course directory is never left with an orphan lesson; and an id whose
/// lesson file already exists is refused as [`AddLessonError::AlreadyExists`], the
/// create-new write making that check atomic so a duplicate never clobbers the
/// existing lesson nor appends a second manifest entry. On success the lesson file
/// is written, a `[[lessons]]` entry appended to `blendtutor.toml`, and the
/// course-relative lesson path returned.
///
/// The manifest is opened first (the not-a-course guard) but its entry is appended
/// last, after the lesson file is written. The only residual non-atomic window is
/// a rare append failure after a good write, which leaves an orphan lesson `list`
/// (manifest-driven) simply omits — a valid, recoverable state. Registering first
/// would be worse: a `list` row pointing at a file that was never written.
pub fn add_lesson(dir: &Path, language: Language, id: &str) -> Result<PathBuf, AddLessonError> {
    if !is_valid_slug(id) {
        return Err(AddLessonError::InvalidId(id.to_string()));
    }
    let mut manifest = open_manifest_for_append(dir)?;
    let rel_path = PathBuf::from(LESSONS_DIR).join(format!("{id}.yaml"));
    std::fs::create_dir_all(dir.join(LESSONS_DIR)).map_err(AddLessonError::Write)?;
    write_without_clobber(&dir.join(&rel_path), &lesson_template(language, id)).map_err(
        |e| match e.kind() {
            std::io::ErrorKind::AlreadyExists => AddLessonError::AlreadyExists(rel_path.clone()),
            _ => AddLessonError::Write(e),
        },
    )?;
    append_lesson_entry(&mut manifest, id).map_err(AddLessonError::Write)?;
    Ok(rel_path)
}

/// Write `contents` to `path`, failing with [`std::io::ErrorKind::AlreadyExists`]
/// rather than overwriting an existing file.
///
/// `create_new` fuses the existence check and the create into one atomic step, so
/// there is no window between "does it exist?" and "write it" for the no-clobber
/// guard to miss (§1.3.1) — unlike a separate `exists()` test then `write`.
fn write_without_clobber(path: &Path, contents: &str) -> std::io::Result<()> {
    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(path)?;
    std::io::Write::write_all(&mut file, contents.as_bytes())
}

/// Open the course manifest in `dir` for appending, or refuse a directory that has
/// no manifest as [`AddLessonError::NotACourse`].
///
/// Opening the manifest up front (it is not written here) turns "this is not a
/// course" into a boundary refusal *before* any lesson file is written (§1.3.1),
/// so a missing manifest can never leave an orphan lesson behind. A `NotFound` is
/// the not-a-course signal; any other open failure is a genuine
/// [`AddLessonError::Write`].
fn open_manifest_for_append(dir: &Path) -> Result<std::fs::File, AddLessonError> {
    std::fs::OpenOptions::new()
        .append(true)
        .open(dir.join(MANIFEST_FILENAME))
        .map_err(|e| match e.kind() {
            std::io::ErrorKind::NotFound => AddLessonError::NotACourse(dir.to_path_buf()),
            _ => AddLessonError::Write(e),
        })
}

/// Append a `[[lessons]]` entry registering lesson `id` to an already-open manifest
/// handle.
///
/// A textual append, not a re-serialize: TOML permits repeated `[[lessons]]`
/// array-of-tables, the [`Manifest`](crate::course::Manifest) model is parse-only,
/// and appending leaves every existing entry's bytes exactly where they were. The
/// id is a validated slug, so the emitted TOML string and `lessons/<id>.yaml` path
/// are safe to embed verbatim.
fn append_lesson_entry(manifest: &mut std::fs::File, id: &str) -> std::io::Result<()> {
    let entry = format!("\n[[lessons]]\nid = \"{id}\"\npath = \"{LESSONS_DIR}/{id}.yaml\"\n");
    std::io::Write::write_all(manifest, entry.as_bytes())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::course::{Course, Manifest};
    use crate::eval::parse_eval_suite;
    use crate::lesson::{Language, Lesson};

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

    #[cfg(unix)]
    #[test]
    fn scaffold_course_refuses_a_broken_symlink_target_instead_of_a_cryptic_write_error() {
        // A broken symlink (its target does not exist) makes `read_dir` report
        // NotFound, the same as a truly-absent path — but the link itself exists.
        // The guard must refuse it before any write, rather than treat it as
        // absent and let `create_dir_all` fail over the symlink inode with a
        // confusing "File exists".
        let dir = tempfile::tempdir().unwrap();
        let link = dir.path().join("course_link");
        let dangling = dir.path().join("nonexistent_target");
        std::os::unix::fs::symlink(&dangling, &link).unwrap();

        let err =
            scaffold_course(&link).expect_err("a broken symlink target must be refused, not built");
        assert!(
            matches!(err, ScaffoldError::TargetNotEmpty(_)),
            "a broken symlink is an existing object, refused before any write, got {err:?}"
        );
        assert!(
            !dangling.exists(),
            "the guard fires before create_dir_all, so the link's target is never created"
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

    #[test]
    fn lesson_template_produces_a_valid_python_lesson() {
        // The pure selector (§2.1) returns a Python lesson the production parser
        // accepts: a template that hardcoded R, or emitted an invalid schema,
        // fails here without touching any filesystem.
        let yaml = lesson_template(Language::Python, "greet");
        let lesson = Lesson::parse(&yaml).expect("the generated python lesson must be valid");
        assert_eq!(lesson.language, Language::Python);
        assert_eq!(lesson.lesson_name.to_string(), "greet");
    }

    #[test]
    fn lesson_template_produces_a_valid_r_lesson() {
        // The twin: the same selector yields a valid R lesson, so language drives
        // the template rather than a hardcoded default.
        let yaml = lesson_template(Language::R, "loops");
        let lesson = Lesson::parse(&yaml).expect("the generated r lesson must be valid");
        assert_eq!(lesson.language, Language::R);
        assert_eq!(lesson.lesson_name.to_string(), "loops");
    }

    #[test]
    fn add_lesson_writes_the_file_and_registers_it_so_the_course_lists_it() {
        // Effectful (§2.2): into a real scaffolded course, add a python lesson and
        // observe the whole AC1 chain in core terms — the file lands under
        // lessons/, parses, and the manifest now resolves it as a discovered row.
        let dir = tempfile::tempdir().unwrap();
        scaffold_course(dir.path()).unwrap();

        let rel = add_lesson(dir.path(), Language::Python, "greet")
            .expect("adding a fresh lesson succeeds");
        assert_eq!(rel, Path::new("lessons/greet.yaml"));
        assert!(
            dir.path().join(&rel).is_file(),
            "the lesson file is written"
        );

        let course = Course::open(dir.path()).expect("the course still opens after registration");
        let greet = course
            .discover()
            .into_iter()
            .filter_map(Result::ok)
            .find(|s| s.id.to_string() == "greet")
            .expect("the new lesson is discovered via the manifest");
        assert_eq!(greet.language, Language::Python);
    }

    #[test]
    fn add_lesson_refuses_an_unsafe_id_before_writing_anything() {
        // The id becomes both a filename stem and a manifest path, so an id with a
        // path separator, `..`, whitespace, or other unslug character is refused at
        // the boundary (§1.3.1) before any write — never allowed to escape the
        // course's lessons/ directory or break the manifest's TOML.
        let dir = tempfile::tempdir().unwrap();
        scaffold_course(dir.path()).unwrap();
        let manifest_before = std::fs::read_to_string(dir.path().join(MANIFEST_FILENAME)).unwrap();

        for bad in ["../evil", "a/b", "", "has space", "dot.dot", "quote\"d"] {
            let err = add_lesson(dir.path(), Language::Python, bad)
                .expect_err("an unsafe id must be refused");
            assert!(
                matches!(err, AddLessonError::InvalidId(_)),
                "id {bad:?} should be an InvalidId refusal, got {err:?}"
            );
        }
        // No lessons/ dir created, manifest byte-identical: the guard precedes every
        // write, so a refused id leaves the course exactly as it was.
        assert!(
            !dir.path().join("lessons").exists(),
            "a refused id must not create the lessons directory"
        );
        assert_eq!(
            std::fs::read_to_string(dir.path().join(MANIFEST_FILENAME)).unwrap(),
            manifest_before,
            "a refused id must not touch the manifest"
        );
    }

    #[test]
    fn add_lesson_refuses_a_non_course_directory_without_leaving_an_orphan() {
        // Running `new lesson` outside a course (no blendtutor.toml) must refuse
        // before any write (§1.3.1): the manifest is opened up front, so a plain
        // directory is rejected as NotACourse and never contaminated with an orphan
        // lessons/<id>.yaml that no manifest registers.
        let dir = tempfile::tempdir().unwrap(); // a bare dir, never `init`-ed

        let err = add_lesson(dir.path(), Language::Python, "greet")
            .expect_err("a directory with no manifest is not a course");
        assert!(
            matches!(err, AddLessonError::NotACourse(_)),
            "a missing manifest is a NotACourse refusal, got {err:?}"
        );
        assert!(
            !dir.path().join(LESSONS_DIR).exists(),
            "a refused non-course add must not write an orphan lesson directory"
        );
    }

    #[test]
    fn add_lesson_error_display_and_source_distinguish_each_variant() {
        use std::io::{Error as IoError, ErrorKind};

        // InvalidId names the rejected id and has no nested source.
        let invalid = AddLessonError::InvalidId("../evil".to_string());
        let invalid_msg = invalid.to_string();
        assert!(
            invalid_msg.contains("../evil"),
            "InvalidId should name the rejected id, got: {invalid_msg}"
        );
        assert!(std::error::Error::source(&invalid).is_none());

        // AlreadyExists names the path and frames the no-clobber refusal, no source.
        let exists = AddLessonError::AlreadyExists(PathBuf::from("lessons/greet.yaml"));
        let exists_msg = exists.to_string();
        assert!(
            exists_msg.contains("lessons/greet.yaml") && exists_msg.contains("exists"),
            "AlreadyExists should name the path and the refusal, got: {exists_msg}"
        );
        assert!(std::error::Error::source(&exists).is_none());

        // NotACourse names the directory and points at `init`, no source.
        let not_course = AddLessonError::NotACourse(PathBuf::from("/tmp/scratch"));
        let not_course_msg = not_course.to_string();
        assert!(
            not_course_msg.contains("/tmp/scratch") && not_course_msg.contains("blendtutor.toml"),
            "NotACourse should name the dir and the missing manifest, got: {not_course_msg}"
        );
        assert!(std::error::Error::source(&not_course).is_none());

        // Write frames itself and exposes the io::Error as its source.
        let write = AddLessonError::Write(IoError::new(ErrorKind::PermissionDenied, "denied"));
        let write_msg = write.to_string();
        assert!(
            write_msg.contains("denied"),
            "Write should carry the io message, got: {write_msg}"
        );
        assert!(std::error::Error::source(&write).is_some());
    }

    #[test]
    fn add_lesson_refuses_a_duplicate_without_clobbering_or_double_registering() {
        // No-clobber (§1.3.1): a second add of the same id is refused before any
        // write, so the original lesson's bytes and the manifest are both
        // untouched — no overwrite, no duplicate `[[lessons]]` entry.
        let dir = tempfile::tempdir().unwrap();
        scaffold_course(dir.path()).unwrap();
        add_lesson(dir.path(), Language::R, "dup").expect("the first add succeeds");

        let lesson_path = dir.path().join(LESSONS_DIR).join("dup.yaml");
        let lesson_before = std::fs::read(&lesson_path).unwrap();
        let manifest_before = std::fs::read_to_string(dir.path().join(MANIFEST_FILENAME)).unwrap();

        let err = add_lesson(dir.path(), Language::Python, "dup")
            .expect_err("a duplicate id must be refused");
        assert!(
            matches!(err, AddLessonError::AlreadyExists(_)),
            "a duplicate is an AlreadyExists refusal, got {err:?}"
        );
        assert_eq!(
            std::fs::read(&lesson_path).unwrap(),
            lesson_before,
            "the existing lesson's bytes must be unchanged (still the R template)"
        );
        assert_eq!(
            std::fs::read_to_string(dir.path().join(MANIFEST_FILENAME)).unwrap(),
            manifest_before,
            "a refused duplicate must not append a second manifest entry"
        );
    }

    #[test]
    fn is_valid_slug_accepts_word_chars_hyphens_and_underscores_but_nothing_else() {
        // Pin both sides of the charset: hyphens, underscores, and digits are part
        // of a real slug (`add-two`, `my_lesson`, `ch2`) and must be accepted, so a
        // dropped allowed-char branch is caught here rather than only when a reject
        // case slips through.
        for ok in ["greet", "add-two", "my_lesson", "ch2", "a", "R2D2"] {
            assert!(is_valid_slug(ok), "{ok:?} should be a valid slug");
        }
        for bad in [
            "",
            "../x",
            "a/b",
            "has space",
            "dot.dot",
            "quote\"d",
            "tab\tx",
            ".",
        ] {
            assert!(!is_valid_slug(bad), "{bad:?} should be rejected");
        }
    }
}
