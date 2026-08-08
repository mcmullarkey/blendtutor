//! smevals eval-dir generation: a pure translator from a lesson + eval suite to
//! the file tree `smevals run`/`build` consume.
//!
//! An eval dir is any directory with an `eval.yaml`, `tasks/`, `configs/`, and
//! `graders/` — the smevals v0.2.0 layout. [`generate_eval_dir`] produces that
//! tree as `(relative_path, contents)` pairs: `eval.yaml`, `configs/default.yaml`,
//! `graders/default.yaml`, and `tasks/case-N.yaml` (one per suite case, 1-based).
//! It is pure (§2.1): no filesystem, no clock, no iteration-order randomness —
//! identical inputs yield byte-identical output, and every value is emitted
//! through an injection-proof YAML scalar discipline proven by round-trip tests
//! (§1.3.1). The model id in `configs/default.yaml` is single-sourced from
//! [`ProviderChoice::Fireworks::default_model`], never a divergent literal.
//!
//! This module owns *only* the lesson+suite → smevals-dir translation. It is
//! NOT the runner (AC-3), the LLM judge (AC-4), or the report command (AC-5);
//! it merely emits the templates those later slices wire into. Script paths in
//! the templates (`scripts/smevals/run.sh`, `scripts/smevals/check_polarity.sh`)
//! are emitted relative to the generated `configs/`/`graders/` file, as smevals
//! resolves them relative to the file that names them.
//!
//! YAML emission: serde-saphyr 0.0.27 is parse-only, so the emitter is
//! hand-rolled. Hostile content (a submission containing `: `, `&anchor`,
//! `---`, or a leading `- `) is emitted as a double-quoted scalar with every
//! control character escaped — byte-exact under re-parse (serde_saphyr's `|+`
//! chomping is broken and `|-` drops trailing newlines, so block scalars cannot
//! round-trip arbitrary strings; the round-trip test in
//! `crates/core/tests/generate_eval_dir.rs` pins this). Safe-by-construction
//! values (slugs, verdict tokens, the pinned script paths) are plain scalars.

use std::error::Error;
use std::fmt;
use std::path::{Path, PathBuf};

use crate::eval::{EvalCase, EvalSuite};
use crate::lesson::Lesson;
use crate::llm::ProviderChoice;

/// The script-path prefix emitted when no enclosing repo root is discoverable:
/// the canonical in-repo course layout (`examples/<course>` is two levels below
/// the repo root, so `configs/` reaches the repo root in four `..` hops).
/// [`write_eval_dir`] recomputes the exact prefix for the actual course location.
const DEFAULT_SCRIPTS_REL: &str = "../../../../scripts/smevals/";

/// The relative path from the generated `configs/` (and `graders/`) directory to
/// the smevals runner, pinned by AC-3. Only the directory portion — the shell
/// fixes up the `..` prefix for the course's actual location.
const RUNNER_REL: &str = "run.sh";
/// The relative path to the polarity checker, pinned by AC-3.
const CHECKER_REL: &str = "check_polarity.sh";
/// The polarity check is `required: true` so a wrong verdict halts grading
/// before any later (AC-4 judge) check runs.
const PASS_THRESHOLD: f64 = 0.8;

/// Why an eval dir could not be generated.
#[derive(Debug)]
pub enum GenError {
    /// The lesson id is not a safe slug: empty, or containing a character
    /// outside ASCII alphanumerics, `-`, `_`. Mirrors `scaffold::is_valid_slug`
    /// (scaffold.rs:320), enforced here so a hostile id can never reach a path
    /// or an emitted template (§1.3.1).
    InvalidLessonId {
        /// The rejected id, quoted back to the caller.
        lesson_id: String,
    },
    /// The suite has no cases. A vacuous "100% pass" is a sneaky pass, so an
    /// empty suite is refused rather than emitting `tasks/` with zero files.
    EmptySuite,
    /// Writing a generated file failed (the effectful shell only — the pure
    /// generator never touches the filesystem).
    Write {
        /// The path that could not be written.
        path: PathBuf,
        /// The underlying I/O failure.
        source: std::io::Error,
    },
}

impl fmt::Display for GenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GenError::InvalidLessonId { lesson_id } => write!(
                f,
                "invalid lesson id {lesson_id:?}: must be non-empty and contain only \
                 ASCII alphanumerics, '-', '_'"
            ),
            GenError::EmptySuite => write!(
                f,
                "refusing to generate an eval dir for an empty eval suite: no cases \
                 to evaluate"
            ),
            GenError::Write { path, source } => {
                write!(f, "writing {} failed: {source}", path.display())
            }
        }
    }
}

impl Error for GenError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            GenError::Write { source, .. } => Some(source),
            _ => None,
        }
    }
}

/// Generate the smevals eval-dir file tree for `suite`, tagged with `lesson_id`.
///
/// Pure and deterministic (§2.1, §5.1): returns `(relative_path, contents)`
/// pairs — `eval.yaml`, `configs/default.yaml`, `graders/default.yaml`, and
/// `tasks/case-N.yaml` for each case in document order — with no filesystem
/// access, so the file set, byte-stability, and injection defense are asserted
/// directly in integration tests. The effectful half is [`write_eval_dir`].
///
/// `lesson_id` must be a slug (see [`GenError::InvalidLessonId`]); the suite
/// must be non-empty (see [`GenError::EmptySuite`]). `lesson` supplies the
/// eval's name-space context (its exercise prompt becomes the eval description).
pub fn generate_eval_dir(
    lesson: &Lesson,
    suite: &EvalSuite,
    lesson_id: &str,
) -> Result<Vec<(PathBuf, String)>, GenError> {
    generate_eval_dir_with(lesson, suite, lesson_id, DEFAULT_SCRIPTS_REL)
}

/// [`generate_eval_dir`] with an explicit script-path prefix, so the effectful
/// shell can emit paths that resolve for the course's actual location.
fn generate_eval_dir_with(
    lesson: &Lesson,
    suite: &EvalSuite,
    lesson_id: &str,
    scripts_rel: &str,
) -> Result<Vec<(PathBuf, String)>, GenError> {
    if !is_valid_lesson_id(lesson_id) {
        return Err(GenError::InvalidLessonId {
            lesson_id: lesson_id.to_string(),
        });
    }
    if suite.cases.is_empty() {
        return Err(GenError::EmptySuite);
    }

    let mut files = vec![
        (
            PathBuf::from("eval.yaml"),
            emit_eval_yaml(lesson, lesson_id),
        ),
        (
            PathBuf::from("configs/default.yaml"),
            emit_configs_yaml(scripts_rel),
        ),
        (
            PathBuf::from("graders/default.yaml"),
            emit_graders_yaml(scripts_rel),
        ),
    ];
    for (index, case) in suite.cases.iter().enumerate() {
        files.push((
            PathBuf::from(format!("tasks/case-{}.yaml", index + 1)),
            emit_task_yaml(lesson_id, index + 1, case),
        ));
    }
    Ok(files)
}

/// The slug rule for a lesson id: non-empty ASCII alphanumerics, `-`, `_`.
///
/// Mirrors `scaffold::is_valid_slug` (scaffold.rs:320) so a generated id can
/// never escape its directory or corrupt a template (§1.3.1). Kept local rather
/// than shared so `scaffold`'s private helper stays private; a divergence here
/// would be caught by the path-safety integration tests.
fn is_valid_lesson_id(id: &str) -> bool {
    !id.is_empty()
        && id
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
}

// ---------------------------------------------------------------------------
// Emitters — one small pure function per file kind (§5.1)
// ---------------------------------------------------------------------------

/// The eval manifest: the eval's name and what is being evaluated.
fn emit_eval_yaml(lesson: &Lesson, lesson_id: &str) -> String {
    format!(
        "name: {}\ndescription: {}\n",
        lesson_id,
        escape_yaml_double_quoted(&lesson.exercise.prompt),
    )
}

/// One task: the submission (as `prompt`, so the runner sees `SMEVALS_PROMPT`)
/// plus the scalar keys that become `SMEVALS_TASK_LESSON`/`SMEVALS_TASK_CASE`/
/// `SMEVALS_TASK_EXPECTED` for the runner and polarity checker (AC-3 contract).
fn emit_task_yaml(lesson_id: &str, case_index: usize, case: &EvalCase) -> String {
    format!(
        "name: case-{case_index}\nlesson: {}\ncase: {case_index}\nprompt: {}\nexpected: {}\n",
        emit_inline_scalar(lesson_id),
        escape_yaml_double_quoted(&case.submission),
        emit_inline_scalar(case.expected.token()),
    )
}

/// The default config: the runner executable and the model, single-sourced
/// from the provider default so the runtime model cannot drift from the browser.
fn emit_configs_yaml(scripts_rel: &str) -> String {
    format!(
        "name: default\nrunner: {}\nmodel: {}\n",
        emit_inline_scalar(&format!("{scripts_rel}{RUNNER_REL}")),
        emit_inline_scalar(ProviderChoice::Fireworks.default_model()),
    )
}

/// The default grader: the polarity check first and `required`, so a wrong
/// verdict halts grading; `pass_threshold` applies to the final check's score
/// (AC-4's judge slots into `checks` after this entry).
fn emit_graders_yaml(scripts_rel: &str) -> String {
    format!(
        "name: default\nchecks:\n  - checker: {}\n    required: true\nscoring:\n  \
         pass_threshold: {PASS_THRESHOLD}\n",
        emit_inline_scalar(&format!("{scripts_rel}{CHECKER_REL}")),
    )
}

// ---------------------------------------------------------------------------
// YAML scalar discipline — the injection-defense core
// ---------------------------------------------------------------------------

/// Emit `value` as a YAML inline scalar: plain when unambiguous, double-quoted
/// otherwise. Plain emission is only used for values that are safe by
/// construction (slugs, verdict tokens, pinned paths); anything containing a
/// structural character (`: `, ` #`, a leading `- `/`&`/`*`/`!`/`"`/`|`/`>`…,
/// newlines) is quoted so it can never inject a sibling key, anchor, document
/// separator, or comment (§1.3.1).
fn emit_inline_scalar(value: &str) -> String {
    if is_plain_safe(value) {
        value.to_string()
    } else {
        escape_yaml_double_quoted(value)
    }
}

/// Whether `value` parses as the same bytes when emitted as a plain YAML scalar.
fn is_plain_safe(value: &str) -> bool {
    if value.is_empty() || value.contains('\n') {
        return false;
    }
    let first = value.as_bytes()[0];
    // Leading whitespace or a comment marker would change the meaning.
    if first.is_ascii_whitespace() || first == b'#' {
        return false;
    }
    // Block/flow indicators at the start: `- ` is a list item, `? `/`: ` a key,
    // `&` an anchor, `*` an alias, `!` a tag, `|`/`>` a block scalar, quotes a
    // quoted scalar, `[`/`{`/`,` flow collections, `%` a directive, `@`/`` ` `` reserved.
    if value.starts_with("- ") || value.starts_with("? ") || value.starts_with(": ") {
        return false;
    }
    if matches!(
        first,
        b'-' | b'?'
            | b':'
            | b'&'
            | b'*'
            | b'!'
            | b'|'
            | b'>'
            | b'"'
            | b'\''
            | b'['
            | b']'
            | b'{'
            | b'}'
            | b','
            | b'%'
            | b'@'
            | b'`'
    ) {
        return false;
    }
    // Mid-string structures: `key: value` splits a map, ` #` starts a comment,
    // trailing whitespace is dropped by the parser.
    if value.contains(": ") || value.contains(" #") {
        return false;
    }
    !value.ends_with(' ') && !value.ends_with('\t')
}

/// Emit `content` as a YAML double-quoted scalar, escaping every character that
/// would otherwise change meaning: `"` `\` and all control characters (C0 +
/// DEL as `\uXXXX`, `\n`/`\t`/`\r` short forms), plus the YAML line separators
/// U+2028/U+2029. The result is a single logical line that re-parses to the
/// exact input bytes — proven byte-exact for hostile content in the integration
/// tests (serde_saphyr's block-scalar `|+` chomping is broken and `|-` drops
/// trailing newlines, so a quoted scalar is the only byte-exact path).
fn escape_yaml_double_quoted(content: &str) -> String {
    let mut out = String::with_capacity(content.len() + 2);
    out.push('"');
    for c in content.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            '\u{2028}' => out.push_str("\\u2028"),
            '\u{2029}' => out.push_str("\\u2029"),
            c if (c as u32) < 0x20 || (c as u32) == 0x7f => {
                out.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

// ---------------------------------------------------------------------------
// Effectful shell + shared discovery helpers
// ---------------------------------------------------------------------------

/// The lesson id for a lesson file: its stem, so `lessons/foo.yaml` is `foo`.
///
/// Shared with AC-5's report command — the single source for "lesson_id = file
/// stem", so the generator and the report command cannot derive it differently.
pub fn lesson_id_from_path(lesson_path: &Path) -> Option<&str> {
    lesson_path.file_stem().and_then(|stem| stem.to_str())
}

/// The course root for `lesson_path`: the nearest ancestor directory containing
/// a `blendtutor.toml` manifest (the course boundary, per scaffold.rs).
///
/// Shared with AC-5's report command, which must find `<course>/.smevals/`
/// before invoking the generator. Returns `None` when no ancestor is a course.
pub fn course_root_for(lesson_path: &Path) -> Option<PathBuf> {
    let mut current = Some(lesson_path);
    while let Some(dir) = current {
        if dir.join("blendtutor.toml").is_file() {
            return Some(dir.to_path_buf());
        }
        current = dir.parent();
    }
    None
}

/// Write the generated eval dir into `dir/.smevals/`, where `dir` is the course
/// root.
///
/// The thin effectful shell (§2.4): it resolves the course's location, computes
/// the script-path prefix that reaches the repo's `scripts/smevals/` from the
/// generated `configs/`, delegates the pure generation, and writes each file
/// (creating directories as needed). The pure [`generate_eval_dir`] never
/// touches the filesystem; this function never emits bytes.
pub fn write_eval_dir(
    dir: &Path,
    lesson: &Lesson,
    suite: &EvalSuite,
    lesson_id: &str,
) -> Result<(), GenError> {
    let dir = dir.canonicalize().map_err(|source| GenError::Write {
        path: dir.to_path_buf(),
        source,
    })?;
    let files = generate_eval_dir_with(lesson, suite, lesson_id, &scripts_rel_from(&dir))?;
    for (path, contents) in &files {
        let target = dir.join(".smevals").join(path);
        if let Some(parent) = target.parent() {
            std::fs::create_dir_all(parent).map_err(|source| GenError::Write {
                path: parent.to_path_buf(),
                source,
            })?;
        }
        std::fs::write(&target, contents).map_err(|source| GenError::Write {
            path: target.clone(),
            source,
        })?;
    }
    Ok(())
}

/// The `scripts/smevals/` prefix (with trailing `/`) that, relative to the
/// generated `configs/` (or `graders/`) directory, reaches the repo's scripts:
/// walk up from the course root to the repo root (the nearest ancestor with a
/// `.git`), then compute the relative descent. Falls back to
/// [`DEFAULT_SCRIPTS_REL`] when no repo root is found (e.g. a course outside a
/// git checkout), which smevals surfaces loudly at run time rather than this
/// function guessing.
fn scripts_rel_from(course_root: &Path) -> String {
    let mut current = Some(course_root);
    let repo_root = loop {
        match current {
            Some(dir) => {
                if dir.join(".git").exists() {
                    break Some(dir);
                }
                current = dir.parent();
            }
            None => break None,
        }
    };
    let Some(repo_root) = repo_root else {
        return DEFAULT_SCRIPTS_REL.to_string();
    };
    let configs_dir = course_root.join(".smevals").join("configs");
    let scripts_dir = repo_root.join("scripts").join("smevals");
    match relative_path(&configs_dir, &scripts_dir) {
        Some(rel) => format!("{}/", rel.to_string_lossy()),
        None => DEFAULT_SCRIPTS_REL.to_string(),
    }
}

/// The path from `from` to `to`, both absolute, as `../..` hops then descent.
/// `None` when the two share no root (unreachable on single-filesystem Unix
/// paths, but total anyway).
fn relative_path(from: &Path, to: &Path) -> Option<PathBuf> {
    let from_parts: Vec<_> = from.components().collect();
    let to_parts: Vec<_> = to.components().collect();
    let common = from_parts
        .iter()
        .zip(&to_parts)
        .take_while(|(a, b)| a == b)
        .count();
    if common == 0 {
        return None;
    }
    let mut out = PathBuf::new();
    for _ in common..from_parts.len() {
        out.push("..");
    }
    for part in &to_parts[common..] {
        out.push(part.as_os_str());
    }
    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::ExpectedVerdict;

    #[test]
    fn lesson_id_is_the_file_stem() {
        assert_eq!(
            lesson_id_from_path(Path::new("lessons/foo.yaml")),
            Some("foo")
        );
        assert_eq!(lesson_id_from_path(Path::new("foo.yaml")), Some("foo"));
        assert_eq!(
            lesson_id_from_path(Path::new("foo.bar.yaml")),
            Some("foo.bar")
        );
        assert_eq!(lesson_id_from_path(Path::new("/")), None);
    }

    #[test]
    fn course_root_is_the_nearest_ancestor_with_a_manifest() {
        let root = tempfile::tempdir().unwrap();
        let course = root.path().join("a").join("b");
        std::fs::create_dir_all(&course).unwrap();
        std::fs::write(course.join("blendtutor.toml"), "").unwrap();
        let lesson = course.join("lessons").join("x.yaml");
        std::fs::create_dir_all(lesson.parent().unwrap()).unwrap();
        std::fs::write(&lesson, "").unwrap();

        assert_eq!(
            course_root_for(&lesson).unwrap(),
            course,
            "the walk-up returns the manifest-bearing ancestor unchanged"
        );
        assert_eq!(course_root_for(Path::new("/nonexistent/x.yaml")), None);
    }

    #[test]
    fn relative_path_walks_up_then_descends() {
        assert_eq!(
            relative_path(
                Path::new("/repo/course/.smevals/configs"),
                Path::new("/repo/scripts/smevals")
            ),
            Some(PathBuf::from("../../../scripts/smevals"))
        );
        assert_eq!(
            relative_path(Path::new("/a/b/c"), Path::new("/a/b/c")),
            Some(PathBuf::from(""))
        );
        assert_eq!(
            relative_path(Path::new("/a"), Path::new("/b")),
            Some(PathBuf::from("../b"))
        );
    }

    #[test]
    fn scripts_rel_reaches_repo_scripts_from_a_nested_course() {
        let repo = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(repo.path().join(".git")).unwrap();
        std::fs::create_dir_all(repo.path().join("scripts/smevals")).unwrap();
        let course = repo.path().join("examples/write-less-code-r");
        std::fs::create_dir_all(&course).unwrap();

        let rel = scripts_rel_from(&course);
        assert_eq!(rel, "../../../../scripts/smevals/");
        // The emitted runner path resolves to the real script (canonicalize
        // requires the leaf to exist, so create the placeholder script).
        std::fs::create_dir_all(course.join(".smevals/configs")).unwrap();
        std::fs::write(repo.path().join("scripts/smevals/run.sh"), "#!/bin/sh\n").unwrap();
        let resolved = course
            .join(".smevals")
            .join("configs")
            .join(&rel)
            .join("run.sh");
        assert_eq!(
            resolved.canonicalize().unwrap(),
            repo.path()
                .join("scripts/smevals/run.sh")
                .canonicalize()
                .unwrap()
        );
    }

    #[test]
    fn scripts_rel_defaults_when_no_repo_root_exists() {
        let dir = tempfile::tempdir().unwrap();
        assert_eq!(scripts_rel_from(dir.path()), DEFAULT_SCRIPTS_REL);
    }

    #[test]
    fn write_eval_dir_persists_the_generated_tree() {
        let repo = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(repo.path().join(".git")).unwrap();
        std::fs::create_dir_all(repo.path().join("scripts/smevals")).unwrap();
        let course = repo.path().join("my-course");
        std::fs::create_dir_all(&course).unwrap();

        let lesson = Lesson::parse(
            "lesson_name: x\nlanguage: R\nexercise:\n  prompt: do it\n  \
             llm_evaluation_prompt: grade {student_code}\n",
        )
        .unwrap();
        let suite = EvalSuite {
            cases: vec![EvalCase {
                submission: "cat(\"hi\\n\")\n".to_string(),
                expected: ExpectedVerdict::Correct,
            }],
        };
        write_eval_dir(&course, &lesson, &suite, "my-lesson").unwrap();

        let eval_dir = course.join(".smevals");
        assert!(eval_dir.join("eval.yaml").is_file());
        assert!(eval_dir.join("configs/default.yaml").is_file());
        assert!(eval_dir.join("graders/default.yaml").is_file());
        assert!(eval_dir.join("tasks/case-1.yaml").is_file());
        let config = std::fs::read_to_string(eval_dir.join("configs/default.yaml")).unwrap();
        // Course is one level below the repo root here → 3 `..` hops.
        assert!(
            config.contains("../../../scripts/smevals/run.sh"),
            "config must reference the runner relative to the course, got: {config}"
        );
    }

    #[test]
    fn empty_suite_is_refused_before_any_emission() {
        let lesson = Lesson::parse(
            "lesson_name: x\nlanguage: R\nexercise:\n  prompt: p\n  \
             llm_evaluation_prompt: grade {student_code}\n",
        )
        .unwrap();
        let err = generate_eval_dir(&lesson, &EvalSuite { cases: vec![] }, "x")
            .expect_err("an empty suite must be refused");
        assert!(matches!(err, GenError::EmptySuite));
    }

    #[test]
    fn hostile_scalar_is_quoted_while_safe_values_stay_plain() {
        assert_eq!(escape_yaml_double_quoted("a\nb"), "\"a\\nb\"");
        assert_eq!(
            escape_yaml_double_quoted("say \"hi\""),
            "\"say \\\"hi\\\"\""
        );
        assert_eq!(escape_yaml_double_quoted("tab\there"), "\"tab\\there\"");
        assert_eq!(escape_yaml_double_quoted(""), "\"\"");
        // Safe-by-construction values stay plain for readable goldens.
        assert_eq!(emit_inline_scalar("case-1"), "case-1");
        assert_eq!(
            emit_inline_scalar("accounts/fireworks/models/deepseek-v4-flash-0731"),
            "accounts/fireworks/models/deepseek-v4-flash-0731"
        );
        assert_eq!(emit_inline_scalar("correct"), "correct");
        // Structural values must be quoted, never plain.
        assert_eq!(emit_inline_scalar("- expected: x"), "\"- expected: x\"");
        assert_eq!(emit_inline_scalar("&anchor"), "\"&anchor\"");
        assert_eq!(emit_inline_scalar("a: b"), "\"a: b\"");
        assert_eq!(emit_inline_scalar("x # c"), "\"x # c\"");
        assert_eq!(emit_inline_scalar("line1\nline2"), "\"line1\\nline2\"");
    }
}
