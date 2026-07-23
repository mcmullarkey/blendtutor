//! Pure transform: [`Lesson`] → Quarto `.qmd` fenced-div snippet.
//!
//! The conversion is a pure function — no I/O, no side effects, deterministic
//! (§2.1). The CLI command in `blendtutor-cli` is the thin effectful shell that
//! reads the file, calls this transform, and writes to stdout (§2.2).
//!
//! ## Field mapping
//!
//! | YAML field            | `.qmd` rendering                          |
//! |----------------------|-------------------------------------------|
//! | `exercise.prompt`    | prose after the opening div               |
//! | `exercise.code_template` | first fenced code block (if `Some`)   |
//! | `lesson.checks`       | ```` ```{.<lang> .checks} ```` block (if non-empty) |
//! | `exercise.solution`  | ```` ```{.<lang> .solution} ```` block (if `Some`) |
//! | `exercise.hints`      | `::: {.hints}` div (if `Some`)           |
//! | `lesson.language`    | `language="<r|python>"` attribute         |
//! | `exercise.gotchas`   | EXCLUDED (no `.qmd` equivalent)          |
//! | `exercise.llm_evaluation_prompt` | EXCLUDED (author-only, ADR-0006) |
//! | `lesson.packages`    | OMITTED (out-of-scope per decomposition)  |

use crate::lesson::{Language, Lesson};

/// The minimum fence length for a fenced code block (CommonMark default).
const MIN_FENCE_LEN: usize = 3;

/// Render a [`Lesson`] as a Quarto `.qmd` fenced-div snippet.
///
/// The output is a self-contained block starting with
/// `::: {.blendtutor language="<r|python>"}` and closing with `:::`. Each
/// optional section (code template, checks, solution, hints) is emitted only
/// when the corresponding field is present, so no empty blocks appear for
/// absent fields (§1.1). Author-only fields (`llm_evaluation_prompt`,
/// `gotchas`) and out-of-scope fields (`packages`) are excluded.
///
/// # Arguments
/// * `lesson` — A valid, parsed lesson (constructed via [`Lesson::parse`]).
///
/// # Returns
/// A `String` containing the `.qmd` fenced-div snippet, terminated by a
/// newline.
pub fn export_lesson_to_qmd(lesson: &Lesson) -> String {
    let lang = language_tag(&lesson.language);
    let mut out = String::new();

    // Opening div with the language attribute.
    out.push_str(&format!("::: {{.blendtutor language=\"{lang}\"}}\n"));

    // Prompt as prose (always present — it is a required field).
    out.push_str(lesson.exercise.prompt.trim_end());
    out.push('\n');

    // Code template as the first fenced code block (if present).
    if let Some(ref template) = lesson.exercise.code_template {
        out.push('\n');
        let fence = fence_for(template);
        out.push_str(&format!("{fence}{lang}\n"));
        out.push_str(template.trim_end());
        out.push('\n');
        out.push_str(&fence);
        out.push('\n');
    }

    // Checks as a classed code block (if non-empty).
    if !lesson.checks.is_empty() {
        out.push('\n');
        let checks_content = lesson.checks.join("\n");
        let fence = fence_for(&checks_content);
        out.push_str(&format!("{fence}{{.{lang} .checks}}\n"));
        out.push_str(&checks_content);
        out.push('\n');
        out.push_str(&fence);
        out.push('\n');
    }

    // Solution as a classed code block (if present).
    if let Some(ref solution) = lesson.exercise.solution {
        out.push('\n');
        let fence = fence_for(solution);
        out.push_str(&format!("{fence}{{.{lang} .solution}}\n"));
        out.push_str(solution.trim_end());
        out.push('\n');
        out.push_str(&fence);
        out.push('\n');
    }

    // Hints as a fenced div (if present).
    if let Some(ref hints) = lesson.exercise.hints {
        out.push('\n');
        out.push_str("::: {.hints}\n");
        out.push_str(hints.trim_end());
        out.push('\n');
        out.push_str(":::\n");
    }

    // Closing div.
    out.push_str(":::\n");

    out
}

/// Map a [`Language`] to its lowercase code-fence tag.
///
/// `R` → `"r"`, `Python` → `"python"`. Lowercase matches Pandoc/Quarto
/// conventions for fenced code block language tags.
fn language_tag(lang: &Language) -> &'static str {
    match lang {
        Language::R => "r",
        Language::Python => "python",
    }
}

/// Compute the fence length needed to safely enclose `content`.
///
/// Returns a string of backticks whose length is one more than the longest
/// run of consecutive backticks in `content`, with a minimum of
/// [`MIN_FENCE_LEN`] (3). This ensures the fence is never broken by
/// backticks inside the content (CommonMark §4.5).
fn fence_for(content: &str) -> String {
    let max_run = longest_backtick_run(content);
    let fence_len = max_run.max(MIN_FENCE_LEN - 1) + 1;
    "`".repeat(fence_len)
}

/// Find the length of the longest run of consecutive backticks in `content`.
fn longest_backtick_run(content: &str) -> usize {
    let mut max_run = 0;
    let mut current_run = 0;
    for ch in content.chars() {
        if ch == '`' {
            current_run += 1;
            max_run = max_run.max(current_run);
        } else {
            current_run = 0;
        }
    }
    max_run
}

#[cfg(test)]
mod tests {
    use super::*;

    const VALID_YAML: &str = r#"
lesson_name: "Adder"
language: R
exercise:
  prompt: "Write a function add_two(x, y)."
  code_template: "add_two <- function(x, y) {}"
  solution: "add_two <- function(x, y) x + y"
  hints: |
    - Remember: R uses '<-' for assignment.
  llm_evaluation_prompt: "Grade this: {student_code}"
"#;

    #[test]
    fn export_opens_with_blendtutor_div_and_language() {
        let lesson = Lesson::parse(VALID_YAML).unwrap();
        let qmd = export_lesson_to_qmd(&lesson);
        assert!(
            qmd.starts_with("::: {.blendtutor language=\"r\"}\n"),
            "should open with the blendtutor div, got:\n{qmd}"
        );
    }

    #[test]
    fn export_closes_with_div_marker() {
        let lesson = Lesson::parse(VALID_YAML).unwrap();
        let qmd = export_lesson_to_qmd(&lesson);
        assert!(
            qmd.trim_end().ends_with(":::"),
            "should close with :::, got:\n{qmd}"
        );
    }

    #[test]
    fn export_excludes_llm_evaluation_prompt() {
        let lesson = Lesson::parse(VALID_YAML).unwrap();
        let qmd = export_lesson_to_qmd(&lesson);
        assert!(
            !qmd.contains("llm_evaluation_prompt"),
            "llm_evaluation_prompt must be absent, got:\n{qmd}"
        );
        assert!(
            !qmd.contains("Grade this"),
            "llm_evaluation_prompt text must be absent, got:\n{qmd}"
        );
    }

    #[test]
    fn export_excludes_gotchas() {
        let yaml = r#"
lesson_name: "Gotchas"
language: R
exercise:
  prompt: "Write a function."
  gotchas: |
    - R uses '<-' for assignment.
  llm_evaluation_prompt: "Grade this: {student_code}"
"#;
        let lesson = Lesson::parse(yaml).unwrap();
        let qmd = export_lesson_to_qmd(&lesson);
        assert!(
            !qmd.contains("gotchas"),
            "gotchas must be absent, got:\n{qmd}"
        );
        assert!(
            !qmd.contains("R uses '<-' for assignment"),
            "gotchas text must be absent, got:\n{qmd}"
        );
    }

    #[test]
    fn export_omits_packages() {
        let yaml = r#"
lesson_name: "Pkg"
language: Python
packages:
  - pandas
exercise:
  prompt: "Write add."
  llm_evaluation_prompt: "Grade this: {student_code}"
"#;
        let lesson = Lesson::parse(yaml).unwrap();
        let qmd = export_lesson_to_qmd(&lesson);
        assert!(
            !qmd.contains("packages"),
            "packages must be omitted, got:\n{qmd}"
        );
        assert!(
            !qmd.contains("pandas"),
            "package names must be omitted, got:\n{qmd}"
        );
    }

    #[test]
    fn export_no_empty_blocks_for_absent_fields() {
        let yaml = r#"
lesson_name: "Minimal"
language: R
exercise:
  prompt: "Write a function."
  llm_evaluation_prompt: "Grade this: {student_code}"
"#;
        let lesson = Lesson::parse(yaml).unwrap();
        let qmd = export_lesson_to_qmd(&lesson);
        assert!(
            !qmd.contains(".solution"),
            "no .solution block for absent solution, got:\n{qmd}"
        );
        assert!(
            !qmd.contains(".checks"),
            "no .checks block for empty checks, got:\n{qmd}"
        );
        assert!(
            !qmd.contains("{.hints}"),
            "no hints div for absent hints, got:\n{qmd}"
        );
    }

    #[test]
    fn export_python_uses_python_language_tag() {
        let yaml = r#"
lesson_name: "Py"
language: Python
exercise:
  prompt: "Write add."
  code_template: "def add(a, b): ..."
  llm_evaluation_prompt: "Grade this: {student_code}"
"#;
        let lesson = Lesson::parse(yaml).unwrap();
        let qmd = export_lesson_to_qmd(&lesson);
        assert!(
            qmd.contains("language=\"python\""),
            "Python lesson should use language=\"python\", got:\n{qmd}"
        );
        assert!(
            qmd.contains("```python\n"),
            "code block should use ```python tag, got:\n{qmd}"
        );
    }

    #[test]
    fn export_backtick_in_template_uses_longer_fence() {
        let yaml = r#"
lesson_name: "Backtick"
language: R
exercise:
  prompt: "Write a function."
  code_template: |
    # Has ``` in it
    f <- function() {}
  llm_evaluation_prompt: "Grade this: {student_code}"
"#;
        let lesson = Lesson::parse(yaml).unwrap();
        let qmd = export_lesson_to_qmd(&lesson);
        assert!(
            qmd.contains("````r\n"),
            "fence should be 4 backticks when content has ```, got:\n{qmd}"
        );
    }

    #[test]
    fn export_different_lessons_produce_different_output() {
        let lesson_a = Lesson::parse(VALID_YAML).unwrap();
        let yaml_b = r#"
lesson_name: "Different"
language: R
exercise:
  prompt: "Write a completely different function."
  llm_evaluation_prompt: "Grade this: {student_code}"
"#;
        let lesson_b = Lesson::parse(yaml_b).unwrap();
        assert_ne!(
            export_lesson_to_qmd(&lesson_a),
            export_lesson_to_qmd(&lesson_b),
            "different lessons must produce different output"
        );
    }

    #[test]
    fn fence_for_no_backticks_returns_three() {
        assert_eq!(fence_for("hello world"), "```");
    }

    #[test]
    fn fence_for_single_backtick_returns_three() {
        assert_eq!(fence_for("a ` b"), "```");
    }

    #[test]
    fn fence_for_triple_backtick_returns_four() {
        assert_eq!(fence_for("a ``` b"), "````");
    }

    #[test]
    fn fence_for_four_backticks_returns_five() {
        assert_eq!(fence_for("a ```` b"), "`````");
    }

    #[test]
    fn longest_backtick_run_detects_runs() {
        assert_eq!(longest_backtick_run("no backticks"), 0);
        assert_eq!(longest_backtick_run("one ` here"), 1);
        assert_eq!(longest_backtick_run("triple ``` here"), 3);
        assert_eq!(longest_backtick_run("`` and ``` mixed"), 3);
    }
}
