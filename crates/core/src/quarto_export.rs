//! Pure transforms: [`Lesson`] → Quarto `.qmd` fenced-div snippet or complete
//! page, plus the static API key page (ADR-0019).
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
//! | `exercise.gotchas`   | `::: {.gotchas}` div (if `Some`)         |
//! | `exercise.success_criteria` | `::: {.success-criteria}` div (if `Some`, ADR-0020) |
//! | `lesson.packages`    | `packages="a,b"` attribute (if non-empty) |
//! | `exercise.llm_evaluation_prompt` | EXCLUDED (author-only, ADR-0006) |

use crate::lesson::{Language, Lesson};

/// What `export_lesson_to_qmd` produces (ADR-0019).
///
/// A sum type rather than a bool so each call site names the shape it wants
/// (§1.2).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportShape {
    /// The bare `::: {.blendtutor}` div, for pasting into an existing page.
    Snippet,
    /// The div preceded by front matter, so the page renders on its own.
    Document,
}

/// The filter reference every exported page declares.
const FILTER_NAME: &str = "mcmullarkey/blendtutor";

/// Front-matter comment for R documents: what `coi: true` buys webR, and that
/// book projects fall back to webR's slower channel (the COI service worker's
/// scope cannot cover book pages), so R still runs there.
const R_BOOK_COI_NOTE: &str = "\
# coi: true lets webR use SharedArrayBuffer for faster R execution.
# In Quarto `type: book` projects the COI service worker cannot control pages,
# so R runs on webR's slower fallback channel instead.
";

/// A complete API key page (ADR-0019), mirroring `demo-book/api-key.qmd`.
const KEY_PAGE_QMD: &str = r#"---
title: "API Key"
filters:
  - mcmullarkey/blendtutor
---

# API Key

AI-powered feedback on these exercises uses the [Fireworks AI](https://fireworks.ai)
API with your own key. Create a key in the
[Fireworks console](https://app.fireworks.ai/account/keys); keys look like
`fw_...`.

## Enter your key

::: {.blendtutor-key}

Loading API key settings…

:::

Your key is stored **only** in this browser's `localStorage`, shared across
every page of the site, and sent **only** in an `Authorization: Bearer` header
to `api.fireworks.ai`.

## Serve over HTTP

`localStorage` and JavaScript ES modules are blocked for pages opened from the
local filesystem (`file://`). Preview with `quarto preview`, or serve the
rendered output directory over HTTP.
"#;

/// The complete API key page, ready to save as `api-key.qmd` (the filter's
/// default `bt-key-page` target is `api-key.html`).
pub fn key_page_qmd() -> &'static str {
    KEY_PAGE_QMD
}

/// The minimum fence length for a fenced code block (CommonMark default).
const MIN_FENCE_LEN: usize = 3;

/// Render a [`Lesson`] as a Quarto `.qmd` fenced-div snippet.
///
/// The output is a self-contained block starting with
/// `::: {.blendtutor language="<r|python>"}` and closing with `:::`. Each
/// optional section (code template, checks, solution, hints, gotchas, success
/// criteria) and the
/// `packages` attribute are emitted only when the corresponding field is
/// present, so no empty blocks appear for absent fields (§1.1). The
/// author-only `llm_evaluation_prompt` is excluded (ADR-0006).
///
/// With [`ExportShape::Document`] the div is preceded by YAML front matter
/// (title, the blendtutor filter, and `coi: true` for R).
///
/// # Arguments
/// * `lesson` — A valid, parsed lesson (constructed via [`Lesson::parse`]).
/// * `shape` — Snippet or complete page.
///
/// # Returns
/// A `String` containing the `.qmd` fenced-div snippet, terminated by a
/// newline.
pub fn export_lesson_to_qmd(lesson: &Lesson, shape: ExportShape) -> String {
    let lang = language_tag(&lesson.language);
    let mut out = match shape {
        ExportShape::Snippet => String::new(),
        ExportShape::Document => front_matter(lesson),
    };

    // Opening div with the language attribute, plus the comma-separated
    // packages attribute the Quarto filter splits (`parse_packages`).
    let packages = if lesson.packages.is_empty() {
        String::new()
    } else {
        format!(" packages=\"{}\"", lesson.packages.join(","))
    };
    out.push_str(&format!(
        "::: {{.blendtutor language=\"{lang}\"{packages}}}\n"
    ));

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

    // Gotchas as a fenced div (if present).
    if let Some(ref gotchas) = lesson.exercise.gotchas {
        out.push('\n');
        out.push_str("::: {.gotchas}\n");
        out.push_str(gotchas.trim_end());
        out.push('\n');
        out.push_str(":::\n");
    }

    // Success criteria as a fenced div (if present) — the filter carries them
    // into the feedback prompt (ADR-0020).
    if let Some(ref criteria) = lesson.exercise.success_criteria {
        out.push('\n');
        out.push_str("::: {.success-criteria}\n");
        out.push_str(criteria.trim_end());
        out.push('\n');
        out.push_str(":::\n");
    }

    // Closing div.
    out.push_str(":::\n");

    out
}

/// Warn when `lesson` carries none of the aids that make the Quarto widget more
/// than a Run button: no `checks`, no `solution`, and no `hints`.
///
/// Pure (§2.1): returns the stderr message for the CLI shell to print, or
/// `None` when any aid is present. Authors mistake such a bare widget for a
/// broken install, so the export names exactly what is missing.
pub fn thin_lesson_warning(lesson: &Lesson) -> Option<String> {
    let has_aid = !lesson.checks.is_empty()
        || lesson.exercise.solution.is_some()
        || lesson.exercise.hints.is_some();
    if has_aid {
        return None;
    }
    Some(
        "warning: lesson has no checks, solution, or hints; the exported \
         exercise will offer only Run and LLM feedback"
            .to_string(),
    )
}

/// Render the YAML front matter that makes an exported lesson a standalone page:
/// title, the blendtutor filter, and — for R only — `coi: true` with a note
/// that book projects run R without isolation (ADR-0015, ADR-0019).
fn front_matter(lesson: &Lesson) -> String {
    let title = yaml_double_quoted(&lesson.lesson_name.to_string());
    let coi = match lesson.language {
        Language::R => format!("coi: true\n{R_BOOK_COI_NOTE}"),
        Language::Python => String::new(),
    };
    format!("---\ntitle: {title}\nfilters:\n  - {FILTER_NAME}\n{coi}---\n\n")
}

/// Quote `value` as a YAML double-quoted scalar, escaping backslashes, quotes,
/// and line breaks so author text can never end the scalar early.
fn yaml_double_quoted(value: &str) -> String {
    let escaped = value
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r");
    format!("\"{escaped}\"")
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
        let qmd = export_lesson_to_qmd(&lesson, ExportShape::Snippet);
        assert!(
            qmd.starts_with("::: {.blendtutor language=\"r\"}\n"),
            "should open with the blendtutor div, got:\n{qmd}"
        );
    }

    #[test]
    fn export_closes_with_div_marker() {
        let lesson = Lesson::parse(VALID_YAML).unwrap();
        let qmd = export_lesson_to_qmd(&lesson, ExportShape::Snippet);
        assert!(
            qmd.trim_end().ends_with(":::"),
            "should close with :::, got:\n{qmd}"
        );
    }

    #[test]
    fn export_excludes_llm_evaluation_prompt() {
        let lesson = Lesson::parse(VALID_YAML).unwrap();
        let qmd = export_lesson_to_qmd(&lesson, ExportShape::Snippet);
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
    fn export_renders_gotchas_as_gotchas_div() {
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
        let qmd = export_lesson_to_qmd(&lesson, ExportShape::Snippet);
        assert!(
            qmd.contains("::: {.gotchas}\n- R uses '<-' for assignment.\n:::\n"),
            "gotchas should render as a closed ::: {{.gotchas}} div, got:\n{qmd}"
        );
    }

    #[test]
    fn export_renders_packages_as_comma_separated_attribute() {
        let yaml = r#"
lesson_name: "Pkg"
language: Python
packages:
  - pandas
  - numpy
exercise:
  prompt: "Write add."
  llm_evaluation_prompt: "Grade this: {student_code}"
"#;
        let lesson = Lesson::parse(yaml).unwrap();
        let qmd = export_lesson_to_qmd(&lesson, ExportShape::Snippet);
        assert!(
            qmd.starts_with("::: {.blendtutor language=\"python\" packages=\"pandas,numpy\"}\n"),
            "packages should be a comma-separated div attribute, got:\n{qmd}"
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
        let qmd = export_lesson_to_qmd(&lesson, ExportShape::Snippet);
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
        assert!(
            !qmd.contains("{.gotchas}"),
            "no gotchas div for absent gotchas, got:\n{qmd}"
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
        let qmd = export_lesson_to_qmd(&lesson, ExportShape::Snippet);
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
        let qmd = export_lesson_to_qmd(&lesson, ExportShape::Snippet);
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
            export_lesson_to_qmd(&lesson_a, ExportShape::Snippet),
            export_lesson_to_qmd(&lesson_b, ExportShape::Snippet),
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

    #[test]
    fn document_shape_prefixes_front_matter_and_keeps_the_snippet_intact() {
        let lesson = Lesson::parse(VALID_YAML).unwrap();
        let snippet = export_lesson_to_qmd(&lesson, ExportShape::Snippet);
        let document = export_lesson_to_qmd(&lesson, ExportShape::Document);
        assert_eq!(document, format!("{}{snippet}", front_matter(&lesson)));
    }

    #[test]
    fn front_matter_adds_coi_only_for_r() {
        let r = Lesson::parse(VALID_YAML).unwrap();
        assert!(front_matter(&r).contains("\ncoi: true\n"));
        let py = Lesson::parse(
            "lesson_name: \"Py\"\nlanguage: Python\nexercise:\n  prompt: \"p\"\n  llm_evaluation_prompt: \"{student_code}\"\n",
        )
        .unwrap();
        assert!(!front_matter(&py).contains("coi"));
    }

    #[test]
    fn yaml_double_quoted_escapes_scalar_terminators() {
        assert_eq!(yaml_double_quoted("plain"), "\"plain\"");
        assert_eq!(yaml_double_quoted("a\"b\\c\nd"), "\"a\\\"b\\\\c\\nd\"");
    }

    #[test]
    fn key_page_has_front_matter_and_mount_div() {
        let page = key_page_qmd();
        assert!(
            page.starts_with(
                "---\ntitle: \"API Key\"\nfilters:\n  - mcmullarkey/blendtutor\n---\n"
            )
        );
        assert!(page.contains("\n::: {.blendtutor-key}\n"));
    }

    #[test]
    fn thin_lesson_warning_names_every_missing_aid() {
        let lesson = Lesson::parse(
            "lesson_name: \"Thin\"\nlanguage: R\nexercise:\n  prompt: \"p\"\n  llm_evaluation_prompt: \"{student_code}\"\n",
        )
        .unwrap();
        let warning = thin_lesson_warning(&lesson).expect("a lesson with no aids warns");
        assert!(warning.starts_with("warning:"), "got: {warning}");
        for field in ["checks", "solution", "hints"] {
            assert!(warning.contains(field), "missing `{field}` in: {warning}");
        }
    }

    #[test]
    fn thin_lesson_warning_is_silent_when_any_aid_is_present() {
        for aid in ["checks:\n  - \"stopifnot(TRUE)\"\n", ""] {
            let extra_exercise = if aid.is_empty() {
                "  hints: |\n    - Try it.\n"
            } else {
                ""
            };
            let yaml = format!(
                "lesson_name: \"Aided\"\nlanguage: R\n{aid}exercise:\n  prompt: \"p\"\n{extra_exercise}  llm_evaluation_prompt: \"{{student_code}}\"\n"
            );
            let lesson = Lesson::parse(&yaml).unwrap();
            assert_eq!(thin_lesson_warning(&lesson), None, "yaml:\n{yaml}");
        }
        let solved = Lesson::parse(VALID_YAML).unwrap();
        assert_eq!(thin_lesson_warning(&solved), None);
    }

    #[test]
    fn export_renders_success_criteria_as_div() {
        let yaml = r#"
lesson_name: "Rubric"
language: R
exercise:
  prompt: "Write pseudocode."
  success_criteria: |
    - Uses only comments
  llm_evaluation_prompt: "Grade this: {student_code}"
"#;
        let lesson = Lesson::parse(yaml).unwrap();
        let qmd = export_lesson_to_qmd(&lesson, ExportShape::Snippet);
        assert!(
            qmd.contains("::: {.success-criteria}\n- Uses only comments\n:::\n"),
            "success criteria should render as a closed div, got:\n{qmd}"
        );
        let bare = Lesson::parse(VALID_YAML).unwrap();
        assert!(!export_lesson_to_qmd(&bare, ExportShape::Snippet).contains("success-criteria"));
    }
}
