//! Integration tests for succinct prompts + expandable hints and gotchas
//! across both example courses.
//!
//! Validates that every lesson in `examples/write-less-code-python/` and
//! `examples/write-less-code-r/`:
//!   1. Validates cleanly (`blendtutor validate` exits 0)
//!   2. Carries non-empty `hints` AND `gotchas` fields, each ≥100 characters
//!   3. Both fields are bullet-formatted (each non-empty line starts with
//!      `- ` or `* `)
//!   4. Both fields contain at least one structural marker (gotcha, tip, etc.)
//!   5. Neither field leaks the reference solution (anti-spoiler)
//!   6. Neither field is a copy of the prompt (non-dupe)
//!   7. Cross-field identity: hints ≠ gotchas (not identical text)
//!   8. Copy-paste-trap lessons retain the literal `stress_6` in the prompt
//!   9. The `{student_code}` placeholder is retained in `llm_evaluation_prompt`
//!
//! Plus a build gate: `blendtutor build` on each course emits lesson JSON
//! whose `hints` AND `gotchas` fields are non-null.

mod common;

use std::path::{Path, PathBuf};

use assert_cmd::Command;
use blendtutor_core::lesson::{Lesson, read_lesson_file};
use serde_json::Value;

/// The Python example course directory, relative to the CLI crate.
const PYTHON_COURSE_DIR: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../examples/write-less-code-python"
);

/// The R example course directory, relative to the CLI crate.
const R_COURSE_DIR: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../examples/write-less-code-r"
);

/// The 5 lesson file names (shared by both courses in manifest order).
const LESSON_FILES: &[&str] = &[
    "01_seed_data.yaml",
    "02_copy_paste_trap.yaml",
    "03_write_a_function.yaml",
    "04_map_over_columns.yaml",
    "05_rule_of_three.yaml",
];

/// Structural markers that hints/gotchas must contain at least one of.
const STRUCTURAL_MARKERS: &[&str] = &[
    "gotcha",
    "watch out",
    "common mistake",
    "troubleshoot",
    "tip",
    "note",
    "important",
    "remember",
    "hint",
];

/// Minimum length for hints/gotchas content.
const MIN_CONTENT_LEN: usize = 100;

fn lesson_path(course_dir: &str, filename: &str) -> PathBuf {
    Path::new(course_dir).join(filename)
}

fn load_lesson(course_dir: &str, filename: &str) -> Lesson {
    read_lesson_file(&lesson_path(course_dir, filename))
        .unwrap_or_else(|e| panic!("lesson {filename} should parse: {e}"))
}

/// Check that every non-empty line starts with `- ` or `* ` (bullet format).
fn is_bullet_formatted(content: &str) -> bool {
    content
        .lines()
        .all(|line| line.is_empty() || line.starts_with("- ") || line.starts_with("* "))
}

/// Assert that no solution fragment (≥15 chars) appears in the given content
/// field (anti-spoiler).
fn assert_no_solution_leak(
    course_name: &str,
    idx: usize,
    filename: &str,
    field_name: &str,
    content: &str,
    lesson: &Lesson,
) {
    if let Some(solution) = lesson.exercise.solution.as_deref() {
        let solution_fragments: Vec<&str> = solution
            .lines()
            .filter(|l| {
                let t = l.trim();
                !t.is_empty()
                    && !t.starts_with('#')
                    && !t.starts_with("import ")
                    && !t.starts_with("library(")
                    && !t.starts_with("survey_data <- data.frame")
                    && !t.starts_with("survey_data = pd.DataFrame")
            })
            .collect();
        for frag in &solution_fragments {
            let frag_trimmed = frag.trim();
            // Skip very short fragments (variable names, single tokens) that
            // hints/gotchas legitimately reference.
            if frag_trimmed.len() < 15 {
                continue;
            }
            assert!(
                !content.contains(frag_trimmed),
                "{course_name} lesson {idx} ({filename}) {field_name} must not leak solution code \
                 fragment: {frag_trimmed:?}"
            );
        }
    }
}

/// Assert all hints-and-gotchas content invariants for a single lesson.
fn assert_hints_and_gotchas_invariants(
    course_name: &str,
    idx: usize,
    filename: &str,
    lesson: &Lesson,
) {
    let hints = lesson
        .exercise
        .hints
        .as_deref()
        .unwrap_or_else(|| panic!("{course_name} lesson {idx} ({filename}) should have hints"));

    let gotchas =
        lesson.exercise.gotchas.as_deref().unwrap_or_else(|| {
            panic!("{course_name} lesson {idx} ({filename}) should have gotchas")
        });

    // Invariant: hints non-empty.
    assert!(
        !hints.trim().is_empty(),
        "{course_name} lesson {idx} ({filename}) hints should be non-empty"
    );

    // Invariant: gotchas non-empty.
    assert!(
        !gotchas.trim().is_empty(),
        "{course_name} lesson {idx} ({filename}) gotchas should be non-empty"
    );

    // Invariant: hints at least 100 characters.
    assert!(
        hints.len() >= MIN_CONTENT_LEN,
        "{course_name} lesson {idx} ({filename}) hints should be >= {MIN_CONTENT_LEN} chars, got {}: {hints:?}",
        hints.len()
    );

    // Invariant: gotchas at least 100 characters.
    assert!(
        gotchas.len() >= MIN_CONTENT_LEN,
        "{course_name} lesson {idx} ({filename}) gotchas should be >= {MIN_CONTENT_LEN} chars, got {}: {gotchas:?}",
        gotchas.len()
    );

    // Invariant: hints bullet-formatted.
    assert!(
        is_bullet_formatted(hints),
        "{course_name} lesson {idx} ({filename}) hints should be bullet-formatted: {hints:?}"
    );

    // Invariant: gotchas bullet-formatted.
    assert!(
        is_bullet_formatted(gotchas),
        "{course_name} lesson {idx} ({filename}) gotchas should be bullet-formatted: {gotchas:?}"
    );

    // Invariant: at least one structural marker in hints.
    let hints_lower = hints.to_lowercase();
    let hints_has_marker = STRUCTURAL_MARKERS.iter().any(|m| hints_lower.contains(m));
    assert!(
        hints_has_marker,
        "{course_name} lesson {idx} ({filename}) hints should contain a structural marker \
         (one of: {STRUCTURAL_MARKERS:?}), got: {hints:?}"
    );

    // Invariant: at least one structural marker in gotchas.
    let gotchas_lower = gotchas.to_lowercase();
    let gotchas_has_marker = STRUCTURAL_MARKERS.iter().any(|m| gotchas_lower.contains(m));
    assert!(
        gotchas_has_marker,
        "{course_name} lesson {idx} ({filename}) gotchas should contain a structural marker \
         (one of: {STRUCTURAL_MARKERS:?}), got: {gotchas:?}"
    );

    // Invariant: solution not leaked into hints (anti-spoiler).
    assert_no_solution_leak(course_name, idx, filename, "hints", hints, lesson);

    // Invariant: solution not leaked into gotchas (anti-spoiler).
    assert_no_solution_leak(course_name, idx, filename, "gotchas", gotchas, lesson);

    // Invariant: hints are not a copy of the prompt (non-dupe).
    assert!(
        !hints.contains(&lesson.exercise.prompt),
        "{course_name} lesson {idx} ({filename}) hints must not be a copy of the prompt"
    );

    // Invariant: gotchas are not a copy of the prompt (non-dupe).
    assert!(
        !gotchas.contains(&lesson.exercise.prompt),
        "{course_name} lesson {idx} ({filename}) gotchas must not be a copy of the prompt"
    );

    // Invariant: cross-field identity — hints ≠ gotchas.
    assert!(
        hints.trim() != gotchas.trim(),
        "{course_name} lesson {idx} ({filename}) hints and gotchas must not be identical"
    );

    // Invariant: {student_code} placeholder retained in llm_evaluation_prompt.
    assert!(
        lesson
            .exercise
            .llm_evaluation_prompt
            .contains("{student_code}"),
        "{course_name} lesson {idx} ({filename}) should retain the {{student_code}} placeholder"
    );
}

// ─── Python course: validate + hints/gotchas invariants ──────────────────────

#[test]
fn python_validate_each_lesson_exits_zero() {
    for filename in LESSON_FILES {
        let path = lesson_path(PYTHON_COURSE_DIR, filename);
        Command::cargo_bin("blendtutor")
            .unwrap()
            .arg("validate")
            .arg(&path)
            .assert()
            .success();
    }
}

#[test]
fn python_hints_and_gotchas_satisfy_all_invariants() {
    for (idx, filename) in LESSON_FILES.iter().enumerate() {
        let lesson = load_lesson(PYTHON_COURSE_DIR, filename);
        assert_hints_and_gotchas_invariants("Python", idx, filename, &lesson);
    }
}

#[test]
fn python_lesson_2_prompt_retains_stress_6() {
    let lesson = load_lesson(PYTHON_COURSE_DIR, LESSON_FILES[1]);
    assert!(
        lesson.exercise.prompt.contains("stress_6"),
        "Python lesson 2 prompt should retain the literal 'stress_6': {:?}",
        lesson.exercise.prompt
    );
}

// ─── R course: validate + hints/gotchas invariants ───────────────────────────

#[test]
fn r_validate_each_lesson_exits_zero() {
    for filename in LESSON_FILES {
        let path = lesson_path(R_COURSE_DIR, filename);
        Command::cargo_bin("blendtutor")
            .unwrap()
            .arg("validate")
            .arg(&path)
            .assert()
            .success();
    }
}

#[test]
fn r_hints_and_gotchas_satisfy_all_invariants() {
    for (idx, filename) in LESSON_FILES.iter().enumerate() {
        let lesson = load_lesson(R_COURSE_DIR, filename);
        assert_hints_and_gotchas_invariants("R", idx, filename, &lesson);
    }
}

#[test]
fn r_lesson_2_prompt_retains_stress_6() {
    let lesson = load_lesson(R_COURSE_DIR, LESSON_FILES[1]);
    assert!(
        lesson.exercise.prompt.contains("stress_6"),
        "R lesson 2 prompt should retain the literal 'stress_6': {:?}",
        lesson.exercise.prompt
    );
}

// ─── Build gate: lesson JSON carries non-null hints AND gotchas ─────────────

/// Run `build --target <target> <course> -o <out>` via the built binary.
fn build(target: &str, course: &str, out: &Path) -> std::process::Output {
    Command::cargo_bin("blendtutor")
        .unwrap()
        .arg("build")
        .arg("--target")
        .arg(target)
        .arg(course)
        .arg("-o")
        .arg(out)
        .output()
        .unwrap()
}

/// Assert every lesson JSON in a built site carries non-null `hints` AND
/// `gotchas` fields.
fn assert_built_lessons_carry_hints_and_gotchas(out: &Path, course_name: &str) {
    for i in 0..LESSON_FILES.len() {
        let json_path = out.join(format!("lessons/{i}.json"));
        let text = std::fs::read_to_string(&json_path)
            .unwrap_or_else(|e| panic!("{course_name} lessons/{i}.json should exist: {e}"));
        let lesson: Value = serde_json::from_str(&text)
            .unwrap_or_else(|e| panic!("{course_name} lessons/{i}.json should parse: {e}"));

        // hints key present and non-null.
        assert!(
            lesson.get("hints").is_some(),
            "{course_name} lesson {i} JSON must carry a 'hints' key (not absent): {lesson}"
        );
        assert!(
            !lesson["hints"].is_null(),
            "{course_name} lesson {i} JSON hints must be non-null: {lesson}"
        );
        let hints_str = lesson["hints"].as_str().unwrap_or_else(|| {
            panic!("{course_name} lesson {i} JSON hints should be a string: {lesson}")
        });
        assert!(
            !hints_str.is_empty(),
            "{course_name} lesson {i} JSON hints should be non-empty: {lesson}"
        );

        // gotchas key present and non-null.
        assert!(
            lesson.get("gotchas").is_some(),
            "{course_name} lesson {i} JSON must carry a 'gotchas' key (not absent): {lesson}"
        );
        assert!(
            !lesson["gotchas"].is_null(),
            "{course_name} lesson {i} JSON gotchas must be non-null: {lesson}"
        );
        let gotchas_str = lesson["gotchas"].as_str().unwrap_or_else(|| {
            panic!("{course_name} lesson {i} JSON gotchas should be a string: {lesson}")
        });
        assert!(
            !gotchas_str.is_empty(),
            "{course_name} lesson {i} JSON gotchas should be non-empty: {lesson}"
        );
    }
}

#[test]
fn build_python_course_emits_non_null_hints_and_gotchas() {
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build("pyodide", PYTHON_COURSE_DIR, &out);
    assert!(
        output.status.success(),
        "Python build should exit 0, got {:?}; stderr={:?}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    assert_built_lessons_carry_hints_and_gotchas(&out, "Python");
}

#[test]
fn build_r_course_emits_non_null_hints_and_gotchas() {
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build("webr", R_COURSE_DIR, &out);
    assert!(
        output.status.success(),
        "R build should exit 0, got {:?}; stderr={:?}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    assert_built_lessons_carry_hints_and_gotchas(&out, "R");
}
