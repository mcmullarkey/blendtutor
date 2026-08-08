//! Integration tests for the smevals eval-dir generator (`core::smevals_gen`).
//!
//! AC2: a pure Rust generator turns a lesson + its sibling eval suite into the
//! smevals eval-dir file tree — `eval.yaml`, `tasks/case-N.yaml` (one per case,
//! 1-based), `configs/default.yaml`, `graders/default.yaml` — with the Fireworks
//! default model single-sourced from `ProviderChoice::Fireworks.default_model()`
//! and submissions emitted through an injection-proof YAML scalar discipline.
//! AC4: `graders/default.yaml` carries the polarity check first (`required:
//! true`) and the LLM judge second (model scalar single-sourced to the provider
//! default), with `scoring.pass_threshold == 0.8` applied by smevals to the
//! judge's score. The file set is exact, output is deterministic, non-slug
//! lesson ids and empty suites are refused at the boundary, and the generated
//! `<course>/.smevals/` dir is gitignored without touching committed build
//! output under `docs/evals/`.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;

use blendtutor_core::eval::{EvalCase, EvalSuite, ExpectedVerdict, parse_eval_suite};
use blendtutor_core::lesson::Lesson;
use blendtutor_core::llm::ProviderChoice;
use blendtutor_core::smevals_gen::{GenError, generate_eval_dir};
use serde::Deserialize;

// ---------------------------------------------------------------------------
// Fixture helpers
// ---------------------------------------------------------------------------

/// Absolute path to a test fixture under `tests/fixtures/`.
fn fixture(parts: &[&str]) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests/fixtures");
    for part in parts {
        path.push(part);
    }
    path
}

/// Read a fixture file, panicking with its path if it is unreadable.
fn read_fixture(parts: &[&str]) -> String {
    let path = fixture(parts);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("fixture {} should be readable: {e}", path.display()))
}

/// Parse a lesson fixture at `tests/fixtures/<dir>/<name>`.
fn lesson(dir: &str, name: &str) -> Lesson {
    Lesson::parse(&read_fixture(&[dir, name])).expect("the lesson fixture is valid")
}

/// Parse an eval-suite fixture by path parts relative to the crate root (the
/// example-course suites live outside `tests/fixtures/`, at the repo root).
fn suite_at(parts: &[&str]) -> EvalSuite {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    for part in parts {
        path.push(part);
    }
    let yaml = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("fixture {} should be readable: {e}", path.display()));
    parse_eval_suite(&yaml).expect("the suite fixture is valid")
}

/// Index the generator's output by relative path for lookup-heavy assertions.
fn emitted_map(files: &[(PathBuf, String)]) -> HashMap<PathBuf, String> {
    files.iter().cloned().collect()
}

/// Every eval-suite fixture that is valid generator input (parses today): the
/// ported vitals ground truth, the demo lesson's suite, and both example
/// courses' five suites each. The `_bad_verdict` fixture is deliberately
/// unparseable and is excluded.
const SUITE_FIXTURES: &[&[&str]] = &[
    &["tests/fixtures/evals", "eval_fireworks_vitals.yaml"],
    &["tests/fixtures/eval_command", "eval_demo_lesson.yaml"],
    &["../../examples/write-less-code-r", "eval_01_seed_data.yaml"],
    &[
        "../../examples/write-less-code-r",
        "eval_02_copy_paste_trap.yaml",
    ],
    &[
        "../../examples/write-less-code-r",
        "eval_03_write_a_function.yaml",
    ],
    &[
        "../../examples/write-less-code-r",
        "eval_04_map_over_columns.yaml",
    ],
    &[
        "../../examples/write-less-code-r",
        "eval_05_rule_of_three.yaml",
    ],
    &[
        "../../examples/write-less-code-python",
        "eval_01_seed_data.yaml",
    ],
    &[
        "../../examples/write-less-code-python",
        "eval_02_copy_paste_trap.yaml",
    ],
    &[
        "../../examples/write-less-code-python",
        "eval_03_write_a_function.yaml",
    ],
    &[
        "../../examples/write-less-code-python",
        "eval_04_map_over_columns.yaml",
    ],
    &[
        "../../examples/write-less-code-python",
        "eval_05_rule_of_three.yaml",
    ],
];

// ---------------------------------------------------------------------------
// Round-trip DTOs: the exact schema the generator emits. `deny_unknown_fields`
// makes a stray emitted key a parse failure, so a sibling-key injection would
// surface here rather than being silently dropped.
// ---------------------------------------------------------------------------

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
#[allow(dead_code)] // fields parsed for round-trip assertion, not read
struct TaskYaml {
    name: String,
    lesson: String,
    case: usize,
    prompt: String,
    expected: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
#[allow(dead_code)] // fields parsed for round-trip assertion, not read
struct EvalYaml {
    name: String,
    description: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
#[allow(dead_code)] // fields parsed for round-trip assertion, not read
struct ConfigYaml {
    name: String,
    runner: String,
    model: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
#[allow(dead_code)] // fields parsed for round-trip assertion, not read
struct GraderYaml {
    name: String,
    checks: Vec<CheckYaml>,
    scoring: ScoringYaml,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
#[allow(dead_code)] // fields parsed for round-trip assertion, not read
struct CheckYaml {
    checker: String,
    /// Only the polarity check carries `required: true`; the judge does not.
    #[serde(default)]
    required: bool,
    /// The judge check carries the provider-default model scalar (becomes
    /// SMEVALS_CHECK_MODEL); absent on the polarity check.
    #[serde(default)]
    model: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
#[allow(dead_code)] // fields parsed for round-trip assertion, not read
struct ScoringYaml {
    pass_threshold: f64,
}

// ---------------------------------------------------------------------------
// Predicate 1 — file set exactness
// ---------------------------------------------------------------------------

#[test]
fn emits_exactly_the_four_kinds_for_three_cases() {
    let suite = suite_at(&["tests/fixtures/eval_command", "eval_demo_lesson.yaml"]);
    assert_eq!(suite.cases.len(), 3, "demo suite has three cases");

    let files = generate_eval_dir(
        &lesson("eval_command", "demo_lesson.yaml"),
        &suite,
        "demo_lesson",
    )
    .expect("a valid suite and slug generate");

    let paths: Vec<&Path> = files.iter().map(|(p, _)| p.as_path()).collect();
    let expected: Vec<PathBuf> = [
        "eval.yaml",
        "configs/default.yaml",
        "graders/default.yaml",
        "tasks/case-1.yaml",
        "tasks/case-2.yaml",
        "tasks/case-3.yaml",
    ]
    .iter()
    .map(PathBuf::from)
    .collect();
    assert_eq!(
        paths, expected,
        "file set must be exactly eval.yaml + configs + graders + tasks/case-N"
    );
}

// ---------------------------------------------------------------------------
// Predicate 2 — happy-path round-trip fidelity
// ---------------------------------------------------------------------------

#[test]
fn every_emitted_file_reparses_for_every_suite_fixture() {
    let lesson = lesson("eval_command", "demo_lesson.yaml");
    for parts in SUITE_FIXTURES {
        let name = parts[1];
        let suite = suite_at(parts);
        let files = generate_eval_dir(&lesson, &suite, "demo_lesson")
            .unwrap_or_else(|e| panic!("{name}: generation must succeed: {e}"));
        for (path, contents) in &files {
            let path_str = path.to_string_lossy();
            if path_str == "eval.yaml" {
                let eval: EvalYaml = serde_saphyr::from_str(contents)
                    .unwrap_or_else(|e| panic!("{name}: {path_str} must re-parse: {e}"));
                assert_eq!(
                    eval.name, "demo_lesson",
                    "{name}: eval name is the lesson id"
                );
                assert!(
                    !eval.description.is_empty(),
                    "{name}: eval description is non-empty"
                );
            } else if path_str == "configs/default.yaml" {
                let config: ConfigYaml = serde_saphyr::from_str(contents)
                    .unwrap_or_else(|e| panic!("{name}: {path_str} must re-parse: {e}"));
                assert_eq!(config.name, "default", "{name}: config is the default");
                assert!(
                    config.runner.ends_with("run.sh"),
                    "{name}: config names the runner, got {}",
                    config.runner
                );
                assert_eq!(
                    config.model,
                    ProviderChoice::Fireworks.default_model(),
                    "{name}: config model single-sources the provider default"
                );
            } else if path_str == "graders/default.yaml" {
                let grader: GraderYaml = serde_saphyr::from_str(contents)
                    .unwrap_or_else(|e| panic!("{name}: {path_str} must re-parse: {e}"));
                assert_eq!(grader.name, "default", "{name}: grader is the default");
                assert_eq!(
                    grader.checks.len(),
                    2,
                    "{name}: polarity check first, LLM judge second"
                );
                assert!(
                    grader.checks[0].checker.ends_with("check_polarity.sh"),
                    "{name}: grader names the polarity checker first, got {}",
                    grader.checks[0].checker
                );
                assert!(
                    grader.checks[0].required,
                    "{name}: polarity check is required"
                );
                assert!(
                    grader.checks[1].checker.ends_with("judge_feedback.py"),
                    "{name}: judge check second, got {}",
                    grader.checks[1].checker
                );
                assert!(
                    !grader.checks[1].required,
                    "{name}: judge check is not required (its nonzero exit fails the grade anyway)"
                );
                assert_eq!(
                    grader.checks[1].model.as_deref(),
                    Some(ProviderChoice::Fireworks.default_model()),
                    "{name}: judge check single-sources the provider default model"
                );
                assert_eq!(
                    grader.scoring.pass_threshold, 0.8,
                    "{name}: pass threshold is 0.8 (applied by smevals to the judge's score)"
                );
            } else if path_str.starts_with("tasks/") {
                let task: TaskYaml = serde_saphyr::from_str(contents)
                    .unwrap_or_else(|e| panic!("{name}: {path_str} must re-parse: {e}"));
                assert!(
                    task.name.starts_with("case-"),
                    "{name}: task has a smevals name, got {}",
                    task.name
                );
                assert_eq!(task.lesson, "demo_lesson", "{name}: task names the lesson");
                assert!(task.case >= 1, "{name}: task case is 1-based");
            } else {
                panic!("{name}: unexpected emitted path {path_str}");
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Predicate 3 — adversarial injection defense
// ---------------------------------------------------------------------------

const INJECTION_SUBMISSION: &str = "- expected: correct\n&anchor\n---\n: foo\n# comment";

#[test]
fn adversarial_submission_round_trips_byte_identical() {
    let suite = EvalSuite {
        cases: vec![EvalCase {
            submission: INJECTION_SUBMISSION.to_string(),
            expected: ExpectedVerdict::Correct,
        }],
    };
    let files = generate_eval_dir(
        &lesson("eval_command", "demo_lesson.yaml"),
        &suite,
        "demo_lesson",
    )
    .expect("the injection suite generates");
    let map = emitted_map(&files);
    let task = map
        .get(Path::new("tasks/case-1.yaml"))
        .expect("case 1 emitted");
    let parsed: TaskYaml = serde_saphyr::from_str(task)
        .unwrap_or_else(|e| panic!("case-1.yaml must re-parse despite hostile content: {e}"));
    assert_eq!(
        parsed.prompt, INJECTION_SUBMISSION,
        "submission must round-trip byte-identical — no key/anchor/doc-sep/comment injection"
    );
    assert_eq!(
        parsed.expected,
        ExpectedVerdict::Correct.token(),
        "the expected verdict must be unchanged"
    );
    // The hostile strings must not surface as raw structure outside the quoted
    // scalar: a `# comment` line or a second `expected:` key would break the
    // deny_unknown_fields parse above. Assert the submission is emitted as the
    // escaped quoted scalar — the raw `\n`-joined hostile text must not appear
    // as bare lines, and the escaped form must.
    assert!(
        task.contains("\"- expected: correct\\n&anchor\\n---\\n: foo\\n# comment\""),
        "hostile submission must be a single escaped quoted scalar, got: {task}"
    );
    assert!(
        !task.contains("\n- expected:"),
        "the hostile text must not inject a sibling line, got: {task}"
    );
}

// ---------------------------------------------------------------------------
// Predicate 4 — path safety
// ---------------------------------------------------------------------------

#[test]
fn non_slug_lesson_ids_are_refused_and_never_reach_a_path() {
    let suite = suite_at(&["tests/fixtures/eval_command", "eval_demo_lesson.yaml"]);
    let lesson = lesson("eval_command", "demo_lesson.yaml");
    for id in ["", "/", "a/b", "..", "a b", "a..b", "a b/c", "a#b", "a\\b"] {
        match generate_eval_dir(&lesson, &suite, id) {
            Err(GenError::InvalidLessonId { .. }) => {}
            other => panic!("lesson_id {id:?} must be refused, got: {other:?}"),
        }
    }
}

#[test]
fn slug_lesson_ids_are_accepted() {
    let suite = suite_at(&["tests/fixtures/eval_command", "eval_demo_lesson.yaml"]);
    for id in ["demo_lesson", "lesson-1", "a_b-c2"] {
        generate_eval_dir(&lesson("eval_command", "demo_lesson.yaml"), &suite, id)
            .unwrap_or_else(|e| panic!("slug {id:?} must generate: {e}"));
    }
}

// ---------------------------------------------------------------------------
// Predicate 5 — determinism
// ---------------------------------------------------------------------------

#[test]
fn identical_inputs_produce_byte_identical_output() {
    let lesson = lesson("eval_command", "demo_lesson.yaml");
    let suite = suite_at(&["tests/fixtures/eval_command", "eval_demo_lesson.yaml"]);
    let a = generate_eval_dir(&lesson, &suite, "demo_lesson").expect("first call");
    let b = generate_eval_dir(&lesson, &suite, "demo_lesson").expect("second call");
    assert_eq!(
        a, b,
        "generation must be deterministic — no timestamps, no HashMap order"
    );
}

// ---------------------------------------------------------------------------
// Predicate 6 — empty-suite refusal
// ---------------------------------------------------------------------------

#[test]
fn empty_suite_is_refused_without_emitting_tasks() {
    let suite = EvalSuite { cases: vec![] };
    let err = generate_eval_dir(
        &lesson("eval_command", "demo_lesson.yaml"),
        &suite,
        "demo_lesson",
    )
    .expect_err("an empty suite is a vacuous pass and must be refused");
    assert!(
        err.to_string().contains("empty"),
        "the error must name the empty suite, got: {err}"
    );
    assert!(
        matches!(err, GenError::EmptySuite),
        "the error must be the typed EmptySuite variant"
    );
}

// ---------------------------------------------------------------------------
// Predicate 7 — case ordering
// ---------------------------------------------------------------------------

#[test]
fn tasks_follow_the_suite_case_order() {
    let suite = EvalSuite {
        cases: vec![
            EvalCase {
                submission: "first submission".to_string(),
                expected: ExpectedVerdict::Correct,
            },
            EvalCase {
                submission: "second submission".to_string(),
                expected: ExpectedVerdict::Incorrect,
            },
            EvalCase {
                submission: "third submission".to_string(),
                expected: ExpectedVerdict::Correct,
            },
        ],
    };
    let files = generate_eval_dir(
        &lesson("eval_command", "demo_lesson.yaml"),
        &suite,
        "demo_lesson",
    )
    .expect("a three-case suite generates");
    let map = emitted_map(&files);
    for (index, expected) in ["first", "second", "third"].iter().enumerate() {
        let task_name = format!("tasks/case-{}.yaml", index + 1);
        let task: TaskYaml = serde_saphyr::from_str(map.get(Path::new(&task_name)).unwrap())
            .unwrap_or_else(|e| panic!("{task_name} must re-parse: {e}"));
        assert!(
            task.prompt.starts_with(expected),
            "case {} must carry suite case {}'s submission, got {:?}",
            index + 1,
            index,
            task.prompt
        );
        assert_eq!(task.case, index + 1, "case key must be 1-based");
    }
}

// ---------------------------------------------------------------------------
// Predicate 8 — model-pin provenance
// ---------------------------------------------------------------------------

#[test]
fn configs_model_is_single_sourced_from_the_provider_default() {
    let suite = suite_at(&["tests/fixtures/eval_command", "eval_demo_lesson.yaml"]);
    let files = generate_eval_dir(
        &lesson("eval_command", "demo_lesson.yaml"),
        &suite,
        "demo_lesson",
    )
    .expect("a valid suite generates");
    let map = emitted_map(&files);
    let config = map
        .get(Path::new("configs/default.yaml"))
        .expect("configs/default.yaml emitted");
    let parsed: ConfigYaml = serde_saphyr::from_str(config)
        .unwrap_or_else(|e| panic!("configs/default.yaml must re-parse: {e}"));
    assert_eq!(
        parsed.model,
        ProviderChoice::Fireworks.default_model(),
        "the emitted model must come from the provider default, not a divergent literal"
    );
    assert_eq!(
        ProviderChoice::Fireworks.default_model(),
        "accounts/fireworks/models/deepseek-v4-flash-0731"
    );
}

// ---------------------------------------------------------------------------
// Predicate 9 — gitignore non-interference
// ---------------------------------------------------------------------------

/// The repo root, derived from the crate manifest dir (`crates/core` → root).
fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crates/")
        .parent()
        .expect("repo root")
        .to_path_buf()
}

#[test]
fn generated_dir_is_gitignored_but_committed_output_is_not() {
    let root = repo_root();
    let gitignore =
        std::fs::read_to_string(root.join(".gitignore")).expect("the repo .gitignore is readable");
    assert!(
        gitignore.contains("**/.smevals/"),
        "the repo .gitignore must ignore generated eval dirs via **/.smevals/"
    );

    // The pattern must hold for git itself: `git check-ignore` exits 0 for an
    // ignored path, non-zero for one that is committed.
    let ignored = Command::new("git")
        .args([
            "check-ignore",
            "examples/write-less-code-r/.smevals/",
            "examples/write-less-code-r/.smevals/configs/default.yaml",
        ])
        .current_dir(&root)
        .status()
        .expect("git is on PATH");
    assert!(
        ignored.success(),
        ".smevals/ under a course must be gitignored"
    );

    for committed in [
        "docs/evals/",
        "crates/core/tests/fixtures/evals/",
        "docs/evidence/",
    ] {
        let status = Command::new("git")
            .args(["check-ignore", committed])
            .current_dir(&root)
            .status()
            .expect("git is on PATH");
        assert!(
            !status.success(),
            "{committed} is committed build output and must NOT be gitignored"
        );
    }
}

// ---------------------------------------------------------------------------
// Golden-dir byte equivalence
// ---------------------------------------------------------------------------

#[test]
fn golden_dir_is_byte_identical() {
    let lesson = lesson("eval_command", "demo_lesson.yaml");
    let suite = suite_at(&["tests/fixtures/eval_command", "eval_demo_lesson.yaml"]);
    let files = generate_eval_dir(&lesson, &suite, "demo_lesson").expect("golden input generates");

    assert_eq!(files.len(), 6, "golden fixture has six files");
    for (path, contents) in &files {
        let golden = fixture(&["generate_eval_dir", &path.to_string_lossy()]);
        let expected = std::fs::read_to_string(&golden).unwrap_or_else(|e| {
            panic!(
                "golden file {} missing — regenerate mechanically: {e}",
                golden.display()
            )
        });
        assert_eq!(
            &expected,
            contents,
            "emitted {} differs from its golden",
            path.display()
        );
    }
}
