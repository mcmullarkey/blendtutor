//! Integration tests for `blendtutor eval-report`.
//!
//! Drives the built binary against a fake `uvx` shim injected on PATH. The
//! shim records each invocation's argv verbatim and scripts per-call exit codes
//! and artifacts, so the command's external contract — smevals pinned in
//! package-spec position, absolute paths only, no `--runs-dir`, stage-named
//! failures, and grade-fail-is-evidence exit semantics — is asserted without
//! touching the network or a real model. The `uvx` argv shape is additionally
//! pinned by the real-package manual smoke (docs/evidence/198/), which these
//! tests stand in for in CI.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Output;

use assert_cmd::Command;

/// The demo lesson + sibling suite fixture under `core`'s fixtures — copied into
/// a throwaway course so a `blendtutor.toml` is discoverable above the lesson.
const DEMO_LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_command/demo_lesson.yaml"
);
const DEMO_SUITE: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_command/eval_demo_lesson.yaml"
);

/// The fake `uvx`: records argv verbatim (one `arg:` line per argument, framed
/// by `call:`/`end-call`), scripts an exit code per call index
/// (`FAKE_UVX_EXIT_1`/`FAKE_UVX_EXIT_2`), and fabricates the artifacts a real
/// smevals run/build would leave: `runs/` for `run` when `FAKE_UVX_MAKE_RUNS=1`,
/// and a build output directory with `index.html` for a *successful* `build`
/// (a partial `PARTIAL.txt` regardless, so the failure path's temp cleanup is
/// asserted against a non-vacuous temp dir).
const SHIM: &str = r#"#!/bin/sh
FAKE_UVX_COUNT="${FAKE_UVX_COUNT:?FAKE_UVX_COUNT is required}"
FAKE_UVX_LOG="${FAKE_UVX_LOG:?FAKE_UVX_LOG is required}"

count=0
if [ -f "$FAKE_UVX_COUNT" ]; then count=$(cat "$FAKE_UVX_COUNT"); fi
count=$((count + 1))
printf '%s' "$count" > "$FAKE_UVX_COUNT"

{
  printf 'call: %s\n' "$count"
  for arg in "$@"; do printf 'arg: %s\n' "$arg"; done
  printf 'end-call\n'
} >> "$FAKE_UVX_LOG"

case "$count" in
  1) code="${FAKE_UVX_EXIT_1:-0}" ;;
  2) code="${FAKE_UVX_EXIT_2:-0}" ;;
  *) code="${FAKE_UVX_EXIT:-0}" ;;
esac

case "$2" in
  run)
    if [ "${FAKE_UVX_MAKE_RUNS:-0}" = "1" ]; then
      mkdir -p "$3/runs"
      : > "$3/runs/run-1.json"
    fi
    ;;
  build)
    mkdir -p "$5"
    printf 'partial\n' > "$5/PARTIAL.txt"
    if [ "$code" = "0" ]; then
      printf '<h1>fake smevals report</h1>\n' > "$5/index.html"
    fi
    ;;
esac

exit "$code"
"#;

/// A throwaway git repo with a course (manifest + lesson + suite), a fake-uvx
/// bin dir, and an empty `docs/` — the layout `eval-report` walks up to.
struct Harness {
    root: tempfile::TempDir,
}

impl Harness {
    fn new() -> Self {
        let root = tempfile::tempdir().expect("a tempdir for the fake repo");
        // The repo-root boundary: like the real repo, a `.git` dir marks it,
        // and `docs/evals/` lives just below.
        fs::create_dir_all(root.path().join(".git")).unwrap();
        let course = root.path().join("examples").join("demo-course");
        fs::create_dir_all(&course).unwrap();
        fs::write(
            course.join("blendtutor.toml"),
            "[[lessons]]\nid = \"demo\"\npath = \"demo_lesson.yaml\"\n",
        )
        .unwrap();
        fs::copy(DEMO_LESSON, course.join("demo_lesson.yaml")).unwrap();
        fs::copy(DEMO_SUITE, course.join("eval_demo_lesson.yaml")).unwrap();

        let bin = root.path().join("bin");
        fs::create_dir_all(&bin).unwrap();
        let shim = bin.join("uvx");
        fs::write(&shim, SHIM).unwrap();
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            fs::set_permissions(&shim, fs::Permissions::from_mode(0o755)).unwrap();
        }
        Harness { root }
    }

    /// The tempfile root, canonicalized so it matches the absolute paths the
    /// command logs (macOS resolves `/var` → `/private/var`).
    fn root_canonical(&self) -> PathBuf {
        self.root.path().canonicalize().unwrap()
    }

    fn course_root(&self) -> PathBuf {
        self.root_canonical().join("examples").join("demo-course")
    }

    fn lesson_path(&self) -> PathBuf {
        self.course_root().join("demo_lesson.yaml")
    }

    /// The smevals eval dir the command generates into (course root, not
    /// lesson parent — the `.smevals/` decision).
    fn gen_dir(&self) -> PathBuf {
        self.course_root().join(".smevals")
    }

    /// The published report location: `<repo>/docs/evals/<lesson>`.
    fn docs_dir(&self) -> PathBuf {
        self.root_canonical()
            .join("docs")
            .join("evals")
            .join("demo_lesson")
    }

    /// The build-into-temp sibling of the report dir — what `-o` actually
    /// receives; the command renames it into `docs_dir()` on success.
    fn temp_dir(&self) -> PathBuf {
        self.root_canonical()
            .join("docs")
            .join("evals")
            .join(".demo_lesson.tmp")
    }

    fn bin_dir(&self) -> PathBuf {
        self.root_canonical().join("bin")
    }

    fn log_path(&self) -> PathBuf {
        self.root_canonical().join("uvx.log")
    }

    fn count_path(&self) -> PathBuf {
        self.root_canonical().join("uvx.count")
    }

    fn uvx_log(&self) -> String {
        fs::read_to_string(self.log_path()).unwrap_or_default()
    }
}

/// Parse the shim log into per-call argv vectors (`["smevals==0.2.0", ...]`).
fn parse_log(text: &str) -> Vec<Vec<String>> {
    let mut calls: Vec<Vec<String>> = Vec::new();
    for line in text.lines() {
        if line.starts_with("call: ") {
            calls.push(Vec::new());
        } else if let Some(arg) = line.strip_prefix("arg: ") {
            calls
                .last_mut()
                .expect("arg lines follow a call line")
                .push(arg.to_string());
        }
    }
    calls
}

/// Run `blendtutor eval-report <lesson>` with the fake-uvx shim on PATH and
/// scripted per-call exits, returning the child's output.
fn eval_report_output(
    harness: &Harness,
    lesson: &Path,
    exit_1: Option<u32>,
    exit_2: Option<u32>,
    make_runs: bool,
) -> Output {
    let mut cmd = Command::cargo_bin("blendtutor").expect("binary `blendtutor` should be built");
    let path = format!(
        "{}:{}",
        harness.bin_dir().display(),
        std::env::var("PATH").unwrap_or_default()
    );
    cmd.arg("eval-report")
        .arg(lesson)
        .env("PATH", path)
        .env("FAKE_UVX_LOG", harness.log_path())
        .env("FAKE_UVX_COUNT", harness.count_path());
    if let Some(code) = exit_1 {
        cmd.env("FAKE_UVX_EXIT_1", code.to_string());
    }
    if let Some(code) = exit_2 {
        cmd.env("FAKE_UVX_EXIT_2", code.to_string());
    }
    if make_runs {
        cmd.env("FAKE_UVX_MAKE_RUNS", "1");
    }
    cmd.output()
        .expect("running `blendtutor eval-report` should produce output")
}

// ── Positive path ──────────────────────────────────────────────────────────

#[test]
fn success_path_invokes_uvx_twice_with_pin_and_absolute_paths() {
    let harness = Harness::new();
    // Pre-seed a stale artifact: a previous report's runs would be false
    // evidence, so `.smevals/` must be cleaned before regenerating.
    fs::create_dir_all(harness.gen_dir()).unwrap();
    let stale = harness.gen_dir().join("stale.txt");
    fs::write(&stale, "stale run\n").unwrap();

    let output = eval_report_output(&harness, &harness.lesson_path(), None, None, false);
    assert!(
        output.status.success(),
        "successful eval-report must exit 0; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Exactly two uvx calls: `run <gen_dir> -g` then `build <gen_dir> -o <docs>`.
    let calls = parse_log(&harness.uvx_log());
    assert_eq!(calls.len(), 2, "exactly two uvx calls, got {calls:?}");
    let gen_dir = harness.gen_dir().canonicalize().unwrap();
    assert_eq!(
        calls[0],
        vec![
            "smevals==0.2.0".to_string(),
            "run".to_string(),
            gen_dir.display().to_string(),
            "-g".to_string(),
        ],
        "run call must pin smevals in package-spec position, eval dir before -g"
    );
    assert_eq!(
        calls[1],
        vec![
            "smevals==0.2.0".to_string(),
            "build".to_string(),
            gen_dir.display().to_string(),
            "-o".to_string(),
            harness.temp_dir().display().to_string(),
        ],
        "build must write into the docs/evals temp dir, then rename it into \
         docs/evals/<lesson> atomically"
    );
    // Every logged path is absolute: a CWD-relative gen_dir would build a
    // report from the wrong directory for a relative lesson path.
    for logged in [&calls[0][2], &calls[1][2], &calls[1][4]] {
        assert!(
            Path::new(logged).is_absolute(),
            "logged path {logged:?} must be absolute"
        );
    }
    // No --runs-dir: stranding runs outside the eval dir would let `build`
    // assemble an empty/stale report.
    assert!(
        !harness.uvx_log().contains("--runs-dir"),
        "eval-report must never pass --runs-dir"
    );

    // The stale pre-seed is gone (cleaned before regenerating) and the fresh
    // eval dir was written back into the course root.
    assert!(
        !stale.exists(),
        "stale .smevals content must be cleaned before generate"
    );
    assert!(
        harness.gen_dir().join("eval.yaml").is_file(),
        "the eval dir must be regenerated"
    );
    // The built report landed (the shim fabricates index.html on a successful
    // build call, standing in for smevals' HTML output).
    assert!(
        harness.docs_dir().join("index.html").is_file(),
        "the report must exist at docs/evals/<lesson>/index.html"
    );
}

// ── Generator failure (negative a) ─────────────────────────────────────────

#[test]
fn missing_sibling_suite_fails_naming_generate_without_invoking_uvx() {
    let harness = Harness::new();
    fs::remove_file(harness.course_root().join("eval_demo_lesson.yaml")).unwrap();

    let output = eval_report_output(&harness, &harness.lesson_path(), None, None, false);
    assert!(
        !output.status.success(),
        "a missing suite must fail the command"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("generate:"),
        "error must name the generate stage, got: {stderr}"
    );
    assert!(
        parse_log(&harness.uvx_log()).is_empty(),
        "uvx must never be invoked when generation fails"
    );
}

#[test]
fn bad_slug_fails_naming_generate_without_invoking_uvx() {
    let harness = Harness::new();
    // A lesson whose file stem is not a slug (spaces) passes the lesson/suite
    // reads but is refused by the generator's id validation.
    let bad = harness.course_root().join("bad slug.yaml");
    fs::copy(harness.course_root().join("demo_lesson.yaml"), &bad).unwrap();
    fs::copy(
        harness.course_root().join("eval_demo_lesson.yaml"),
        harness.course_root().join("eval_bad slug.yaml"),
    )
    .unwrap();

    let output = eval_report_output(&harness, &bad, None, None, false);
    assert!(
        !output.status.success(),
        "a bad lesson slug must fail the command"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("generate:"),
        "error must name the generate stage, got: {stderr}"
    );
    assert!(
        parse_log(&harness.uvx_log()).is_empty(),
        "uvx must never be invoked when generation fails"
    );
}

// ── Run-stage exit semantics (negative b) ──────────────────────────────────

#[test]
fn run_failure_with_runs_is_evidence_and_proceeds_to_build() {
    let harness = Harness::new();
    // run exits 1 (a grade fail) but produced runs/ → grade-fail is evidence,
    // not a gate: build still runs and the command exits 0.
    let output = eval_report_output(&harness, &harness.lesson_path(), Some(1), None, true);
    assert!(
        output.status.success(),
        "run exit 1 with runs/ must still exit 0 on a successful build; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let calls = parse_log(&harness.uvx_log());
    assert_eq!(
        calls.len(),
        2,
        "build must still be invoked after a run grade-fail"
    );
    assert!(
        harness.docs_dir().join("index.html").is_file(),
        "the report must be built from the grade-fail runs"
    );
}

#[test]
fn run_failure_without_runs_fails_naming_run_and_skips_build() {
    let harness = Harness::new();
    // run exits 1 with no usable artifacts → a harness failure, not a grade:
    // the command fails naming the run stage and never invokes build.
    let output = eval_report_output(&harness, &harness.lesson_path(), Some(1), None, false);
    assert!(
        !output.status.success(),
        "run failure with empty runs/ must fail the command"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("run:"),
        "error must name the run stage, got: {stderr}"
    );
    let calls = parse_log(&harness.uvx_log());
    assert_eq!(
        calls.len(),
        1,
        "build must not run without usable artifacts, got {calls:?}"
    );
    assert!(
        !harness.docs_dir().join("index.html").exists(),
        "no report may be published without runs"
    );
}

// ── Build-stage semantics (negative c) ─────────────────────────────────────

#[test]
fn build_failure_names_build_and_preserves_prior_committed_report() {
    let harness = Harness::new();
    // Pre-existing committed report: it must survive a failed rebuild.
    let keep = harness.docs_dir().join("KEEP.txt");
    fs::create_dir_all(harness.docs_dir()).unwrap();
    fs::write(&keep, "committed report marker\n").unwrap();

    let output = eval_report_output(&harness, &harness.lesson_path(), None, Some(1), false);
    assert!(
        !output.status.success(),
        "a failed build must fail the command"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("build:"),
        "error must name the build stage, got: {stderr}"
    );
    assert!(
        keep.exists(),
        "a prior committed report must survive a failed rebuild"
    );
    assert!(
        !harness.temp_dir().exists(),
        "the partial temp build must be discarded on failure"
    );
}

#[test]
fn run_and_build_both_fail_naming_build_with_run_exit_in_context() {
    let harness = Harness::new();
    // Both stages fail: the command fails naming build, with the run stage's
    // exit code in the error context so the two failures aren't conflated.
    let output = eval_report_output(&harness, &harness.lesson_path(), Some(1), Some(1), true);
    assert!(
        !output.status.success(),
        "run+build failure must fail the command"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("build:"),
        "error must name the build stage, got: {stderr}"
    );
    assert!(
        stderr.contains("run stage"),
        "build error must surface the run stage's exit code, got: {stderr}"
    );
}

// ── Missing uvx (negative f) ───────────────────────────────────────────────

#[test]
fn missing_uvx_is_a_clean_stage_named_error_not_a_panic() {
    let harness = Harness::new();
    // A PATH with no uvx at all: the shim dir is excluded, so `uvx` spawn
    // fails with NotFound and the command must report it cleanly.
    let empty = harness.root.path().join("empty-bin");
    fs::create_dir_all(&empty).unwrap();
    let output = Command::cargo_bin("blendtutor")
        .expect("binary `blendtutor` should be built")
        .arg("eval-report")
        .arg(harness.lesson_path())
        .env("PATH", empty)
        .output()
        .expect("running `blendtutor eval-report` should produce output");
    assert!(
        !output.status.success(),
        "a missing uvx must fail the command"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("uvx"),
        "error must name uvx and the install hint, got: {stderr}"
    );
    assert!(
        !stderr.contains("panicked at"),
        "a missing uvx must never panic, got: {stderr}"
    );
}
