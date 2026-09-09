//! Integration tests for `blendtutor eval <lesson>` — scoring feedback quality.
//!
//! `eval` loads a lesson and its sibling `eval_<lesson>.yaml` suite, runs every
//! synthetic submission through the exact `run` pipeline (execute → grade → ask
//! the LLM for a verdict), and scores each verdict's polarity against the case's
//! expected polarity, reporting an aggregate accuracy and per-case results.
//!
//! Like the `run` tests, the provider is a `wiremock` stub reached through the
//! `BLENDTUTOR_PROVIDER_URL` override and the interpreter is the real `Rscript`,
//! so these skip-with-notice when R is absent. One server returns a distinct
//! verdict per submission via [`mount_feedback_for`], keyed on a token the
//! submission fences into the request body.

mod common;

use std::path::Path;

use assert_cmd::Command as AssertCommand;
use common::{blendtutor_output, mount_feedback_for, rscript_absent};
use wiremock::MockServer;

/// The demo lesson and its sibling eval suite (three cases: alpha/beta/gamma).
const EVAL_LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_command/demo_lesson.yaml"
);

/// The demo lesson's sibling eval suite (three cases: alpha/beta/gamma).
const EVAL_SUITE: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/eval_command/eval_demo_lesson.yaml"
);

/// Mount the three scripted verdicts that, against the suite's expected
/// polarities `[correct, incorrect, correct]`, yield two matches and one
/// mismatch (accuracy 2/3): alpha→correct (match), beta→incorrect (match),
/// gamma→incorrect (mismatch).
async fn mount_three_case_provider(server: &MockServer) {
    mount_feedback_for(server, "alpha", true, "alpha looks right").await;
    mount_feedback_for(server, "beta", false, "beta is off").await;
    mount_feedback_for(server, "gamma", false, "gamma is off").await;
}

/// Run `blendtutor eval <lesson> [extra args]` against `server`, inside
/// spawn_blocking so the test runtime keeps serving the mock while the child's
/// requests are in flight.
async fn eval_against(server: &MockServer, extra: &[&str]) -> std::process::Output {
    eval_lesson_against(server, EVAL_LESSON, extra).await
}

/// Like [`eval_against`], but for an arbitrary lesson path — the
/// `--write-report` tests point at a lesson inside a tempdir course.
async fn eval_lesson_against(
    server: &MockServer,
    lesson: &str,
    extra: &[&str],
) -> std::process::Output {
    let uri = server.uri();
    let mut args = vec!["eval".to_string(), lesson.to_string()];
    args.extend(extra.iter().map(|s| s.to_string()));
    tokio::task::spawn_blocking(move || blendtutor_output(args, uri))
        .await
        .expect("the blocking command task should join")
}

/// A tempdir course for the `--write-report` tests: a 3-line `blendtutor.toml`
/// manifest plus the demo lesson and its sibling suite copied from the
/// `eval_command` fixtures. `course_root_for` only checks manifest existence,
/// so the minimal marker is a valid course. The course dir is canonicalized so
/// paths the binary prints (which resolve symlinked tempdirs) match the paths
/// the test asserts on. Returns the tempdir (the caller keeps it alive), the
/// absolute lesson path, and the course root.
fn write_report_course() -> (tempfile::TempDir, String, std::path::PathBuf) {
    let tmp = tempfile::tempdir().expect("a tempdir for the course");
    let course = tmp.path().join("course");
    std::fs::create_dir_all(&course).expect("create the course dir");
    std::fs::write(
        course.join("blendtutor.toml"),
        "[[lessons]]\nid = \"eval-demo\"\npath = \"demo_lesson.yaml\"\n",
    )
    .expect("write the manifest marker");
    std::fs::copy(EVAL_LESSON, course.join("demo_lesson.yaml")).expect("copy the lesson");
    std::fs::copy(EVAL_SUITE, course.join("eval_demo_lesson.yaml")).expect("copy the suite");
    let course = course.canonicalize().expect("canonicalize the course dir");
    let lesson = course.join("demo_lesson.yaml");
    (tmp, lesson.to_string_lossy().into_owned(), course)
}

/// AC1 — `eval` scores each case match/mismatch and reports the aggregate
/// accuracy.
///
/// Three cases, two of whose verdicts match their expected polarity and one of
/// which does not, must report `2/3` and surface exactly one mismatch in the
/// per-case rows. Asserting the fraction *and* the single mismatch row is
/// load-bearing: a stub that prints a constant accuracy or omits per-case
/// results cannot pass, because the mismatch is driven by the mock's mixed
/// verdicts flowing through the real scoring path.
#[tokio::test]
async fn eval_reports_aggregate_accuracy_and_per_case_results() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_three_case_provider(&server).await;

    let output = eval_against(&server, &[]).await;

    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    assert_eq!(
        output.status.code(),
        Some(0),
        "eval should succeed; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        stdout.contains("2/3"),
        "stdout should report the aggregate accuracy 2/3, got: {stdout:?}"
    );
    assert_eq!(
        stdout.matches("[match]").count(),
        2,
        "two cases should be reported as matches, got: {stdout:?}"
    );
    assert_eq!(
        stdout.matches("[mismatch]").count(),
        1,
        "exactly one case should be reported as a mismatch, got: {stdout:?}"
    );
}

/// AC2 — `eval --format json` emits one JSON document carrying per-case
/// verdicts, their expected polarity, the derived `matched` flag, and the
/// aggregate accuracy — the artifact a built site embeds without re-scoring.
///
/// The whole stdout must parse as a *single* JSON value (so no log line or
/// second document interleaves), `accuracy` must be the number `2/3` (not a
/// string), and each case must carry non-null `expected`/`actual` strings and a
/// boolean `matched` consistent with `expected == actual` — pinning the array to
/// `[true, true, false]` proves the serialized flags are the real derived scores.
#[tokio::test]
async fn eval_json_emits_per_case_verdicts_and_aggregate() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_three_case_provider(&server).await;

    let output = eval_against(&server, &["--format", "json"]).await;

    assert_eq!(
        output.status.code(),
        Some(0),
        "eval --format json should succeed; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("stdout should be exactly one JSON document");

    let accuracy = doc["accuracy"]
        .as_f64()
        .expect("accuracy should be a JSON number, not a string");
    assert!(
        (accuracy - 2.0 / 3.0).abs() < 1e-12,
        "accuracy should be 2/3, got {accuracy}"
    );

    let cases = doc["cases"].as_array().expect("cases should be an array");
    assert_eq!(cases.len(), 3, "one entry per case");
    let matched: Vec<bool> = cases
        .iter()
        .map(|case| {
            assert!(
                case["expected"].is_string(),
                "expected should be a non-null string: {case}"
            );
            assert!(
                case["actual"].is_string(),
                "actual should be a non-null string: {case}"
            );
            assert!(
                case["feedback_message"].is_string(),
                "feedback_message should be a non-null string, never omitted: {case}"
            );
            let matched = case["matched"]
                .as_bool()
                .expect("matched should be a boolean");
            assert_eq!(
                matched,
                case["expected"] == case["actual"],
                "matched must reflect expected == actual: {case}"
            );
            matched
        })
        .collect();
    assert_eq!(matched, vec![true, true, false]);
    // Each case carries its verdict's verbatim message, incl. the Correct
    // verdict (alpha) — a constant or Incorrect-only message cannot pass.
    let messages: Vec<&str> = cases
        .iter()
        .map(|case| {
            case["feedback_message"]
                .as_str()
                .expect("feedback_message is a string")
        })
        .collect();
    assert_eq!(
        messages,
        vec!["alpha looks right", "beta is off", "gamma is off"]
    );
}

/// AC3 — `--case N` (1-based) selects a single case, still scored through the
/// real pipeline, and only the selected case makes a provider request.
///
/// `--case 2` must yield exactly one scored case whose verbatim
/// `feedback_message` is the beta verdict ("beta is off") with `matched: true`.
/// The wiremock `received_requests` count of exactly one catches a
/// run-all-then-filter implementation, which would make three requests
/// (negative g).
#[tokio::test]
async fn eval_case_selection_scores_only_the_requested_case() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_three_case_provider(&server).await;

    let output = eval_against(&server, &["--format", "json", "--case", "2"]).await;

    assert_eq!(
        output.status.code(),
        Some(0),
        "a selected case should succeed; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("stdout should be exactly one JSON document");
    let cases = doc["cases"].as_array().expect("cases should be an array");
    assert_eq!(cases.len(), 1, "only the requested case is scored");
    assert_eq!(
        cases[0]["feedback_message"].as_str(),
        Some("beta is off"),
        "case 2 is the beta case with its verbatim feedback message"
    );
    assert_eq!(
        cases[0]["matched"], true,
        "beta expects incorrect and is graded incorrect, so it matches"
    );

    let requests = server
        .received_requests()
        .await
        .expect("the mock server should record requests");
    assert_eq!(
        requests.len(),
        1,
        "only the selected case may make a feedback request"
    );
}

/// AC4 — `--case 1` selects the FIRST authored case (1-based indexing).
///
/// The alpha case expects correct and is graded correct ("alpha looks right");
/// selecting it by 1 proves the index is 1-based rather than 0-based (a
/// 0-based implementation would return the beta case for `--case 1`).
#[tokio::test]
async fn eval_case_selection_is_one_based() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_three_case_provider(&server).await;

    let output = eval_against(&server, &["--format", "json", "--case", "1"]).await;

    assert_eq!(
        output.status.code(),
        Some(0),
        "a selected case should succeed; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("stdout should be exactly one JSON document");
    let cases = doc["cases"].as_array().expect("cases should be an array");
    assert_eq!(cases.len(), 1);
    assert_eq!(
        cases[0]["feedback_message"].as_str(),
        Some("alpha looks right"),
        "case 1 is the alpha case (1-based indexing)"
    );
}

/// AC5 — an out-of-range `--case N` exits 1 and names the suite size on stderr.
///
/// Both below the first case (`0`) and past the last (`4`) for a 3-case suite
/// must be rejected with the suite size `3` named — never clamped (negative d)
/// and never a bare exit 1 without the size (negative e).
#[tokio::test]
async fn eval_case_out_of_range_exits_1_naming_the_suite_size() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_three_case_provider(&server).await;

    for bad in ["0", "4"] {
        let output = eval_against(&server, &["--format", "json", "--case", bad]).await;
        assert_eq!(
            output.status.code(),
            Some(1),
            "case {bad} is out of range for a 3-case suite and must exit 1"
        );
        let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        assert!(
            stderr.contains('3'),
            "stderr should name the suite size 3, got: {stderr:?}"
        );
    }
}

/// AC6 — a mismatched single case still exits 0: `eval` measures feedback
/// quality, it is not a pass/fail gate, so a low accuracy is not an error
/// (negative f). Case 3 (gamma) expects correct but is graded incorrect.
#[tokio::test]
async fn eval_case_mismatch_exit0() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_three_case_provider(&server).await;

    let output = eval_against(&server, &["--format", "json", "--case", "3"]).await;

    assert_eq!(
        output.status.code(),
        Some(0),
        "a mismatched case is low accuracy, not an error; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("stdout should be exactly one JSON document");
    assert_eq!(
        doc["cases"][0]["matched"], false,
        "gamma expects correct but is graded incorrect"
    );
}

/// AC7 (F1+F2) — the human render shows each mismatched case's grader feedback
/// verbatim under its row and ends with a next-steps footer naming the
/// mismatched cases and the knobs that shape grading.
///
/// The three mock messages are distinct, so attribution is provable: only
/// gamma's message (the mismatch) may appear on a `grader:` line — alpha's and
/// beta's (both matches) must not. The footer must not add a second bracketed
/// `[mismatch]` token: AC1's exactly-one count stays the pin.
#[tokio::test]
async fn eval_human_shows_grader_feedback_and_next_steps_footer() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_three_case_provider(&server).await;

    let output = eval_against(&server, &[]).await;

    assert_eq!(
        output.status.code(),
        Some(0),
        "eval should succeed; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    assert!(
        stdout.contains("grader: gamma is off"),
        "the mismatched case's verbatim feedback must be shown, got: {stdout:?}"
    );
    assert!(
        !stdout.contains("grader: alpha looks right") && !stdout.contains("grader: beta is off"),
        "match rows carry no feedback line, got: {stdout:?}"
    );
    assert!(
        stdout.contains("mismatched cases: 3"),
        "the footer must name the mismatched case numbers, got: {stdout:?}"
    );
    assert!(
        stdout.contains("--case N"),
        "the footer must point at re-running a single case, got: {stdout:?}"
    );
    assert!(
        stdout.contains("llm_evaluation_prompt"),
        "the footer must name the grading knobs, got: {stdout:?}"
    );
}

/// AC7 negative — an all-matched run (accuracy 3/3) emits NO footer and NO
/// guidance lines: the footer is mismatch-driven, not unconditional.
#[tokio::test]
async fn eval_human_full_match_emits_no_footer() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    // gamma graded correct against an expected-correct case → all three match.
    mount_feedback_for(&server, "alpha", true, "alpha looks right").await;
    mount_feedback_for(&server, "beta", false, "beta is off").await;
    mount_feedback_for(&server, "gamma", true, "gamma looks right").await;

    let output = eval_against(&server, &[]).await;

    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    assert!(
        stdout.contains("3/3"),
        "all three cases should match, got: {stdout:?}"
    );
    assert!(
        !stdout.contains("grader:"),
        "a full match emits no feedback lines, got: {stdout:?}"
    );
    assert!(
        !stdout.contains("mismatched cases") && !stdout.contains("--case"),
        "a full match emits no footer or guidance lines, got: {stdout:?}"
    );
}

/// Decisions — a non-numeric `--case` is a clap parse error (exit 2); only a
/// numeric-but-out-of-bounds value is our exit-1 error class. No provider
/// request is ever made for a parse failure.
#[tokio::test]
async fn eval_case_non_numeric_is_a_parse_error_exit_2() {
    let server = MockServer::start().await;

    let output = eval_against(&server, &["--format", "json", "--case", "abc"]).await;

    assert_eq!(
        output.status.code(),
        Some(2),
        "a non-numeric --case is a clap parse error, not an out-of-range error"
    );
    let requests = server
        .received_requests()
        .await
        .expect("the mock server should record requests");
    assert_eq!(
        requests.len(),
        0,
        "a parse failure makes no provider request"
    );
}

// ── AC-8: `--write-report` — durable eval-report.json at the course root ──

/// AC-8 positive — `eval <lesson> --write-report` (default human format, run
/// from a CWD that is NOT the course root) exits 0, keeps the human accuracy
/// render unchanged, and writes the full-shape report at the course root —
/// never at the CWD — with a confirmation naming the artifact.
///
/// A second run with `--format json --write-report` pins the single
/// serialization path: the file bytes must equal the JSON stdout modulo the
/// trailing newline `writeln` adds, so no second serializer can drift.
#[tokio::test]
async fn eval_write_report_writes_full_shape_report_at_course_root() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_three_case_provider(&server).await;
    let (_tmp, lesson, course) = write_report_course();

    let output = eval_lesson_against(&server, &lesson, &["--write-report"]).await;

    assert_eq!(
        output.status.code(),
        Some(0),
        "--write-report should succeed; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    assert!(
        stdout.contains("2/3"),
        "--write-report must not change the human accuracy render, got: {stdout:?}"
    );

    let report_path = course.join("eval-report.json");
    assert!(
        report_path.exists(),
        "the report must be written at the course root"
    );
    assert!(
        !Path::new("eval-report.json").exists(),
        "the report must NOT be written at the CWD"
    );

    let file = std::fs::read_to_string(&report_path).expect("read the written report");
    let doc: serde_json::Value =
        serde_json::from_str(&file).expect("the report should be one JSON document");
    let cases = doc["cases"].as_array().expect("cases should be an array");
    assert_eq!(
        cases.len(),
        3,
        "the full suite's case count, not a partial one"
    );
    let accuracy = doc["accuracy"]
        .as_f64()
        .expect("accuracy should be a JSON number, not a string");
    assert!(
        (accuracy - 2.0 / 3.0).abs() < 1e-12,
        "accuracy should be 2/3, got {accuracy}"
    );

    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("eval-report.json"),
        "the confirmation must name the artifact, got: {combined:?}"
    );

    // Single serialization path: `--format json` stdout == file bytes (+ the
    // newline `writeln` appends). A second serializer would drift here.
    let json_output =
        eval_lesson_against(&server, &lesson, &["--format", "json", "--write-report"]).await;
    assert_eq!(
        json_output.status.code(),
        Some(0),
        "the json-format run should succeed; stderr: {}",
        String::from_utf8_lossy(&json_output.stderr)
    );
    let file_bytes = std::fs::read(&report_path).expect("read the re-written report");
    assert_eq!(
        Some(file_bytes.as_slice()),
        json_output.stdout.strip_suffix(b"\n"),
        "the file must be byte-identical to the --format json stdout (modulo the trailing newline)"
    );
}

/// AC-8 N1 — a lesson with no `blendtutor.toml` ancestor refuses (exit 1)
/// naming the missing course root, writes no file anywhere, and makes no
/// provider request: the refusal precedes any scoring side effect.
#[tokio::test]
async fn eval_write_report_no_course_root() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_three_case_provider(&server).await;
    // A plain dir with the lesson + suite but NO manifest marker.
    let tmp = tempfile::tempdir().expect("a tempdir for the manifest-less dir");
    let dir = tmp.path().join("no-course");
    std::fs::create_dir_all(&dir).expect("create the dir");
    std::fs::copy(EVAL_LESSON, dir.join("demo_lesson.yaml")).expect("copy the lesson");
    std::fs::copy(EVAL_SUITE, dir.join("eval_demo_lesson.yaml")).expect("copy the suite");
    let lesson = dir
        .canonicalize()
        .expect("canonicalize the dir")
        .join("demo_lesson.yaml")
        .to_string_lossy()
        .into_owned();

    let output = eval_lesson_against(&server, &lesson, &["--write-report"]).await;

    assert_eq!(
        output.status.code(),
        Some(1),
        "no course root must refuse; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    assert!(
        stderr.contains("blendtutor.toml"),
        "stderr must name the missing manifest, got: {stderr:?}"
    );
    assert!(
        !dir.join("eval-report.json").exists(),
        "no report may be written at the lesson's dir"
    );
    assert!(
        !Path::new("eval-report.json").exists(),
        "no report may be written at the CWD"
    );
    let requests = server
        .received_requests()
        .await
        .expect("the mock server should record requests");
    assert_eq!(
        requests.len(),
        0,
        "the refusal must precede any scoring side effect"
    );
}

/// AC-8 N2 — a pre-existing report is overwritten with a warning, not a
/// refusal: exit 0, stderr says "overwrit…" and names the path, and the file
/// is replaced with the new full-shape content.
#[tokio::test]
async fn eval_write_report_overwrite_warns() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_three_case_provider(&server).await;
    let (_tmp, lesson, course) = write_report_course();
    std::fs::write(course.join("eval-report.json"), r#"{"sentinel":true}"#)
        .expect("pre-create the sentinel report");

    let output = eval_lesson_against(&server, &lesson, &["--write-report"]).await;

    assert_eq!(
        output.status.code(),
        Some(0),
        "overwriting one's own course report proceeds; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    assert!(
        stderr.contains("overwrit"),
        "the warning must say the file is overwritten, got: {stderr:?}"
    );
    assert!(
        stderr.contains("eval-report.json"),
        "the warning must name the overwritten path, got: {stderr:?}"
    );
    let doc: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(course.join("eval-report.json")).expect("read the replaced file"),
    )
    .expect("the replaced file should be one JSON document");
    assert!(
        doc["sentinel"].is_null(),
        "the sentinel content must be gone, got: {doc}"
    );
    let accuracy = doc["accuracy"]
        .as_f64()
        .expect("the replacement carries a numeric accuracy");
    assert!(
        (accuracy - 2.0 / 3.0).abs() < 1e-12,
        "the replacement is the new full-shape report, accuracy 2/3, got {accuracy}"
    );
}

/// AC-8 N3 — `--case N --write-report` refuses (exit 1) naming both flags and
/// writes nothing: a single-case report would render as the course-level
/// accuracy in the built site — a misleading durable artifact.
#[tokio::test]
async fn eval_write_report_partial_refused() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_three_case_provider(&server).await;
    let (_tmp, lesson, course) = write_report_course();

    let output = eval_lesson_against(&server, &lesson, &["--case", "1", "--write-report"]).await;

    assert_eq!(
        output.status.code(),
        Some(1),
        "a partial report must be refused; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    assert!(
        stderr.contains("--case"),
        "the error must name --case, got: {stderr:?}"
    );
    assert!(
        stderr.contains("--write-report"),
        "the error must name --write-report, got: {stderr:?}"
    );
    assert!(
        !course.join("eval-report.json").exists(),
        "a refused partial report must write no file"
    );
}

/// AC-8 N4 — an unwritable course root propagates the write failure (exit 1),
/// names the error, and leaves no `.tmp` leftover. Skipped when the process
/// can write despite `0o555` (root), where chmod is ineffective.
#[cfg(unix)]
#[tokio::test]
async fn eval_write_report_failure_propagates() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_three_case_provider(&server).await;
    let (_tmp, lesson, course) = write_report_course();

    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(&course, std::fs::Permissions::from_mode(0o555))
        .expect("chmod the course dir read-only");
    // Root ignores mode bits — probe before asserting, and skip when the
    // write would succeed anyway.
    let probe = course.join(".perm-probe");
    if std::fs::write(&probe, b"x").is_ok() {
        let _ = std::fs::remove_file(&probe);
        let _ = std::fs::set_permissions(&course, std::fs::Permissions::from_mode(0o755));
        eprintln!("SKIP: privileged user — chmod 0o555 ineffective, skipping write-failure test");
        return;
    }

    let output = eval_lesson_against(&server, &lesson, &["--write-report"]).await;

    // Restore before the tempdir drop, which needs to delete the dir.
    let _ = std::fs::set_permissions(&course, std::fs::Permissions::from_mode(0o755));
    assert_eq!(
        output.status.code(),
        Some(1),
        "a write failure must propagate, not be swallowed; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    assert!(
        stderr.contains("eval-report"),
        "stderr must name the write error, got: {stderr:?}"
    );
    assert!(
        !course.join("eval-report.json").exists(),
        "no report may be written"
    );
    assert!(
        !course.join(".eval-report.json.tmp").exists(),
        "no .tmp leftover may survive the failure"
    );
}

/// AC-8 item 8 — the bidirectional contract: the file `--write-report` writes
/// is exactly what `build` consumes. Build the same tempdir course and assert
/// the eval-results page shows the accuracy derived from the written file
/// (a wrong-schema write that eval exits 0 on would fail the build here).
#[tokio::test]
async fn eval_write_report_build_roundtrip() {
    if rscript_absent() {
        return;
    }
    let server = MockServer::start().await;
    mount_three_case_provider(&server).await;
    let (tmp, lesson, course) = write_report_course();

    let output = eval_lesson_against(&server, &lesson, &["--write-report"]).await;
    assert_eq!(
        output.status.code(),
        Some(0),
        "--write-report should succeed; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let out = tmp.path().join("site");
    let build_output = AssertCommand::cargo_bin("blendtutor")
        .expect("binary `blendtutor` should be built")
        .args(["build", "--target", "webr"])
        .arg(&course)
        .arg("-o")
        .arg(&out)
        .output()
        .expect("running `blendtutor build` should produce output");
    assert!(
        build_output.status.success(),
        "build must consume the written report; stderr: {:?}",
        String::from_utf8_lossy(&build_output.stderr)
    );

    let page = std::fs::read_to_string(out.join("eval-results.html"))
        .expect("the built site must include an eval-results.html page");
    // The expected figure is derived from the written report, parsed
    // independently here — a placeholder or constant cannot pass.
    let report: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(course.join("eval-report.json")).expect("read the written report"),
    )
    .expect("the written report should be one JSON document");
    let accuracy = report["accuracy"]
        .as_f64()
        .expect("the written report carries a numeric accuracy");
    let pct = (accuracy * 100.0).round() as i64;
    assert!(
        page.contains(&format!("{pct}%")),
        "eval-results.html must show the written report's accuracy ({pct}%); page={page}"
    );
}

/// AC-8 item 7 — `eval --help` advertises `--write-report`. The flag lives on
/// the existing Eval variant; the 9-subcommand surface stays pinned in cli.rs.
#[test]
fn eval_help_lists_write_report_flag() {
    let output = AssertCommand::cargo_bin("blendtutor")
        .expect("binary `blendtutor` should be built")
        .args(["eval", "--help"])
        .output()
        .expect("running `blendtutor eval --help` should succeed");
    assert!(
        output.status.success(),
        "`eval --help` should exit 0, got {:?}",
        output.status
    );
    let stdout = String::from_utf8(output.stdout).expect("help output should be UTF-8");
    assert!(
        stdout.contains("--write-report"),
        "`eval --help` must advertise --write-report, got:\n{stdout}"
    );
}
