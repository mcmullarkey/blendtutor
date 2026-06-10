//! Integration tests for `blendtutor build --target webr`.
//!
//! Exercises static-site assembly end to end via the built binary: point
//! `build` at an R course and inspect the directory it emits — the slice's
//! "when I build, I get a deployable webR site" behavior. The pure assembly
//! rules (`plan_site`) are unit-tested in `blendtutor-core`; here we observe the
//! files that actually land on disk.

use std::path::{Path, PathBuf};

use assert_cmd::Command;
use blendtutor_core::llm::{CHECKS_LABEL, CLOSE_CODE, OPEN_CODE, OUTPUT_LABEL};
use blendtutor_core::site::EvalSummary;
use serde_json::Value;

/// An R-only course: a manifest and two valid R lessons, each carrying checks
/// and a reference solution. Lives under `core`'s fixtures (the schema's home)
/// and is referenced cross-crate by path.
const R_COURSE: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/r-course"
);

/// A Python-only course: a manifest and one valid Python lesson carrying checks
/// and a reference solution. Lives under `core`'s fixtures (the schema's home)
/// and is referenced cross-crate by path, mirroring `R_COURSE`.
const PYTHON_COURSE: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/python-course"
);

/// A one-lesson R course bundled with a Slice-13 `eval-report.json` (accuracy
/// 0.67). Drives Slice 19 AC1: a build folds the report's accuracy into the
/// site's eval-results page.
const COURSE_WITH_EVAL_REPORT: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/course_with_eval_report_0_67"
);

/// The same one-lesson R course with no eval report alongside it. Drives Slice 19
/// AC2: a build still succeeds and the eval-results page says so explicitly.
const COURSE_WITHOUT_EVAL_REPORT: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/course_without_eval_report"
);

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

#[test]
fn build_webr_emits_a_deployable_r_lesson_site() {
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build("webr", R_COURSE, &out);
    assert!(
        output.status.success(),
        "`build --target webr` on an R course should exit 0, got {:?}; stderr={:?}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    // The page shell, the in-browser runner (which boots webR), the shared runner
    // core the runner imports, and the COOP/COEP shim all land. The core is now a
    // shared asset (factored in #17), and the webR runner `import`s it — so a
    // regression that stopped emitting it would break webR at runtime; assert it
    // lands here, mirroring the pyodide test (the symmetric twin).
    for name in [
        "index.html",
        "lesson-runner.js",
        "lesson-runner-core.js",
        "coi-serviceworker.js",
    ] {
        assert!(out.join(name).is_file(), "the built site is missing {name}");
    }

    // The shim is *referenced* from index.html — present-but-dead breaks webR on
    // GitHub Pages (no SharedArrayBuffer), so a bare file on disk is not enough.
    let index = std::fs::read_to_string(out.join("index.html")).unwrap();
    assert!(
        index.contains("coi-serviceworker.js"),
        "index.html must reference the coi-serviceworker shim; index.html={index}"
    );

    // The webR boot is wired into the runner (not a separate file, per the spec).
    let runner = std::fs::read_to_string(out.join("lesson-runner.js")).unwrap();
    assert!(
        runner.to_lowercase().contains("webr"),
        "the lesson runner must boot webR; runner={runner}"
    );

    // One JSON per manifest entry — count matches the course, neither dropped nor
    // duplicated — and each carries the fields the JS contract consumes.
    let mut ids: Vec<String> = std::fs::read_dir(out.join("lessons"))
        .expect("the site has a lessons/ directory")
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "json"))
        .map(|path| {
            let text = std::fs::read_to_string(&path).unwrap();
            let lesson: Value = serde_json::from_str(&text)
                .unwrap_or_else(|e| panic!("lesson JSON {path:?} did not parse: {e}"));
            assert!(
                lesson["title"].is_string(),
                "lesson JSON needs a title: {lesson}"
            );
            assert!(
                lesson["checks"].is_array(),
                "lesson JSON needs checks: {lesson}"
            );
            assert!(
                lesson["solution"].is_string(),
                "lesson JSON needs a solution the runner can submit: {lesson}"
            );
            lesson["id"]
                .as_str()
                .expect("lesson JSON needs an id")
                .to_string()
        })
        .collect();
    ids.sort();
    assert_eq!(
        ids,
        vec!["add-two".to_string(), "square".to_string()],
        "exactly the course's two lessons are serialized, by slug"
    );
}

#[test]
fn build_pyodide_emits_a_deployable_python_lesson_site() {
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build("pyodide", PYTHON_COURSE, &out);
    assert!(
        output.status.success(),
        "`build --target pyodide` on a Python course should exit 0, got {:?}; stderr={:?}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    // The page shell, the in-browser runner, the shared core, and the COOP/COEP
    // shim all land — the same site contract the webR target produces, carried
    // verbatim across the BuildTarget seam (§3.2).
    for name in [
        "index.html",
        "lesson-runner.js",
        "lesson-runner-core.js",
        "coi-serviceworker.js",
    ] {
        assert!(out.join(name).is_file(), "the built site is missing {name}");
    }

    let index = std::fs::read_to_string(out.join("index.html")).unwrap();

    // index.html boots the Pyodide runtime — not webR. A pyodide build that copied
    // webR's shell verbatim would miss this (the AC1 negative).
    assert!(
        regex_lite_contains_pyodide_boot(&index),
        "index.html must reference the Pyodide runtime boot (pyodide.js / loadPyodide); \
         index.html={index}"
    );

    // The shim is *referenced* from index.html — present-but-dead leaves the page
    // un-isolated, so a bare file on disk is not enough.
    assert!(
        index.contains("coi-serviceworker.js"),
        "index.html must reference the coi-serviceworker shim; index.html={index}"
    );

    // The runner boots Pyodide (loadPyodide), so it is genuinely the Python runtime
    // and not a verbatim copy of the webR runner.
    let runner = std::fs::read_to_string(out.join("lesson-runner.js")).unwrap();
    assert!(
        runner.to_lowercase().contains("pyodide"),
        "the lesson runner must boot Pyodide; runner={runner}"
    );

    // One JSON per manifest entry, carrying the contract fields the runner reads,
    // and a title derived from the source course (not a stub).
    let mut ids: Vec<String> = std::fs::read_dir(out.join("lessons"))
        .expect("the site has a lessons/ directory")
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "json"))
        .map(|path| {
            let text = std::fs::read_to_string(&path).unwrap();
            let lesson: Value = serde_json::from_str(&text)
                .unwrap_or_else(|e| panic!("lesson JSON {path:?} did not parse: {e}"));
            assert_eq!(
                lesson["title"], "Add Two Numbers",
                "lesson JSON title must come from the source course, not a stub: {lesson}"
            );
            assert!(
                lesson["checks"].is_array(),
                "lesson JSON needs checks: {lesson}"
            );
            assert!(
                lesson["solution"].is_string(),
                "lesson JSON needs a solution the runner can submit: {lesson}"
            );
            lesson["id"]
                .as_str()
                .expect("lesson JSON needs an id")
                .to_string()
        })
        .collect();
    ids.sort();
    assert_eq!(
        ids,
        vec!["add-two".to_string()],
        "exactly the course's one lesson is serialized, by slug"
    );
}

/// Whether `index.html` references a Pyodide runtime boot, matching the AC1 probe's
/// `pyodide(.js|.asm|/v[0-9])|loadPyodide` grep without pulling in a regex crate:
/// it is enough that the page names `loadPyodide` or a versioned `pyodide` asset.
fn regex_lite_contains_pyodide_boot(html: &str) -> bool {
    html.contains("loadPyodide") || html.contains("pyodide.js") || html.contains("pyodide/v")
}

#[test]
fn build_refuses_a_language_target_mismatch_before_writing_anything() {
    // An R course built for the Pyodide (Python) target is a language/target
    // mismatch: it must be refused at the boundary (§1.3.1) and emit *nothing*,
    // so a half-written, broken site never ships.
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build("pyodide", R_COURSE, &out);
    assert!(
        !output.status.success(),
        "an R course built for the pyodide target must fail; stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    // The failure names the language/target mismatch — not a generic parse error
    // — so this stays red until the *refusal* exists, not merely until the args
    // are rejected.
    let stderr = String::from_utf8_lossy(&output.stderr).to_lowercase();
    assert!(
        stderr.contains("language") || stderr.contains("does not match"),
        "the failure should name the language/target mismatch; stderr={stderr}"
    );

    // Nothing was written: plan refused before write_site ran.
    assert!(
        !out.exists(),
        "a refused build must not create the output directory"
    );
}

/// Recursively collect every emitted file's (path, contents) under `root`. The
/// built site is all text (HTML / JS / JSON), so a lossy decode is faithful and
/// lets one predicate scan the whole shipped artifact for a forbidden literal.
fn emitted_files(root: &Path) -> Vec<(PathBuf, String)> {
    let mut collected = Vec::new();
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        for entry in std::fs::read_dir(&dir).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                stack.push(path);
            } else {
                let bytes = std::fs::read(&path).unwrap();
                collected.push((path, String::from_utf8_lossy(&bytes).into_owned()));
            }
        }
    }
    collected
}

#[test]
fn build_webr_ships_the_byok_anthropic_feedback_seam() {
    // Slice 18: a built site carries the BYOK feedback backend so a learner can
    // get LLM feedback with their own key — stored only in the tab, sent only to
    // the provider, and never baked into a shipped file.
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build("webr", R_COURSE, &out);
    assert!(
        output.status.success(),
        "`build --target webr` should exit 0, got {:?}; stderr={:?}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    // The FeedbackBackend contract + byok-anthropic impl ship as their own asset,
    // loaded by the page shell, which also carries the submit control the flow
    // drives (the `[data-action=submit]` the rodney probe clicks).
    let feedback = std::fs::read_to_string(out.join("feedback.js"))
        .expect("the built site must ship feedback.js (the FeedbackBackend + byok-anthropic impl)");
    let index = std::fs::read_to_string(out.join("index.html")).unwrap();
    assert!(
        index.contains("feedback.js"),
        "index.html must load feedback.js; index={index}"
    );
    assert!(
        index.contains(r#"data-action="submit""#),
        "index.html must carry the submit control the BYOK flow drives; index={index}"
    );

    // AC1: submitting with no key prompts for one with a *dual* disclosure — both
    // phrases live in the impl that renders the prompt (the UI matches them
    // case-insensitively; asserted lowercased against the source).
    let lower = feedback.to_lowercase();
    assert!(
        lower.contains("stored only in this tab"),
        "feedback.js must disclose tab-only key storage; feedback={feedback}"
    );
    assert!(
        lower.contains("sent only to anthropic"),
        "feedback.js must disclose provider-only transmission; feedback={feedback}"
    );

    // AC2: the BYOK call hits Anthropic directly from the browser — the key rides
    // `x-api-key`, the direct-browser-access opt-in is required for the call to be
    // allowed, and the key is read from the sessionStorage slot (tab-scoped).
    assert!(
        feedback.contains("x-api-key"),
        "byok-anthropic must send the key in the x-api-key header"
    );
    assert!(
        feedback.contains("anthropic-dangerous-direct-browser-access"),
        "a direct browser call to Anthropic needs the dangerous-direct-browser-access opt-in"
    );
    assert!(
        feedback.contains("anthropic_api_key"),
        "the key is read from the tab-scoped sessionStorage `anthropic_api_key` slot"
    );

    // The `?provider=` override is host-gated (a fix(review) hardening): the BYOK
    // base URL honors it only for a local stub, so a crafted production link can't
    // redirect the key off-Anthropic. This JS has no unit-test harness, so the
    // shipped contract is the regression gate. Pin the *live guard shape* — the host
    // comparison and the credential rejection — not a bare "localhost" substring: that
    // literal also appears in the explanatory comment, so deleting the guard while
    // leaving the comment would pass and silently re-open the exfiltration vector.
    for guard in [
        r#"url.hostname === "localhost""#,
        r#"url.hostname === "127.0.0.1""#,
        // One fragment pins BOTH credential fields, in order — a future edit dropping
        // `!url.password` can't pass while leaving `!url.username`.
        "!url.username && !url.password",
    ] {
        assert!(
            feedback.contains(guard),
            "feedback.js must host-gate the ?provider= override with the live guard `{guard}`"
        );
    }

    // Slice (issue #46) — the live Anthropic model picker. The hardcoded MODEL is
    // lifted to a named *fallback* feeding a `<select>` populated from a live
    // `/v1/models` query; the request builder takes the chosen model as an explicit
    // argument; and the models query reuses the SAME host-gated `providerBaseUrl()`
    // seam as messages. JS has no unit harness, so these shipped-contract tokens are
    // the regression gate — the live behavior (populate, default, fallback,
    // override-routing) is the rodney arm in the PR evidence.
    for token in [
        "parseModels(",                   // §2.2 pure model-list extraction
        "listModels(",                    // §2.2 effectful fetch wrapping parseModels
        "${baseUrl}/v1/models",           // AC4 §3.4 models URL derives from providerBaseUrl()
        r#"createElement("select")"#,     // AC1 the picker `<select>` is built dynamically
        "feedbackRequest(prompt, model)", // AC2 §1.2 model is an explicit request argument
    ] {
        assert!(
            feedback.contains(token),
            "feedback.js must ship the live model-picker seam token `{token}`; feedback={feedback}"
        );
    }
    // AC1/AC3 §1.2: the fallback literal stays the named default the empty- or
    // failed-query branch resolves to (also pinned alive by the no-`sk-ant` scan).
    assert!(
        feedback.contains("claude-opus-4-8"),
        "feedback.js must keep claude-opus-4-8 as the named fallback model"
    );
    // AC2 adversarial: the builder must NOT close over the bare module constant — a
    // picker rendered for show while the body still ships `model: MODEL` would pass a
    // default-only test vacuously. Pin the absence of the old hardcoded binding so a
    // non-default selection is the only thing that can drive the request `model`.
    assert!(
        !feedback.contains("model: MODEL"),
        "feedbackRequest must take the model as an argument, not close over MODEL"
    );

    // §1.2/§3.2: the JS request mirrors the Rust contract. The prompt delimiters are
    // pinned to the *same exported constants* `build_prompt` emits, so the
    // learner-side prompt structure cannot drift from the author-side one — and the
    // student code is delimited (the Slice-10 injection-hardened structure).
    for token in [OPEN_CODE, CLOSE_CODE, OUTPUT_LABEL, CHECKS_LABEL] {
        assert!(
            feedback.contains(token),
            "feedback.js must delimit the prompt with {token} (the Slice-10 structure); feedback={feedback}"
        );
    }
    for field in ["is_correct", "feedback_message", "respond_with_feedback"] {
        assert!(
            feedback.contains(field),
            "byok-anthropic must speak the `{field}` tool contract (mirrors the Rust Feedback DTO)"
        );
    }

    // AC1 + AC2 negative: an API key literal must never be baked into any shipped
    // file — scan the *whole* emitted site, not just feedback.js.
    for (path, contents) in emitted_files(&out) {
        assert!(
            !contents.contains("sk-ant"),
            "an Anthropic key literal must never ship; found in {path:?}"
        );
    }
}

#[test]
fn build_pyodide_ships_the_same_shared_feedback_seam() {
    // feedback.js is target-agnostic — the Anthropic BYOK call is identical whether
    // the lesson runs in webR or Pyodide — so it is a *shared* asset (§4.2): the
    // Pyodide build ships the byte-identical impl its shell references, not a fork.
    // (Symmetric twin of the webR test, mirroring the shared-core/shim assertion.)
    let tmp = tempfile::tempdir().unwrap();
    let webr_out = tmp.path().join("webr");
    let pyo_out = tmp.path().join("pyodide");
    assert!(build("webr", R_COURSE, &webr_out).status.success());
    assert!(build("pyodide", PYTHON_COURSE, &pyo_out).status.success());

    let pyo_index = std::fs::read_to_string(pyo_out.join("index.html")).unwrap();
    assert!(
        pyo_index.contains("feedback.js"),
        "the pyodide shell must also load feedback.js; index={pyo_index}"
    );
    assert!(
        pyo_index.contains(r#"data-action="submit""#),
        "the pyodide shell must carry the submit control too; index={pyo_index}"
    );

    let webr_feedback = std::fs::read_to_string(webr_out.join("feedback.js")).unwrap();
    let pyo_feedback = std::fs::read_to_string(pyo_out.join("feedback.js")).unwrap();
    assert_eq!(
        webr_feedback, pyo_feedback,
        "feedback.js must be byte-identical across targets (shared, not forked)"
    );
}

#[test]
fn eval_report_page_reflects_actual_accuracy() {
    // Slice 19 AC1: a course bundled with a Slice-13 eval report builds a site
    // whose eval-results page shows the report's *actual* accuracy — read from the
    // JSON, never recomputed (§3.2) or hardcoded — and renders the validated
    // state, not the not-validated marker.
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build("webr", COURSE_WITH_EVAL_REPORT, &out);
    assert!(
        output.status.success(),
        "build of a course with an eval report should exit 0; stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    let page = std::fs::read_to_string(out.join("eval-results.html"))
        .expect("the built site must include an eval-results.html page");

    // The expected figure is derived from the report JSON, parsed independently
    // here — so a placeholder/zero/different constant fails this. The page must
    // reflect the report's real accuracy, never a hardcoded literal.
    let report: Value = serde_json::from_str(
        &std::fs::read_to_string(Path::new(COURSE_WITH_EVAL_REPORT).join("eval-report.json"))
            .unwrap(),
    )
    .unwrap();
    let accuracy = report["accuracy"]
        .as_f64()
        .expect("the eval report carries a numeric accuracy");
    let pct = (accuracy * 100.0).round() as i64;
    assert!(
        page.contains(&format!("{pct}%")),
        "eval-results.html must show the report's accuracy ({pct}%); page={page}"
    );

    // ...and it is *positively* the validated state (§1.2): the validated marker is
    // present AND the not-validated marker is absent, so a page that emitted
    // neither state — or the wrong one — cannot pass.
    assert!(
        page.contains(EvalSummary::VALIDATED_MARKER),
        "a present report must render the explicit validated state; page={page}"
    );
    assert!(
        !page.contains(EvalSummary::NOT_VALIDATED_MARKER),
        "a present report must not render the not-validated marker; page={page}"
    );
}

#[test]
fn missing_report_builds_with_not_validated_page() {
    // Slice 19 AC2: a course with no eval report still builds (exit 0) and its
    // eval-results page carries an explicit not-validated marker — never a
    // real-looking 0%/empty view indistinguishable from a validated result (§1.2).
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build("webr", COURSE_WITHOUT_EVAL_REPORT, &out);
    assert!(
        output.status.success(),
        "build of a report-less course must still exit 0; stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    let page = std::fs::read_to_string(out.join("eval-results.html"))
        .expect("the built site must include an eval-results.html page even without a report");
    assert!(
        page.contains(EvalSummary::NOT_VALIDATED_MARKER),
        "a report-less build must render the explicit not-validated state; page={page}"
    );
}

#[test]
fn corrupt_eval_report_fails_the_build_loudly() {
    // The third branch of the report read, the symmetric twin of present-OK (AC1)
    // and absent (AC2): a present-but-unreadable eval-report.json must fail the
    // build, never silently drop to not-validated. Without this, a regression that
    // swallowed the parse error would still pass AC1 + AC2 while a corrupt artifact
    // quietly unvalidated a course — exactly the failure the loud error prevents.
    let tmp = tempfile::tempdir().unwrap();
    let course = tmp.path().join("course");
    std::fs::create_dir_all(&course).unwrap();
    for asset in ["blendtutor.toml", "add_two.yaml"] {
        std::fs::copy(
            Path::new(COURSE_WITHOUT_EVAL_REPORT).join(asset),
            course.join(asset),
        )
        .unwrap();
    }
    // A valid course bundled with a corrupt report.
    std::fs::write(course.join("eval-report.json"), "{ not valid json").unwrap();

    let out = tmp.path().join("site");
    let output = build("webr", course.to_str().unwrap(), &out);
    assert!(
        !output.status.success(),
        "a corrupt eval report must fail the build, not silently unvalidate; stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stderr = String::from_utf8_lossy(&output.stderr).to_lowercase();
    assert!(
        stderr.contains("eval report"),
        "the failure should name the eval report problem; stderr={stderr}"
    );
    // Fail-fast: the read is refused before the site is planned, so no partial
    // output dir is left behind (mirrors the language-mismatch refusal).
    assert!(
        !out.exists(),
        "a build refused on a corrupt report must not create the output directory"
    );
}
