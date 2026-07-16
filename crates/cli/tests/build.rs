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
        "styles.css",
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

    // AC-1: styles.css is linked, no inline <style>, semantic regions present,
    // load order preserved.
    // Accept both self-closing <link ... /> and HTML5 <link ... >
    let link_self_closing = index.contains(r#"<link rel="stylesheet" href="styles.css" />"#);
    let link_html5 = index.contains(r#"<link rel="stylesheet" href="styles.css">"#);
    assert!(
        link_self_closing || link_html5,
        "index.html must link styles.css; index={index}"
    );
    assert!(
        !index.contains("<style>"),
        "index.html must not contain inline <style>; index={index}"
    );
    assert!(
        index.contains(r#"<header class="site-header">"#),
        "index.html must contain <header class=\"site-header\">"
    );
    assert!(
        index.contains(r#"<main class="workspace">"#),
        "index.html must contain <main class=\"workspace\">"
    );
    assert!(
        index.contains(r#"<footer class="site-footer">"#),
        "index.html must contain <footer class=\"site-footer\">"
    );
    let coi_pos = index
        .find(r#"src="coi-serviceworker.js""#)
        .expect("coi-serviceworker.js must be referenced");
    let link_pos = index
        .find(r#"href="styles.css""#)
        .expect("styles.css link must be present");
    assert!(
        coi_pos < link_pos,
        "coi-serviceworker.js must appear before styles.css link"
    );

    // AC-2: workspace CSS contract — token usage, data-status badge selectors,
    // no class-based status selectors, no hardcoded hex in workspace rules.
    let css = std::fs::read_to_string(out.join("styles.css")).unwrap();
    let workspace_marker = "/* === workspace === */";
    assert!(
        css.contains(workspace_marker),
        "styles.css must have a workspace section marker"
    );
    let after_marker = css
        .split(workspace_marker)
        .nth(1)
        .expect("workspace section");
    let workspace_var_refs: Vec<&str> = after_marker
        .lines()
        .filter(|l| l.contains("var(--bt-"))
        .collect();
    assert!(
        workspace_var_refs.len() >= 6,
        "workspace section must have >= 6 var(--bt-) refs, got {}",
        workspace_var_refs.len()
    );
    assert!(
        !css.contains(".status-idle")
            && !css.contains(".status-running")
            && !css.contains(".status-pass")
            && !css.contains(".status-fail"),
        "styles.css must not contain .status-* class selectors"
    );
    let status_selector_count = css.matches(r#"#lesson-status[data-status="idle"]"#).count()
        + css
            .matches(r#"#lesson-status[data-status="running"]"#)
            .count()
        + css.matches(r#"#lesson-status[data-status="pass"]"#).count()
        + css.matches(r#"#lesson-status[data-status="fail"]"#).count();
    assert_eq!(
        status_selector_count, 5,
        "expected exactly 5 #lesson-status[data-status=\"...\"] rules \
         (4 status values + 1 dark-mode idle override)"
    );
    // No hardcoded hex in workspace rules — scan the workspace section up to the
    // dark-mode @media block (which carries hex token overrides by design).
    // Match exactly 3, 6, or 8 hex digits (full color, shorthand, or alpha).
    let before_dark_mode = after_marker
        .split("/* ── Dark mode ")
        .next()
        .unwrap_or(after_marker);
    let hex_pat =
        regex_lite::Regex::new(r"#[0-9a-fA-F]{6}(?:[0-9a-fA-F]{2})?\b|#[0-9a-fA-F]{3}\b").unwrap();
    assert!(
        !hex_pat.is_match(before_dark_mode),
        "workspace rules must not contain hardcoded hex color literals"
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
        "styles.css",
    ] {
        assert!(out.join(name).is_file(), "the built site is missing {name}");
    }

    let index = std::fs::read_to_string(out.join("index.html")).unwrap();

    // AC-1: styles.css is linked, no inline <style>, semantic regions present,
    // load order preserved.
    let link_self_closing = index.contains(r#"<link rel="stylesheet" href="styles.css" />"#);
    let link_html5 = index.contains(r#"<link rel="stylesheet" href="styles.css">"#);
    assert!(
        link_self_closing || link_html5,
        "index.html must link styles.css; index={index}"
    );
    assert!(
        !index.contains("<style>"),
        "index.html must not contain inline <style>; index={index}"
    );
    assert!(
        index.contains(r#"<header class="site-header">"#),
        "index.html must contain <header class=\"site-header\">"
    );
    assert!(
        index.contains(r#"<main class="workspace">"#),
        "index.html must contain <main class=\"workspace\">"
    );
    assert!(
        index.contains(r#"<footer class="site-footer">"#),
        "index.html must contain <footer class=\"site-footer\">"
    );
    let coi_pos = index
        .find(r#"src="coi-serviceworker.js""#)
        .expect("coi-serviceworker.js must be referenced");
    let link_pos = index
        .find(r#"href="styles.css""#)
        .expect("styles.css link must be present");
    assert!(
        coi_pos < link_pos,
        "coi-serviceworker.js must appear before styles.css link"
    );

    // AC-2: workspace CSS contract — same as webr build (shared styles.css).
    let css = std::fs::read_to_string(out.join("styles.css")).unwrap();
    let workspace_marker = "/* === workspace === */";
    assert!(
        css.contains(workspace_marker),
        "styles.css must have a workspace section marker"
    );
    let after_marker = css
        .split(workspace_marker)
        .nth(1)
        .expect("workspace section");
    let workspace_var_refs: Vec<&str> = after_marker
        .lines()
        .filter(|l| l.contains("var(--bt-"))
        .collect();
    assert!(
        workspace_var_refs.len() >= 6,
        "workspace section must have >= 6 var(--bt-) refs, got {}",
        workspace_var_refs.len()
    );
    assert!(
        !css.contains(".status-idle")
            && !css.contains(".status-running")
            && !css.contains(".status-pass")
            && !css.contains(".status-fail"),
        "styles.css must not contain .status-* class selectors"
    );
    let status_selector_count = css.matches(r#"#lesson-status[data-status="idle"]"#).count()
        + css
            .matches(r#"#lesson-status[data-status="running"]"#)
            .count()
        + css.matches(r#"#lesson-status[data-status="pass"]"#).count()
        + css.matches(r#"#lesson-status[data-status="fail"]"#).count();
    assert_eq!(
        status_selector_count, 5,
        "expected exactly 5 #lesson-status[data-status=\"...\"] rules \
         (4 status values + 1 dark-mode idle override)"
    );
    // No hardcoded hex in workspace rules — exclude the dark-mode @media block
    // (which carries hex token overrides by design).
    let before_dark_mode = after_marker
        .split("/* ── Dark mode ")
        .next()
        .unwrap_or(after_marker);
    let hex_pat =
        regex_lite::Regex::new(r"#[0-9a-fA-F]{6}(?:[0-9a-fA-F]{2})?\b|#[0-9a-fA-F]{3}\b").unwrap();
    assert!(
        !hex_pat.is_match(before_dark_mode),
        "workspace rules must not contain hardcoded hex color literals"
    );

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
        "listModels(",                    // §2.2 effectful fetch wrapping parseModels
        "${baseUrl}${modelsPath}", // AC4 §3.4 models URL derives from providerBaseUrl() via variable
        r#"createElement("select")"#, // AC1 the picker `<select>` is built dynamically
        "feedbackRequest(prompt, model)", // AC2 the model is an explicit request argument
    ] {
        assert!(
            feedback.contains(token),
            "feedback.js must ship the live model-picker seam token `{token}`; feedback={feedback}"
        );
    }
    // AC5: parseModels must stay provider-agnostic — the function signature takes
    // only `json`, not a `provider` parameter, so it returns the same sorted model
    // list regardless of provider (filtering is the caller's book).
    assert!(
        feedback.contains("function parseModels(json) {"),
        "parseModels must stay provider-agnostic (AC5); feedback={feedback}"
    );

    // AC1/AC3: the fallback literal stays the named default the empty- or failed-
    // query branch resolves to (also pinned alive by the no-`sk-ant` scan).
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

    // Issue #52: the model picker is provider-aware — both auth shapes coexist
    // in the file; selection is provider-conditional, not global (predicate 1).
    assert!(
        feedback.contains("Bearer"),
        "feedback.js must carry Bearer auth (Fireworks) alongside x-api-key (Anthropic); feedback={feedback}"
    );
    assert!(
        feedback.contains("x-api-key"),
        "feedback.js must retain x-api-key auth (Anthropic) alongside Bearer (Fireworks); feedback={feedback}"
    );

    // Issue #52: fallback model is provider-conditional — both Fireworks and
    // Anthropic fallbacks coexist (predicate 2).
    assert!(
        feedback.contains("accounts/fireworks/models/deepseek-v4-flash"),
        "feedback.js must carry the Fireworks fallback model alongside claude-opus-4-8; feedback={feedback}"
    );
    assert!(
        feedback.contains("claude-opus-4-8"),
        "feedback.js must retain claude-opus-4-8 as the Anthropic fallback model; feedback={feedback}"
    );

    // Issue #52: models URL is provider-aware — the doubled /v1/v1 path must not
    // appear (predicate 3: Fireworks base URL already carries /v1, so /v1/models
    // would double).
    assert!(
        !feedback.contains("/v1/v1/models"),
        "feedback.js must NOT contain the doubled /v1/v1/models path; feedback={feedback}"
    );
    // Positive pin: modelsPath is provider-conditional — a regression to a
    // hardcoded "/v1/models" would pass the negative /v1/v1 check while breaking
    // Fireworks at runtime (Fireworks base URL already carries /v1).
    assert!(
        feedback.contains("provider === \"fireworks\" ? \"/models\" : \"/v1/models\""),
        "modelsPath must be provider-conditional (Fireworks /models, Anthropic /v1/models); \
         feedback={feedback}"
    );

    // Issue #52: the provider discriminator threads end-to-end through the model
    // picker — listModels receives a `provider` field (predicate 4).
    assert!(
        feedback.contains("listModels({ baseUrl, apiKey, provider")
            || feedback.contains("listModels({ baseUrl, apiKey, provider,"),
        "listModels must receive the provider discriminator; feedback={feedback}"
    );

    // Issue #52 sneaky-pass 1 guard: handleSubmit must thread providerId into
    // renderModelPicker — dropping `provider: providerId` from the call site
    // (line ~586) passes every other assertion (listModels and renderModelPicker
    // still reference `provider` as a parameter) but at runtime `source.provider`
    // is undefined → Anthropic fallback → Fireworks key sent as x-api-key → 401
    // → empty roster → claude-opus-4-8 regardless of provider.
    assert!(
        feedback.contains("provider: providerId"),
        "handleSubmit must thread providerId into renderModelPicker (sneaky-pass 1 guard); \
         feedback={feedback}"
    );

    // Issue #52 sneaky-pass 3 guard: modelRoster must take a provider parameter
    // AND the call site must thread it — reverting to `modelRoster(models)`
    // silently returns [MODEL] for the Fireworks picker, hiding all
    // non-Anthropic models even when the function definition still nominally
    // names the parameter.
    assert!(
        feedback.contains("modelRoster(models, provider)"),
        "modelRoster must take a provider parameter (sneaky-pass 3 guard); \
         feedback={feedback}"
    );
    assert!(
        feedback.contains("modelRoster(await listModels"),
        "modelRoster call site must thread provider from listModels result \
         (sneaky-pass 3 call-site guard); feedback={feedback}"
    );
    assert!(
        feedback.contains("}), provider)"),
        "modelRoster call site must thread provider as the 2nd arg (sneaky-pass 3); feedback={feedback}"
    );

    // Terminal sneaky-pass-3 guard: the provider-conditional fallback ternary
    // must appear in BOTH modelRoster (line ~197) and renderModelPicker (line
    // ~522) — a regression that removes it from either function while leaving
    // the other copy intact would pass a `feedback.contains(...)` vacuously.
    // Count >= 2 pins the actual fallback *behavior* (not just seams).
    let fallback_ternary = "provider === \"fireworks\" ? FIREWORKS_MODEL : MODEL";
    let ternary_count = feedback.matches(fallback_ternary).count();
    assert!(
        ternary_count >= 2,
        "the provider-conditional fallback ternary must appear in BOTH modelRoster and \
         renderModelPicker (count {}); feedback={feedback}",
        ternary_count
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
fn build_webr_ships_the_byok_fireworks_feedback_seam() {
    // AC1: a built site carries the byokFireworks FeedbackBackend alongside
    // byokAnthropic — interchangeable contract, OpenAI-compatible Fireworks
    // backend (issue #50). The 9 anti-copy-paste tripwires guard against a
    // verbatim Anthropic clone that swapped only the URL/auth.
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build("webr", R_COURSE, &out);
    assert!(
        output.status.success(),
        "`build --target webr` should exit 0, got {:?}; stderr={:?}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    let feedback = std::fs::read_to_string(out.join("feedback.js"))
        .expect("the built site must ship feedback.js");

    // AC1: the 9 anti-copy-paste tripwires — a verbatim Anthropic clone would
    // miss these tokens (the Negative case):

    // 1. The factory function must be named byokFireworks, not a copy of byokAnthropic
    assert!(
        feedback.contains("byokFireworks("),
        "feedback.js must define the byokFireworks factory; feedback={feedback}"
    );

    // 2. The endpoint must be /chat/completions, not /v1/messages
    assert!(
        feedback.contains("/chat/completions"),
        "byokFireworks must POST /chat/completions; feedback={feedback}"
    );

    // 3. The auth scheme is Bearer, not x-api-key
    assert!(
        feedback.contains("Bearer"),
        "byokFireworks must send the key as a Bearer token; feedback={feedback}"
    );

    // 4. The header name is "Authorization", not "x-api-key"
    assert!(
        feedback.contains("\"Authorization\""),
        "byokFireworks must use the Authorization header; feedback={feedback}"
    );

    // 5. The tool definition uses parameters (OpenAI shape), not input_schema (Anthropic)
    assert!(
        feedback.contains("parameters"),
        "byokFireworks must use parameters (not input_schema); feedback={feedback}"
    );

    // 6. tool_choice uses type: function (OpenAI), not type: tool (Anthropic)
    assert!(
        feedback.contains("\"type\": \"function\""),
        "byokFireworks must use type: function in tool_choice; feedback={feedback}"
    );

    // 7. Response parsing accesses tool_calls (OpenAI), not content[] (Anthropic)
    assert!(
        feedback.contains("tool_calls"),
        "byokFireworks must read tool_calls from the response; feedback={feedback}"
    );

    // 8. Response parsing accesses choices (OpenAI), not data.content (Anthropic)
    assert!(
        feedback.contains("choices"),
        "byokFireworks must read choices from the response; feedback={feedback}"
    );

    // 9. The arguments field is a JSON string — JSON.parse is mandatory
    assert!(
        feedback.contains("JSON.parse"),
        "byokFireworks must JSON.parse the function arguments; feedback={feedback}"
    );

    // Named fallback model const (mirrors MODEL = "claude-opus-4-8"). Pins both
    // the const name so a regression that inlines the value still names the const,
    // and the absence of the inline literal so the request always reads from the
    // const (mirrors the Anthropic `!model: MODEL` pin).
    assert!(
        feedback.contains("FIREWORKS_MODEL"),
        "byokFireworks must define a FIREWORKS_MODEL const; feedback={feedback}"
    );
    assert!(
        feedback.contains("accounts/fireworks/models/deepseek-v4-flash"),
        "byokFireworks must carry the named fallback model const; feedback={feedback}"
    );
    assert!(
        !feedback.contains("model: \"accounts/fireworks/models/deepseek-v4-flash\""),
        "the Fireworks request must read the model from a const, not an inline literal; feedback={feedback}"
    );

    // The tool name constant is reused (not forked)
    assert!(
        feedback.contains("respond_with_feedback"),
        "byokFireworks must reuse the shared TOOL_NAME; feedback={feedback}"
    );

    // Structural pins (dead-function guard): name: and getFeedback must appear
    // in the byokFireworks factory return
    assert!(
        feedback.contains("name:"),
        "byokFireworks must return an object with a name field; feedback={feedback}"
    );
    assert!(
        feedback.contains("getFeedback"),
        "byokFireworks must expose a getFeedback method; feedback={feedback}"
    );

    // The request builder must take the model as an explicit argument, not close
    // over FIREWORKS_MODEL (mirrors the `feedbackRequest(prompt, model)` pin in
    // the Anthropic test).
    assert!(
        feedback.contains("fireworksRequest(prompt, model)"),
        "fireworksRequest must take the model as an argument, not close over FIREWORKS_MODEL; feedback={feedback}"
    );

    // Mapping pins (bidirectional-contract guard): the response mapper returns
    // correct: and message: (mirrors the Rust Verdict contract)
    assert!(
        feedback.contains("correct:"),
        "byokFireworks response mapper must return correct:; feedback={feedback}"
    );
    assert!(
        feedback.contains("message:"),
        "byokFireworks response mapper must return message:; feedback={feedback}"
    );

    // AC1 + AC2 negative: no emitted file may contain the Fireworks key prefix
    // `fw_` (a baked-in key literal) or `/v1/v1/chat/completions` (doubled path).
    // `fireworks_api_key` is now a legitimate sessionStorage slot name (the
    // PROVIDERS map in issue #51), so it is NOT scanned here.
    // Scan the *whole* emitted site, not just feedback.js (mirrors the Anthropic
    // `sk-ant` whole-site scan).
    for (path, contents) in emitted_files(&out) {
        for forbidden in ["fw_", "/v1/v1/chat/completions"] {
            assert!(
                !contents.contains(forbidden),
                "no emitted file may contain `{forbidden}`; found in {path:?}"
            );
        }
    }
}

#[test]
fn build_webr_ships_the_multi_provider_feedback_seam() {
    // Issue #51 (AC2): a built site carries a provider chooser + per-provider
    // routing — the PROVIDERS map drives the key slot, base URL, fallback model,
    // and which FeedbackBackend handleSubmit constructs. Fireworks is the
    // pre-selected default.
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build("webr", R_COURSE, &out);
    assert!(
        output.status.success(),
        "`build --target webr` should exit 0, got {:?}; stderr={:?}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    let feedback = std::fs::read_to_string(out.join("feedback.js"))
        .expect("the built site must ship feedback.js");

    // AC1: a provider chooser <select data-byok="provider"> exists with
    // Fireworks as the selected default. The chooser is built dynamically via
    // DOM API (not a template literal), so we assert the programmatic
    // pattern: the select element carries data-byok="provider", the
    // DEFAULT_PROVIDER constant names fireworks, and the rendering code sets
    // option.selected based on the default.
    assert!(
        feedback.contains(r#"data-byok="provider""#),
        "feedback.js must ship a provider chooser with data-byok=provider; \
         feedback={feedback}"
    );
    assert!(
        feedback.contains("DEFAULT_PROVIDER"),
        "feedback.js must define a DEFAULT_PROVIDER constant; feedback={feedback}"
    );
    assert!(
        feedback.contains(r#""fireworks""#),
        "feedback.js must reference the fireworks provider id; feedback={feedback}"
    );
    assert!(
        feedback.contains(r#""Anthropic""#),
        "feedback.js must reference the Anthropic provider label; feedback={feedback}"
    );
    // Opportunistic (#51 review): pin the full assignment so a default-swap
    // regression that changes the constant without updating the wiring is caught.
    assert!(
        feedback.contains(r#"DEFAULT_PROVIDER = "fireworks""#),
        "feedback.js must set DEFAULT_PROVIDER to fireworks; feedback={feedback}"
    );
    // The option.selected wiring makes the default effective at render time.
    assert!(
        feedback.contains("id === DEFAULT_PROVIDER") && feedback.contains("option.selected = true"),
        "feedback.js must wire id === DEFAULT_PROVIDER to option.selected; feedback={feedback}"
    );

    // AC2: a closed-set PROVIDERS map carries both fireworks and anthropic with
    // their keySlot, baseUrl, fallbackModel, and factory.
    for provider_token in [
        "PROVIDERS",
        "fireworks_api_key",
        "anthropic_api_key",
        "https://api.fireworks.ai/inference/v1",
        "https://api.anthropic.com",
        "accounts/fireworks/models/deepseek-v4-flash",
        "claude-opus-4-8",
        "byokFireworks",
        "byokAnthropic",
    ] {
        assert!(
            feedback.contains(provider_token),
            "feedback.js must ship the PROVIDERS map token `{provider_token}`; \
             feedback={feedback}"
        );
    }

    // AC3: handleSubmit routes through PROVIDERS[providerId].factory, not a
    // hardcoded backend name (dead-chooser guard).
    assert!(
        feedback.contains("PROVIDERS[providerId].factory"),
        "handleSubmit must route through PROVIDERS[providerId].factory, not hardcode a backend; feedback={feedback}"
    );

    // AC4: fireworks_api_key is a distinct sessionStorage slot AND
    // anthropic_api_key persists (no key-slot leakage).
    assert!(
        feedback.contains("fireworks_api_key"),
        "feedback.js must define fireworks_api_key as a distinct key slot; \
         feedback={feedback}"
    );
    assert!(
        feedback.contains("anthropic_api_key"),
        "feedback.js must keep anthropic_api_key as a key slot; \
         feedback={feedback}"
    );

    // AC5: providerBaseUrl(providerId) returns the Fireworks URL for a Fireworks
    // selection in the hard-default path.
    assert!(
        feedback.contains("https://api.fireworks.ai/inference/v1"),
        "feedback.js must carry the Fireworks base URL; feedback={feedback}"
    );
    assert!(
        feedback.contains("https://api.anthropic.com"),
        "feedback.js must keep the Anthropic base URL; feedback={feedback}"
    );

    // AC6: host-gate tokens all persist (the gate did not regress).
    for guard in [
        r#"url.hostname === "localhost""#,
        r#"url.hostname === "127.0.0.1""#,
        "!url.username && !url.password",
    ] {
        assert!(
            feedback.contains(guard),
            "feedback.js must host-gate the ?provider= override with `{guard}`"
        );
    }

    // AC7: the ?provider= override value is still parsed via new URL() (it
    // remains a URL test-seam, NOT a provider-name selector).
    assert!(
        feedback.contains("new URL(override)"),
        "?provider= override must still be parsed via new URL(); feedback={feedback}"
    );

    // AC8: per-provider disclosure — BOTH per-provider phrases appear.
    let lower = feedback.to_lowercase();
    assert!(
        lower.contains("sent only to fireworks"),
        "feedback.js must disclose Fireworks-only transmission; feedback={feedback}"
    );
    assert!(
        lower.contains("sent only to anthropic"),
        "feedback.js must disclose Anthropic-only transmission; feedback={feedback}"
    );

    // AC9: byokAnthropic is NOT removed.
    assert!(
        feedback.contains("byokAnthropic"),
        "byokAnthropic must still be present in feedback.js; feedback={feedback}"
    );

    // AC10: a negative scan of EVERY emitted file rejects the Fireworks key
    // prefix fw_ (symmetric to the existing sk-ant scan in
    // build_webr_ships_the_byok_anthropic_feedback_seam).
    //
    // Also re-scan for sk-ant since this is an independent test that must also
    // guard against that regression.
    for (path, contents) in emitted_files(&out) {
        assert!(
            !contents.contains("fw_"),
            "a Fireworks key prefix must never ship baked in; found in {path:?}"
        );
        assert!(
            !contents.contains("sk-ant"),
            "an Anthropic key prefix must never ship baked in; found in {path:?}"
        );
    }
}

// AC11 (byte-identity across targets) is exercised by
// build_pyodide_ships_the_same_shared_feedback_seam below — that test stays
// green unchanged.

/// Extract the declaration block (text between `{` and `}`) following `selector`
/// in `css`. Uses brace-counting for correctness — regex can't reliably match
/// nested braces.
fn css_decl_block<'a>(css: &'a str, selector: &str) -> &'a str {
    let pos = css
        .find(selector)
        .unwrap_or_else(|| panic!("selector `{selector}` not found"));
    let rest = &css[pos + selector.len()..];
    let brace = rest
        .find('{')
        .unwrap_or_else(|| panic!("selector `{selector}` not followed by {{"));
    let body = &rest[brace + 1..];
    let mut depth = 1u32;
    for (i, ch) in body.char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    return &body[..i];
                }
            }
            _ => {}
        }
    }
    panic!("unclosed declaration block after selector `{selector}`");
}

#[test]
fn build_webr_styles_byok_feedback_panel() {
    // AC-3: style the BYOK feedback panel via data-byok / data-correct /
    // #byok-disclosure attribute selectors. Pure CSS — feedback.js untouched.
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");
    let output = build("webr", R_COURSE, &out);
    assert!(
        output.status.success(),
        "`build --target webr` should exit 0, got {:?}; stderr={:?}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    let css = std::fs::read_to_string(out.join("styles.css"))
        .expect("the built site must ship styles.css");

    // (1) — the BYOK/feedback section marker exists
    let feedback_marker = "/* === feedback / BYOK === */";
    assert!(
        css.contains(feedback_marker),
        "styles.css must have a /* === feedback / BYOK === */ section marker"
    );

    // Everything after the marker is the BYOK section
    let byok_section = css
        .split(feedback_marker)
        .nth(1)
        .expect("BYOK section after marker");

    // (1) — all 11 required selectors present
    let selectors: &[&str] = &[
        r#"[data-byok="key-prompt"]"#,
        r#"[data-byok="provider"]"#,
        r#"[data-byok="model-picker"]"#,
        r#"[data-byok="model"]"#,
        r#"[data-byok="models-loading"]"#,
        r#"[data-byok="pending"]"#,
        r#"[data-byok="verdict"]"#,
        r#"[data-byok="verdict"][data-correct="true"]"#,
        r#"[data-byok="verdict"][data-correct="false"]"#,
        r#"[data-byok="error"]"#,
        "#byok-disclosure",
    ];
    for selector in selectors {
        assert!(
            byok_section.contains(selector),
            "BYOK section must contain selector `{selector}`"
        );
    }

    // (2) — each BYOK selector block references >= 1 var(--bt-*) token
    let var_pat = regex_lite::Regex::new(r"var\(--bt-").unwrap();
    let var_count = var_pat.find_iter(byok_section).count();
    assert!(
        var_count >= 11,
        "BYOK section must have >= 11 var(--bt-) refs, got {}",
        var_count
    );

    // (4) — verdict true/false selectors produce DIFFERENT declarations
    let true_block = css_decl_block(
        byok_section,
        r#"[data-byok="verdict"][data-correct="true"]"#,
    );
    let false_block = css_decl_block(
        byok_section,
        r#"[data-byok="verdict"][data-correct="false"]"#,
    );
    assert_ne!(
        true_block, false_block,
        "verdict true and false blocks must have different declarations"
    );
    // True block references success-bg (green-tinted)
    assert!(
        true_block.contains("var(--bt-color-success-bg)"),
        "verdict true block must reference `--bt-color-success-bg`; block=`{true_block}`"
    );
    // False block references danger-bg (amber/red-tinted)
    assert!(
        false_block.contains("var(--bt-color-danger-bg)"),
        "verdict false block must reference `--bt-color-danger-bg`; block=`{false_block}`"
    );

    // (5) — #byok-disclosure has color or font-size distinct from body
    let disclosure_block = css_decl_block(&css, "#byok-disclosure");
    assert!(
        disclosure_block.contains("color") || disclosure_block.contains("font-size"),
        "#byok-disclosure block must have 'color' or 'font-size' property; \
         block=`{disclosure_block}`"
    );

    // (3) — >= 1 new AC-3 token declared in :root AND referenced in BYOK block
    assert!(
        css.contains("--bt-color-success-bg"),
        "styles.css must declare `--bt-color-success-bg` in :root"
    );
    assert!(
        css.contains("--bt-color-danger-bg"),
        "styles.css must declare `--bt-color-danger-bg` in :root"
    );
    assert!(
        byok_section.contains("var(--bt-color-success-bg)")
            || byok_section.contains("var(--bt-color-danger-bg)"),
        "at least one AC-3 token must be USED in BYOK section"
    );

    // (2) — no hardcoded hex in BYOK rules (tokens must carry color values).
    // Exclude the dark-mode @media block which carries hex token overrides by design.
    let before_dark_mode = byok_section
        .split("/* ── Dark mode ")
        .next()
        .unwrap_or(byok_section);
    let hex_pat =
        regex_lite::Regex::new(r"#[0-9a-fA-F]{6}(?:[0-9a-fA-F]{2})?\b|#[0-9a-fA-F]{3}\b").unwrap();
    assert!(
        !hex_pat.is_match(before_dark_mode),
        "BYOK rules must not contain hardcoded hex color literals"
    );

    // styles.css byte-identical across targets (shared asset invariant)
    let pyo_tmp = tempfile::tempdir().unwrap();
    let pyo_out = pyo_tmp.path().join("site");
    assert!(
        build("pyodide", PYTHON_COURSE, &pyo_out).status.success(),
        "pyodide build for byte-identity check should succeed"
    );
    let pyo_css = std::fs::read_to_string(pyo_out.join("styles.css"))
        .expect("pyodide site must ship styles.css");
    assert_eq!(
        css, pyo_css,
        "styles.css must be byte-identical across targets (shared asset)"
    );

    // feedback.js byte-identity (AC invariant: feedback.js untouched)
    let webr_feedback = std::fs::read_to_string(out.join("feedback.js"))
        .expect("the built webr site must ship feedback.js");
    let pyo_feedback = std::fs::read_to_string(pyo_out.join("feedback.js"))
        .expect("the built pyodide site must ship feedback.js");
    assert_eq!(
        webr_feedback, pyo_feedback,
        "feedback.js must be byte-identical across targets (shared asset)"
    );
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

// ── Dark-mode token overrides (AC-1) ──────────────────────────────

/// Parse a hex color string into (R, G, B) in [0.0, 1.0].
/// Accepts `#rgb`, `#rrggbb`, or `#rrggbbaa` (alpha ignored).
fn hex_to_rgb(hex: &str) -> (f64, f64, f64) {
    let hex = hex.trim_start_matches('#');
    let (r, g, b) = match hex.len() {
        3 => {
            let r = u8::from_str_radix(&hex[0..1].repeat(2), 16).unwrap();
            let g = u8::from_str_radix(&hex[1..2].repeat(2), 16).unwrap();
            let b = u8::from_str_radix(&hex[2..3].repeat(2), 16).unwrap();
            (r, g, b)
        }
        6 | 8 => {
            let r = u8::from_str_radix(&hex[0..2], 16).unwrap();
            let g = u8::from_str_radix(&hex[2..4], 16).unwrap();
            let b = u8::from_str_radix(&hex[4..6], 16).unwrap();
            (r, g, b)
        }
        _ => panic!("unexpected hex length: {hex}"),
    };
    (r as f64 / 255.0, g as f64 / 255.0, b as f64 / 255.0)
}

/// sRGB channel to linear (WCAG 2.1 §1.4.3).
fn srgb_to_linear(c: f64) -> f64 {
    if c <= 0.04045 {
        c / 12.92
    } else {
        ((c + 0.055) / 1.055).powf(2.4)
    }
}

/// Relative luminance per WCAG 2.1 §1.4.3.
fn relative_luminance(hex: &str) -> f64 {
    let (r, g, b) = hex_to_rgb(hex);
    0.2126 * srgb_to_linear(r) + 0.7152 * srgb_to_linear(g) + 0.0722 * srgb_to_linear(b)
}

/// WCAG contrast ratio (lighter + 0.05) / (darker + 0.05).
fn contrast_ratio(l1: f64, l2: f64) -> f64 {
    let lighter = l1.max(l2);
    let darker = l1.min(l2);
    (lighter + 0.05) / (darker + 0.05)
}

/// Strip `/* ... */` CSS comments from a string (non-nestable per CSS spec).
fn strip_css_comments(s: &str) -> String {
    let mut result = String::new();
    let mut in_comment = false;
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '/' && chars.peek() == Some(&'*') && !in_comment {
            in_comment = true;
            chars.next(); // consume '*'
        } else if c == '*' && chars.peek() == Some(&'/') && in_comment {
            in_comment = false;
            chars.next(); // consume '/'
        } else if !in_comment {
            result.push(c);
        }
    }
    result
}

/// Parse CSS declarations from a `{ ... }` block body into (name, value) pairs.
/// Strips CSS comments from the WHOLE block before splitting on `;`. This
/// ordering is load-bearing: a `;` inside a `/* ... */` comment would otherwise
/// create a false split, and per-piece stripping starts each piece with
/// `in_comment=false`, so the `*/` of a comment opened in a prior piece is
/// treated as literal text — swallowing the declaration that follows it.
fn parse_css_declarations(block: &str) -> Vec<(String, String)> {
    let cleaned = strip_css_comments(block);
    cleaned
        .split(';')
        .filter_map(|decl| {
            let trimmed = decl.trim();
            if trimmed.is_empty() {
                return None;
            }
            let mut parts = trimmed.splitn(2, ':');
            let name = parts.next()?.trim().to_string();
            let value = parts.next()?.trim().to_string();
            if name.is_empty() || value.is_empty() {
                return None;
            }
            Some((name, value))
        })
        .collect()
}

/// Return the byte position of the closing `}` for `selector`'s block.
fn selector_block_end(css: &str, selector: &str) -> usize {
    let pos = css
        .find(selector)
        .unwrap_or_else(|| panic!("selector `{selector}` not found"));
    let after_sel = &css[pos + selector.len()..];
    let brace = after_sel
        .find('{')
        .unwrap_or_else(|| panic!("selector `{selector}` not followed by {{"));
    let body = &after_sel[brace + 1..];
    let mut depth = 1u32;
    for (i, ch) in body.char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    return pos + selector.len() + brace + 1 + i;
                }
            }
            _ => {}
        }
    }
    panic!("unclosed declaration block after selector `{selector}`");
}

#[test]
fn build_dark_mode_token_overrides() {
    // Build both targets
    let tmp = tempfile::tempdir().unwrap();
    let webr_out = tmp.path().join("webr");
    let pyo_out = tmp.path().join("pyodide");

    assert!(
        build("webr", R_COURSE, &webr_out).status.success(),
        "webr build should succeed"
    );
    assert!(
        build("pyodide", PYTHON_COURSE, &pyo_out).status.success(),
        "pyodide build should succeed"
    );

    // Clause 10: Both index.html shells reference styles.css
    for (label, out) in [("webr", &webr_out), ("pyodide", &pyo_out)] {
        let index = std::fs::read_to_string(out.join("index.html")).unwrap();
        let link_self_closing = index.contains(r#"<link rel="stylesheet" href="styles.css" />"#);
        let link_html5 = index.contains(r#"<link rel="stylesheet" href="styles.css">"#);
        assert!(
            link_self_closing || link_html5,
            "{label} index.html must link styles.css; index={index}"
        );
    }

    let css = std::fs::read_to_string(webr_out.join("styles.css")).unwrap();
    let pyo_css = std::fs::read_to_string(pyo_out.join("styles.css")).unwrap();

    // Clause 9: Cross-target byte-identity
    assert_eq!(
        css, pyo_css,
        "styles.css must be byte-identical across targets"
    );

    let hex_pat =
        regex_lite::Regex::new(r"#[0-9a-fA-F]{6}(?:[0-9a-fA-F]{2})?\b|#[0-9a-fA-F]{3}\b").unwrap();

    // Clause 1: @media block appears AFTER closing `}` of light :root.
    // Search from after the light :root block to avoid matching the selector
    // in the file header comment.
    let light_root_close = selector_block_end(&css, ":root");
    let after_root = &css[light_root_close..];
    let media_pos = after_root
        .find("@media (prefers-color-scheme: dark)")
        .expect("@media (prefers-color-scheme: dark) block must exist");
    assert!(
        media_pos > 0,
        "@media block must appear after light :root closing brace (searched from post-root slice)"
    );

    // Clause 2: Media block contains a :root sub-block, extracted via brace-counting
    let media_body = css_decl_block(after_root, "@media (prefers-color-scheme: dark)");
    let dark_root_body = css_decl_block(media_body, ":root");

    // Parse light :root tokens
    let light_root_body = css_decl_block(&css, ":root");
    let light_tokens: Vec<(String, String)> = parse_css_declarations(light_root_body)
        .into_iter()
        .filter(|(name, _)| name.starts_with("--bt-color-"))
        .collect();
    let dark_tokens: Vec<(String, String)> = parse_css_declarations(dark_root_body)
        .into_iter()
        .filter(|(name, _)| name.starts_with("--bt-color-"))
        .collect();

    // Clause 3: All 21 tokens from light :root declared in dark :root
    assert_eq!(
        light_tokens.len(),
        21,
        "light :root must have exactly 21 --bt-color-* tokens"
    );
    assert_eq!(
        dark_tokens.len(),
        21,
        "dark :root must have exactly 21 --bt-color-* tokens"
    );
    for (name, _) in &light_tokens {
        assert!(
            dark_tokens.iter().any(|(dn, _)| dn == name),
            "dark :root must declare `{name}`"
        );
    }

    // Build a lookup map for quick access
    let light_map: std::collections::HashMap<&str, &str> = light_tokens
        .iter()
        .map(|(n, v)| (n.as_str(), v.as_str()))
        .collect();
    let dark_map: std::collections::HashMap<&str, &str> = dark_tokens
        .iter()
        .map(|(n, v)| (n.as_str(), v.as_str()))
        .collect();

    // Clause 4: Each dark value DIFFERS from its light counterpart
    for (name, light_val) in &light_tokens {
        let dark_val = dark_map[name.as_str()];
        assert_ne!(
            light_val, dark_val,
            "dark `{name}` value must differ from light value"
        );
    }

    // Clause 5: Directional sanity
    // surface/bg tokens must darken (RGB sum strictly lower)
    for token_name in &[
        "--bt-color-surface",
        "--bt-color-surface-code",
        "--bt-color-success-bg",
        "--bt-color-danger-bg",
        "--bt-color-status-idle",
    ] {
        let light_val = light_map[token_name];
        let dark_val = dark_map[token_name];
        let light_rgb = hex_to_rgb(light_val);
        let dark_rgb = hex_to_rgb(dark_val);
        let light_total = light_rgb.0 + light_rgb.1 + light_rgb.2;
        let dark_total = dark_rgb.0 + dark_rgb.1 + dark_rgb.2;
        assert!(
            dark_total < light_total,
            "`{token_name}` must darken (RGB sum {:.3} < {:.3}): \
             light={light_val}, dark={dark_val}",
            dark_total,
            light_total
        );
    }
    // text tokens must lighten (RGB sum strictly higher)
    for token_name in &["--bt-color-text-primary", "--bt-color-text-secondary"] {
        let light_val = light_map[token_name];
        let dark_val = dark_map[token_name];
        let light_rgb = hex_to_rgb(light_val);
        let dark_rgb = hex_to_rgb(dark_val);
        let light_total = light_rgb.0 + light_rgb.1 + light_rgb.2;
        let dark_total = dark_rgb.0 + dark_rgb.1 + dark_rgb.2;
        assert!(
            dark_total > light_total,
            "`{token_name}` must lighten (RGB sum {:.3} > {:.3}): \
             light={light_val}, dark={dark_val}",
            dark_total,
            light_total
        );
    }

    // Clause 5b: Syntax-token anti-invisibility (AC-3 carry-over). The 7 syntax
    // tokens color code-editor syntax spans rendered on the --bt-color-surface-
    // code background. If a syntax token EQUALS surface-code, the token is
    // invisible against the code surface (anti-invisibility invariant). This
    // catches a regression where a syntax token is accidentally set to the
    // surface-code value in either light or dark mode. Checked for BOTH modes
    // because a token invisible in one mode defeats the editor's purpose.
    for token_name in &[
        "--bt-color-syntax-keyword",
        "--bt-color-syntax-string",
        "--bt-color-syntax-number",
        "--bt-color-syntax-comment",
        "--bt-color-syntax-variable",
        "--bt-color-syntax-function",
        "--bt-color-syntax-operator",
    ] {
        let light_syntax = light_map[token_name];
        let light_surface_code = light_map["--bt-color-surface-code"];
        assert_ne!(
            light_syntax, light_surface_code,
            "light `{token_name}` must not equal surface-code (would be invisible)"
        );
        let dark_syntax = dark_map[token_name];
        let dark_surface_code = dark_map["--bt-color-surface-code"];
        assert_ne!(
            dark_syntax, dark_surface_code,
            "dark `{token_name}` must not equal surface-code (would be invisible)"
        );
    }

    // Clause 6: No --bt-font-*, --bt-space-*, --bt-shadow-*, --bt-radius-* in dark :root
    let dark_all_tokens: Vec<(String, String)> = parse_css_declarations(dark_root_body);
    for (name, _) in &dark_all_tokens {
        assert!(
            !name.starts_with("--bt-font-"),
            "dark :root must not contain `{name}` (non-color token)"
        );
        assert!(
            !name.starts_with("--bt-space-"),
            "dark :root must not contain `{name}` (non-color token)"
        );
        assert!(
            !name.starts_with("--bt-shadow-"),
            "dark :root must not contain `{name}` (non-color token)"
        );
        assert!(
            !name.starts_with("--bt-radius-"),
            "dark :root must not contain `{name}` (non-color token)"
        );
    }

    // Clause 7: Zero hex literals outside both :root blocks
    // Compute intervals of both :root block bodies
    let light_body = css_decl_block(&css, ":root");
    let light_body_start = light_body.as_ptr() as usize - css.as_ptr() as usize;
    let light_body_end = light_body_start + light_body.len();

    // Use after_root slice to avoid matching @media comment in header
    let media_body_in_css = css_decl_block(after_root, "@media (prefers-color-scheme: dark)");
    // The :root inside the media block — need the body range in the original CSS
    let drb = css_decl_block(media_body_in_css, ":root");
    let dark_body_start = drb.as_ptr() as usize - css.as_ptr() as usize;
    let dark_body_end = dark_body_start + drb.len();

    for m in hex_pat.find_iter(&css) {
        let pos = m.start();
        let in_light = pos >= light_body_start && pos < light_body_end;
        let in_dark = pos >= dark_body_start && pos < dark_body_end;
        assert!(
            in_light || in_dark,
            "hex literal `{}` at position {} must be inside a :root block (not outside)",
            m.as_str(),
            pos
        );
    }

    // Clause 8: WCAG contrast ratios for sampled pairs (dark mode)
    let dark_pairs: Vec<(&str, &str, &str, f64)> = vec![
        ("text-primary on surface", "text-primary", "surface", 4.5),
        (
            "text-secondary on surface",
            "text-secondary",
            "surface",
            4.5,
        ),
        (
            "text-primary on surface-code",
            "text-primary",
            "surface-code",
            4.5,
        ),
        (
            "text-primary on success-bg",
            "text-primary",
            "success-bg",
            4.5,
        ),
        (
            "text-primary on danger-bg",
            "text-primary",
            "danger-bg",
            4.5,
        ),
        ("status-pass on surface", "status-pass", "surface", 3.0),
        ("status-fail on surface", "status-fail", "surface", 3.0),
        (
            "status-running on surface",
            "status-running",
            "surface",
            3.0,
        ),
        ("status-idle on surface", "status-idle", "surface", 3.0),
        ("surface on status-idle", "surface", "status-idle", 4.5),
        ("brand on surface", "brand", "surface", 3.0),
        ("border on surface", "border", "surface", 3.0),
        ("border on surface-code", "border", "surface-code", 3.0),
        ("border on success-bg", "border", "success-bg", 3.0),
        ("border on danger-bg", "border", "danger-bg", 3.0),
        // Verdict <strong> contrast — status-* on verdict bg
        (
            "status-pass on success-bg",
            "status-pass",
            "success-bg",
            4.5,
        ),
        ("status-fail on danger-bg", "status-fail", "danger-bg", 4.5),
    ];
    for (label, fg_token, bg_token, min_ratio) in &dark_pairs {
        // Resolve full token names
        let fg_name = format!("--bt-color-{fg_token}");
        let bg_name = format!("--bt-color-{bg_token}");
        let fg_val = dark_map[fg_name.as_str()];
        let bg_val = dark_map[bg_name.as_str()];
        let l_fg = relative_luminance(fg_val);
        let l_bg = relative_luminance(bg_val);
        let cr = contrast_ratio(l_fg, l_bg);
        assert!(
            cr >= *min_ratio,
            "{label}: contrast ratio {cr:.2} < {min_ratio} (fg={fg_val}, bg={bg_val}, \
             L_fg={l_fg:.4}, L_bg={l_bg:.4})"
        );
    }

    // Light-mode contrast check — parallel to dark check above.
    // Pins light-mode pairs that could regress from dark-mode-only fixes.
    // Only includes pairs with real text-on-background usages in light mode:
    //   - border tokens are 1px lines, not meaningful UI components (3:1 N/A)
    //   - status-idle is used as a background, not text, in light mode
    //   - status-on-surface pairs cover the pill/verdict badge use cases
    let light_pairs: Vec<(&str, &str, &str, f64)> = vec![
        ("text-primary on surface", "text-primary", "surface", 4.5),
        (
            "text-secondary on surface",
            "text-secondary",
            "surface",
            4.5,
        ),
        (
            "text-primary on surface-code",
            "text-primary",
            "surface-code",
            4.5,
        ),
        // Idle pill text in light mode uses text-secondary on status-idle.
        // This pair would have caught the cycle-1 light-mode regression.
        (
            "text-secondary on status-idle (light)",
            "text-secondary",
            "status-idle",
            4.5,
        ),
        ("status-pass on surface", "status-pass", "surface", 3.0),
        ("status-fail on surface", "status-fail", "surface", 3.0),
        (
            "status-running on surface",
            "status-running",
            "surface",
            3.0,
        ),
        ("brand on surface", "brand", "surface", 3.0),
    ];
    for (label, fg_token, bg_token, min_ratio) in &light_pairs {
        let fg_name = format!("--bt-color-{fg_token}");
        let bg_name = format!("--bt-color-{bg_token}");
        let fg_val = light_map[fg_name.as_str()];
        let bg_val = light_map[bg_name.as_str()];
        let l_fg = relative_luminance(fg_val);
        let l_bg = relative_luminance(bg_val);
        let cr = contrast_ratio(l_fg, l_bg);
        assert!(
            cr >= *min_ratio,
            "LIGHT {label}: contrast ratio {cr:.2} < {min_ratio} (fg={fg_val}, bg={bg_val}, \
             L_fg={l_fg:.4}, L_bg={l_bg:.4})"
        );
    }
}

// ── Issue #72: lesson packages field ──────────────────────────────

/// Write a temp course with a single Python lesson whose YAML is `lesson_yaml`,
/// returning the course directory path. The manifest lists one lesson `add-two`
/// at `add.yaml`.
fn write_packages_course(tmp: &tempfile::TempDir, lesson_yaml: &str) -> PathBuf {
    let course = tmp.path().join("course");
    std::fs::create_dir_all(&course).unwrap();
    std::fs::write(
        course.join("blendtutor.toml"),
        "[[lessons]]\nid = \"add-two\"\npath = \"add.yaml\"\n",
    )
    .unwrap();
    std::fs::write(course.join("add.yaml"), lesson_yaml).unwrap();
    course
}

/// A minimal Python lesson that declares `packages: [pandas]`.
const LESSON_PY_WITH_PACKAGES: &str = r#"lesson_name: "Add Two Numbers"
language: Python
packages:
  - pandas
exercise:
  prompt: "Write add(a, b)."
  code_template: "def add(a, b):\n    ..."
  solution: "def add(a, b):\n    return a + b"
  llm_evaluation_prompt: "Grade this: {student_code}"
"#;

/// A minimal Python lesson with NO `packages` key — must emit `"packages": []`.
const LESSON_PY_WITHOUT_PACKAGES: &str = r#"lesson_name: "Add Two Numbers"
language: Python
exercise:
  prompt: "Write add(a, b)."
  code_template: "def add(a, b):\n    ..."
  solution: "def add(a, b):\n    return a + b"
  llm_evaluation_prompt: "Grade this: {student_code}"
"#;

#[test]
fn build_emits_lesson_json_with_packages_array() {
    // Issue #72: a lesson declaring `packages: [pandas]` must serialize the
    // packages into `lessons/0.json` as a JSON array — the browser runner reads
    // this to call installPackages/loadPackage, and the local Python runner
    // reads it to spawn `uv run --with pandas`.
    let tmp = tempfile::tempdir().unwrap();
    let course = write_packages_course(&tmp, LESSON_PY_WITH_PACKAGES);
    let out = tmp.path().join("site");

    let output = build("pyodide", course.to_str().unwrap(), &out);
    assert!(
        output.status.success(),
        "build should exit 0, got {:?}; stderr={:?}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    let lesson: Value =
        serde_json::from_str(&std::fs::read_to_string(out.join("lessons/0.json")).unwrap())
            .unwrap();
    assert_eq!(
        lesson["packages"],
        serde_json::json!(["pandas"]),
        "lesson JSON must carry the packages array: {lesson}"
    );
}

#[test]
fn build_emits_empty_packages_array_when_lesson_has_no_packages() {
    // Issue #72 negative: a lesson WITHOUT a `packages` key must emit
    // `"packages": []` (empty array, NOT null and NOT absent) — stable contract
    // shape so the JS runtime never has to handle absence.
    let tmp = tempfile::tempdir().unwrap();
    let course = write_packages_course(&tmp, LESSON_PY_WITHOUT_PACKAGES);
    let out = tmp.path().join("site");

    let output = build("pyodide", course.to_str().unwrap(), &out);
    assert!(
        output.status.success(),
        "build should exit 0, got {:?}; stderr={:?}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    let lesson: Value =
        serde_json::from_str(&std::fs::read_to_string(out.join("lessons/0.json")).unwrap())
            .unwrap();
    assert!(
        lesson["packages"].is_array(),
        "packages must be an array (not null/absent): {lesson}"
    );
    assert!(
        lesson["packages"].as_array().unwrap().is_empty(),
        "packages must be an empty array when lesson has no packages key: {lesson}"
    );
}

// ── Issue #99: BYOK-builder embedded API key (--embed-key) ──────────

/// Run `build` with optional `--password` and `--embed-key` flags.
fn build_with_flags(
    target: &str,
    course: &str,
    out: &Path,
    password: Option<&str>,
    embed_key: Option<&str>,
) -> std::process::Output {
    let mut cmd = Command::cargo_bin("blendtutor").unwrap();
    cmd.arg("build")
        .arg("--target")
        .arg(target)
        .arg(course)
        .arg("-o")
        .arg(out);
    if let Some(pw) = password {
        cmd.arg("--password").arg(pw);
    }
    if let Some(ek) = embed_key {
        cmd.arg("--embed-key").arg(ek);
    }
    cmd.output().unwrap()
}

/// A distinctive test Fireworks key — long enough to be a realistic key, short
/// enough to be readable. The `_` in `fw_` is NOT in the base64 alphabet, so
/// it can never appear in the encrypted ciphertext.
const TEST_FW_KEY: &str = "fw_testkey123456789abcdef";

/// A distinctive test Anthropic key. The `-` in `sk-ant-` is NOT in the base64
/// alphabet, so it can never appear in the encrypted ciphertext.
const TEST_ANT_KEY: &str = "sk-ant-testkey123456789abcdef";

#[test]
fn embed_key_with_password_succeeds_and_no_plaintext_key_ships() {
    // AC-3 (issue #99): building with --embed-key + --password embeds the API
    // key into the encrypted payload. The key is never visible as plaintext in
    // any emitted file — it rides inside the AES-256-GCM ciphertext, and the
    // base64 alphabet excludes `_` and `-`, so `fw_` and `sk-ant` can never
    // appear in the base64-encoded ciphertext.
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build_with_flags(
        "webr",
        R_COURSE,
        &out,
        Some("secret-pw"),
        Some(&format!("fireworks:{TEST_FW_KEY}")),
    );
    assert!(
        output.status.success(),
        "`build --embed-key fireworks:... --password ...` should exit 0, got {:?}; stderr={:?}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    // Clause 2: no plaintext API key in ANY emitted file. The key is inside the
    // encrypted base64 payload — base64 alphabet (A-Za-z0-9+/=) excludes `_`
    // and `-`, so `fw_testkey...` can never appear in the ciphertext.
    for (path, contents) in emitted_files(&out) {
        assert!(
            !contents.contains(TEST_FW_KEY),
            "the embedded API key must never ship as plaintext; found in {path:?}"
        );
        assert!(
            !contents.contains("fw_testkey"),
            "the fw_ key prefix+body must never ship as plaintext; found in {path:?}"
        );
    }

    // The decrypt shell is present (index.html is a decrypt shell, not plaintext).
    let index = std::fs::read_to_string(out.join("index.html")).unwrap();
    assert!(
        index.contains("data-encrypted-payload="),
        "index.html must be a decrypt shell (encrypted payload)"
    );
    assert!(
        !index.contains("<header class=\"site-header\">"),
        "index.html must not contain plaintext page content"
    );

    // The shared assets (feedback.js, etc.) are still emitted unchanged.
    for infra in [
        "lesson-runner.js",
        "lesson-runner-core.js",
        "coi-serviceworker.js",
        "config.js",
        "feedback.js",
        "styles.css",
        "codemirror.js",
    ] {
        assert!(
            out.join(infra).is_file(),
            "the built site is missing {infra}"
        );
    }
}

#[test]
fn embed_key_without_password_is_a_hard_error() {
    // AC-3 negative: --embed-key without --password must fail hard (exit != 0)
    // and create NO output directory — the key is encrypted into the payload,
    // so without a password there is nothing to encrypt with.
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build_with_flags(
        "webr",
        R_COURSE,
        &out,
        None,
        Some(&format!("fireworks:{TEST_FW_KEY}")),
    );
    assert!(
        !output.status.success(),
        "--embed-key without --password must fail; stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stderr = String::from_utf8_lossy(&output.stderr).to_lowercase();
    assert!(
        stderr.contains("password"),
        "the failure should name the missing --password; stderr={stderr}"
    );
    // No output dir created — fail-fast before write_site.
    assert!(
        !out.exists(),
        "a build refused for --embed-key without --password must not create the output directory"
    );
}

#[test]
fn embed_key_provider_prefix_mismatch_is_a_hard_error() {
    // AC-3 negative: a provider/key prefix mismatch (e.g. fireworks:sk-ant-xxx)
    // must fail hard — the provider and key must be consistent.
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build_with_flags(
        "webr",
        R_COURSE,
        &out,
        Some("secret-pw"),
        Some(&format!("fireworks:{TEST_ANT_KEY}")),
    );
    assert!(
        !output.status.success(),
        "fireworks:sk-ant-... must fail (provider/key mismatch); stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        !out.exists(),
        "a refused build must not create the output directory"
    );

    // Symmetric: anthropic with a fw_ key also fails.
    let out2 = tmp.path().join("site2");
    let output2 = build_with_flags(
        "webr",
        R_COURSE,
        &out2,
        Some("secret-pw"),
        Some(&format!("anthropic:{TEST_FW_KEY}")),
    );
    assert!(
        !output2.status.success(),
        "anthropic:fw_... must fail (provider/key mismatch); stderr={:?}",
        String::from_utf8_lossy(&output2.stderr)
    );
    assert!(
        !out2.exists(),
        "a refused build must not create the output directory"
    );
}

#[test]
fn embed_key_emits_cli_warnings_to_stderr() {
    // AC-3: the build emits CLI warnings to stderr about the embedded key being
    // unique/single-use and the need to set a spend limit/budget/cost.
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build_with_flags(
        "webr",
        R_COURSE,
        &out,
        Some("secret-pw"),
        Some(&format!("fireworks:{TEST_FW_KEY}")),
    );
    assert!(output.status.success(), "build should succeed");
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Warning keyword: "WARNING" or "⚠" or "CAUTION"
    let lower = stderr.to_lowercase();
    assert!(
        lower.contains("warning") || stderr.contains("⚠") || lower.contains("caution"),
        "stderr must contain a warning keyword (WARNING/⚠/CAUTION); stderr={stderr}"
    );
    // Unique/single-use keyword
    assert!(
        lower.contains("unique") || lower.contains("single-use"),
        "stderr must mention the key is unique/single-use; stderr={stderr}"
    );
    // Spend limit/budget/cost keyword
    assert!(
        lower.contains("spend limit") || lower.contains("budget") || lower.contains("cost"),
        "stderr must mention spend limit/budget/cost; stderr={stderr}"
    );
}

#[test]
fn embed_key_feedback_js_has_apply_embedded_key() {
    // AC-3: feedback.js contains an applyEmbeddedKey function that reads
    // window.__btEmbeddedKey, calls storeKey + storeProvider, and clears the
    // global. It is called at init before submitButton.addEventListener.
    // handleSubmit is UNCHANGED.
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build_with_flags(
        "webr",
        R_COURSE,
        &out,
        Some("secret-pw"),
        Some(&format!("fireworks:{TEST_FW_KEY}")),
    );
    assert!(output.status.success(), "build should succeed");

    let feedback = std::fs::read_to_string(out.join("feedback.js"))
        .expect("the built site must ship feedback.js");

    // applyEmbeddedKey function exists.
    assert!(
        feedback.contains("function applyEmbeddedKey"),
        "feedback.js must define applyEmbeddedKey; feedback={feedback}"
    );
    // Reads window.__btEmbeddedKey.
    assert!(
        feedback.contains("window.__btEmbeddedKey"),
        "feedback.js must read window.__btEmbeddedKey; feedback={feedback}"
    );
    // Calls storeKey + storeProvider.
    assert!(
        feedback.contains("storeKey(") && feedback.contains("storeProvider("),
        "feedback.js applyEmbeddedKey must call storeKey + storeProvider; feedback={feedback}"
    );
    // Clears the global after reading.
    assert!(
        feedback.contains("window.__btEmbeddedKey = undefined")
            || feedback.contains("window.__btEmbeddedKey = null"),
        "feedback.js must clear window.__btEmbeddedKey after reading; feedback={feedback}"
    );
    // applyEmbeddedKey is called at init (before submitButton.addEventListener).
    let apply_pos = feedback
        .find("applyEmbeddedKey()")
        .expect("feedback.js must call applyEmbeddedKey() at init");
    let submit_pos = feedback
        .find("submitButton.addEventListener")
        .expect("feedback.js must wire submitButton.addEventListener");
    assert!(
        apply_pos < submit_pos,
        "applyEmbeddedKey() must be called before submitButton.addEventListener"
    );

    // handleSubmit is unchanged — it still reads the key from sessionStorage
    // (readKey) and routes through PROVIDERS[providerId].factory. The embedded
    // key is pre-loaded into sessionStorage by applyEmbeddedKey, so phase 1
    // (key-entry prompt) is skipped naturally.
    assert!(
        feedback.contains("async function handleSubmit"),
        "feedback.js must still define handleSubmit; feedback={feedback}"
    );
    assert!(
        feedback.contains("readKey(providerId)"),
        "handleSubmit must still read the key from sessionStorage; feedback={feedback}"
    );
    assert!(
        feedback.contains("PROVIDERS[providerId].factory"),
        "handleSubmit must still route through PROVIDERS[providerId].factory; feedback={feedback}"
    );
}

#[test]
fn embed_key_decrypt_shell_handles_embedded_key_payload() {
    // AC-3: the decrypt shell (decrypt-shell.html) handles the JSON payload
    // {"html":"...","embeddedKey":{...}} — after decrypting, it extracts
    // embeddedKey and sets window.__btEmbeddedKey before rendering the page.
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build_with_flags(
        "webr",
        R_COURSE,
        &out,
        Some("secret-pw"),
        Some(&format!("fireworks:{TEST_FW_KEY}")),
    );
    assert!(output.status.success(), "build should succeed");

    let index = std::fs::read_to_string(out.join("index.html")).unwrap();
    // The decrypt shell must try JSON.parse on the decrypted content and check
    // for embeddedKey.
    assert!(
        index.contains("JSON.parse"),
        "decrypt shell must try JSON.parse on decrypted content; index={index}"
    );
    assert!(
        index.contains("embeddedKey"),
        "decrypt shell must check for embeddedKey in parsed JSON; index={index}"
    );
    assert!(
        index.contains("window.__btEmbeddedKey"),
        "decrypt shell must set window.__btEmbeddedKey; index={index}"
    );
}

#[test]
fn embed_key_cross_target_byte_identity() {
    // AC-3: feedback.js is byte-identical across targets (shared asset). The
    // applyEmbeddedKey function is in the shared feedback.js, not forked per
    // target.
    let tmp = tempfile::tempdir().unwrap();
    let webr_out = tmp.path().join("webr");
    let pyo_out = tmp.path().join("pyodide");

    assert!(
        build_with_flags(
            "webr",
            R_COURSE,
            &webr_out,
            Some("secret-pw"),
            Some(&format!("fireworks:{TEST_FW_KEY}"))
        )
        .status
        .success()
    );
    assert!(
        build_with_flags(
            "pyodide",
            PYTHON_COURSE,
            &pyo_out,
            Some("secret-pw"),
            Some(&format!("fireworks:{TEST_FW_KEY}"))
        )
        .status
        .success()
    );

    let webr_feedback = std::fs::read_to_string(webr_out.join("feedback.js")).unwrap();
    let pyo_feedback = std::fs::read_to_string(pyo_out.join("feedback.js")).unwrap();
    assert_eq!(
        webr_feedback, pyo_feedback,
        "feedback.js must be byte-identical across targets (shared asset, applyEmbeddedKey not forked)"
    );

    // The decrypt shell is also shared (same template, different encrypted
    // payloads). The JS logic (JSON.parse, embeddedKey check) must be identical.
    let webr_index = std::fs::read_to_string(webr_out.join("index.html")).unwrap();
    let pyo_index = std::fs::read_to_string(pyo_out.join("index.html")).unwrap();
    assert!(
        webr_index.contains("JSON.parse") && webr_index.contains("embeddedKey"),
        "webr decrypt shell must handle embeddedKey"
    );
    assert!(
        pyo_index.contains("JSON.parse") && pyo_index.contains("embeddedKey"),
        "pyodide decrypt shell must handle embeddedKey"
    );
}

#[test]
fn embed_key_existing_negative_scans_still_pass() {
    // AC-3: the existing negative scan tests (sk-ant, fw_) still pass when
    // --embed-key is used — the key is inside the encrypted base64 payload, and
    // the base64 alphabet excludes `_` and `-`, so `fw_` and `sk-ant` never
    // appear in any emitted file.
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("site");

    let output = build_with_flags(
        "webr",
        R_COURSE,
        &out,
        Some("secret-pw"),
        Some(&format!("fireworks:{TEST_FW_KEY}")),
    );
    assert!(output.status.success(), "build should succeed");

    // Scan ALL emitted files for fw_ and sk-ant — neither should appear.
    for (path, contents) in emitted_files(&out) {
        assert!(
            !contents.contains("fw_"),
            "fw_ must never ship as plaintext (it's inside encrypted base64); found in {path:?}"
        );
        assert!(
            !contents.contains("sk-ant"),
            "sk-ant must never ship as plaintext; found in {path:?}"
        );
    }

    // Also test with an Anthropic key.
    let out2 = tmp.path().join("site2");
    let output2 = build_with_flags(
        "webr",
        R_COURSE,
        &out2,
        Some("secret-pw"),
        Some(&format!("anthropic:{TEST_ANT_KEY}")),
    );
    assert!(
        output2.status.success(),
        "build with anthropic key should succeed"
    );
    for (path, contents) in emitted_files(&out2) {
        assert!(
            !contents.contains("sk-ant"),
            "sk-ant must never ship as plaintext (it's inside encrypted base64); found in {path:?}"
        );
        assert!(
            !contents.contains("fw_"),
            "fw_ must never ship as plaintext; found in {path:?}"
        );
    }
}
