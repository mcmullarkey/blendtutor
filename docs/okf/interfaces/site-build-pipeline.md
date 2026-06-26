---
type: Interface
title: Site build pipeline
description: plan_site → write_site public API + include_str! asset embedding.
resource: crates/core/src/site/mod.rs:436
tags: [rust, static-site, build, assets, include-str]
timestamp: 2026-06-25
pure: true
---

# Responsibility

The public API surface for building a static site: plan files (pure) then write them (effectful). All assets embedded at compile time via `include_str!` — no runtime asset loading, no codegen, no templating.

# Interface

- `pub fn plan_site(lessons: &[(LessonSlug, Lesson)], target: BuildTarget, eval: &EvalSummary) -> Result<SiteFiles, PlanError>` — pure
- `pub fn write_site(out_dir: &Path, site: &SiteFiles) -> std::io::Result<()>` — effectful
- `pub fn eval_summary_from_report_json(json: &str) -> Result<EvalSummary, EvalReportError>` — pure

**Asset embedding** (`include_str!`, 9 assets):
- `assets/shared/lesson-runner-core.js`, `assets/shared/coi-serviceworker.js`, `assets/shared/feedback.js`, `assets/shared/styles.css`, `assets/shared/codemirror.js`
- `assets/webr/{index.html,lesson-runner.js}`, `assets/pyodide/{index.html,lesson-runner.js}`

**`assemble` output order** (deterministic):
1. `index.html` (target shell)
2. `lesson-runner.js` (target adapter)
3. `lesson-runner-core.js` (shared)
4. `coi-serviceworker.js` (shared)
5. `feedback.js` (shared)
6. `styles.css` (shared)
7. `codemirror.js` (shared — vendored CM6 ESM bundle)
8. `lessons/{i}.json` per lesson (keyed by index, not slug)
9. `lessons.json` (ordered slug index)
10. `eval-results.html` (target-independent, folded after dispatch)

# Dependencies

- Implemented by [site](/modules/site.md).
- Assets define [js-runtime-seam](/interfaces/js-runtime-seam.md).
- Types in [site-types](/data-models/site-types.md).
- Called by [cli](/modules/cli.md) build command.

# Invariants

- No LLM calls, no code execution at build time (ADR-0008).
- Per-lesson JSON keyed by index — slug is never a filesystem path component.
- `SiteLesson::from_lesson` drops `llm_evaluation_prompt` (the prompt template never ships to the browser).
- `write_site` writes files verbatim — the only effectful step.

# Pure/Effectful

`plan_site` + `eval_summary_from_report_json` pure. `write_site` effectful (FS).

# Citations

- `crates/core/src/site/mod.rs:301` (`assemble`), `:436` (`plan_site`), `:467` (`write_site`)
- `crates/core/src/site/{webr,pyodide}.rs` (`include_str!`)
- ADR-0008 (static site build)
