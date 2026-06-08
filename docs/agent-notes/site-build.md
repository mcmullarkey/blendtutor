---
topic: site-build
created: 2026-06-08
slices: [16]
---

How `blendtutor build --target <runtime>` assembles a static, browser-deployable
lesson site (`core::site`). See ADR-0008 for the decision record.

- 2026-06-08 (#16): The assembly is a **pure/effectful split** (§2.1, §2.3).
  `plan_site(&[(LessonSlug, Lesson)], BuildTarget) -> Result<SiteFiles, PlanError>`
  decides which files exist and their contents with no filesystem touch (so a
  whole site is snapshot-testable); `write_site(&Path, &SiteFiles)` is the only
  effectful step. Loading the course's full lessons is a separate effectful
  `Course::load_lessons` — **fail-fast** (one bad lesson fails the build), unlike
  `discover`'s per-row partial tolerance: a site must never ship missing a page.
- 2026-06-08 (#16): The browser contract is a dedicated **`SiteLesson` DTO**, not
  the internal `Lesson` (§3.2). It drops `llm_evaluation_prompt` (a CLI/LLM
  concern never shipped to the client) and carries `id/title/prompt/code_template/
  checks/solution`. A Rust-side change that keeps this JSON shape leaves the JS
  runtime untouched. The reference `solution` is a new optional field on
  `Exercise` (lesson schema); it rides the contract because AC2's browser probe
  needs `window.__bt.lessons[i].solution`. Note: shipping `solution` exposes the
  answer to a learner who reads the JSON — accepted for v0 (the AC mandates it);
  a future slice may gate/omit it per target.
- 2026-06-08 (#16): Per-lesson JSON is **keyed by index** (`lessons/0.json`), not
  by slug. A course may be shared (untrusted), and a slug with `..`/`/` used as a
  filename would escape the output dir — so the slug only ever rides *inside* the
  JSON as `id`. `lessons.json` is the ordered slug index the runner enumerates.
  Same threat model: the runner builds the lesson `<option>`s with the DOM `Option`
  API, never `innerHTML`, so a malicious title can't inject HTML.
- 2026-06-08 (#16): Assets (`index.html`, `lesson-runner.js`,
  `coi-serviceworker.js`) are **static client files** under
  `crates/core/assets/<target>/`, embedded with `include_str!` and copied verbatim
  (§4.1 — assembly, not codegen). `core::site` never runs code or calls an LLM at
  build time. `serde_json` moved from a dev-dependency to a runtime dependency of
  `core` because the build serializes lessons at run time.
- 2026-06-08 (#16): The `BuildTarget { Webr, Pyodide }` seam carries the language
  each runtime serves (Webr→R, Pyodide→Python). `plan_site` **refuses a
  language/target mismatch before emitting anything** (§1.3.1) → no output dir is
  created on a refused build. Slice 16 only implements Webr; `Pyodide` returns
  `PlanError::TargetUnsupported` until Slice 17. The mutation gate (`cargo mutants
  --in-diff`, core only) needs the Pyodide arm covered — reach it in a unit test
  with `course_basic`'s Python lesson filtered out, no dedicated Python fixture
  required.
- 2026-06-08 (#16): COOP/COEP — GitHub Pages can't set headers, so the vendored
  upstream **`coi-serviceworker`** shim (MIT) re-serves responses cross-origin
  isolated, enabling `SharedArrayBuffer` for webR. The runner uses webR's **default
  (Automatic) channel**: SharedArrayBuffer when isolated (the shim's job), and a
  graceful fallback (PostMessage) otherwise — so a host where isolation fails still
  runs. Don't pin a channel; it breaks the fallback.
- 2026-06-08 (#16): **rodney/headless-Chrome gotcha for SAB-dependent slices.**
  This rodney's headless Chrome does NOT enable `SharedArrayBuffer` /
  `crossOriginIsolated` even with correct COOP/COEP headers, and rodney can't pass
  Chrome flags (only `--insecure`; `--show` from the help text is not in the
  binary). The SW serves correct headers to *subresources* but the top-level
  document never isolates. To verify webR in-browser anyway: serve the built site
  with COOP/COEP set **directly** on a fresh port (avoid a previously-registered
  SW) and no-op the shim locally; webR's Automatic channel then boots via
  PostMessage and runs the exact same R checks. Verified correct→`pass`,
  wrong→`fail` live; evidence in `docs/evidence/16/`.
