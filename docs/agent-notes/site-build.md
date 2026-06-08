---
topic: site-build
created: 2026-06-08
slices: [16, 17]
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
- 2026-06-08 (#17): The Pyodide target proved the `BuildTarget` seam (§3.4): a
  second target added `site::pyodide` + `assets/pyodide/` reusing `plan_site`/
  `write_site` and the `SiteLesson` JSON unchanged. Factoring the cross-target
  scaffolding **out of `webr`** was the bulk of the work (§4.2): `assemble(
  TargetAssets, lessons)` now owns the per-lesson JSON layout, and the runner core
  (`lesson-runner-core.js`) + the COOP/COEP shim moved to `assets/shared/`. Each
  target now ships only its own `index.html` + a thin `lesson-runner.js` adapter
  `{ name, boot(), run(code, checks) -> {output, ok} }` atop the shared core.
  `assemble` takes a named-field `TargetAssets` (not two positional `&str`) so the
  shell/runner can't transpose (§1.4). `PlanError::TargetUnsupported` is **gone** —
  both targets are built now, so the placeholder variant is unrepresentable (§1.1).
- 2026-06-08 (#17): The status DOM contract is **`data-status`** (idle / running /
  pass / fail) on `#lesson-status`, set by the shared core and polled by the rodney
  probe (`[data-status="pass"]`). Slice 16's webR shell used `data-state`; unified
  to `data-status` here when the shell/runner split landed.
- 2026-06-08 (#17): **Pyodide boots main-thread and needs no SharedArrayBuffer /
  cross-origin isolation** for the basic runner — so the slice-16 headless-Chrome
  SAB gotcha does *not* block Pyodide boot. The only headless issue is the
  `coi-serviceworker` reload-to-isolate loop, so for rodney **neuter the served
  shim copy** (overwrite `out/coi-serviceworker.js` with a no-op); the shipped
  artifact keeps the real shim (AC1 asserts it present + referenced). Pyodide loads
  from `cdn.jsdelivr.net/pyodide/v0.27.2`; the runner grades via `runPythonAsync`
  in a fresh `toPy({})` namespace and **destroys the result PyProxy** (a
  non-primitive trailing value leaks otherwise). Verified correct→`pass`,
  wrong→`fail` (AssertionError) live; evidence in `docs/evidence/17/`.
- 2026-06-08 (#17): The AC1 spec probe wrote `rg -Eq '...'`, a **ripgrep flag
  typo** — ripgrep's `-E` takes an encoding argument (that is grep syntax), so
  `-Eq` parses `q` as the encoding and errors. The behaviour is correct under the
  intended `rg -q '...'`; only the probe's flag was wrong.
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
- 2026-06-08 (#19): The **eval-results page is folded into `plan_site` itself**,
  after the per-target `webr::plan`/`pyodide::plan` dispatch — not into `assemble`.
  It is target-independent: the same `EvalSummary` renders the same
  `eval-results.html` for both runtimes (§4.1), so `webr`/`pyodide`/`assemble`
  signatures stayed untouched. Validation is a represented
  `EvalSummary { Validated { accuracy }, NotValidated }` (§1.2), never a
  missing-file inference: the CLI shell (`build::load_eval_summary`) does the
  effectful check of `<course>/eval-report.json` and hands core an *explicit*
  state — absent → `NotValidated` (build still exits 0), present → parsed,
  present-but-unreadable → build fails (so a corrupt report never silently
  unvalidates a course).
- 2026-06-08 (#19): The build **consumes the Slice-13 eval artifact as-is** (§3.2):
  `site::eval_summary_from_report_json` reads only `accuracy` via a private read
  DTO — deliberately **not** a round-trip through `EvalReport`. `EvalReport` is
  `Serialize`-only, and its `EvalReport::new` *recomputes* accuracy from the cases
  (the §1.1 derived-invariant from [[eval]]); deserializing through it would
  re-score at build time, violating "as-is". The figure is range-guarded to
  `[0.0, 1.0]` at the read boundary (§1.3.1) — a hand-edited `accuracy: 2.0` is an
  `EvalReportError::AccuracyOutOfRange`, so the page can never render `200%`.
  Convention: the report rides next to the course manifest as `eval-report.json`
  (the `blendtutor eval --format json` output). evidence in `docs/evidence/19/`.
