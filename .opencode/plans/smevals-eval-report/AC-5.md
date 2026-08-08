---
ac: 5
depends_on: AC-2 (smevals gen dir), AC-3 (runner), AC-4 (judge)
risk: medium (external `uvx smevals` round-trip unverifiable in-repo)
status: complete
---

## AC spec: `blendtutor eval-report <lesson>` subcommand

Turn-key eval-report generation: generate the smevals eval dir (AC-2 pure fn)
at the course root → `uvx smevals==0.2.0 run <gen_dir> -g` → `uvx
smevals==0.2.0 build <gen_dir> -o <docs/evals/.<lesson>.tmp>` → atomic rename
into `docs/evals/<lesson>/`. Pin smevals==0.2.0. Subcommand, NOT a shell
script. Grade-fail is evidence, not a gate.

### Executable Spec
- **predicate:** Fake-`uvx` shim (PATH-injected, logs argv per invocation, exits
  `$FAKE_UVX_EXIT`) + success path → log == exactly 2 calls,
  `["uvx", "smevals==0.2.0", "run", <abs_gen_dir>, "-g"]` then
  `["uvx", "smevals==0.2.0", "build", <abs_gen_dir>, "-o", <abs_docs_evals_lesson>]`
  AND every logged path `is_absolute()` AND no `--runs-dir` token anywhere AND
  exit code == 0 AND `docs/evals/<lesson>/index.html` exists AND a pre-seeded
  stale file in `.smevals/` is gone (stale-clean) AND a pre-seeded marker file
  in pre-existing `docs/evals/<lesson>/` survives when build fails. Failure
  paths: generator `Err` → exit 1, stderr names `generate`, shim log empty;
  run-exit-1-with-artifacts → build still invoked, exit 0; run-exit-1-with-
  empty-runs → exit 1 names `run`; build-exit-1 → exit 1 names `build`;
  uvx absent → clean stage-named error, no panic (stderr contains `uvx`, no
  `panicked at`).
- **probe:** `cargo test -p blendtutor-cli --test eval_report_cli` (integration
  test builds `Command::cargo_bin("blendtutor")`, PATH-injects tempdir shim
  `uvx`; constructs its own env — `blendtutor_output_env` can't hold a dynamic
  PATH). Secondary: `cargo test -p blendtutor-cli` full suite (cli.rs
  PLANNED_SUBCOMMANDS 8→9 regression). Manual smoke (USER DECISION): real-key
  run of `blendtutor eval-report examples/write-less-code-r/lessons/01_seed_data.yaml`,
  committing `docs/evals/<lesson>/` as the smoke evidence.
- **negative:** (a) generator `Err` (missing sibling suite / bad slug) → uvx
  never invoked, exit 1 names `generate`; (b) run exits 1 but produced runs →
  still exit 0; (c) build fails → exit 1 names `build` AND prior committed
  report untouched; (d) relative gen_dir would pass naive string equality —
  `is_absolute()` catches CWD-relative regression; (e) `--runs-dir` sneaky-pass
  strands runs outside the eval dir — token-scan catches it; (f) panics on
  missing uvx — assert no `panicked at`.
- **verification:** code · cargo test integration against fake-uvx shim +
  manual real-key smoke (committed docs/evals/<lesson>/ per user decision)
- **fixture status:** NEW — crates/cli/tests/eval_report_cli.rs, shim generated
  per-test in tempdir. Reuses eval_command pair (demo_lesson.yaml +
  eval_demo_lesson.yaml) copied into a throwaway course. "No sibling suite"
  case deletes the suite in-harness.
- **rubric anchor:** §1.2 (Commands::EvalReport variant), §2.3 (pure generator
  in core, thin effectful shell in cli), §5 (single orchestration fn,
  stage-named anyhow::Context)

### Progress
- [x] 2026-08-08 — RED: eval_report_cli.rs (8 arms) + cli.rs 8→9; all fail on
  missing subcommand (f55e81e)
- [x] 2026-08-08 — impl: main.rs variant + dispatch; commands/eval_report.rs
  orchestration; sibling_suite_path → commands/mod.rs shared (be8ac6d)
- [x] 2026-08-08 — GREEN: 10/10 integration+regression; negative control
  (pin-break → test fails); full suite 123 pass; clippy+fmt clean
- [x] 2026-08-08 — evidence docs/evidence/198/ (test-suite.log, manual shim
  run: stdout + uvx log + report tree)
- [x] 2026-08-08 — manual real-key smoke RAN (exit 0, external contract
  verified) — but committed-report deliverable BLOCKED by AC-2/AC-3 lesson
  id-vs-path gap (all 4 cases fail read_lesson_file; docs/evals/ NOT
  committed). See Surprises & Discoveries + report to Director.
- [ ] push + PR
- [ ] committed docs/evals/01_seed_data/ report (after AC-2/AC-3 gap fix)

### Decision Log
- **build-into-temp + atomic rename** (`-o docs/evals/.<lesson>.tmp` → on
  success rm old + rename; on failure remove temp, prior committed report
  survives). docs.yml rm -rf precedent REJECTED — local re-run with committed
  prior report loses data on build failure.
- **Grade-fail is evidence:** run non-zero + runs/ non-empty → proceed, exit 0
  on build success; run non-zero + runs/ empty → exit 1 names `run`; build
  failure → exit 1 names `build`; both fail → build error carries run exit.
- **docs/evals at repo root** (nearest `.git` ancestor walk-up), not
  lesson/course-relative — matches AC-6 Pages publish of repo `docs/evals/`.
- **`uvx smevals==0.2.0`** pin in package-spec position (verified real argv
  shape against `uvx smevals==0.2.0 --help`; `-g` last per AC-3 note).
- **Shared helpers:** `lesson_id_from_path` + `course_root_for` from core
  (single source); `sibling_suite_path` moved to commands/mod.rs so eval and
  eval-report derive it identically.

### Surprises & Discoveries
- macOS `/var` vs `/private/var`: the shim-logged absolute paths differ from
  `tempfile::tempdir()`'s raw path. Test harness canonicalizes its root before
  asserting paths — otherwise `-o` equality fails only on macOS.
- `gen` is a reserved keyword in edition-2024 Rust (generators) — the unit-test
  variable `let gen = …` fails to compile. Renamed to `eval_dir`.
- Real smevals 0.2.0 confirmed: `run <path> -g` records Runs as plain files in
  `<path>/runs/` (default — `--runs-dir` is the escape hatch to avoid);
  `build <path> -o <dir>` "adds to an existing site" — exactly why the temp
  dir must be fresh (a stale temp would merge, not replace).
- anyhow's `Error: <context>` for a bare `.context("generate")` renders
  WITHOUT a trailing colon ("Error: generate\n\nCaused by: …"), so the
  stage-name stderr asserts need `generate:` embedded in the context message.
- macOS has no `timeout` (GNU coreutils) — AC-3's run.sh depends on it, so the
  real-key smoke needs a PATH-injected `exec "$@"` timeout shim locally; smoke
  still exercises the full uvx → smevals → run.sh → blendtutor eval round-trip.
- **AC-2/AC-3 lesson id-vs-path contract gap (BLOCKER for the committed
  report):** AC-2's `emit_task_yaml` emits `lesson: <lesson_id>` (slug, e.g.
  `01_seed_data`), AC-3's run.sh passes `$SMEVALS_TASK_LESSON` verbatim to
  `blendtutor eval <lesson>`, which requires a FILE PATH — `read_lesson_file`
  fails NotFound on every case (3 retries × 2s each = the ~4s/case observed).
  The real-key smoke is the FIRST end-to-end wiring of the real blendtutor
  binary through smevals — AC-3/AC-4 CI used stub blendtutor (argv shape
  asserted, never resolved), so no suite caught it. All generated task.yaml
  files carry the slug; run.sh cannot map slug → path (it lacks the course
  root). Fix belongs to AC-2 (emit a resolvable lesson reference) or AC-3
  (runner resolves the id against a course env) — NOT AC-5 logic, which
  correctly generates + drives + publishes. Docs/evals/01_seed_data left
  UNCOMMITTED (an all-fail report would embed misleading 0.0 evidence into
  the Pages artifact).
- Spec's smoke command path `examples/write-less-code-r/lessons/01_seed_data.yaml`
  is wrong — the repo layout has lessons at the course root
  (`examples/write-less-code-r/01_seed_data.yaml`). The `lessons/` subdir does
  not exist; the first smoke attempt failed with a clean stage-named
  "generate: reading lesson" error (which itself demonstrated the error path).
