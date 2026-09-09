---
ac: 8
depends_on: AC-6, AC-7
risk: medium
status: complete
---

**Verdicts (in-spec resolutions):**
1. Overwrite → WARN-and-proceed (B wins). Re-running eval on own course is the common case; refuse forces manual rm every iteration = friction; --force = bloat. Committed-artifact protection comes from git, not exit 1. Warning MUST name path + word "overwrit" (stderr).
2. Partial report → REFUSE (A wins). --case N --write-report exits 1 before any write: durable artifact flows to build → site renders as course-level accuracy; 1-case report is a misleading durable artifact. Error names --case + --write-report.
3. Shared constant → eval.rs-local const, doc-comment pointing to build.rs:14 as contract source. Extraction touches frozen build.rs/core boundary; mandatory build round-trip test (P8) makes duplication safe — drift fails the round-trip.
4. Shape → FULL {cases, accuracy} via serde_json::to_string(&report) — byte-identical to --format json stdout path (output.rs:415). Single serialization path, zero drift. Committed examples' minimal shape = hand-written shortcut, not canonical; consumer reads accuracy only, both parse.

## AC spec: `eval --write-report` persists build-consumable full-shape eval-report.json at course root, smevals-free

### Executable Spec
- predicate: Given a tempdir course (3-line blendtutor.toml marker + demo_lesson.yaml + eval_demo_lesson.yaml copied from crates/core/tests/fixtures/eval_command/) and a wiremock stub mounting three scripted verdicts (alpha→correct, beta→incorrect, gamma→incorrect — crates/cli/tests/eval.rs:29-33 harness), when `blendtutor eval <lesson> --write-report` runs from a CWD that is NOT the course root, with default human format, then ALL of:
  1. exit code == 0
  2. stdout still contains the human accuracy line (2/3) — --write-report is format-independent, terminal render unchanged (no silent no-op gated on --format json)
  3. <course_root>/eval-report.json exists AND does NOT exist at CWD (course root = nearest blendtutor.toml ancestor via course_root_for, smevals_gen.rs:361 — catches write-to-CWD sneaky-pass)
  4. file parses as one JSON doc; doc["cases"].as_array().len() == 3 (non-empty, full-case count — catches partial/empty-cases write); doc["accuracy"].as_f64() == Some(2.0/3.0) (number in [0,1], not string; catches placeholder/stale content)
  5. file bytes byte-identical to serde_json::to_string(&report) — same serialization path as --format json stdout (output.rs:414-415); running with --format json --write-report yields file content == stdout JSON (no second serialization path to drift)
  6. confirmation emitted: stdout or stderr contains the string eval-report.json on success (catches silent-write sneaky-pass)
  7. blendtutor eval --help contains --write-report; subcommand surface stays 9 (AC-6 P5 — flag on existing Eval variant, not a new subcommand)
  8. bidirectional contract (mandatory): blendtutor build --target webr <course_root> -o <out> succeeds on the same tempdir course AND <out>/eval-results.html contains the accuracy percentage derived from the written file (write side + read side agree; catches wrong-schema write that eval exits 0 on but build rejects — schema pinned by eval_summary_from_report_json, site/mod.rs:392)
- probe:
  cargo test -p blendtutor-cli --test eval eval_write_report
  cargo test -p blendtutor-cli --test eval eval_write_report_no_course_root
  cargo test -p blendtutor-cli --test eval eval_write_report_overwrite_warns
  cargo test -p blendtutor-cli --test eval eval_write_report_partial_refused
  cargo test -p blendtutor-cli --test eval eval_write_report_failure_propagates
  cargo test -p blendtutor-cli --test eval eval_write_report_build_roundtrip
  (Rscript-dependent, same skip-with-notice harness as existing eval tests, eval.rs:58-60; round-trip test pattern mirrors crates/cli/tests/build.rs:1292-1303 tempdir course)
- negative:
  - N1 no course root: lesson in dir with no blendtutor.toml ancestor (course_root_for → None) → exit 1, stderr names missing course root, NO file written anywhere (catches silent-no-op; check runs BEFORE any scoring side effect)
  - N2 overwrite: pre-create <course_root>/eval-report.json with sentinel bytes → exit 0 (proceeds), stderr contains "overwrit" + names the path, file replaced with new full-shape content (catches silent-overwrite; warn-and-proceed verdict)
  - N3 partial report refused: --case 1 --write-report → exit 1, stderr names --case and --write-report as incompatible, NO file written (single-case accuracy rendered as course-level by build = misleading durable artifact)
  - N4 write failure propagates: <course_root> read-only (chmod 0o555; skip-with-notice when running as root where chmod is ineffective) → exit 1, stderr names write error, no partial/leftover .tmp artifact (catches swallowed-error sneaky-pass)
- verification: code — cargo integration tests in crates/cli/tests/eval.rs (built binary, real course-root walk, real file I/O, real build consumer)
- fixture status: NEW — tempdir setup inside crates/cli/tests/eval.rs (copy two existing fixture files from crates/core/tests/fixtures/eval_command/ + write 3-line blendtutor.toml marker; course_root_for only checks manifest existence, smevals_gen.rs:364). Existing eval_command/ fixture dir has NO blendtutor.toml (glob-verified) — reusing as-is would hit N1. No new fixture tree.
- rubric anchor: §1.3.1 (refusal before effects — no-course/partial checks precede any write; range-checked consumer at read boundary), §2.3 (effectful shell around pure core — run_eval→EvalReport stays pure; write is the single effectful step after emit_eval, mirroring build's plan_site/write_site split), §3.2 (consume as-is — artifact schema frozen, consumer reads accuracy only), §5.1 (thin orchestration — write logic its own small fn, testable without patches)
- assertion-quality: behavioral-integration with value-equality core (built binary → file-system state → parse → exact accuracy/cases assertions → build round-trip; structural, no implementation-mirroring). ⚠️ Pattern flag (both proposers): docs/test-strategy/assertion-quality.md does NOT exist in this repo (glob empty) — classification uses role-prompt-implied vocabulary; Director should reconcile doc path or drop the pointer from spec prompts.

### Design Intent
- Types (§1): no new types — EvalReport (Serialize-only, eval.rs:295-299) is the typed artifact; --write-report is a plain bool clap flag (not enum); course_root_for returns Option<PathBuf> — None = "no course" refusal state; overwrite/partial refusal keeps "silently clobbered artifact" + "1-case report masquerading as course accuracy" unrepresentable as success.
- Pure/effectful (§2): run_eval stays pure-ish core producing EvalReport; the write is exactly one effectful step AFTER scoring + emit_eval, never interleaved with scoring. Single serialization path (serde_json::to_string(&report)) shared with --format json stdout. crates/core untouched.
- Boundaries (§3): course root = nearest blendtutor.toml ancestor — existing domain joint (course_root_for, shared with eval_report.rs), no new walker invented. Flag lives on eval (AC-6 P5 forbids new subcommand). Schema consumed as-is by build.
- Module responsibility (§4): eval.rs owns "measure + optionally persist"; build.rs stays consumer-only; no new module. eval.rs doc comment (:29-31, "measures, it is not a gate") MUST be updated to name the new side effect + eval-report.json filename.
- Function discipline (§5): run() gains one bool param; write step is its own fn write_report_artifact(report, course_root) -> anyhow::Result<PathBuf> returning the written path so run() prints the confirmation. Filename const local to eval.rs with doc-comment pointer to build.rs:14 as contract source (drift caught by round-trip test).

### Technical Context
- Files touched: crates/cli/src/commands/eval.rs:29-58 (flag param, canonicalize lesson_path BEFORE course_root_for — mirrors eval_report.rs:56; refusal checks; write step; updated doc comment), crates/cli/src/main.rs:71-81 + 143-147 (#[arg(long)] write_report: bool on Eval variant + dispatch), crates/cli/tests/eval.rs (new tests: positive + N1-N4 negatives + round-trip; 0 existing tests change — clap bool defaults false), docs/book/src/whole-game.md:44-80 (eval region: --write-report becomes the native durable path, replaces manual --format json > redirect), docs/book/src/creating-lessons.md:319-332 (same region), README.md (optional one-line mention — builder discretion, not spec-pinned)
- Files NOT touched (frozen): crates/cli/src/commands/build.rs (consumer, EVAL_REPORT_FILE:14 + load fn — see shared-const verdict), crates/core/src/eval.rs (EvalReport Serialize shape frozen), crates/core/src/site/mod.rs:389-415 (eval_summary_from_report_json consumer frozen), crates/cli/src/output.rs:26-33 (OutputFormat — NO new variant), crates/cli/src/commands/eval_report.rs (smevals path stays uvx-gated, unchanged), scripts/eval-course.sh + scripts/tests/eval-course.sh (smevals-path writers, untouched)
- Implementation requirements (in-spec, non-negotiable):
  - Canonicalize first: canonicalize lesson_path before course_root_for — relative path from non-course-root CWD walks relative ancestors and misses the manifest (same bug eval_report.rs:56 fixed).
  - Atomic write: write <course_root>/.eval-report.json.tmp then std::fs::rename; on Windows rename fails if target exists — remove target first (small non-atomic window, acceptable for dev tool; same remove-then-rename pattern as eval_report.rs replace_dir :206-214).
  - Confirmation: success message names eval-report.json and the resolved course-root path (stdout or stderr; tests accept either).
  - --format interaction: file ALWAYS full-JSON regardless of --format; --format controls stdout only.
  - Shape note: committed examples/write-less-code-*/eval-report.json use minimal {"accuracy": 1} — hand-written shortcut, NOT canonical; consumer accepts both. Overwrite warning (not refusal) lets users regenerate example-course artifacts deliberately.
- Expected test migration: 1 file extended (crates/cli/tests/eval.rs), 0 existing tests changed. commands::eval::run signature gains bool param; main.rs dispatch mechanical. Literal grep done (A): 63 repo-wide eval-report.json matches all accounted (consumer + smevals path + docs); no existing test asserts "eval does not write files".

### Dependencies
- Depends on: AC-6 (9-subcommand surface pinned, P5), AC-7 (serialized on crates/cli/tests/eval.rs + whole-game.md eval region)
- Blocks: nothing downstream (AC-8, wave-2 tail)
- Conflict set: crates/cli/tests/eval.rs + crates/cli/src/commands/eval.rs + main.rs Eval arm (shared with AC-7), whole-game.md eval region (shared with AC-7) — serialize AC-7 → AC-8
- Risk: medium

### Pattern Detectors
- Bidirectional-contract flag: TRIGGERED — writer (eval --write-report) + reader (build → eval_summary_from_report_json). Predicate #8 (build round-trip) MANDATORY; without it a wrong-schema write passes write-side tests but build fails. Round-trip test lives in eval.rs (writer is subject); if builder moves it to build.rs, no duplicate write-side setup.
- Dual-consumer field flag: TRIGGERED → RESOLVED — artifact flows to build (reads accuracy only) + user (inspects/diffs/commits). Verdict: FULL shape; minimal-shape committed examples are hand-written shortcuts; overwrite warning (not refusal) permits deliberate regeneration.
- Refusal-arm enumeration: all four arms have explicit tests (N1 no-course, N3 partial, N4 write-failure exit-1; N2 overwrite = warn-not-refuse, tested for warning content + replacement).
- Doc-pointer flag: docs/test-strategy/assertion-quality.md missing from repo — both proposers flagged. Needs Director/user reconciliation. Does NOT block this AC.

**Disagreement level: moderate** (divergence on overwrite policy + shared const + refusal-arm enumeration; converged on surface, shape, fixture, verification medium). All needs-clarification resolved in-spec.

### Progress
- [x] RED suite committed (5041599): 7 tests in crates/cli/tests/eval.rs — positive items 1-6 (incl. `--format json` byte-identity), N1-N4, build round-trip, `eval --help` flag pin. All failed on the absent flag (clap exit 2 / missing help entry).
- [x] Implementation committed (0592438): `#[arg(long)] write_report: bool` on Eval variant + dispatch; canonicalize-before-course_root_for; refusals precede scoring; `write_report_artifact` (.tmp + rename, Windows remove-then-rename retry, no .tmp leftover on failure); confirmation on stderr (keeps `--format json` stdout byte-pure); doc comment names the side effect + filename.
- [x] Docs committed (dc8a170): whole-game.md eval region, creating-lessons.md Step 8, README loop one-liner.
- [x] Evidence: docs/evidence/230/ (test-suite.log 16/16 + real-binary cli-transcript.log covering positive, overwrite, build round-trip 67%, N1, N3).
- [x] Full workspace `cargo test` green; clippy clean; 9-subcommand surface untouched (cli.rs pin unchanged).

### Decision Log
- Confirmation + overwrite warning go to **stderr**, not stdout: the spec allows either, but `--format json --write-report` must keep stdout a single pure JSON document for the byte-identity pin (item 5) — a stdout confirmation would break it. Matches the repo's "stdout is the data stream" pattern (embed-key WARNING precedent).
- Atomic write = rename fast-path with remove-then-rename **retry** on failure (Windows target-exists), not unconditional remove-first: keeps the unix rename atomic, satisfies the Windows caveat, and the failure path removes the `.tmp` so no leftover survives.
- N4 `.tmp`-leftover guarantee is enforced by cleanup-on-rename-failure plus the fact that the read-only-dir failure hits at `fs::write(tmp)` (nothing created yet).

### Surprises & Discoveries
- macOS tempdir paths are symlinked (`/var/folders/...` → `/private/var/folders/...`): the binary canonicalizes the lesson before walking ancestors, so paths it prints are `/private/...` while a naive test assertion on the raw tempdir path would mismatch. Fix: the test helper canonicalizes the course dir before returning it, so asserted and printed paths agree.
- The `eval --help` flag pin (item 7) does not match the `eval_write_report` test filter, so it must be run under its own name (`eval_help_lists_write_report_flag`) — the spec's probe list filters by `eval_write_report*` and would silently skip it.

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open
