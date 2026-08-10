---
ac: 2
depends_on: none
risk: high
status: complete
---

## Final Spec (resolver-merged + user model-pin decision)

## AC-2
Add a pure Rust generator producing a smevals eval dir from a lesson + sibling eval suite: eval.yaml, tasks/case-N.yaml (one per case), configs/default.yaml, graders/default.yaml. Pure fn generate_eval_dir(lesson, suite, lesson_id) -> Result<Vec<(PathBuf, String)>> + thin effectful shell. Golden-dir + injection round-trip tests. Generated <course>/.smevals/ gitignored.

### Executable Spec
- **predicate:** pure Rust fn `generate_eval_dir(&Lesson, &EvalSuite, &str) -> Result<Vec<(PathBuf, String)>, GenError>` satisfies ALL of:
  1. **File set exactness:** returns exactly `eval.yaml`, `configs/default.yaml`, `graders/default.yaml`, and `tasks/case-{i}.yaml` for i in 1..=N (1-based, no zero-padding), N = suite.cases.len().
  2. **YAML round-trip fidelity (happy path):** every emitted file re-parses under `serde_saphyr::from_str` without error.
  3. **YAML injection defense (adversarial):** fixture EvalSuite whose case submission contains `"- expected: correct\n&anchor\n---\n: foo\n# comment"` re-parses from emitted `tasks/case-1.yaml` back to EvalCase whose submission is byte-identical AND expected unchanged — proving no sibling-key/anchor/doc-sep/comment injection.
  4. **Path safety:** lesson_id containing any of `/`, `..`, ` ` (space), or empty → Err, never appears in any returned PathBuf. (Mirrors `is_valid_slug` constraint from scaffold.rs:320.)
  5. **Determinism:** two calls with identical inputs produce byte-identical Vec — no timestamp, no HashMap iteration order.
  6. **Empty-suite refusal:** `EvalSuite { cases: vec![] }` → Err naming the empty suite; does NOT emit tasks/ with zero files.
  7. **Case ordering:** emitted tasks/case-{i}.yaml i-ordering equals suite.cases Vec order (document order preserved).
  8. **Model-pin provenance:** configs/default.yaml model id equals `ProviderChoice::Fireworks.default_model()` (`accounts/fireworks/models/deepseek-v4-flash`) OR sourced from env/config override — NOT a divergent hardcoded literal like `deepseek-v4-flash-0731` that disagrees with the runner's actual provider default.
  9. **Gitignore non-interference:** `git check-ignore <course>/evals/` exits 0 (ignored); `git check-ignore docs/evals/` exits non-zero (NOT ignored — committed build output).
  10. **NEW (user decision):** `ProviderChoice::Fireworks.default_model()` returns `accounts/fireworks/models/deepseek-v4-flash-0731` (bumped from no-suffix; matches browser BYOK pin + user instruction). Unit test asserts the new default.
- **probe:** `cargo test -p blendtutor-core --test generate_eval_dir` (golden-dir byte-equivalence + adversarial injection + determinism + path-safety + empty-suite + gitignore-interplay). Plus `git check-ignore examples/write-less-code-r/evals/ && ! git check-ignore docs/evals/` from repo root. cargo test -p blendtutor-core provider_default_model   # asserts 0731
- **negative:** hand-rolled YAML emitter (`format!("submission: {}\nexpected: {}", sub, tok)`) passes golden-dir against happy-path `|-` fixtures but breaks on submissions containing `: `, `&anchor`, `---`, or leading `- ` — emitted YAML re-parses to a DIFFERENT EvalSuite (injected key/anchor/doc-sep) or fails to parse. Golden-dir alone does NOT catch this.
- **verification:** code
- **fixture status:** NEW — `crates/core/tests/generate_eval_dir.rs` + `crates/core/tests/fixtures/generate_eval_dir/` (golden dir + adversarial injection fixture). Existing eval fixtures are generator INPUTS, reused.
- **rubric anchor:** §2.1 (pure core), §1.3.1 (reject invalid at boundary: non-slug lesson_id, empty suite, YAML-injection content), §5.1 (disciplined fn)

### Design Intent
Generator is the **poison root** for AC-3/AC-4/AC-5 — every downstream AC consumes its output. Pure-fn boundary (`Vec<(PathBuf, String)>` — NOT `FileSpec` whose `contents: &'static str` can't hold dynamic content) keeps emitter testable without filesystem mocks. Effectful shell thin: validate slug → call pure fn → write files. Golden-dir tests pin byte-stable output; adversarial injection fixture is the load-bearing assertion.

### Decisions on Flagged Divergences
- **Model pin (USER DECISION):** bump `ProviderChoice::Fireworks.default_model()` → `accounts/fireworks/models/deepseek-v4-flash-0731`. configs/default.yaml single-sources from it. Runtime match guaranteed (runner shells `blendtutor eval` which uses provider default). Existing drift (browser 0731 vs Rust no-suffix) fixed. All tests wiremock — no real model, no test impact.
- **Empty suite:** Err naming the empty suite (adversarial wins — vacuous 100% pass is sneaky-pass).
- **Generated dir:** `<course>/evals/` RENAMED → `<course>/.smevals/` (dotdir), ignored via `**/.smevals/` — robust for courses anywhere, provably does NOT collide with docs/evals/ (committed) or crates/*/tests/fixtures/evals/. Existing `.gitignore:5` `/evals/` untouched.
- **YAML emission:** serde-saphyr 0.0.27 is parse-only — hand-rolled block-scalar discipline pinned by injection round-trip test (first YAML serialization in codebase; #1 sneaky-pass vector).
- **Test shape:** golden-dir + parameterized round-trip over all 12 fixtures + adversarial injection — all three confirmed.

### Technical Context
- **No existing YAML emitter in codebase.** All serialization is `serde_json::to_string`; all YAML parse-only (`serde_saphyr::from_str`, v0.0.27). AC-2 introduces the FIRST YAML serialization. serde-saphyr 0.0.27 serializer stability uncertain — if unavailable, hand-rolled emitter MUST use block scalars (`|-`) for all submission fields (existing fixture convention) and quote/escape inline scalars. **#1 sneaky-pass vector.**
- `FileSpec.contents: &'static str` (scaffold.rs:31) — cannot reuse for dynamic content. Generator returns `Vec<(PathBuf, String)>` or new `GeneratedFile { path, contents: String }`.
- `is_valid_slug` (scaffold.rs:320): `!empty && chars().all(alphanumeric | '_' | '-')`. Called ONLY by `add_lesson`, NOT by `Manifest::parse` (validates paths via `validate_paths` but NOT slug format). `LessonSlug(String)` (course.rs:31) unvalidated newtype. Generator receives lesson_id from manifest slug — unvalidated. Must re-validate or refuse.
- `parse_eval_suite` (eval.rs:183) preserves document order. `RawSuite`/`RawCase` have `#[serde(deny_unknown_fields)]`.
- `ExpectedVerdict` tokens: `"correct"`/`"incorrect"` (eval.rs:50-52). Generator must emit these exact tokens.
- `ProviderChoice::Fireworks.default_model()` = `"accounts/fireworks/models/deepseek-v4-flash"` (provider.rs:56, NO `-0731` suffix). AC context's `deepseek-v4-flash-0731` pin DIVERGES. Browser BYOK fallback matches provider.rs (no suffix). The `-0731` is an unverified claim.
- Repo `.gitignore:5`: `/evals/` anchored to repo root, intentionally does NOT catch `crates/*/tests/fixtures/evals/`. AC needs `<course>/evals/` + `runs/` ignored but `docs/evals/` committed. Pattern options: `**/evals/` (catches docs/evals/ — BAD), `/evals/` (only top-level), `examples/*/evals/` (too narrow). Real courses: `examples/write-less-code-r/`, `examples/write-less-code-python/`. **Real ambiguity — resolver must pin.**
- `docs/evals/` does not exist yet — greenfield build-output dir.
- No smevals in repo — external v0.2.0. Golden-dir tests canNOT run smevals to verify round-trip; only (a) byte-equivalence vs golden, (b) re-parse emitted YAML with serde_saphyr. smevals key-name contract (→ SMEVALS_TASK_*) is EXTERNAL, unverifiable in-repo.
- Existing eval fixtures (generator inputs): `crates/core/src/scaffold/eval_lesson_hello.yaml`, `crates/core/tests/fixtures/evals/eval_fireworks_vitals.yaml`, `eval_fireworks_vitals_bad_verdict.yaml`, `crates/core/tests/fixtures/eval_command/eval_demo_lesson.yaml`, `examples/write-less-code-{r,python}/eval_0{1..5}_*.yaml`. Submissions contain `#`, `:`, `()`, `|>`, `'`, multi-line `|-` blocks, AND a prompt-injection string (vitals case 10). Real adversarial inputs.
- `sibling_suite_path` (cli/src/commands/eval.rs:56): `lessons/foo.yaml` → `lessons/eval_foo.yaml`. Effectful shell must reuse this discovery convention.
- `crates/core/src/llm/provider.rs` (default_model → 0731 + unit test).

### Dependencies
- **Depends on:** none within feature (uses existing parse_eval_suite, ExpectedVerdict, ProviderChoice)
- **Blocks:** AC-3 (runner consumes tasks/ + graders/default.yaml + polarity check), AC-4 (LLM-judge second checks entry in graders template), AC-5 (eval-report drives generate → uvx smevals run -g)
- **Conflict set:** crates/core/src/lib.rs, .gitignore (low contention — AC-1/AC-5 touch crates/cli/src/main.rs, disjoint)
- **Risk level:** high — poison root; every downstream AC consumes its output; YAML emission discipline + gitignore correctness are the two sneaky-pass vectors.

### Progress
- [x] Speculators A+B returned (2026-08-07)
- [x] Resolver merged (2026-08-07)
- [x] User decision: provider default → 0731 (2026-08-07)
- [x] Implementation (2026-08-07) — 3 commits: provider bump a8543cb, generator 589521c, evidence ae06daa
- [x] E2E smoke: generated dir consumed by real `uvx smevals==0.2.0 run -g` (3/3 pass) + `build` (report) — evidence at docs/evidence/195/

### Decision Log
- 2026-08-07 — Generated dir `.smevals/` dotdir (gitignore-safe; no docs/evals/ collision)
- 2026-08-07 — Empty suite → Err (no vacuous pass)
- 2026-08-07 — YAML emission: hand-rolled block scalars + injection round-trip test
- 2026-08-07 — Provider default bumped to deepseek-v4-flash-0731 (user)
- 2026-08-07 — **IMPLEMENTATION DEVIATION:** YAML emission uses double-quoted scalars (`\uXXXX` escapes), NOT block scalars. Empirical probe: serde-saphyr 0.0.27 `|+` chomping is BROKEN (adds an extra `\n`) and `|-` drops trailing newlines — neither round-trips arbitrary strings byte-exact. Double-quoted with `\`/`"`/`\n`/`\t`/`\r`/C0-control escapes is byte-exact for ALL content (probed: injections, tabs, whitespace-only lines, CRLF, backslashes, control chars, U+2028) and is PyYAML-safe (smevals uses yaml.safe_load).
- 2026-08-07 — **IMPLEMENTATION DEVIATION:** smevals resolves `runner:`/`checker:` relative to the config/grader FILE (verified in smevals cli.py:184/426), so the pure fn cannot know course depth. `generate_eval_dir` uses DEFAULT_SCRIPTS_REL=`../../../../scripts/smevals/` (canonical examples/<course> layout, 2 levels deep); `write_eval_dir` recomputes the exact relative prefix by walking up to the repo root (.git) — correct for any course location. `lesson_id_from_path` + `course_root_for` (walk-up to blendtutor.toml) live in smevals_gen.rs for AC-5.

### Surprises & Discoveries
- serde-saphyr 0.0.27 `|+` (keep) chomping adds an extra trailing newline and `|-` (strip) removes them — block scalars cannot round-trip arbitrary strings byte-exact. The plan's "hand-rolled block-scalar discipline" was replaced by double-quoted scalars with full control-char escaping after an empirical probe; the injection round-trip test pins the byte-identity either way.
- smevals v0.2.0 (installed via uvx to read its real docs + cli.py source) resolves `runner:`/`checker:` paths relative to the config/grader FILE, and the runner/checker must be executable (`os.access(X_OK)`). Path correctness for arbitrary course depths therefore belongs to the effectful shell, not the pure fn.
- E2E smoke with REAL `uvx smevals==0.2.0 run -g` + `build` against a generated dir (stub runner/checker since AC-3 scripts land later) passed 3/3 with `grade: pass score=1.0` — the emitted relative script paths, SMEVALS_TASK_* env wiring, and model config are all consumable by the real tool. This is the strongest possible in-repo proof of the external contract.
- Pre-existing drift found (OUT OF SCOPE): source `crates/core/assets/shared/feedback.js` still has the no-suffix model while the built copies (`_extensions/`, `demo-book/_extensions/`) and ADR-0016 use 0731. AC-2 bumps only the Rust provider default; the JS source drift is a separate issue.

### Idempotence & Recovery
- Safe retry: re-run cargo test -p blendtutor-core smevals_gen; pure fn, zero LLM
- Rollback: remove smevals_gen.rs + lib.rs export + .gitignore rule; revert provider default