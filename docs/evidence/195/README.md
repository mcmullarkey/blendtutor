# Issue #195 — E2E Evidence

AC-2: pure Rust generator emitting a smevals eval dir (`eval.yaml`,
`tasks/case-N.yaml`, `configs/default.yaml`, `graders/default.yaml`) from a
lesson + sibling eval suite, plus the Fireworks provider default bump to
`deepseek-v4-flash-0731`. Verification: code.

## What changed

1. **`generate_eval_dir(&Lesson, &EvalSuite, &str) -> Result<Vec<(PathBuf, String)>, GenError>`**
   — pure, deterministic emitter in `crates/core/src/smevals_gen.rs`. File set
   exact (`eval.yaml` + `configs/` + `graders/` + one `tasks/case-N.yaml` per
   case, 1-based), submissions emitted as injection-proof double-quoted YAML
   scalars (every control char escaped; proven byte-exact under re-parse, where
   serde-saphyr's block-scalar chomping is lossy), model id single-sourced from
   `ProviderChoice::Fireworks.default_model()`, non-slug lesson ids and empty
   suites refused at the boundary.
2. **`write_eval_dir`** — thin effectful shell: computes the `scripts/smevals/`
   relative prefix for the course's actual location (walk-up to the repo root),
   delegates generation, writes `<course>/.smevals/`. Shared helpers
   `lesson_id_from_path` (file stem) + `course_root_for` (walk-up to
   `blendtutor.toml`) for AC-5.
3. **Provider default** — `ProviderChoice::Fireworks.default_model()` →
   `accounts/fireworks/models/deepseek-v4-flash-0731` (matches ADR-0016 browser
   BYOK pin); unit test pinned to the new value.
4. **Gitignore** — `**/.smevals/` added; provably does NOT match
   `docs/evals/` (committed build output) or `crates/*/tests/fixtures/evals/`.

## Evidence

| File | Proves |
|------|--------|
| `probe-generate-eval-dir.log` | 11/11 integration tests: file set, round-trip over all 12 fixtures, adversarial injection byte-identity, path safety, determinism, empty-suite refusal, ordering, model provenance, gitignore interplay, golden-dir byte equivalence |
| `probe-provider-default-model.log` | `provider_default_model_is_pinned_to_0731` + updated recognizability test |
| `probe-gitignore.log` | `git check-ignore` exits 0 for `.smevals/`, non-zero for `docs/evals/`, `tests/fixtures/evals/`, `docs/evidence/` |
| `e2e-smevals-smoke.log` | **Real-tool round trip**: the generated dir was consumed by `uvx smevals==0.2.0 run -g` (3/3 cases, `grade: pass score=1.0` — proving the emitted runner/checker relative paths resolve and are executable, the `SMEVALS_TASK_*` env wiring works, and the model id from `configs/default.yaml` was read) then `uvx smevals==0.2.0 build` (static report `index.html` produced) |

## Why unit tests alone were insufficient

The generator's output is an external contract (smevals v0.2.0). Golden-dir
byte comparison proves *stability*; the smoke run proves *consumability* —
the emitted `runner:`/`checker:` paths must resolve relative to the generated
`configs/`/`graders/` files and be executable, which only the real smevals CLI
exercises. The smoke used stub runner/checker scripts (AC-3 ships the real
ones later); the generated dir, paths, env wiring, and model config were all
real.

## Status

All probes pass. `cargo test -p blendtutor-core` (186 tests) and
`cargo test -p blendtutor-cli` green.
