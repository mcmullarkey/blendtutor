# Issue #209 — summary

## What was done

Ran a real `blendtutor eval-report` on the scaffold starter lesson
(`lesson_hello.yaml`) with a real FIREWORKS_API_KEY (2 paid judge calls) and
committed the generated report to `docs/evals/lesson_hello/` as cited evidence
for the whole-game tutorial chapter (AC-1 of whole-game-tutorial). No code
changes — committed artifact only, as the issue requires.

## Outcome

- **Key:** FIREWORKS_API_KEY found in environment (real key). Used for the run.
- **Smoke:** `eval-report` exit 0. case-1 graded **pass score=0.96**
  (`cat("hello\n")` → expected `correct`, verdict `correct`); case-2 graded
  **fail score=0.56** (`cat("goodbye\n")` → expected `incorrect`, verdict
  `incorrect`). The "1 run(s) graded as fail" → `smevals run` exit 1 →
  grade-fail-is-evidence path: runs recorded → report built and published.
- **Probe:** 19-assertion canonical probe (plan AC-1.md) **PASS** — after
  correcting the probe's prompt-assertion escaping (see below).
- **Commit hygiene:** 11 files, all under `docs/evals/lesson_hello/`; zero
  `.smevals/` tracked; porcelain clean; commit scoped (no `01_seed_data` sweep).
- **Merge state:** PR created, left UNMERGED for the Director.

## Spec deviations (required by the implementation; no code changed)

1. **Temp course in-repo at `target/hello-course/` instead of `/tmp`.**
   `eval_report::run` resolves the report output via `repo_root_for()` (walks up
   from the course root for `.git`) and the runner script path via
   `scripts_rel_from()` (same walk). A `/tmp` course has no `.git` ancestor →
   report can't be published to the repo's `docs/evals/` and the runner path
   falls back to a broken `../../../../scripts/smevals/run.sh`. The issue's
   `/tmp` instruction (physical .smevals separation) is unreachable with the
   current implementation. `target/` is gitignored, so the anti-leak guarantee
   holds: `/target` + `**/.smevals/` gitignores + scoped
   `git add docs/evals/lesson_hello/`.
2. **`timeout` (GNU coreutils) missing on macOS** — `run.sh:50` uses it; first
   run failed with `timeout: command not found` (exit 127, no runs recorded).
   Fixed with `brew install coreutils` + PATH prefix
   (`/opt/homebrew/opt/coreutils/libexec/gnubin`). No repo change.
3. **`blendtutor` not on default PATH** — run.sh resolves it from PATH; prefixed
   `$PWD/target/debug`. No repo change.
4. **Probe prompt-assertion escaping bug (spec, not artifact):** both the
   plan-file probe and the issue-body probe encode the expected prompt with a
   real newline / dumps-doubled backslash; the genuine artifact carries literal
   backslash-n (scaffold YAML block scalar `|-`; correct R source). Corrected
   to `== 'cat("hello\\n")'`. Details in `probe.log`.

## Evidence files

- `smoke.log` — real eval-report run transcript, grades/scores, verdict lines.
- `probe.log` — 19-assertion probe result (PASS), encoding correction, timing note.
