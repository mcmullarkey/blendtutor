---
ac: 1
depends_on: none
risk: low
status: spec
---

# AC-1 — Commit real eval-report evidence for scaffold lesson_hello

## Executable Spec (resolved)
**Merge decision: Speculator B's spec adopted wholesale** (A's structural checks subsumed; B's anti-canned-copy content assertions are load-bearing). Verification = `code` (one-off python3+git probe, not a repo test). Rubric anchor = none (evidence-integrity AC).

### Executable Spec

- **predicate:** ALL of the following hold on the committed tree (run from repo root after `git commit`):

  **Existence + stem contract:**
  1. `docs/evals/lesson_hello/` is a directory (not file, not symlink).
  2. `docs/evals/hello/` does NOT exist (manifest id `hello` ≠ file stem `lesson_hello` — wrong-stem sneaky-pass).
  3. `docs/evals/lesson_hello/index.json` exists and parses as JSON.
  4. `docs/evals/lesson_hello/evals/lesson_hello/eval.json` exists and parses as JSON.

  **eval.json is FOR lesson_hello (anti-canned-copy):**
  5. `eval.json` → `.eval.name` == `"lesson_hello"` (not `"01_seed_data"`, not `"hello"`).
  6. `eval.json` → `.eval.description` contains substring `"hello"` AND `"cat()"` (lesson_hello's exercise prompt — rules out 01_seed_data whose description mentions `survey_data`).
  7. `eval.json` → `.eval.tasks` is an array of exactly 2 elements (lesson_hello has 2 cases; 01_seed_data has 4).
  8. `eval.json` → `.eval.tasks[0].prompt` == `"cat(\"hello\\n\")"` AND `.eval.tasks[0].expected` == `"correct"`.
  9. `eval.json` → `.eval.tasks[1].prompt` == `"cat(\"goodbye\\n\")"` AND `.eval.tasks[1].expected` == `"incorrect"`.
  10. `eval.json` → every `.eval.tasks[].lesson` ends with `"/lesson_hello.yaml"` (absolute path baked in is the known AC-5 issue — acceptable; but the FILENAME must be lesson_hello.yaml, not 01_seed_data.yaml).

  **Runs are real (anti-stub, anti-stale):**
  11. `docs/evals/lesson_hello/evals/lesson_hello/runs/` contains subdirs `case-1` and `case-2`.
  12. Each case's deepest timestamped run dir contains `output.txt` whose first line matches `^verdict: (correct|incorrect)$` (the runner contract — a stub would omit this).
  13. Each case's `run.yaml` → `task.lesson` ends with `/lesson_hello.yaml` (anti-wrong-lesson).
  14. Each case's `run.yaml` → `task.prompt` is `cat("hello\n")` (case-1) or `cat("goodbye\n")` (case-2) (anti-wrong-lesson).
  15. `index.json` → `.evals[0].slug` == `"lesson_hello"` AND `.evals[0].runs` == 2 AND `.evals[0].graded` == 2.

  **Commit hygiene (anti-sweep, anti-leak):**
  16. `git ls-files docs/evals/lesson_hello/` is non-empty (the tree is tracked).
  17. `git ls-files | grep -c '\.smevals'` == 0 (ephemeral dir never committed).
  18. `git status --porcelain` is empty (clean working tree — no untracked .smevals, no swept 01_seed_data changes).
  19. Every path in `git diff --name-only HEAD~1 HEAD` starts with `docs/evals/lesson_hello/` (scoped commit — no unrelated docs/evals/ changes swept in by `git add docs/evals/`).

- **probe:**

  Prerequisite (manual, real-key — builder runs locally, NOT in CI):
  ```bash
  # From repo root. Init temp course in /tmp so .smevals can never leak into repo.
  cargo run -p blendtutor-cli -- init /tmp/hello-course
  FIREWORKS_API_KEY=<real-key> cargo run -p blendtutor-cli -- eval-report /tmp/hello-course/lesson_hello.yaml
  # eval-report exits 0 (grade-fail-is-evidence: even all-fail still exits 0 if runs recorded)
  git add docs/evals/lesson_hello/
  git commit -m "docs(evals): commit real eval-report for lesson_hello"
  ```

  Structural assertions (code — python3 inline script, runnable from repo root after commit):
  ```bash
  python3 - <<'PY'
  import json, os, re, subprocess, sys

  ROOT = os.getcwd()
  D = os.path.join(ROOT, "docs", "evals", "lesson_hello")
  WRONG_STEM = os.path.join(ROOT, "docs", "evals", "hello")
  fails = []

  def check(cond, msg):
      if not cond: fails.append(msg)

  # 1-2: existence + wrong-stem
  check(os.path.isdir(D), "docs/evals/lesson_hello/ is not a directory")
  check(not os.path.exists(WRONG_STEM), "docs/evals/hello/ exists (wrong stem — manifest id, not file stem)")

  # 3-4: JSON files parse
  idx_path = os.path.join(D, "index.json")
  eval_json_path = os.path.join(D, "evals", "lesson_hello", "eval.json")
  check(os.path.isfile(idx_path), "index.json missing")
  check(os.path.isfile(eval_json_path), "evals/lesson_hello/eval.json missing")
  if os.path.isfile(idx_path):
      idx = json.load(open(idx_path))
  else:
      idx = {}
  if os.path.isfile(eval_json_path):
      ej = json.load(open(eval_json_path))
  else:
      ej = {}

  ev = ej.get("eval", {})
  tasks = ev.get("tasks", [])

  # 5-7: name, description, task count
  check(ev.get("name") == "lesson_hello", f"eval.name={ev.get('name')!r} != 'lesson_hello'")
  desc = ev.get("description", "")
  check("hello" in desc and "cat()" in desc, f"eval.description does not mention hello+cat(): {desc[:80]!r}")
  check(len(tasks) == 2, f"expected 2 tasks, got {len(tasks)}")

  # 8-9: task prompts + expected polarities
  if len(tasks) >= 2:
      check(tasks[0].get("prompt") == 'cat("hello\n")', f"task[0].prompt={tasks[0].get('prompt')!r}")
      check(tasks[0].get("expected") == "correct", f"task[0].expected={tasks[0].get('expected')!r}")
      check(tasks[1].get("prompt") == 'cat("goodbye\n")', f"task[1].prompt={tasks[1].get('prompt')!r}")
      check(tasks[1].get("expected") == "incorrect", f"task[1].expected={tasks[1].get('expected')!r}")

  # 10: lesson paths end with /lesson_hello.yaml
  for i, t in enumerate(tasks):
      lp = t.get("lesson", "")
      check(lp.endswith("/lesson_hello.yaml"), f"task[{i}].lesson={lp!r} does not end with /lesson_hello.yaml")

  # 11-14: runs are real
  runs_dir = os.path.join(D, "evals", "lesson_hello", "runs")
  for case in ["case-1", "case-2"]:
      case_dir = os.path.join(runs_dir, case)
      check(os.path.isdir(case_dir), f"runs/{case}/ missing")
      if not os.path.isdir(case_dir): continue
      run_dirs = []
      for dp, dn, fn in os.walk(case_dir):
          if "output.txt" in fn and "run.yaml" in fn:
              run_dirs.append(dp)
      check(len(run_dirs) >= 1, f"runs/{case}/ has no complete run dir (output.txt+run.yaml)")
      if not run_dirs: continue
      rd = sorted(run_dirs)[0]
      out = open(os.path.join(rd, "output.txt")).read()
      first_line = out.split("\n")[0]
      check(bool(re.match(r"^verdict: (correct|incorrect)$", first_line)),
            f"runs/{case}/ output.txt first line={first_line!r} does not match 'verdict: <polarity>'")
      ry = open(os.path.join(rd, "run.yaml")).read()
      check("lesson_hello.yaml" in ry, f"runs/{case}/ run.yaml does not reference lesson_hello.yaml")
      check(("cat(" in ry), f"runs/{case}/ run.yaml has no cat() prompt")

  # 15: index.json summary
  evals = idx.get("evals", [])
  check(len(evals) == 1, f"index.json evals has {len(evals)} entries, expected 1")
  if evals:
      check(evals[0].get("slug") == "lesson_hello", f"index.json slug={evals[0].get('slug')!r}")
      check(evals[0].get("runs") == 2, f"index.json runs={evals[0].get('runs')} != 2")
      check(evals[0].get("graded") == 2, f"index.json graded={evals[0].get('graded')} != 2")

  # 16-17: git tracking
  tracked = subprocess.check_output(["git", "ls-files", "docs/evals/lesson_hello/"], text=True).strip()
  check(len(tracked) > 0, "docs/evals/lesson_hello/ has no tracked files")
  all_tracked = subprocess.check_output(["git", "ls-files"], text=True)
  smevals_count = len([l for l in all_tracked.splitlines() if ".smevals" in l])
  check(smevals_count == 0, f"{smevals_count} .smevals paths tracked (should be 0)")

  # 18: clean working tree
  status = subprocess.check_output(["git", "status", "--porcelain"], text=True).strip()
  check(status == "", f"working tree not clean: {status!r}")

  # 19: scoped commit
  diff = subprocess.check_output(["git", "diff", "--name-only", "HEAD~1", "HEAD"], text=True).strip()
  bad = [p for p in diff.splitlines() if not p.startswith("docs/evals/lesson_hello/")]
  check(len(bad) == 0, f"commit contains paths outside docs/evals/lesson_hello/: {bad}")

  if fails:
      print("FAIL:", file=sys.stderr)
      for f in fails: print(f"  - {f}", file=sys.stderr)
      sys.exit(1)
  print("OK: all 19 assertions hold")
  PY
  ```

- **negative:** The cheapest broken implementation: copy `docs/evals/01_seed_data/` → `docs/evals/lesson_hello/` and commit. Passes "docs/evals/lesson_hello/ exists" but fails assertions 5-9 (eval.name="01_seed_data", description mentions survey_data, 4 tasks not 2, prompts are survey_data not cat-hello). Second cheapest: create `docs/evals/hello/` using the manifest id (`hello`) instead of the file stem (`lesson_hello`) — passes "a report exists" but breaks the URL contract `/evals/lesson_hello/` that AC-2/AC-4 hard-code (assertion 2 catches this). Third: `git add docs/evals/` sweeping 01_seed_data changes into the commit (assertion 19 catches this).

- **verification:** code (bash/python structural assertions on committed tree) + manual (real-key run prerequisite — FIREWORKS_API_KEY required for 2 paid judge calls; cannot be faked or CI-automated without the key)

- **fixture status:** NEW — `docs/evals/lesson_hello/` (entire committed tree is new; no existing fixture). Scaffold source assets exist at `crates/core/src/scaffold/{lesson_hello.yaml,eval_lesson_hello.yaml,blendtutor.toml}`.

- **rubric anchor:** §2.2 (effectful shell — `eval_report::run` is the thin orchestration that produces the artifact; the committed tree IS the effect's observable output) + grade-fail-is-evidence convention (AC-5 smevals-eval-report: non-zero exit + has_runs → proceed to build; do NOT gate merge on judge score)

## Design Intent

This AC produces a **committed evidence artifact**, not code. The design intent is evidence integrity: the committed tree must be provably a REAL fresh `blendtutor eval-report` run against `lesson_hello.yaml`, not a copy, stub, or wrong-lesson report.

- **§2.2 (separate pure from effectful):** `eval_report::run` is the effectful shell; the committed tree is its observable output. The predicate asserts the output's shape, not the shell's internals.
- **§1.3.1 (guard fires first):** `clean_stale()` runs before regeneration so stale runs are never built into the new report. The fresh timestamps + matching task prompts are the downstream signal.
- **§4.1 (document module responsibility):** The report's `eval.json` is self-describing — `name`, `description`, `tasks` pin it to a specific lesson. The predicate exploits this to rule out canned copies.

The adversarial lens: **what's the cheapest broken implementation that passes "docs/evals/lesson_hello/ exists"?** Answer: copy 01_seed_data and rename. The predicate's content assertions (name, description, task count, task prompts, lesson path filename) make that fake fail loudly.

## Technical Context

**Files involved:**
- `crates/cli/src/commands/eval_report.rs:48` — `run(lesson_path)` — the command. Canonicalizes path → derives lesson_id from file stem → finds course_root (blendtutor.toml ancestor) → cleans stale .smevals/ → generates eval dir → `uvx smevals==0.2.0 run -g` → `uvx smevals==0.2.0 build -o .<stem>.tmp` → `replace_dir` (atomic rename into `docs/evals/<stem>/`).
- `crates/core/src/scaffold.rs:60` — `scaffold_plan()` — writes `blendtutor.toml` (id="hello", path="lesson_hello.yaml"), `lesson_hello.yaml`, `eval_lesson_hello.yaml`, `README.md`, `.gitignore`.
- `crates/core/src/scaffold/lesson_hello.yaml` — R lesson, exercise prompt "Write R code that prints the word \"hello\" on its own line, using cat()."
- `crates/core/src/scaffold/eval_lesson_hello.yaml` — 2 cases: `cat("hello\n")`→correct, `cat("goodbye\n")`→incorrect.
- `crates/core/src/scaffold/blendtutor.toml` — manifest with `id = "hello"` (NOT "lesson_hello" — the manifest id ≠ file stem; eval-report uses file stem for the output dir).
- `.gitignore:10` — `**/.smevals/` (repo-level ignore).
- `docs/evals/01_seed_data/` — existing committed smoke evidence (4 cases, survey_data) — the canned-copy target the predicate must rule out.

**Architecture notes:**
- `lesson_id_from_path()` (`smevals_gen.rs:352`) returns the file STEM = `lesson_hello`. This is what determines `docs/evals/lesson_hello/`. The manifest id `hello` is NOT used for the output path — a common confusion point.
- `course_root_for()` walks ancestors for `blendtutor.toml`. `repo_root_for()` walks ancestors for `.git`. The course must be inside a git checkout for `docs/evals/` to be found.
- `clean_stale()` removes `<course>/.smevals/` before regenerating — anti-stale-evidence.
- `replace_dir()` — atomic rename: remove old report, rename temp into place. On failure, prior committed report survives.
- grade-fail-is-evidence: if `smevals run` exits non-zero but `has_runs()` is true (runs/ non-empty), proceed to build. Non-zero exit with NO runs = harness failure → error.
- The committed report carries ABSOLUTE paths in `eval.json` tasks[].lesson and `run.yaml` task.lesson (known AC-5 issue). Acceptable for evidence; the predicate asserts the FILENAME (`lesson_hello.yaml`) not the full path.

**FIREWORKS_API_KEY availability:**
- The builder needs a real `FIREWORKS_API_KEY` for 2 paid judge calls (one per case). Without it, `smevals run` fails (judge HTTP 403/timeout), `has_runs()` may be false → error exit → no report produced.
- **Fail path:** if the key is unavailable, the builder MUST report to the Director that AC-1 is blocked. Do NOT fake a report (no stub, no copy). The AC cannot be satisfied without a real run.
- The judge uses `accounts/fireworks/models/deepseek-v4-flash-0731` (provider default). `judge_feedback.py` sends explicit `User-Agent: blendtutor-smevals-judge/0.1` (AC-5 fix — Python-urllib default UA banned by Fireworks Cloudflare edge, error 1010).

**Judge score pathology:**
- lesson_hello grades a trivial `cat("hello\n")`. The LLM judge may score oddly. This is EXPECTED — grade-fail-is-evidence means the report is still valid evidence regardless of score.
- Do NOT gate merge on score. The predicate asserts the report's STRUCTURE (2 cases ran, verdicts recorded), not the scores.
- If ALL cases fail (both graded incorrect), still commit — it's evidence per convention.

**Where to init the temp course:**
- Recommend `/tmp/hello-course` (outside repo) so `.smevals/` can never leak into the repo regardless of .gitignore.
- The committed report will carry `/tmp/hello-course/lesson_hello.yaml` as the absolute path in eval.json/run.yaml — acceptable (known AC-5 issue).

## Dependencies
- Depends on: none (scaffold lesson_hello.yaml exists at course root)
- Blocks: AC-2 (cites path+URL /evals/lesson_hello/), AC-4 (pins built whole-game.html contains evals/lesson_hello)
- Conflict set: docs/evals/lesson_hello/ only

## Divergence Log
Predicate = B's 19 assertions (A subsumed — B catches canned-copy). Probe = B's python3 inline (one-off). Wrong-stem guard + commit-scoping kept. Fail-path contract + /tmp init → Technical Context. Absolute-path tolerance kept. A's "code/bash+manual" verification dropped → `code` (invalid medium; "manual" was procedural key-caveat). Rubric anchor dropped → none. Disagreement = minor.

## Progress
- [ ] Real eval-report run + commit — pending

## Decision Log
- 2026-08-10 — Resolver: B adopted wholesale; verification code; rubric none

## Surprises & Discoveries
- (none yet)

## Idempotence & Recovery
- Safe retry: re-run eval-report (clean_stale wipes .smevals first); re-run probe
- Rollback: git rm -r docs/evals/lesson_hello/ + commit revert