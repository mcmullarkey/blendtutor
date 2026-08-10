# Plan: evidence-hardening

## Feature Goal

Make committed eval evidence portable + reproducible: scrub worktree-specific paths, fix generator fallback depth.

## ACs

| AC | Issue # | Status |
|----|---------|--------|
| AC-215 | #215 | spec |
| AC-216 | #216 | spec |

## Dependencies

- No dependencies between ACs.
- Conflict sets disjoint: AC-215 (docs + scripts) vs AC-216 (`crates/core`).

## AC-215 — Scrub worktree absolute paths from committed smevals evidence + enforce via check-docs.sh pin

# AC-215: Scrub worktree absolute paths from committed smevals evidence + enforce via check-docs.sh pin

## Executable Spec
- **predicate:** (1) `creating-lessons.md` Step 9 (commit block, ~349-355) gains a scrub instruction naming the `worktree-issue-N/` segment (and sibling non-portable forms: `worktree-*`, `blendtutor-*` checkout prefixes) directing builders to strip the `/Users/.../portfolio/<checkout>/` prefix down to a repo-root-relative path in `lesson`/`runner`/`checker` fields of committed `eval.json`/`run.yaml` before `git add docs/evals`; (2) `scripts/check-docs.sh` gains a pin section running `rg -l '/Users/' docs/evals/` and exiting non-zero on any match (builder commits a destructive-proof: pollute a scratch eval file → check fails → revert); (3) retroactive scrub applied: `rg -l '/Users/' docs/evals/` returns zero files, and every scrubbed `eval.json` parses via `json.load` and every `run.yaml` via `yaml.safe_load`; (4) already-repo-relative evidence paths preserved untouched — `../../../../scripts/smevals/run.sh` still grep-matches in `docs/evals/lesson_hello/evals/lesson_hello/eval.json` and `docs/evals/01_seed_data/evals/01_seed_data/eval.json`; (5) `whole-game.md:119-121` wording updated so it no longer claims the `lesson` field holds an absolute path (repo-relative from the recording checkout; still non-portable evidence-of-a-run).
- **probe:**
  ```bash
  bash scripts/check-docs.sh && rg -n "worktree-issue-N/" docs/book/src/creating-lessons.md && test -z "$(rg -l '/Users/' docs/evals/)" && grep -qF '../../../../scripts/smevals/run.sh' docs/evals/lesson_hello/evals/lesson_hello/eval.json && ! rg -n 'holds the absolute path' docs/book/src/whole-game.md && uv run python -c "import json,yaml,glob; [json.load(open(f)) for f in glob.glob('docs/evals/**/eval.json',recursive=True)]; [yaml.safe_load(open(f)) for f in glob.glob('docs/evals/**/run.yaml',recursive=True)]"
  ```
- **negative:** Step 9 lacks a scrub instruction, or scrub guidance lives only in `whole-game.md` read-side gotcha rather than the commit block; instruction names a literal-only form (e.g. just `worktree-issue-209`) not the `worktree-issue-N/` class; any `/Users/` survives under `docs/evals/`; enforcement pin missing from `check-docs.sh`, not wired to exit non-zero, or grep too narrow (only `worktree-issue-`, no `/Users/` catch-all); scrub sed corrupts JSON/YAML (parse failures); scrub nukes already-relative `../../../../` config/checker/runner paths; `whole-game.md` still asserts an absolute `lesson` path after the scrub landed (doc self-contradiction).
- **verification:** code · `bash` probe chain (grep pins + destructive-proof logged to `docs/evidence/215/destructive-proof.log` per #218 evidence-bundle convention)
- **fixture status:** existing — `docs/evals/lesson_hello/evals/lesson_hello/eval.json:1`, `.../runs/case-1/.../run.yaml:3,9`, `.../runs/case-2/.../run.yaml:3,9`, `docs/evals/01_seed_data/evals/01_seed_data/eval.json:1`, `.../runs/case-{1,2,3,4}/.../run.yaml:3,11,12` (8 files, become scrubbed); NEW text in `docs/book/src/creating-lessons.md` (~line 356, after Step 9 commit block); NEW pin section in `scripts/check-docs.sh`; `docs/book/src/whole-game.md:119-121` wording edit
- **rubric anchor:** *(none — docs-hygiene chore; neither proposer cited §N.M and no rubric signal is exercised)*

## Design Intent
- **Types / interfaces (§1):** N/A — no types; enforcement is a grep pin, not a type invariant.
- **Pure / effectful (§2):** Scrub is a pure text transform on committed data files; enforcement pin is a pure guard (read-only grep in check-docs.sh).
- **Boundary cuts (§3):** Convention lives at the commit boundary (Step 9, where evidence enters git), not at generation time — generation-time fix is #216's lane (crates/core), deliberately disjoint.
- **Module responsibility (§4):** `check-docs.sh` is the repo's docs-hygiene gate (C1-C10 pins); the evidence-path pin joins it rather than spawning an orphan script. `creating-lessons.md` Step 9 owns the commit-time convention prose.
- **Function discipline (§5):** One pin pattern (fail-on-`/Users/` + destructive-proof + revert), consistent with existing check-docs pin sections; scrub instruction is one focused addition to Step 9, not a scattered convention.

## Technical Context
- **Files likely touched:** `docs/book/src/creating-lessons.md` (Step 9, ~349-360); `scripts/check-docs.sh` (NEW pin section); `docs/book/src/whole-game.md:119-121` (wording); 8 files under `docs/evals/{lesson_hello,01_seed_data}/` (retroactive scrub); NEW `docs/evidence/215/` evidence bundle (`probe.log`, `destructive-proof.log`, `parse-check.log`) per #218 convention.
- **Architecture notes:** Enforcement baseline must be clean *before* the pin lands in one commit — retroactive scrub and pin wiring are coupled; splitting across PRs makes CI red on main. Scrub target form confirmed: strip `<abs-prefix>/worktree-issue-N/` (or sibling checkout prefix), leaving repo-root-relative (`target/hello-course/lesson_hello.yaml`, `scripts/smevals/run.sh`); existing depth-relative forms (`../../../../...`) untouched. No overlap with #216 (`crates/core` code fix). User-level convention reference (`~/.config/opencode/references/agent-self-validation-convention.md`) is outside this repo — cannot be committed here.

## Dependencies
- **Depends on:** none blocking (polluted artifacts + `check-docs.sh` infrastructure from merged #209/#214/#218)
- **Blocks:** none
- **Conflict set:** none with #216 (docs+scripts vs `crates/core`); scripts/check-docs.sh last touched by merged #212
- **Risk level:** medium (touches CI-adjacent gate + 8 committed data files; mechanical but pin wiring + scrub coupling must land in one commit)

## Pattern Detectors
- **out-of-repo convention location:** `~/.config/opencode/references/agent-self-validation-convention.md` and `builder.md:47` live outside the blendtutor repo — a blendtutor PR cannot commit there. If the convention should also live user-level, that is a **separate follow-up issue** (dotclaude/opencode config repo), not this AC. Flagged, not resolved: user decision needed on whether to file it.
- **docs/evidence/ pollution class:** ~40+ files under `docs/evidence/` contain worktree absolute paths — `probe-report.json` `worktree` field (intentional provenance per builder-vision-probe.md:175 schema; confirmed excluded from scrub) AND raw cargo/build logs where scrubbing destroys verbatim-fidelity. Explicitly **out of scope** for this AC ("committed *smevals* evidence" = `docs/evals/`); candidate follow-up issue to define a docs/evidence/ policy. Flagged, not resolved.

### Progress
- [ ] — pending impl

### Decision Log
- 2026-08-10 — user locked option (a) path-scrub convention (strip worktree-issue-N/ segment, apply going forward); retroactive scrub merged in (locked (a) fixes mechanism not baseline); enforcement scoped to docs/evals/ per AC noun phrase; 2 follow-ups flagged not resolved

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run probe chain after any edit; scrub is idempotent (no-op once clean)
- Rollback: revert commit; check-docs.sh pin is additive

## AC-216 — smevals_gen `scripts_rel_from` computes exact depth from `scripts/smevals` marker; refuse when no such ancestor exists

# AC-216: smevals_gen `scripts_rel_from` computes exact depth from `scripts/smevals` marker; refuse when no such ancestor exists

## Executable Spec
- **predicate:**
  1. **P1 (multi-depth, marker-subsume):** `scripts_rel_from(course)` returns `Some(rel)` s.t. for tempdir courses at depths D=1, D=2, D=3 below a repo root containing `scripts/smevals/{run.sh,check_polarity.sh,judge_feedback.py}` (`.git` present AND a no-`.git` variant of the same suite), `configs/default.yaml`'s `runner:` prefix **and** `graders/default.yaml`'s `checker:` prefix — both threaded from the same `scripts_rel` via `generate_eval_dir_with` (smevals_gen.rs:160-165) — canonicalize-resolve from `<course>/.smevals/configs/` to the real scripts dir. D=1 → 3 hops, D=2 → 4 hops, D=3 → 5 hops, all in one passing build (anti single-depth sneaky-pass).
  2. **P2 (refusal, no silent swallow):** course under a filesystem with **no** ancestor containing `scripts/smevals/` (with or without `.git`) → `write_eval_dir` returns `Err(GenError::NoRepoRoot { course_root })`, NOT `Ok` with `DEFAULT_SCRIPTS_REL`. `scripts_rel_from` returns `None` (not `DEFAULT_SCRIPTS_REL`) in this arm; `relative_path`-`None` (no common root) folds into the same `None`.
  3. **P3 (non-regression):** existing `.git`-ancestor tests pass unchanged under the marker walk-up; `generate_eval_dir` (pure, smevals_gen.rs:126) still emits exactly `DEFAULT_SCRIPTS_REL` — golden dir `golden_dir_is_byte_identical` (crates/core/tests/generate_eval_dir.rs:614) passes un-re-staged.
  4. **P4 (fix is exercised in pure layer):** in-module unit test calls `generate_eval_dir_with(..., "../../../scripts/smevals/")` (3 hops) and asserts the emitted `configs/default.yaml` AND `graders/default.yaml` carry the 3-hop prefix as **plain, unquoted** YAML scalars (no `: `, ` #`, or structural chars — same charset as current output, `emit_inline_scalar` behavior preserved).
- **probe:**
  ```
  rtk cargo test -p blendtutor-core --lib scripts_rel && rtk cargo test -p blendtutor-core --lib golden_non_default && rtk cargo test -p blendtutor-core --test generate_eval_dir
  ```
- **negative:**
  1. **Silent-fallback sneaky-pass:** any arm that can now produce `DEFAULT_SCRIPTS_REL` from inside `scripts_rel_from` (vs. the pure layer's deliberate default) fails this slice — depth-1 course receiving 4 hops (OLD behavior) must fail the canonicalize-equal assertion.
  2. **Dual-consumer drift:** runner prefix ≠ checker prefix after the fix fails P1 (they share one `scripts_rel` by construction — the test asserts over both files, not just `configs/`).
  3. **YAML discipline regression:** computed path must remain a plain YAML scalar — no escaping/quoting introduced by the fix.
  4. **No-`.git`-but-marker case must NOT error** (.git-less release tarballs / export-quarto'd courses work — refusal in this arm is a false-negative).
- **verification:** code · `cargo test -p blendtutor-core --lib` (unit, in `smevals_gen.rs` tests mod) + `cargo test -p blendtutor-core --test generate_eval_dir` (golden, unchanged)
- **fixture status:**
  - golden fixtures `crates/core/tests/fixtures/generate_eval_dir/` — EXISTING, committed; expect byte-identical; "re-baseline" = regenerate via fixed `generate_eval_dir` + `git diff --exit-code` confirm, then commit-as-unchanged (no content change expected)
  - `scripts_rel_reaches_repo_scripts_from_a_nested_course` — EXISTING crates/core/src/smevals_gen.rs:529 (keep, depth-2 case of P1)
  - `write_eval_dir_persists_the_generated_tree` — EXISTING :562 (strengthen depth-1 assertion from string-containment to canonicalize-equal)
  - `scripts_rel_defaults_when_no_repo_root_exists` — EXISTING :557 (REWRITE → `write_eval_dir_errors_when_no_scripts_ancestor`: asserts `Err(GenError::NoRepoRoot)` and `scripts_rel_from(...) == None`)
  - NEW in-module unit tests: `scripts_rel_resolves_at_depth_1_below_repo_root` (3 hops), `scripts_rel_resolves_at_depth_3_below_repo_root` (5 hops), `scripts_rel_resolves_without_git_when_marker_present` (no-`.git`, marker only), `golden_non_default_depth_emits_plain_3hop_prefix` (P4)
  - NEW enum variant `GenError::NoRepoRoot { course_root: PathBuf }` + `Display` arm (smevals_gen.rs:57-99 region)
- **rubric anchor:** §1.1 (wrong-depth path unrepresentable in the effectful write path — refusal replaces silent four-hop guess), §2.1 (pure `generate_eval_dir` keeps `DEFAULT_SCRIPTS_REL` precisely because it has no FS context; effectful shell must compute or refuse)

## Design Intent
- **Types / interfaces (§1):** error state encoded as `GenError::NoRepoRoot` — no sentinel string can flow into emitted YAML; `scripts_rel_from`'s `Option` makes "can't compute" explicit at the type, `?`-hooked into `GenError` at the one effectful call site.
- **Pure / effectful (§2):** pure generator untouched (`generate_eval_dir` → `generate_eval_dir_with(..., DEFAULT_SCRIPTS_REL)`, smevals_gen.rs:132); all FS probing confined to `scripts_rel_from`; the refusal happens in `write_eval_dir` before any `create_dir_all`/`fs::write` (no partial tree on error).
- **Boundary cuts (§3):** fix stays inside `smevals_gen.rs`; `eval_report.rs`'s own `repo_root_for` (:219, docs/evals placement) is a separate concern and is NOT merged into the gen root-finder.
- **Module responsibility (§4):** module header comment at :417-423 rewrites to say: root = nearest ancestor containing `scripts/smevals/`; refusal, not the four-hop default, when absent; default belongs to the pure layer only.
- **Function discipline (§5):** `scripts_rel_from` stays one-thing (walk + compute or `None`); error mapping lives in `write_eval_dir` (`.ok_or(...)?`); walk marker test is a single `.is_dir()` check per level (no new helper needed).

## Technical Context
- **Files likely touched:**
  - `crates/core/src/smevals_gen.rs`:
    - :424-446 — `scripts_rel_from` signature → `fn scripts_rel_from(course_root: &Path) -> Option<String>`; walk-up terminal `dir.join("scripts/smevals").is_dir()` replaces `dir.join(".git").exists()` (:429); delete both `DEFAULT_SCRIPTS_REL` return arms (:437-438, :444) → `None`; `relative_path` result mapped directly (its `None` propagates).
    - :399 — resolve first (`&scripts_rel_from(&dir).ok_or(GenError::NoRepoRoot { course_root: dir.clone() })?`), then generate, so no work is wasted; keep between canonicalize (:390) and generate call (:394).
    - :57-99 — add `NoRepoRoot { course_root: PathBuf }` variant, `Display` arm ("no repo root (an ancestor containing scripts/smevals/) found above {path}"), `source()` stays `None`-armed.
    - :472-onwards test mod — rewrites/additions per fixture status above.
  - `crates/core/tests/fixtures/generate_eval_dir/` — regenerate + confirm unchanged (no commit expected).
  - `crates/core/tests/generate_eval_dir.rs` — untouched (golden test passes as-is).
  - `crates/cli/tests/` — CHECK ONLY (done: no "no repo root"/`repo_root` message assertions outside eval_report.rs's own unit test :235 — no CLI test churn required). If the builder finds one later, it's files-of-scope.
- **Architecture notes:** marker-based root finding subsumes the `.git` case in this repo (repo root always has `scripts/smevals/`, confirmed — `scripts/smevals/run.sh` exists). Courses inside a `.git` project that is NOT blendtutor now refuse instead of guessing — behavior change, intentional (§1.1). The 4-hop default survives only as the pure layer's documented convention (:40-42 comment remains accurate). Caller count stays 2 (write_eval_dir:399, test) — signature change is local.

## Dependencies
- **Depends on:** none
- **Blocks:** none (evidence-hardening wave is otherwise docs-side)
- **Conflict set:** `crates/core/src/smevals_gen.rs` only; zero overlap with #215 (docs convention, no crates/core)
- **Risk level:** low — private/local function area, single call site changed, golden integrity asserted by existing test

### Progress
- [ ] — pending impl

### Decision Log
- 2026-08-10 — synthesized A (marker walk-up, subsumes .git) + B (refuse-no-fallback): marker root + GenError::NoRepoRoot refusal; DEFAULT_SCRIPTS_REL deleted from effectful path entirely; golden byte-identical (re-baseline = regenerate + diff-confirm); P4 non-4-hop as in-module unit test (generate_eval_dir_with is private, cannot be committed-fixture integration test); no CLI test churn

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run cargo test -p blendtutor-core --lib; pure generator untouched so golden deterministic
- Rollback: revert commit; signature change local to smevals_gen.rs

## Open Questions

1. **out-of-repo convention location:** user-level reference (`~/.config/opencode/references/agent-self-validation-convention.md`) — file follow-up issue in dotclaude/opencode config repo?
2. **docs/evidence/ pollution policy:** ~40+ files under `docs/evidence/` contain worktree absolute paths (probe-report.json `worktree` field intentional, raw logs unscrubbable) — file follow-up issue to define docs/evidence/ policy?