---
ac: 7
depends_on: AC-1, AC-2, AC-5, AC-6
risk: low
status: spec
---

## Final Spec (resolver-merged)

## AC-7
Document the turn-key eval-report flow: creating-lessons Step 9 + README + agent-notes + readme.rs pin. Run scripts/check-docs.sh as structural probe.

### Executable Spec
- **predicate:** ALL of —
  1. `grep -c '^## Step 9 — Generate the eval report' docs/book/src/creating-lessons.md` == 1
  2. `grep -c '^## Step 10 — Build a browser site' docs/book/src/creating-lessons.md` == 1 (old Step 9 at line 325 renumbered)
  3. `grep -c '^## Step 9' docs/book/src/creating-lessons.md` == 1 (no duplicate Step 9)
  4. New Step 9 section sits between old Step 8 (line 307 "Score the grading prompt") and Step 10
  5. `sed -n '/^## Step 9 — Generate/,/^## Step 10/p' docs/book/src/creating-lessons.md | grep -c 'eval-report\.json'` == 0 — **anti-conflation**: new step must NOT reference the OLD Slice-13 eval-report.json site-build artifact (6+ live refs: creating-lessons.md:344/362/365, README.md:121, site-build.md:98/110, build.rs:14 — copy-paste trap)
  6. Step 9 content greps (one grep each on the Step 9 section): `blendtutor eval-report` present; cost warning present (grep -i 'cost\|paid\|spend'); `FIREWORKS_API_KEY` present; local-only note present; grade-fail-is-evidence (exit 0 on completed-but-low-quality) present; `.smevals/` vs `docs/evals/<lesson>/` distinction present (both tokens); `git add docs/evals` scoping present; `uv` prerequisite present; `smevals` named
  7. `grep 'eval-report' README.md` >= 1 AND README authoring-workflow string (line 56) includes eval-report between eval and build (grep 'eval.*eval-report.*build' README.md)
  8. README "What is blendtutor?" bullet (lines 19-21) gains one-line pointer to docs/evals/<lesson>/ eval evidence
  9. docs/agent-notes/eval.md gains dated entries for AC-1/2/3/5 decisions tagged `(#AC-N)`, frontmatter gains `acs: [1, 2, 3, 5]`
  10. crates/cli/tests/readme.rs required array gains `("eval-report command", "blendtutor eval-report")` and the file docstring's workflow mention updated
  11. `cargo test -p blendtutor --test readme` exits 0
- **probe:**
  ```bash
  bash scripts/check-docs.sh && \
  grep -q '^## Step 9 — Generate the eval report' docs/book/src/creating-lessons.md && \
  grep -q '^## Step 10 — Build a browser site' docs/book/src/creating-lessons.md && \
  test "$(grep -c '^## Step 9' docs/book/src/creating-lessons.md)" -eq 1 && \
  test "$(sed -n '/^## Step 9 — Generate/,/^## Step 10/p' docs/book/src/creating-lessons.md | grep -c 'eval-report\.json')" -eq 0 && \
  grep -q 'blendtutor eval-report' README.md && \
  grep -qE 'eval.*eval-report.*build' README.md && \
  grep -q 'AC-1' docs/agent-notes/eval.md && grep -q '^acs:' docs/agent-notes/eval.md && \
  cargo test -p blendtutor --test readme
  ```
  NOTE: check-docs.sh is necessary but NOT sufficient (builds mdBook, rustdoc -D warnings, example sites, README example links, SUMMARY.md — checks NO prose/step/README-pointer content). The content greps are the real probe. check-docs.sh is a local mirror, NOT run in CI.
- **negative:** (A) Renumbering miss — Step 9 inserted but old "Step 9 — Build" not renumbered → duplicate Step 9 (#3); or Step 9 missing while Step 10 exists (#1). (B) Naming conflation — Step 9 references eval-report.json (OLD artifact), misleading authors; check-docs.sh + heading greps pass → #5 catches. (C) Missing cost warning / git-scoping / .smevals-vs-committed distinction → author incurs paid spend unknowingly or commits megabytes of ephemeral <course>/.smevals/ LLM output → #6 greps. (D) README bullet added but workflow string (line 56) untouched → author's mental model incomplete → #7.
- **verification:** code · bash (check-docs.sh + content greps + readme.rs test). Prose quality/tone is a manual skim — not gated, not load-bearing.
- **fixture status:** docs/book/src/creating-lessons.md:307-325 · README.md:19-21,56 · docs/agent-notes/eval.md:4,92 · crates/cli/tests/readme.rs:32-41 · scripts/check-docs.sh (exists)
- **rubric anchor:** — (no §N.M signal exercised by a docs AC)

### Design Intent
- **Types / interfaces (§1):** No new types. readme.rs pin encodes "README documents the full authoring loop" as a build-failing invariant — the doc contract made unrepresentable-to-break.
- **Pure / effectful (§2):** Content greps are pure text predicates; check-docs.sh + cargo test are the thin effectful shell. No runtime code touched.
- **Boundary cuts (§3):** Three audiences, three surfaces — book (creating-lessons.md: author how-to), README (repo front door: what/why + loop), agent-notes/eval.md (internal decision log). Each gets only what its audience needs.
- **Module responsibility (§4):** eval.md header stays "the eval-case model..." — entries appended per dated-entry convention, tagged (#AC-N) to keep this feature's unit distinct from integer slices. readme.rs docstring updated to name eval-report in the workflow it pins.
- **Function discipline (§5):** readme.rs change is one tuple in an existing array + docstring line — no new test fn.

### Decisions on Flagged Divergences
- **Test migration:** readme.rs pin IN scope (1 file). Resolver confirmed B right, A's "0 test migration" wrong. readme.rs substring gotcha: needle `"blendtutor eval"` is substring of `"blendtutor eval-report"` — existing eval pin cannot detect a missing eval-report pin; explicit tuple required.
- **agent-notes tag convention (was needs-clarification):** resolved — `(#AC-N)` tags + new `acs: [1, 2, 3, 5]` frontmatter field. Rationale: feature's natural unit is the AC (no slice numbers exist; issue numbers unknown at spec time); dated-entry convention preserved; acs: frontmatter additive, cannot collide with existing slices: [12, 13].
- **README scope:** bullet (lines 19-21) + workflow string update (line 56, eval → eval-report → build) — confirmed.
- **Anti-conflation scope:** new Step 9 must not reference eval-report.json (grep #5). Clarifying OLD refs (344/362/365, README:121) is OUT of scope — they correctly describe the real site-build artifact; optional follow-up, not a mandate.
- **Content depth:** full adversarial content greps retained (cost warning, FIREWORKS_API_KEY, local-only, grade-fail-is-evidence, .smevals vs docs/evals, git add docs/evals, uv prerequisite, smevals named).
- **Step mislabel correction:** AC text says "after existing Step 8"; existing Step 8 = "Score the grading prompt" (line 307, the blendtutor eval step). Step 7 = "Write an eval suite" (line 266). Insertion point = after line 323 regardless of label.

### Technical Context
- **Files likely touched:** docs/book/src/creating-lessons.md (insert after line 323, renumber 325), README.md (lines 19-21 bullet, line 56 workflow string), docs/agent-notes/eval.md (frontmatter line 4, entries after line 92), crates/cli/tests/readme.rs (lines 1-11 docstring, 32-41 array)
- **Architecture notes:**
  - Step numbering (verified): Step 7 = Write an eval suite (266), Step 8 = Score the grading prompt (307), Step 9 = Build a browser site (325) → Step 10. New Step 9 = Generate the eval report.
  - Naming conflation surface: creating-lessons.md:344/362/365 + README.md:121 + site-build.md:98/110 + build.rs:14 (EVAL_REPORT_FILE) + eval-course.sh:4/149/155. NEW blendtutor eval-report → smevals HTML at docs/evals/<lesson>/ — different artifact, path, format.
  - check-docs.sh: builds mdBook (line 30), rustdoc -D warnings (24), example sites (60-63), README examples/r/ + examples/python/ links (202-205), SUMMARY.md (208). NOT in CI — local mirror; test_docs_pages_artifact.sh Phase 2 runs it when quarto/mdbook/cargo present.
  - Renumbering risk LOW — verify with one `rg 'Step 9' docs/` before edit.
  - Pages URL publish lag: URL 404s on branch until merge to main; Step 9 should warn.
- **Expected test migration:** 1 file (crates/cli/tests/readme.rs).

### Dependencies
- **Depends on:** AC-1 (eval-report flag surface), AC-2 (.smevals/ generated dir), AC-5 (command semantics: exit 0, cost, first real report committed), AC-6 (docs/evals/ → /evals/ URL)
- **Blocks:** none (terminal docs AC)
- **Conflict set:** README.md (shared with AC-5's first-report commit — serialize after AC-5), creating-lessons.md, eval.md, readme.rs
- **Risk level:** low

Resolver note: disagreement=minor — divergences resolved: 1-file test migration (B right), README bullet+workflow, anti-conflation new-step-only, tag conv (#AC-N)+acs frontmatter; step mislabel corrected, no user input needed.

### Progress
- [x] Speculators A+B returned (2026-08-07)
- [x] Resolver merged (2026-08-07)
- [ ] Implementation

### Decision Log
- 2026-08-07 — readme.rs pin in scope (1-file migration); substring gotcha: "blendtutor eval" ⊂ "blendtutor eval-report"
- 2026-08-07 — agent-notes tags (#AC-N) + acs: [1,2,3,5] frontmatter
- 2026-08-07 — README bullet + workflow string both updated
- 2026-08-07 — Anti-conflation: new Step 9 no eval-report.json ref; OLD refs out of scope
- 2026-08-07 — Step mislabel corrected: Step 8 = "Score the grading prompt" (not "write eval suite")

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run bash scripts/check-docs.sh + grep probes + cargo test --test readme
- Rollback: remove Step 9 section, restore Step 9 Build heading, revert README/eval.md/readme.rs changes