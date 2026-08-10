---
id: 216
type: task
status: To Do
order: 2
project: evidence-hardening
wave: 1
issue: 216
pr:
status_since: 2026-08-10
depends_on: none
owner: user
---
# [bug] smevals generator scripts_rel_from fallback emits wrong-depth runner path

Fix the fallback depth in crates/core/src/smevals_gen.rs so the emitted runner/checker path resolves from the evals/<lesson>/eval.json nesting depth. Verify: generated eval.json runner path resolves to the real scripts/smevals/run.sh. Re-baseline golden dir + generate_eval_dir tests. Full spec: issue #216.