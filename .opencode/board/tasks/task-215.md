---
id: 215
type: task
status: In Progress
order: 1
project: evidence-hardening
wave: 1
issue: 215
pr:
status_since: 2026-08-10
depends_on: none
owner: user
---
# [chore] Scrub worktree-specific absolute paths in committed smevals evidence

Decide: (a) path-scrub step in the evidence-commit convention (strip worktree-issue-N/ segment), OR (b) document worktree provenance as accepted convention, OR (c) regenerate evidence from the main checkout. Apply the chosen convention going forward. Full spec: issue #215.

Decision: user locked (a) path-scrub convention on 2026-08-10. Spec resolved: retroactive scrub of 8 polluted docs/evals/ files + Step 9 convention text + check-docs.sh /Users/ pin. 2 follow-ups flagged (user-level convention ref, docs/evidence/ policy). See .opencode/plans/evidence-hardening/AC-215.md.