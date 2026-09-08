---
ac: 4
depends_on: none
risk: low
status: complete
---

### AC-4 — book slim
- Spec mode: spec-resolved
- Key files: `docs/book/src/creating-lessons.md` (458 → ≤150 lines), `docs/book/src/introduction.md`, `docs/book/src/examples.md`, `docs/book/src/SUMMARY.md`, `docs/book/src/api-reference.md`
- Invariants/constraints:
  - Do NOT touch `whole-game.md` (the bar), `docs/adr/*` (historical records), `docs/agent-notes/*` (agent-facing, not user docs), `docs/evidence/*` (historical).
  - Move detail OUT, don't delete info that has no other home: architecture/API depth belongs to rustdoc (`/api`) — link, don't inline.
  - `scripts/check-docs.sh` + `test_docs_pages_artifact.sh` build the mdBook — any SUMMARY.md change must keep `mdbook build docs/book` green.
- Prior art: `whole-game.md`.
- Verification: manual + code (mdbook build green in CI).
- Test seam: existing docs build (`scripts/check-docs.sh`).

### Progress
- [x] 2026-09-08T21:45Z — creating-lessons.md 469→146 lines (≤150 AC met; 144 at slim commit, +2 review-cycle-1: restored `find -exec` why-comment, concrete stdin example; +0 review-cycle-2: restored eval-file-by-hand guidance, Step 9 failure condition — in-line edits); introduction.md trimmed 24→22; examples/api-reference/SUMMARY audited, no changes needed; whole-game/adr/agent-notes untouched. mdbook build + check-docs.sh + test_docs_pages_artifact.sh (24/24) green. Commit b997a55, PR #232.

### Decision Log
- Kept the 11-step structure verbatim so check-docs.sh's "scrub per creating-lessons.md Step 9" error-message pointer (check-docs.sh:104-110, docs.yml:131) stays accurate.
- Lesson YAML schema compressed to an abridged example with inline field comments (replaces the "Key fields" bullet list); full YAML linked to examples/write-less-code-{r,python}/.
- Field-level semantics linked to rustdoc (Lesson, Exercise, Manifest, ManifestEntry, EvalSuite) per "link, don't inline".
- Prose written as single long lines (not 72-col wrapped like whole-game.md) — the ≤150 hard AC wins over wrap style; 10 lines >200 chars.
- Kept the Step 9 worktree-path scrub recipe verbatim: it has no other home (whole-game.md only notes the repo-relative lesson field) and check-docs.sh's /Users/ leak error points here.

### Surprises & Discoveries
- check-docs.sh:261 greps the BUILT creating-lessons.html for 'export-quarto' — any rewrite must keep that string; verified present (3×) in built HTML.
- examples.md (28 lines) and api-reference.md (12 lines) were already tight — audit found no fluff; SUMMARY.md needed no chapter changes, so it was left untouched (no mdbook SUMMARY risk).
- test_docs_pages_artifact.sh greps only workflows/scripts, not book content — book rewrites can't break it (confirmed 24/24 pass).

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open
