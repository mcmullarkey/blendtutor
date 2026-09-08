---
ac: 4
depends_on: none
risk: low
status: spec
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
- (none yet)

### Decision Log
- (none yet)

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open
