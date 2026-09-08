---
ac: 3
depends_on: AC-2
risk: low
status: spec
---

### AC-3 — README slim
- Spec mode: spec-resolved
- Key files: `README.md`
- Invariants/constraints:
  - Quality bar = `docs/book/src/whole-game.md` tone: one idea per section, commands first, prose subordinate.
  - Hard pins (must survive): `quarto add mcmullarkey/blendtutor` install command, BYOK mention, min-Quarto requirement, demo link — `scripts/tests/test_quarto_distribution.sh` asserts a 15-clause README predicate; grep its exact clauses before rewriting and keep every pinned string.
  - Ceiling: ≤ ~130 lines (from 373). Install section leads with the `curl | sh` one-liner; `cargo install --path crates/cli` becomes the fallback.
- Prior art: whole-game.md structure.
- Verification: manual (succinctness judgment) + code (`test_quarto_distribution.sh` pins stay green).
- Test seam: existing `test_quarto_distribution.sh` (grep it verbatim first — it is the structural contract).

### Progress
- (none yet)

### Decision Log
- (none yet)

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open
