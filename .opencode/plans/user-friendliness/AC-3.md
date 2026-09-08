---
ac: 3
depends_on: AC-2
risk: low
status: complete
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
- [x] 2026-09-08 — Pins updated RED-first in test_demo_docs.sh (c4b5650): dead /demo/ URL flipped to negative pin, c3 relabeled to example-sites surface, c9 region re-pinned 288-342 → 105-135, new c12 line ceiling ≤150. RED confirmed: 3 failures for expected reasons against the 383-line README.
- [x] 2026-09-08 — README rewritten 373 → 149 lines (ddd7f18). All suites green: test_demo_docs.sh 22/22, test_quarto_distribution.sh 91/91, cargo --test readme 1/1, check-docs.sh green. Evidence at docs/evidence/225/.

### Decision Log
- Instructor loop collapsed to ONE commented command block (7 commands, semantic comments) instead of 7 heading+prose subsections — whole-game commands-first bar; readme.rs's 9 token pins all satisfied by the block.
- Quick-start shows ONE merged copy-paste document (YAML header + div) instead of two separate blocks; the `{.r .checks}` grading sub-block moved to a prose mention to hit the ceiling.
- c12 line-ceiling pin set at ≤150 (not ~130 literal): headroom for legitimate one-line additions without tripping CI, while still failing monolith regression. README landed at 149.
- Deferred #227 demo-link swap: runnable R now points at the CLI-built example sites (anchor link, no full URL — c7's anti-conflation pin kept intact); c3 regex unchanged, only relabeled.
- BYOK pinned model ID kept despite concision pass — grep confirmed no other home in docs/book or demo-book (no-info-loss invariant).

### Surprises & Discoveries
- grep-based README pins are line-based: any pinned phrase that wraps across a line break silently fails (c2 "static fallback", c3 "R exercises run interactively via webR", c4 "COI does not take effect" all broke on reflow). Resolution: composed the demo paragraph so every pinned phrase sits intact on one line; future README edits must respect this. Worth a note in any future README-pin authoring.
- test_quarto_distribution.sh clause 7's awk range skips the `#### Quick start` heading line itself (`{flag=1;next}`), so "hand-written bootstrap" in the heading does not count — the phrase must appear in the section BODY. Cost one green-chase cycle.
- Fresh worktree target/ makes cargo test + check-docs.sh exceed the 120s default tool timeout (memory anchor confirmed: 10GB root target/ slows builds ~45x); reran with 600s timeout — green both times.

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open
