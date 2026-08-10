# Issue #212 — Whole-game discoverability pins (AC-4)

Wire discoverability: README pointer + check-docs.sh pins. LOCAL-ONLY per user
decision (2026-08-10) — CI docs.yml NOT touched.

## Changes
- `README.md` :60-61 — pointer to deployed whole-game chapter
  (`https://mcmullarkey.github.io/blendtutor/whole-game.html`) after the
  authoring-workflow loop line. Append-only; readme.rs pinned string at :58
  untouched.
- `scripts/check-docs.sh` :238-250 — AC-4 pin block after the examples.html
  pins (:236), before the footer. Unguarded (no `if`, no `|| true` — unlike the
  optional evals-assemble block at :95-101), uncommented, grepping `$book_out`
  BUILT HTML (not `docs/book/src` source). Existing pin idiom
  `grep -q ... || { echo ...; exit 1; }` preserved.

## Probe results (all PASS)
| C | Predicate | Result |
|---|-----------|--------|
| C1 | SUMMARY.md source pin contains whole-game | PASS |
| C2 | built `$book_out/whole-game.html` exists + renders `evals/lesson_hello` | PASS |
| C3 | built `$book_out/creating-lessons.html` renders `export-quarto` | PASS |
| C4 | README contains `whole-game.html` (deployed-URL convention) | PASS |
| C5 | pins unguarded (no `if`, no `\|\| true`) | PASS |
| C6 | pin lines uncommented (not `^#`) | PASS |
| C7 | pins after `mdbook build` (:31), after :236, before footer | PASS |
| C8 | `bash scripts/check-docs.sh` exits 0 | PASS (exit 0) |
| C9 | `cargo test -p blendtutor-cli --test readme` exits 0 | PASS (1 passed) |
| C10 | MIRROR_OK `-eq 11` unchanged (test_docs_pages_artifact.sh:304) | PASS |

## Destructive proof (run once, reverted)
Removed `- [The whole game](./whole-game.md)` from `docs/book/src/SUMMARY.md` →
`bash scripts/check-docs.sh` exited 1 with
`docs: SUMMARY.md missing whole-game chapter entry` (proves pins unguarded).
Entry restored; `git diff` clean on SUMMARY.md.

## Evidence files
- `probe.log` — structural pins C1-C6 output
- `check-docs-e2e.log` — full `scripts/check-docs.sh` run, exit 0
- `destructive-proof.log` — SUMMARY-entry removal run, exit 1
- `readme-rs.log` — `cargo test -p blendtutor-cli --test readme` output
- `mirror-count.log` — C10 grep of `MIRROR_OK -eq 11`
