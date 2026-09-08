# Issue #227 — demo-standalone deletion evidence

Captured 2026-09-08 on branch `227-demo-standalone-removal` (worktree `../worktree-issue-227`).

## Files

| File | What it proves |
|------|----------------|
| `grep-proof.txt` | Zero demo-standalone / fix-demo-coi / pages-live / verify-live / test_demo_standalone_render references remain in live wiring (`.github/`, `scripts/`, `rodney-probes/`, `crates/`, `demo-book/`, `examples/`, `docs/book/`, `.gitignore`, `README.md`) outside the pin test itself. Remaining repo-wide hits are enumerated and categorized (historical evidence, plan records, agent-note lesson, absence pins). |
| `test-suite.log` | `scripts/tests/test_docs_pages_artifact.sh` — 35 passed, 0 failed. Phase 1/1b structural + deletion pins (incl. the #227 absence pins that were RED before the deletion, commit `b14651c`); Phase 2 = full `check-docs.sh` end-to-end; Phase 3 evals fixture sub-phase. |
| `check-docs-e2e.log` | `scripts/check-docs.sh` exit 0 — full local build (rustdoc -D warnings + mdBook + webr/pyodide example sites + demo-book quarto render + assemble + layout asserts + mirror contract against the edited docs.yml). Final line confirms the updated layout message (demo book at /demo-book/, no /demo/). |
| `quarto-distribution.log` | `scripts/tests/test_quarto_distribution.sh` — 91 passed, 0 failed. README pins intact (README intentionally untouched; demo-link swap deferred to #225). |

## Consumer analysis (pages-live suite fate)

Sole consumer chain of `rodney-probes/pages-live*`: docs.yml `verify-live` job →
`scripts/tests/test_verify_live_wiring.sh` (pins the job). No other consumer:
ci.yml's `rodney-probes` job runs `key-page-probe.js` + `feedback-probe.js` only.
Per the issue's decision tree → suite deleted, not repointed (verify-live removed
outright per user decision). `scripts/rodney-chrome.sh` KEPT — still consumed by
key-page/feedback probes + ci.yml.

## Mirror contract

`check-docs.sh` and `docs.yml` edited in lockstep; pinned by the 7-needle mirror
grep in `test_docs_pages_artifact.sh` Phase 1 (was 12 needles pre-#227).
