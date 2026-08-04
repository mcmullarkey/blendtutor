# AC-5 E2E Evidence — README live demo docs (issue #157)

Verification medium: `code` (shell content-check; no browser/network/deploy
dependency). This is a docs+test slice — the user-visible artifact is
README.md; the evidence is the content-pin suite passing against the shipped
prose plus a summary of the README diff.

## Artifacts

| File | What it proves |
|------|----------------|
| `test-suite.log` | `bash scripts/tests/test_demo_docs.sh` → 22 PASS, 0 FAIL, exit 0. All 11 clauses green against the edited README. |
| `distribution-readme-group.log` | `test_quarto_distribution.sh` Group 1 (README, clauses 1-13) → 27 PASS, 0 FAIL, exit 0. Hidden shared contract not regressed. |
| this file | README diff summary + clause mapping |

## README diff summary (region :288-341, extend-in-place)

- **COI blockquote (:303-309):** extended in place — added live-demo capability
  pointer ("Live demos of both project types — and what runs in each — are
  linked in the Demo book section below") + ADR-0015 link
  (`docs/adr/0015-opt-in-coi-cross-origin.md`). Headings/phrases untouched:
  `Book-mode limitation` count == 1, `COI does not function in Quarto` count == 1.
- **Demo book section:** intro now points at the live deploy
  (`https://mcmullarkey.github.io/blendtutor/demo-book/`, trailing slash) while
  keeping the relative `demo-book/` link. Added explicit capability mapping:
  Python exercises fully interactive (Pyodide, no COI), every page ships a
  **static fallback**, and R exercises **do NOT run** in book mode (editors
  mount, execution unavailable). "interactive exercises" generalized to
  "exercises" — R is not interactive in book mode, the old phrasing overclaimed.
- **New `#### Standalone demo` subsection** (before "Viewing the demo book"):
  `/demo/` live URL (trailing slash), COI active, R exercises run
  interactively via webR, Python exercises run interactively via Pyodide.
- **Viewing note:** reworded to name the literal `static fallback` while
  keeping the `static exercise content` phrase pinned by
  test_quarto_distribution.sh clause 13.
- **Third limitation mention:** kept consistent ("editors mount but execution
  unavailable") + cross-ref to the Standalone demo above.
- Not touched: :138-152 old `/examples/` Rust-binary sites (c7 guards against
  conflation); `## License` boundary preserved; no new top-level parallel
  section (both URLs land at lines 315 and 338, within the 288-341 region pin).

## Negative-case evidence (from red run)

`test_demo_docs.sh` on the pre-edit README failed 10 checks — exactly the
clauses this AC adds (c1 URLs, c2 capability, c3 standalone mapping, c9 region,
c10 ADR pointer), confirming the pins are load-bearing rather than vacuous.

## Merge-order gate (process, not test)

`/demo-book/` + `/demo/` resolve only after the AC-2 Pages deploy is live.
Probe is source-grep only (no curl) by design; the Director blocks merge until
the AC-2 verify-live pass.
