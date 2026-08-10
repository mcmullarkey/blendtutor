# Decomposition: whole-game-tutorial

## Feature Goal

Give new users one continuous end-to-end walkthrough of the blendtutor authoring lifecycle, in the spirit of the R4DS "Whole Game" chapter but package-scale and ruthlessly succinct: scaffold a course → add/edit a lesson → validate → run → eval → generate + read the eval report → deploy as (a) a chapter in a Quarto book via the `blendtutor` Quarto extension and (b) a standalone static site on GitHub Pages. Exact copy-paste commands, minimal prose, no emojis. It lands as a new mdBook chapter (`docs/book/src/whole-game.md` + SUMMARY entry) shipped with the published docs site, and as a side effect documents the currently-undocumented `export-quarto` command in `creating-lessons.md` so it is discoverable outside the walkthrough.

## Verified Codebase Facts (checked against source, 2026-08-10)

- `crates/cli/src/main.rs`: `Commands` enum already exposes `Init/New/Validate/List/Run/Eval(--case)/EvalReport/Build(--target webr|pyodide)/ExportQuarto` — every command the chapter needs exists. No code changes required anywhere.
- `crates/cli/src/commands/export_quarto.rs`: thin shell over `blendtutor_core::quarto_export::export_lesson_to_qmd`; prints the fenced-div `.qmd` snippet to stdout, exit 0 on success. **Undocumented** — absent from `creating-lessons.md`, README, and `crates/cli/tests/readme.rs` required-tokens array.
- Scaffold templates (`crates/core/src/scaffold.rs`): `blendtutor init` writes `blendtutor.toml` (one `[[lessons]]` entry: `id = "hello"`, `path = "lesson_hello.yaml"`), `lesson_hello.yaml` (R, `cat("hello\n")` exercise), sibling `eval_lesson_hello.yaml` (2 cases: 1 correct, 1 incorrect), README.md, `.gitignore`. So a full init→eval-report walkthrough needs zero hand-authored files.
- `docs/book/src/creating-lessons.md`: 10 steps (Step 9 = `eval-report`, Step 10 = `build`). A new "embed in a Quarto book" step slots at the end (Step 11) without renumbering — export-quarto is an alternative *output*, not a pipeline step between eval and build.
- `docs/book/src/SUMMARY.md`: 4 chapters. New chapter entry goes here; mdBook derives whole-game.html from the source name.
- `docs/evals/` currently holds only `01_seed_data/` — the committed-report evidence convention from smevals-eval-report AC-5.
- `scripts/check-docs.sh`: structural probe — mdBook build, rustdoc `-D warnings`, docs.yml needle greps, `SUMMARY.md missing examples page` grep, rendered-HTML link pins (`examples/r/` in built `examples.html`). Established pattern for a pin: grep `SUMMARY.md` for the source name AND grep the built `[chapter].html` for rendered links. Expected runtime ~2-3 min (quarto + mdbook + cargo).
- `crates/cli/tests/readme.rs`: README pins are substring needles over a lowercased haystack (`blendtutor init/new/validate/run/eval/eval-report/build`, `github pages`, COOP/COEP). **Substring gotcha already documented**: `"blendtutor eval"` is a prefix of `"blendtutor eval-report"`. All pins are inclusion checks — append-only README edits cannot break them; an edit that *removes* a pinned phrase would.
- Pages URL shape (docs.yml assemble pattern, unchanged): `/evals/<lesson-file-stem>/` — a report for `lesson_hello.yaml` publishes at `/evals/lesson_hello/`.
- README.md lines ~156-264 already document the Quarto extension install (`quarto add mcmullarkey/blendtutor`, `filters:`, fenced divs); the chapter cross-links it rather than duplicating.

## AC Table

| AC | Description | Dependencies | Conflict Set | Risk | Medium |
|----|-------------|--------------|--------------|------|--------|
| 1 | Generate one real `blendtutor eval-report` for the scaffold starter (`lesson_hello.yaml`) and commit the report to `docs/evals/lesson_hello/` as the chapter's cited evidence | none | `docs/evals/lesson_hello/` (new committed tree only — no code changes) | low | code (bash: init temp course, run eval-report, commit output) + manual (real-key run) |
| 2 | Write the whole-game chapter (`docs/book/src/whole-game.md` + SUMMARY.md entry): one continuous init→new→validate→run→eval→eval-report→build→deploy walkthrough using the scaffold starter, a "reading the eval report" section (scores, polarity match, judge quality metrics, 0.8 threshold, grade-fail-is-evidence), both deploy paths, copy-paste commands, no emojis/slop | AC-1 (chapter cites the committed report + its Pages URL) | `docs/book/src/whole-game.md` (new), `docs/book/src/SUMMARY.md` | med | code (check-docs.sh: mdBook renders whole-game.html) + manual (tone skim per user constraint) |
| 3 | Document `blendtutor export-quarto` where users look for commands: new step at the end of `creating-lessons.md` (after Step 10) covering the export → fence-div → extension → quarto render flow, cross-linked from/to the whole-game chapter's Quarto deploy section | none (AC-2 cross-link target exists as long as both land before AC-4's pins are added — schedule serializes) | `docs/book/src/creating-lessons.md` | low | code (check-docs.sh) + manual (tone skim) |
| 4 | Wire discoverability: README whole-game pointer (append-only, one line+link near the authoring-workflow section), check-docs.sh pins (SUMMARY contains whole-game; built whole-game.html renders a rendered `/evals/lesson_hello/` link; built creating-lessons.html contains `export-quarto`) | AC-1, AC-2, AC-3 | `README.md`, `scripts/check-docs.sh` | low | code |

## Dependency DAG

```
AC-1 ──────────────┐
                   ├──→ AC-2 ──→ AC-4
AC-3 ──────────────┘          ↗
```

- AC-1 has no code predecessor — `eval-report` already exists (smevals-eval-report AC-5 landed); it only produces the evidence artifact.
- AC-2 depends on AC-1 for the committed report path and live URL it cites.
- AC-3 is independent of AC-1/AC-2 in *content* (the command already exists); it is serialized after AC-2 anyway because AC-4's link pins reference both AC-2's and AC-3's output, and the cross-links between the two chapters read better once both documents exist. (True file-level conflict: none.)
- AC-4 pins everything, so it lands last.

## Hot Conflict Files

- **None in the classic sense.** `SUMMARY.md` is AC-2 only. `creating-lessons.md` is AC-3 only. `README.md` is AC-4 only. `check-docs.sh` is AC-4 only. `docs/evals/` is AC-1 only.
- Watch-item (not a conflict): `README.md` has the readme.rs substring-token test — AC-4 must be append-only; never rewrite the pinned authoring-workflow lines. Note the existing gotcha: `blendtutor eval` is a prefix of `blendtutor eval-report`, so adding export-quarto text cannot accidentally satisfy or break the eval pin — but reformatting the existing lines could.

## Suggested Batch Schedule

- **Batch 1 (parallel): AC-1, AC-3** — disjoint files, zero overlap. AC-1 needs a real `FIREWORKS_API_KEY` (paid judge calls, ~2 cases — trivial cost) and produces non-deterministic judge scores; that is expected and is precisely the evidence convention.
- **Batch 2 (sequential): AC-2** — depends on AC-1's committed evidence/URL. Also the longest-prose AC, so giving it its own batch keeps review focused.
- **Batch 3 (sequential): AC-4** — pins AC-2/AC-3/AC-1 artifacts; must not be a structural test for things that don't exist yet.

## Cross-AC Contracts

- **Evidence path / URL (AC-1 → AC-2, AC-4):** committed report at `docs/evals/lesson_hello/`; published at `https://mcmullarkey.github.io/blendtutor/evals/lesson_hello/`. Lesson-file stem is the URL segment (`lesson_hello.yaml` → `lesson_hello`), same rule as the existing `01_seed_data/` report. AC-2 prose and AC-4's rendered-link pin both hard-code this path — if AC-1's run produces a different stem, both downstream pins break loudly (that is the intent).
- **Command spellings (all ACs):** `blendtutor init <dir>`, `blendtutor new lesson --lang r <id>`, `blendtutor validate <lesson>`, `blendtutor run <lesson>`, `blendtutor eval <lesson>`, `blendtutor eval-report <lesson>`, `blendtutor build <course> --target webr|pyodide -o <dir>`, `blendtutor export-quarto <lesson>`. Verified against main.rs — no invented flags.
- **Chapter placement (decision, spec phase confirms):** SUMMARY order Introduction → **The whole game** → Creating Lessons → Architecture → Example sites → API reference (R4DS-style: whole game early, before the field guide). URL: `/whole-game.html`.
- **Quarto deploy contract in docs (AC-2, AC-3):** extension install = `quarto add mcmullarkey/blendtutor`; `_quarto.yml` gains `filters: [mcmullarkey/blendtutor]`; fenced div `::: {.blendtutor language="r"}` (README.md:156-264 is the existing precedent both docs cross-link rather than restate).
- **Deploy targets in docs (AC-2):** (a) Quarto book → `quarto render` + Pages; (b) standalone → `blendtutor build` + static Pages with COI serviceworker caveat for webR (one short sentence, link to README's existing deployment section).
- **Style invariant (AC-2, AC-3):** zero emojis; every section anchored by a fenced bash block of literal commands; prose only where a command alone would be ambiguous. Tone is a manual skim (per AC-7 docs precedent) but the no-emoji property is mechanically checkable (grep) if speculators want it.

## Design-Intent Notes (per AC)

### AC-1 — committed lesson_hello eval report
- Running example is the scaffold starter, not an example course: `blendtutor init /tmp/wg-course` → `blendtutor eval-report /tmp/wg-course/lesson_hello.yaml`. Note binding decision #2 names `lesson_hello.yaml` at course ROOT, matching scaffold.rs (`LESSON_FILENAME` written beside the manifest, not under `lessons/`) — the memory entry "[lesson files live at course ROOT]" from AC-5 concerns `examples/`, not scaffold; here root placement is correct by design.
- `eval-report` walks to the repo-root `.git` (course_root_for canonicalization, AC-5 lesson) and writes into the *repo's* `docs/evals/` regardless of where the invoking cwd is — run from repo root and pass an absolute lesson path to keep it boring.
- Requires a real `FIREWORKS_API_KEY`, one human invocation, commit the output. Non-deterministic judge scores arefine (grade-fail-is-evidence). If the report is pathologically bad that IS still evidence, but if judging the commit-on-merit question arises, defer to the writer of AC-1's PR — do not gate merge on judge score.
- No Rust changes. `.gitignore` unchanged (docs/evals/ is the committed boundary).

### AC-2 — whole-game chapter
- Structure sketch (spec phase owns final outline, this is the intended spine): **Scaffold** (init, tree of produced files) → **Add/author a lesson** (one `new` + pointer to creating-lessons.md for field reference — do NOT repeat the full lesson-field walkthrough) → **Validate** → **Run** (one stdin submission) → **Eval** (accuracy headline) → **Eval report + how to read it** (scores, polarity-match per case, judge feedback-quality metrics, `pass_threshold` semantics, grade-fail-is-evidence; link the committed `/evals/lesson_hello/` report as live example) → **Deploy A: standalone site** (build, Pages, COI caveat, one cross-link) → **Deploy B: Quarto book** (export-quarto, paste, extension, render, cross-link README + AC-3's step).
- mdBook is plain markdown — no front matter needed; chapter title via `# The whole game`.
- Reading-the-report section is deliberately command-anchored: show the real `lesson_hello` report URL and one `index.json` excerpt, not a smevals API doc.

### AC-3 — export-quarto step in creating-lessons.md
- Decision (binding per triage; resolver confirms): export-quarto lives as a *new step* in `creating-lessons.md` AND drives the Quarto deploy section of whole-game. Rationale: the command is undiscoverable today (`--help` only); whole-game alone buries it under a narrative chapter title. Whole-game cross-links the step rather than duplicating it.
- Placement: Step 11, after Build — export-quarto is an alternative output of a finished lesson, not a pipeline stage. No renumbering of existing steps (hot-diff avoidance).
- Content shape: command, example fenced-div output for `lesson_hello.yaml`, the `quarto add` extension install, and the fenced-div-in-`.qmd` paste step; redirect detailed extension docs to README.

### AC-4 — discoverability + structural pins
- README: append a "See the whole game end-to-end" pointer (one line) linking the published `/whole-game.html` + optionally note `export-quarto` inclusion — append-only, never edit pinned workflow lines.
- check-docs.sh additions (mirrors existing SUMMARY/examples pattern, ~6 lines): grep `whole-game.md` in `docs/book/src/SUMMARY.md`; assert built `docs/book/book/whole-game.html` exists; grep built whole-game.html for `evals/lesson_hello`; grep built `creating-lessons.html` for `export-quarto`. Guard nothing — these are unconditional once AC-1/2/3 land, so AC-4 MUST be last.
- Do NOT extend readme.rs unless reviewers want an export-quarto pin — adding new CLI docs to README isn't a user requirement here.

## Open Questions

- [resolved] **Chapter title wording**: **"The whole game"** (user choice) — SUMMARY order: Introduction → The whole game → Creating Lessons → Architecture → Example sites → API reference. URL stays `/whole-game.html`.
- [resolved] **eval-report for lesson_hello while scaffolding in /tmp**: **FRESH report** — run eval-report on the scaffold `lesson_hello.yaml`, commit `docs/evals/lesson_hello/` (user chose fresh over citing `01_seed_data`). AC-1 stays, 4 ACs, 3 batches.

(Non-blocking, resolved during decomposition: whole-game chapter duplication with creating-lessons.md — resolved by hyperlink discipline; export-quarto home — resolved to creating-lessons.md Step 11 + whole-game section.)
