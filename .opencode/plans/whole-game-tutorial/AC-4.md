---
ac: 4
depends_on: AC-1, AC-2, AC-3
risk: low
status: spec
---

# AC-4 — Discoverability pins (README pointer + check-docs.sh pins)

## Executable Spec (resolved)
**Merge decision: Speculator B's C1-C10 adopted wholesale** (A's 4 pins subsumed). Resolver additions: (1) USER DECISION 2026-08-10 — pins are LOCAL-ONLY enforcement (Option A, decomposition as written): check-docs.sh fails local runs; CI docs.yml does NOT enforce content pins (builds inline); test_docs_pages_artifact.sh Phase 2 skips in CI (no mdbook). Decomposition's "fail CI" wording was inaccurate. Do NOT touch docs.yml or scripts/tests/test_docs_pages_artifact.sh. (2) Path correction: MIRROR_OK check at scripts/tests/test_docs_pages_artifact.sh:304 (B dropped `tests/`). (3) Anti-pattern warning: the evals assemble block at check-docs.sh:95-101 IS guarded (if [ -d docs/evals ]) — AC-4 pins MUST NOT copy that pattern.

- **predicate:** ALL of the following hold (run from repo root after AC-1/2/3 land — AC-4 MUST be last):

  **C1 — SUMMARY pin (source, established pattern):** `scripts/check-docs.sh` contains an uncommented line matching `grep.*whole-game.*SUMMARY\.md` — pins the SUMMARY entry. Stronger variant: `grep -qF './whole-game.md'` pins the link target, not just the word.

  **C2 — Built whole-game.html pin (NOT source — anti-sneaky-pass):** contains an uncommented line matching `grep.*evals/lesson_hello.*book_out/whole-game\.html\|grep.*evals/lesson_hello.*\$book_out/whole-game` — pins the BUILT HTML, NOT the source. A source-grep pin is the cheapest broken implementation (source can be right while built HTML is wrong due to render bug / fence collision).

  **C3 — Built creating-lessons.html pin (NOT source):** contains an uncommented line matching `grep.*export-quarto.*book_out/creating-lessons\.html\|grep.*export-quarto.*\$book_out/creating-lessons` — pins BUILT HTML, NOT source (AC-3's speculator-b identified nested-fence collision as a latent render bug; built-HTML pin catches truncation).

  **C4 — README pointer pin:** contains an uncommented line matching `grep.*whole-game.*README\.md`. The needle must be `whole-game.html` (built page name), NOT `whole-game.md` (source — dead link from repo-root context).

  **C5 — Unconditional (no guard, no `|| true`):** No uncommented line matching `whole-game|export-quarto` also matches `\|\| true`. Pins must NOT be wrapped in `if [ -f ... ]; then ... fi` or suffixed with `|| true`. The existing evals assemble (check-docs.sh:95-101) IS guarded — AC-4's pins must NOT follow that pattern. `set -euo pipefail` (line 15) ensures any unguarded failing grep exits the script.

  **C6 — Uncommented (anti-sneaky-pass):** Pin lines are matched by `grep -v '^#' scripts/check-docs.sh` — a commented-out pin doesn't run.

  **C7 — Post-build ordering (enforced by set -e):** The pins run AFTER `mdbook build docs/book` (check-docs.sh:31).

  **C8 — End-to-end pass (no regression):** `bash scripts/check-docs.sh` exits 0.

  **C9 — readme.rs not broken (append-only discipline):** `cargo test -p blendtutor-cli --test readme` exits 0. If the builder REWRITES line 58 (the workflow string) to inline the pointer, any dropped command substring breaks readme.rs — probe catches this.

  **C10 — MIRROR_OK count unchanged:** `scripts/tests/test_docs_pages_artifact.sh` line 304 still asserts `MIRROR_OK -eq 11`. AC-4's new pins are NOT docs.yml mirror needles — no count update needed.

- **probe:**
  ```bash
  set -e
  # C1-C4: Structural — pins exist in check-docs.sh, on BUILT HTML (not source), uncommented
  grep -v '^#' scripts/check-docs.sh | grep -q 'whole-game.*SUMMARY\.md'        # C1 SUMMARY pin
  grep -v '^#' scripts/check-docs.sh | grep -q 'evals/lesson_hello.*whole-game\.html'  # C2 built HTML (NOT source)
  grep -v '^#' scripts/check-docs.sh | grep -q 'export-quarto.*creating-lessons\.html' # C3 built HTML (NOT source)
  grep -v '^#' scripts/check-docs.sh | grep -q 'whole-game.*README\.md'       # C4 README pin
  # C2/C3 anti-sneaky-pass: pins are NOT on source .md files
  ! grep -v '^#' scripts/check-docs.sh | grep -q 'src/whole-game\.md.*evals/lesson_hello'
  ! grep -v '^#' scripts/check-docs.sh | grep -q 'src/creating-lessons\.md.*export-quarto'
  # C5: Unconditional — no || true on pin lines
  ! grep -v '^#' scripts/check-docs.sh | grep -E 'whole-game|export-quarto' | grep -q '|| true'
  # C8: End-to-end — check-docs.sh builds book + runs all pins (post-build ordering via set -e)
  bash scripts/check-docs.sh
  # C9: readme.rs — README append didn't break existing substring needles
  cargo test -p blendtutor-cli --test readme
  # C10: MIRROR_OK count unchanged
  test "$(grep -c 'MIRROR_OK -eq 11' scripts/tests/test_docs_pages_artifact.sh)" -eq 1
  ```

  **Negative probe (unconditionality — run separately, destructive):**
  ```bash
  # Proves pins are NOT guarded: remove SUMMARY entry → mdbook builds without whole-game.html → pin fails
  cp docs/book/src/SUMMARY.md /tmp/SUMMARY.bak
  grep -v 'whole-game' docs/book/src/SUMMARY.md > /tmp/SUMMARY.tmp && mv /tmp/SUMMARY.tmp docs/book/src/SUMMARY.md
  if bash scripts/check-docs.sh 2>/dev/null; then
    echo "FAIL: whole-game pin is guarded — script passed despite missing page" >&2; exit 1
  fi
  cp /tmp/SUMMARY.bak docs/book/src/SUMMARY.md
  ```

- **negative:** 1. Source-grep instead of built-HTML-grep (MOST LIKELY — source has content, built HTML might not due to render bug; C2/C3 anti-sneaky-pass catches). 2. Guarded pin (`if [ -f ... ]; then ... fi` — skipped when artifact missing; C5 catches `|| true` variant, negative probe catches `if` variant). 3. Commented-out pin (C6 `grep -v '^#'` catches). 4. Wrong README link target (`./docs/book/src/whole-game.md` source instead of deployed URL; C4 needle `whole-game.html` catches). 5. README rewrite of pinned lines (C9 cargo test readme catches). 6. Pin before build (C8 end-to-end via set -e). 7. Breaking existing checks (C8 catches).

- **verification:** code

- **fixture status:** `scripts/check-docs.sh:218-236` (existing pin region — append after line 236) · `README.md:56-58` (authoring-workflow section — append one line+link, append-only) · `crates/cli/tests/readme.rs:27-67` (existing — NOT modified, read-only regression check) · `scripts/tests/test_docs_pages_artifact.sh:290-304` (existing — NOT modified, MIRROR_OK count unchanged) · NEW pin lines in check-docs.sh (4 pins)

- **rubric anchor:** §1 (Encode invariants in types — pins make a missing/wrong whole-game chapter a check failure, not a silent deploy) + §4 (Document module responsibility — check-docs.sh owns the local mirror contract; pins document what it asserts)

### Design Intent

AC-4 wires durable regression pins so that future changes to AC-1/2/3 artifacts (removing whole-game.md, dropping the evals/lesson_hello citation, deleting Step 11's export-quarto content) fail locally via `bash scripts/check-docs.sh`. The pins are CROSS-AC CONTRACTS: C2 assumes AC-2's whole-game.md cites `/evals/lesson_hello/` (AC-2 speculator-b clause 12 + 32); C3 assumes AC-3's creating-lessons.md Step 11 documents `blendtutor export-quarto`. If AC-2/3 don't land the content, the pins fail — that's intended. This is why AC-4 MUST land last: the pins are unconditional (no guard), so they fail CI/local if any dependency artifact is missing.

The README pointer is a DISCOVERABILITY wire — a one-line link near the authoring-workflow section (README.md:56-58) pointing to the deployed whole-game chapter, following the existing README link pattern (deployed URLs, e.g., lines 146-149, 293, 316). Append-only discipline: add a new line, never rewrite the pinned workflow string at line 58.

### Technical Context

**check-docs.sh structure (238 lines):**
- Line 15: `set -euo pipefail` — any failing grep exits the script (unconditional pins enforced)
- Line 19: `book_out="docs/book/book"` — built HTML output dir
- Line 31: `mdbook build docs/book` — build step (pins must run AFTER this)
- Lines 95-101: guarded evals assemble (`if [ -d docs/evals ]`) — the ONLY guarded block; AC-4's pins must NOT follow this pattern
- Lines 218-222: existing README pins (`grep -q 'examples/r/' README.md` etc.)
- Line 225: existing SUMMARY pin (`grep -q 'examples' docs/book/src/SUMMARY.md`)
- Lines 231-236: existing built-HTML pins (`grep -q 'examples/r/' "$book_out/examples.html"` etc.)
- **Insertion point:** after line 236 (end of existing built-HTML pins), before final `echo "docs: OK"` at line 238

**README.md structure (370 lines):**
- Line 56: `## Authoring workflow` heading; Line 58: the pinned workflow string (readme.rs needles)
- Lines 146-149, 293, 316: existing deployed-URL links
- **Insertion point:** after line 58 (before `### blendtutor init` at line 60). Append-only.

**readme.rs (68 lines):** Line 27 lowercased haystack; Lines 33-47: 9 substring needles; append-only safe (contains-check; removing pinned phrases breaks). AC-4 does NOT touch readme.rs — read-only regression check.

**test_docs_pages_artifact.sh (487 lines):** Lines 290-301 MIRROR_OK needle list (11); Line 304 `MIRROR_OK -eq 11`. AC-4's new pins are NOT mirror needles → count stays 11 → AC-4 does NOT touch this file.

**docs.yml (CI workflow):** Line 61 `mdbook build docs/book` (inline, NOT via check-docs.sh); Line 137 Pages artifact root. Does NOT grep built HTML for whole-game/evals/lesson_hello/export-quarto.

**CI-enforcement note (resolver decision, USER DECISION 2026-08-10):** Pins are LOCAL-ONLY (Option A, decomposition as written). check-docs.sh fails local runs; CI docs.yml does NOT enforce content pins (builds inline); test_docs_pages_artifact.sh Phase 2 skips in CI (no mdbook). Decomposition's "fail CI" wording was inaccurate. Do NOT touch docs.yml or scripts/tests/test_docs_pages_artifact.sh. Path correction: MIRROR_OK check at scripts/tests/test_docs_pages_artifact.sh:304 (B dropped `tests/`).

**Expected test migration: 0 files.**

### Dependencies

- **depends_on:** AC-1 (produces `docs/evals/lesson_hello/`), AC-2 (produces whole-game.md + SUMMARY entry), AC-3 (produces creating-lessons.md Step 11 with export-quarto) — pins reference all three artifacts.
- **MUST be last:** pins are unconditional (C5) — if AC-4 lands before AC-2/3, pins fail. Schedule serializes: AC-1 → AC-2 → AC-3 → AC-4.
- **depended_on_by:** none.

## Pattern Detectors

1. **Rodney-feasibility:** NOT triggered — shell-script pins + README line.
2. **Bidirectional-contract:** NOT triggered in form sense. CROSS-AC CONTRACT: C2 pins AC-2's built whole-game.html for `evals/lesson_hello`; C3 pins AC-3's built creating-lessons.html for `export-quarto`. One-directional regression pins.
3. **Route-existence verify:** TRIGGERED. README pointer link target (`https://mcmullarkey.github.io/blendtutor/whole-game.html`) resolves only after deploy. Builder verifies source target (`docs/book/book/whole-game.html` exists after mdbook build) but NOT deployed URL (404s until merge + deploy). Established convention (existing README links have same lag) — not new risk.
4. **Verification file-path:** probe exercises scripts/check-docs.sh + crates/cli/tests/readme.rs (cross-file regression check).
5. **Refusal-arm enumeration:** `set -euo pipefail` (line 15) — any failing grep exits. Existing pins use `|| { echo ...; exit 1; }` pattern. AC-4's pins follow this. The only guard is `if [ -d docs/evals ]` (95-101) — AC-4 pins must NOT be inside it (C5).
6. **Producer-shape change:** NOT triggered.
7. **UI sneaky-pass:** NOT triggered.
8. **Numeric contradiction:** NOT triggered.
9. **Dual-consumer field:** NOT triggered.
10. **Test file migration:** TRIGGERED — grepped test files for check-docs refs: test_docs_pages_artifact.sh (23 refs; Phase 2 line 328 runs bash "$CHECK_DOCS" — locally/dev; SKIPs in CI ci.yml job because mdbook/cargo absent). MIRROR_OK count (11, line 304) NOT affected. Expected test migration: 0 files.

### ⚠️ CI-ENFORCEMENT GAP (PRIMARY ADVERSARIAL FINDING)

**The decomposition says "AC-4 adds durable pins so regressions in AC-1/2/3 fail CI." But AC-4's pins in check-docs.sh are NOT CI-enforced:**

- `ci.yml:156` runs `test_docs_pages_artifact.sh`. Phase 1 (structural pins on check-docs.sh, MIRROR_OK count) runs in CI. Phase 2 (end-to-end `bash check-docs.sh`) SKIPs because this CI job has only quarto (no mdbook/cargo) — test_docs_pages_artifact.sh:323-326.
- `docs.yml` runs `mdbook build docs/book` inline (line 61) but does NOT run check-docs.sh and does NOT grep built HTML for whole-game/evals/lesson_hello/export-quarto.
- So AC-4's pins (C1-C4) are local-only — enforced when a developer runs `bash scripts/check-docs.sh` locally, NOT in CI.

**Impact:** If a future PR removes `whole-game.md` from SUMMARY (but keeps the file), mdbook build succeeds, docs.yml deploys successfully, CI stays green. Only local check-docs.sh catches. If a future PR edits whole-game.md to drop the evals/lesson_hello citation, same: CI green, local red.

**Partial CI enforcement:** The SUMMARY pin (C1) is INDIRECTLY CI-enforced — if SUMMARY references whole-game.md but the file is deleted, mdbook build fails in docs.yml (line 61), failing CI. But this only catches a DELETED file, not WRONG CONTENT (missing citation, wrong URL). Content pins (C2, C3) have NO CI enforcement.

**Resolver decision needed:**
- **Option A (decomposition as written):** AC-4's pins local-only (check-docs.sh). Risk: regressions slip through if developers skip local checks. Conflict set stays README.md, scripts/check-docs.sh.
- **Option B (CI-enforced):** AC-4 also adds content pins to docs.yml inline build (after mdbook build, grep built HTML). Expands conflict set to docs.yml. CI-enforced but touches a file outside the decomposition's conflict set.
- **Option C (CI-enforced via test):** AC-4 adds structural assertions to test_docs_pages_artifact.sh Phase 1 (grep check-docs.sh for new pin needles, add to a count). Expands conflict set to test_docs_pages_artifact.sh. CI-enforced structurally (pin exists in script) but not functionally (Phase 2 SKIPs in CI).

**Recommendation:** Flag for resolver. The decomposition says "fail CI" but the pins as specified (check-docs.sh only) don't fail CI. Resolver should clarify with the user whether "fail CI" means "fail the docs.yml workflow" (Option B) or "fail local check-docs.sh that developers run before pushing" (Option A).

Friction: Decomposition says "fail CI" but check-docs.sh is local-only (Phase 2 SKIPs in CI's ci.yml job; docs.yml runs inline without content pins) — the "fail CI" claim doesn't match the pin location, requiring resolver clarification on whether to expand AC-4's conflict set to docs.yml for true CI enforcement.

## Progress
- [ ] README pointer + check-docs.sh pins — pending

## Decision Log
- 2026-08-10 — User: local-only pin enforcement (Option A, decomposition as written)
- 2026-08-10 — Resolver: B adopted; C5 unconditional kept; anti-pattern (guarded evals block) flagged

## Surprises & Discoveries
- (none yet)

## Idempotence & Recovery
- Safe retry: re-run structural greps + bash scripts/check-docs.sh + cargo test -p blendtutor-cli --test readme
- Rollback: remove README pointer line + pin block from check-docs.sh