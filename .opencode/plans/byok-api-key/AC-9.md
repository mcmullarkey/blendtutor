---
ac: 9
depends_on: 7
risk: low
status: complete
---

**Predicate:** All clauses hold:
1. README.md `## BYOK` section (scoped via awk section extraction) contains `Fireworks` AND literal `accounts/fireworks/models/deepseek-v4-flash-0731` AND `localStorage`; and does NOT contain `sessionStorage` NOR `ANTHROPIC_API_KEY` within the BYOK section.
2. Whole-README: literal `Anthropic-only` absent (kills stale :44 browser claim). `ANTHROPIC_API_KEY` env var doc at :43 (CLI section, supported via rig/ADR-0006) is KEPT — only the :44 browser claim is fixed.
3. BYOK section states feedback is auto-mounted; BYOK section does NOT contain `manual opt-in` or `not auto-mounted` (reversal of filter-runtime-bootstrap decision, per AC-3).
4. BYOK section contains a sentence stating `file://` breaks localStorage sharing across pages AND ES modules — book must be served over HTTP.
5. BYOK section contains `connect-src` AND `api.fireworks.ai` with RECOMMENDATION framing (`recommend|consider|should` present); section does NOT frame CSP as `enforced|applied|configured` (no CSP-header mechanism exists on GitHub Pages; coi-serviceworker covers COOP/COEP only).
6. Security wording: README states key stored in browser localStorage (readable by any JS on the origin; XSS can steal it — do not reuse critical keys) and sent only in the `Authorization` header to `api.fireworks.ai`; README contains NO overclaim literals `fully secure|vault|completely safe|cannot be stolen`.
7. `docs/adr/0016-*.md` exists and references `api-key`, Fireworks CORS, ADR-0009, and ADR-0014.
8. ADR-0009 status line (:3) updated to note partial supersession by ADR-0016 (Fireworks-only UX; :79-87 multi-provider chooser + :99-102 sessionStorage slots stale); ADR-0014 status line (:3) updated to `Accepted; superseded by ADR-0016` (title "shared sessionStorage" + :29-33/:46 stale).
9. `scripts/tests/test_quarto_distribution.sh` exits 0 — Clause 10 (:238-260) rewritten in lockstep in the SAME PR: :251 assertion flipped from `not auto-mounted|manual` to an auto-mounted assertion; :256 `ANTHROPIC_API_KEY` assertion kept green via surviving CLI doc at :43 (or rescoped to BYOK section only).
10. `scripts/tests/test_demo_docs.sh` exits 0 — c9 line-pin (288-342) not shifted by README edits (adjust pins in lockstep if needed).
11. `scripts/check-docs.sh` exits 0.
12. `.opencode/plans/filter-runtime-bootstrap.md:78` annotated in-place SUPERSEDED (or ADR-0016 explicitly records the auto-mount reversal — annotate-in-place preferred).
13. ADR-0016 references out-of-conflict-set stale surfaces as corrected-on-next-regen: `docs/okf/interfaces/js-runtime-seam.md:57` (GENERATED — reference-only, do NOT hand-edit; regenerate via okf-bundle skill) and `docs/agent-notes/feedback.md:53` (hand-curated — EDITED directly in this PR, included in conflict set).

**Probe:**
```bash
awk '/^## BYOK/,/^## /' README.md | grep -q 'Fireworks' \
  && awk '/^## BYOK/,/^## /' README.md | grep -q 'accounts/fireworks/models/deepseek-v4-flash-0731' \
  && awk '/^## BYOK/,/^## /' README.md | grep -q 'localStorage' \
  && ! awk '/^## BYOK/,/^## /' README.md | grep -q 'sessionStorage' \
  && ! awk '/^## BYOK/,/^## /' README.md | grep -q 'ANTHROPIC_API_KEY' \
  && ! grep -q 'Anthropic-only' README.md \
  && awk '/^## BYOK/,/^## /' README.md | grep -qi 'auto-mounted' \
  && ! awk '/^## BYOK/,/^## /' README.md | grep -qiE 'manual opt-in|not auto-mounted' \
  && awk '/^## BYOK/,/^## /' README.md | grep -q 'file://' \
  && awk '/^## BYOK/,/^## /' README.md | grep -q 'connect-src' \
  && awk '/^## BYOK/,/^## /' README.md | grep -qiE 'recommend|consider|should' \
  && ! awk '/^## BYOK/,/^## /' README.md | grep -qiE 'CSP.*(enforced|applied|configured)' \
  && awk '/^## BYOK/,/^## /' README.md | grep -q 'Authorization' \
  && ! grep -qiE 'fully secure|vault|completely safe|cannot be stolen' README.md \
  && ls docs/adr/0016-*.md \
  && grep -q 'api-key' docs/adr/0016-*.md \
  && grep -qi 'CORS' docs/adr/0016-*.md \
  && grep -q 'ADR-0009' docs/adr/0016-*.md \
  && grep -q 'ADR-0014' docs/adr/0016-*.md \
  && grep -q 'superseded' docs/adr/0009-*.md \
  && grep -q 'superseded' docs/adr/0014-*.md \
  && bash scripts/tests/test_quarto_distribution.sh \
  && bash scripts/tests/test_demo_docs.sh \
  && bash scripts/check-docs.sh \
  && grep -qi 'SUPERSEDED' .opencode/plans/filter-runtime-bootstrap.md
```

**Negative:**
1. **Test-lockstep sneaky-pass:** builder keeps `manual`/`ANTHROPIC_API_KEY` claims in BYOK section to keep :251/:256 green without rewriting Clause 10 → README internally contradicts AC-3/AC-5. Caught by clauses 1+3 (BYOK-scoped negatives) — test edits and doc edits land in the SAME PR.
2. README updated but no ADR-0016 → decision unrecorded; clause 7 fails.
3. CSP overclaim: README implies CSP enforced/applied by the book → false security claim on GitHub Pages; clause 5 negative fails.
4. Security overclaim: README claims key "fully secure"/"vault"/"completely safe" → dishonest framing of XSS-inherent client-side key risk; clause 6 negative fails.
5. Stale claims survive in ADR-0009/ADR-0014 without supersession status → next reader trusts sessionStorage/multi-provider docs; clause 8 fails.
6. README edit shifts demo-doc line pins → test_demo_docs.sh c9 pin (288-342) red; clause 10 fails. Fix by adjusting pins in lockstep, not reverting docs.
7. Generated okf doc hand-edited → regen overwrites silently; clause 13 enforces reference-only + okf-bundle regen path.
8. Pre-existing ADR-0016 collision: docs/adr/0018:94 references a possibly-retracted 0016 — builder MUST `ls docs/adr/0016-*` before creating; if a pre-existing 0016 exists, renumber to next free slot and update all references.

**Verification:** code (grep/awk-based shell tests + existing test scripts)

**Fixture status:** EDIT README.md | NEW docs/adr/0016-*.md (verify no pre-existing 0016 — 0018:94 anomaly) | EDIT docs/adr/0009-*.md (status field) | EDIT docs/adr/0014-*.md (status field) | EDIT docs/agent-notes/feedback.md:53 | EDIT scripts/tests/test_quarto_distribution.sh Clause 10 (:238-260) | EDIT scripts/tests/test_demo_docs.sh (c9 pin only if shifted) | ANNOTATE .opencode/plans/filter-runtime-bootstrap.md:78

**Rubric anchor:** §4 (module responsibility — docs name what/where/what-NOT: ADR-0016 records the reversal and supersession chain; README BYOK section owns the user-facing contract)

**Design Intent:**
- **Types/interfaces (§1):** N/A — documentation slice. The "contract" is prose invariants enforced by grep assertions (auto-mounted, localStorage, Fireworks-only literals).
- **Pure/effectful (§2):** N/A — no runtime code. Test-script edits are the only executable surface; assertions are pure text checks.
- **Boundary cuts (§3):** Docs cut at ownership boundary: README = user-facing BYOK contract; ADRs = decision history with supersession chain (0009 → 0016, 0014 → 0016); generated okf docs excluded from hand-edit (regen boundary respected); hand-curated agent-notes edited directly.
- **Module responsibility (§4):** ADR-0016 header names what (BYOK storage/provider/auto-mount decisions), where (supersedes ADR-0014 storage claims, partially supersedes ADR-0009), what NOT (does not re-document CLI ANTHROPIC_API_KEY — that stays in ADR-0006/rig).
- **Function discipline (§5):** Each grep/awk clause asserts one claim; Clause 10 rewrite in test_quarto_distribution.sh keeps one assertion per line, scoped to BYOK section where possible.

**Technical Context:**
- README stale claims (verbatim locations): :43-44 (CLI env var KEEP :43, fix :44 browser Anthropic-only claim), :267-268, :270-271, :273-284, :286 — rewrite BYOK section Fireworks-only.
- ADR chain: numbering 0001-0015, 0017, 0018 present — 0016 free BUT 0018:94 references possibly-retracted ADR-0016 → builder verifies `ls docs/adr/0016-*` empty before creating. ADR-0009 (:3 Extended; :79-87 multi-provider chooser; :99-102 sessionStorage slots stale). ADR-0014 (title "shared sessionStorage"; :3 Accepted → `Accepted; superseded by ADR-0016`; :29-33/:46 stale).
- Test-lockstep (mandatory, same PR): test_quarto_distribution.sh Clause 10 :238-260 — :251 currently greps `not auto-mounted|manual` (FAILS once auto-mount documented → flip to auto-mounted assertion); :256 greps `ANTHROPIC_API_KEY` (stays green via CLI :43 doc — or rescope assertion to BYOK section). test_demo_docs.sh c9 line-pin 288-342 — adjust only if README edit shifts lines.
- Out-of-conflict surfaces: docs/okf/interfaces/js-runtime-seam.md:57 GENERATED (okf-bundle frontmatter) → reference-only in ADR-0016, regenerate via okf-bundle skill (respects generated boundary; avoids silent regen-overwrite); docs/agent-notes/feedback.md:53 hand-curated → EDIT directly, in conflict set; .opencode/plans/filter-runtime-bootstrap.md:78 historical plan → annotate SUPERSEDED in place.
- Research-verified facts to document: Fireworks CORS open from GitHub Pages origin; localStorage shared same-origin across pages; file:// breaks localStorage sharing + ES modules (serve over HTTP); CSP connect-src = recommendation for self-hosted deployments only (Pages can't set headers; coi-serviceworker = COOP/COEP only); XSS→key-theft inherent to client-side keys (mitigations already shipped: no third-party scripts, textContent-only rendering).

**Dependencies:** depends-on: 7 (docs surface settled by demo-book rewrite) | blocks: none | conflict set: README.md, docs/adr/0016-*.md (NEW), docs/adr/0009-*.md, docs/adr/0014-*.md, docs/agent-notes/feedback.md, scripts/tests/test_quarto_distribution.sh, scripts/tests/test_demo_docs.sh, .opencode/plans/filter-runtime-bootstrap.md | notes: test-lockstep mandatory (Clause 10 + c9 pins in SAME PR); okf js-runtime-seam.md referenced-not-edited; verify no pre-existing ADR-0016

**Clarifications resolved:**
- NC-1 test_quarto_distribution.sh in conflict set → YES. Stale test asserting manual against auto-mount README = discipline failure; lockstep in same PR.
- NC-2 ADR strategy → NEW ADR-0016 + superseded-by status fields on ADR-0009 (:3 → "Extended; partially superseded by ADR-0016") and ADR-0014 (:3 → "Accepted; superseded by ADR-0016"). Amending ADR-0014 can't fix its stale title; new ADR preserves decision history. Builder verifies 0016 slot empty (0018:94 anomaly).
- NC-3 out-of-conflict surfaces → split handling: agent-notes/feedback.md:53 EDITED (hand-curated, in conflict set); okf js-runtime-seam.md:57 REFERENCE-ONLY in ADR-0016 + regen via okf-bundle (respects generated boundary); filter-runtime-bootstrap.md:78 annotated SUPERSEDED in place.
- NC-4 CSP framing → RECOMMENDATION ("we recommend adding CSP connect-src https://api.fireworks.ai for self-hosted deployments"); negative on enforced/applied/configured literals.
- NC-5 CLI section → KEEP :43 ANTHROPIC_API_KEY (rig/ADR-0006 supports it); fix ONLY :44 browser Anthropic-only claim.
- Security framing → honest: localStorage readable by any JS on origin, XSS theft risk explicit, Authorization-header-only transit, no overclaim literals.

**needs-clarification:** NONE

### Progress
- [x] AC-9 spec resolved — 2026-08-06
- [x] implementation — complete 2026-08-07 (PR #180, commits ace721e + 9799c3a)

### Decision Log
- 2026-08-06 — NEW ADR-0016 supersedes ADR-0014/partially ADR-0009 (status-field annotations, bodies untouched); test_quarto_distribution.sh Clause 10 lockstep in same PR; CSP framed as recommendation not enforcement; CLI ANTHROPIC_API_KEY doc kept (:43); security wording honest (no overclaims); okf referenced-not-edited, agent-notes edited.
- 2026-08-07 — PROBE DEFECT corrected: issue-body `awk '/^## BYOK/,/^## /'` self-terminates on the heading (heading matches both range patterns → extract always 1 line; unsatisfiable for ANY implementation). Corrected to heading-excluding `awk '/^## BYOK/{f=1} f{print} f && /^## / && !/^## BYOK/{exit}'`; intent unchanged, corrected probe materialized in-repo (test_quarto_distribution.sh Clause 10) + evidence at docs/evidence/170/probe.log. Director should run the corrected probe for acceptance.

### Surprises & Discoveries
- The issue-body probe's awk range `/^## BYOK/,/^## /` is unsatisfiable: the heading `## BYOK (Bring Your Own Key)` matches BOTH patterns, so the range covers only the heading line (POSIX awk, mawk, gawk all identical). Every BYOK-scoped clause could never pass with any implementation. Fixed the extraction to heading-excluding awk; spec intent was unambiguous so no spec re-resolution needed, but the Director's acceptance run must use the corrected probe (or the in-repo Clause 10 checks).
- The README BYOK rewrite had to document the auto-mount mechanism (`exercise-feedback.js` + `mountAllFeedback`) explicitly: removing the old manual-opt-in block would otherwise drop both literals, breaking test_quarto_distribution.sh Clause 10's existing pins. The mechanism is accurate per blendtutor.lua (imports + calls mountAllFeedback in the injected bootstrap).
- Promoting `### BYOK` to top-level `## BYOK` required relocating it after the Quarto Extension section (before `## License`): in-place promotion would have swallowed the COI + Demo book subsections. Demo-section scope anchor in test_demo_docs.sh updated `^## License` → `^## BYOK`; c9 pins (291/314) still hold within 288–342.
- Pre-existing uncommitted working-tree changes for issue #179 (test_quarto_bootstrap.sh + test_quarto_feedback.py maxFeedbackPerSession emission tests) were present before this slice — left unstaged, NOT part of PR #180.

### Idempotence & Recovery
- Safe retry: re-run the corrected probe bash chain (docs/evidence/170/probe.log) after interrupted edit.
- Rollback: git revert the PR.