---
ac: 9
depends_on: AC-3, AC-5
risk: medium
status: complete
---

## AC-9: Opt-in cross-origin isolation via coi='true' filter attribute

### Executable Spec
- **predicate:** 7 clauses: coi="true" activates, false/yes/empty rejected, dedup, byte-parity, per-page isolation, no R exercises still registers, coi-yaml (document-level) activates.
- **probe:** scripts/tests/test_coi_filter.sh
- **negative:** Script src 404s. Filter hardcodes script tag.
- **verification:** code
- **fixture status:** coi-true.qmd, coi-false.qmd, coi-yes.qmd, coi-empty.qmd, coi-double.qmd, coi-python-only.qmd, coi-book/, coi-yaml.qmd, test_coi_filter.sh (all NEW)
- **rubric anchor:** §1.2, §1.3.1, §3.2, §3.4, §4.2, ADR-0008

### Design Intent
- §1: coi="true" string enum; YAML boolean
- §2: Filter logic pure; serviceworker registration effectful
- §3: COI activation separate from exercise runtime; filter decides, runtime registers
- §4: blendtutor.lua owns coi detection; coi-serviceworker.js owned separately
- §5: One activation path per page; no duplicate registrations

### Technical Context
- Files: _extensions/blendtutor/blendtutor.lua (edit), _extensions/blendtutor/assets/coi-serviceworker.js (NEW, synced), scripts/sync-quarto-assets.sh (edit), scripts/tests/test_coi_filter.sh (NEW), quarto-fixture/coi-*.qmd (NEW), ci.yml (edit)
- Source: crates/core/assets/shared/coi-serviceworker.js (already exists, vendored verbatim ADR-0008)
- COI activated by BOTH div attr coi="true" AND document-level YAML coi: true

### Dependencies
- Depends on: AC-3 (sync script), AC-5 (webR adapter — COI is optional optimization)
- Conflict set: blendtutor.lua, sync-quarto-assets.sh, ci.yml
- ADR: ADR-0015

### Progress
- [x] Create plan file (2026-07-23)
- [x] RED: write test_coi_filter.sh + fixtures, confirm fail (2026-07-23)
- [x] ADR-0015 (2026-07-23)
- [x] Implement: blendtutor.lua COI detection + script injection (2026-07-23)
- [x] Implement: sync script + vendored coi-serviceworker.js (2026-07-23)
- [x] GREEN: test_coi_filter.sh passes — 14/14 (2026-07-23)
- [x] CI step added to ci.yml (2026-07-23)
- [x] Evidence at docs/evidence/114/ (2026-07-23)

### Decision Log
- 2026-07-23 — COI activated by BOTH div attr AND YAML (user decision from issue)
- 2026-07-23 — coi="true" checked on ANY div, not just .blendtutor — COI is page-level concern (§3)
- 2026-07-23 — YAML coi: true checked via doc.meta["coi"] == true (boolean) or string "true" (robustness)
- 2026-07-23 — Script src path: _extensions/blendtutor/assets/coi-serviceworker.js (matches existing asset pattern)

### Surprises & Discoveries
- 2026-07-23 — Pandoc represents YAML `coi: true` as Lua boolean `true`, not as MetaInlines or MetaString. The comparison `doc.meta["coi"] == true` works directly. Also handles string "true" for quoted YAML as robustness.
- 2026-07-23 — The coi-book fixture generates HTML files in subdirectories not covered by the existing `quarto-fixture/*.html` gitignore pattern. Added `quarto-fixture/coi-book/*.html` to .gitignore.

### Idempotence & Recovery
- Safe retry: re-run test_coi_filter.sh
- Rollback: revert blendtutor.lua changes, remove coi-serviceworker.js from assets
