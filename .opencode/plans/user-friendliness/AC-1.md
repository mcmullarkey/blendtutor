---
ac: 1
depends_on: none
risk: medium
status: spec
---

### AC-1 — release workflow
- Spec mode: spec-resolved
- Key files: `.github/workflows/release.yml` (NEW), `Cargo.toml`, `crates/cli/Cargo.toml` (bin name = `blendtutor`; confirm `[[bin]]`/package name)
- Invariants/constraints:
  - Repo convention: least-privilege `permissions:`, toolchain pinned via `rust-toolchain.toml`, no `continue-on-error`/`|| true` anywhere (CI doctrine — see ci.yml comments).
  - Trigger: `v*` tags + `workflow_dispatch`. Artifact contract (consumed by AC-2 — pin it HERE): `blendtutor-<tag>-<target>.tar.gz` + `blendtutor-<tag>-sha256sums.txt`, release name = tag.
  - Precedent test: YAML pin tests use awk job-block extraction (see `scripts/tests/test_verify_live_wiring.sh` Phase 1).
- Prior art: uv's release flow (astral-sh/uv release.yml); repo's own `docs.yml` build-job structure.
- Verification: code (YAML pin test, fail-closed) — verification of an actual release is manual/post-tag.
- Test seam: NEW `scripts/tests/test_release_yml.sh` modeled on `test_verify_live_wiring.sh` Phase 1.
- Literal grep requirement: no model-ID/constant touched. Asset-name literal `blendtutor-<tag>-*` appears only here + AC-2 test — cross-grep these two files at PR time.

### Progress
- (none yet)

### Decision Log
- (none yet)

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open
