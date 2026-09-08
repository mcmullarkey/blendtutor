---
ac: 1
depends_on: none
risk: medium
status: complete
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
- [x] 2026-09-08 RED suite `scripts/tests/test_release_yml.sh` committed (bbd7141) — awk job-block extraction, fail-closed clauses (23 at commit; this ledger over-claimed 25 — corrected here)
- [x] 2026-09-08 `release.yml` implemented + ci.yml check-job wiring (6399e9a) — 23/23 green
- [x] 2026-09-08 Mutation negative-control: global tarball-literal rename → contract clause FAIL → reverted → green
- [x] 2026-09-08 Evidence at `docs/evidence/223/` (test-suite.log + yaml-parse.log)
- [x] 2026-09-08 Review cycle 1 (PR #231): checksums + tarball upload args pinned in extracted gh-release-create step block; tar root layout (`-C dist blendtutor`) pinned in build job — 25 clauses, 25/25 green
- [x] 2026-09-08 Review cycle 2 (PR #231): repo-context clause (checkout OR GH_REPO in release job — gh ignores GITHUB_REPOSITORY; job had neither, `gh release create` after `cd dist` would fail "could not determine git repo" on first tag push) + `actions/checkout@v5` fix (d915db8, dec6cc0); ref_name sanitization clause + `TAG="${GITHUB_REF_NAME//\//-}"` fix across build/release jobs, upload name via pack-step output (b0fa3a3, 2e99892); Generate-step checksums-filename clause; anchored tags-filter grep — 28 clauses, 28/28 green, 3 mutation negative controls (remove checkout / mutate Generate filename / tags→branches) each FAILed exactly the new clause

### Decision Log
- Release job gated `if: startsWith(github.ref, 'refs/tags/v')`: workflow_dispatch from a branch builds artifacts but skips publishing — otherwise `gh release create` would mint a junk tag named after the branch. Gate pinned by the test (distinct from the banned `if: always()`).
- aarch64-linux cross-compile via `gcc-aarch64-linux-gnu` apt package + `CARGO_TARGET_AARCH64_UNKNOWN_LINUX_GNU_LINKER` env (set unconditionally — unused by the other 3 targets). Verified Cargo.lock has no openssl-sys (rustls only), so no cross C toolchain beyond binutils needed.
- Tarball contains the bare `blendtutor` binary at its root (staged from `target/<triple>/release/`), matching what AC-2's install.sh will extract to `~/.local/bin`.
- Checksums generated in the release job (after download-artifact merge), not per-build-job — one file covers all 4 tarballs.
- Upload args (tarball glob + checksums filename) pinned in the extracted gh-release-create step block, not the release-job block — a job-level grep cannot tell the Generate step's output filename from the create step's upload arg, so dropping the upload arg left the suite green (review cycle 1 fix-now). Step blocks extracted with a literal `index()` match so parens/braces in step names cannot break the awk pattern.
- Repo context via `actions/checkout@v5` (not `GH_REPO` env) in the release job — checkout is the docs.yml verify-live precedent and also gives `--generate-notes` a real commit history to diff against. gh resolves the repo from GH_REPO env or git remotes under cwd only; GITHUB_REPOSITORY is ignored.
- ref_name sanitized per-step (`TAG="${GITHUB_REF_NAME//\//-}"` in pack/generate/create run blocks) rather than workflow-level env — GitHub expressions have no string-replace, so shell expansion is the only sanitization point; the upload step's `name:`/`path:` read the pack step's `tag` output because `with:` cannot shell-expand. On a slashed tag the release step would mint the sanitized tag name — acceptable (dispatch never reaches the release job; slashed v* tags are pathological).
- No ADR: release workflow is CI plumbing, not a new codebase boundary (ADRs end at 0018; ADR-0019 left reserved).

### Surprises & Discoveries
- Pin-test grep patterns beginning with `-` (e.g. `-p blendtutor-cli`, `--title "..."`) are parsed as grep OPTIONS, not patterns — must use `grep -F -e '<pattern>'` (same convention as test_verify_live_wiring.sh:311). Two clauses silently misfired on first green run.
- Mutation negative-control needed a GLOBAL rename: the tarball contract literal appears 3× in the build job (tar step + upload-artifact name + path), so a single-occurrence mutation still passed. Global rename → clause FAIL → revert → green, proving teeth.
- pyyaml parses the workflow `on:` key as boolean `True` (YAML 1.1 quirk) — evidence script reads `d.get(True) or d.get('on')`.
- Structural pin tests are blind to runtime executability: cycle 2's fix-now (no repo context) passed all 25 structural clauses while the release job could never execute `gh release create` on a fresh runner. Workflow/job pin tests need a repo-context clause (checkout OR GH_REPO) alongside auth pins — source-level greps cannot prove a job's steps compose into a runnable command environment.
- upload-artifact v4 rejects `/` in artifact names outright, so the slash-branch dispatch hazard was worse than a malformed filename — the build job itself would fail. Sanitization is load-bearing for the dispatch path, not cosmetic.

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open
