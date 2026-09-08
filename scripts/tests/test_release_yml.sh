#!/usr/bin/env bash
# Executable spec for issue #223 — tag-triggered release workflow.
#
# Verifies the release.yml structural predicate via awk job-block extraction
# (modeled on test_verify_live_wiring.sh Phase 1 — a file-wide grep cannot
# tell WHICH job a key landed in, so every job-scoped pin extracts the block):
#
#   Trigger — `v*` tag push + workflow_dispatch (on-block extraction).
#   Matrix — 4 targets: linux x86_64/aarch64, macOS x86_64/aarch64.
#   Artifact contract (consumed by AC-2 install.sh — pinned HERE):
#     blendtutor-<tag>-<target>.tar.gz + blendtutor-<tag>-sha256sums.txt;
#     the tarball contains the bare `blendtutor` binary staged from the
#     target-triple dir; release name = tag.
#   Doctrine — least-privilege permissions (workflow-level read, release job
#     write, build job NOT write), toolchain pinned via rust-toolchain.toml
#     (rustup show pattern; no dtolnay action), release job gated to v* tag
#     refs, no continue-on-error / || true / if: always() anywhere.
#   Wiring — ci.yml check job runs this test.
#
# Negative cases (from the spec):
#   - release.yml missing → early-exit clause
#   - tag filter dropped (push on branches would cut releases) → on-block clause
#   - workflow_dispatch dropped → on-block clause
#   - a matrix target dropped (AC-2 uname mapping dangles) → 4/4 target clause
#   - tarball renamed (breaks AC-2 byte-match) → contract clause
#   - binary staged from a non-target dir (host/target mixup) → binary-path clause
#   - tar root layout changed (install.sh extracts the bare `blendtutor`
#     member by that exact path) → tar-root clause
#   - checksums upload arg dropped from `gh release create` (Generate step
#     still writes the file, but the release ships none) → create-step clause
#   - tarball upload arg dropped from `gh release create` (release ships
#     checksums only) → create-step clause
#   - release name hardcoded / not the tag → gh release create clause
#   - release job runs on branch dispatch (creates a junk tag named after the
#     branch) → gate clause
#   - release job without repo context (gh resolves the repo from git
#     remotes under cwd or GH_REPO env — NEVER from GITHUB_REPOSITORY — so
#     `gh release create` after `cd dist` fails "could not determine git
#     repo" on a fresh runner) → repo-context clause
#   - contents: write at workflow level (build jobs get release powers) →
#     least-privilege clauses
#   - continue-on-error / || true / if: always() → doctrine clauses
#   - toolchain version/action pinned in the workflow (rust-toolchain.toml
#     bypassed) → toolchain clauses
#   - test not wired into ci.yml → wiring clause
#
# Usage: bash scripts/tests/test_release_yml.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PASS=0
FAIL=0

ok() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
ko() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

RELEASE_FILE=".github/workflows/release.yml"
CI_FILE=".github/workflows/ci.yml"

# ---------------------------------------------------------------------------
# Helpers — job-block extraction + job header line numbers
# (same awk patterns as test_verify_live_wiring.sh)
# ---------------------------------------------------------------------------

# Extract one job block: starts at the 2-space-indented job header (skipped so
# the range cannot self-close), ends at the next 2-space-indented job header.
job_block() {
  local file="$1" job="$2"
  awk -v job="^  $job:" '$0 ~ job {f=1;next} f&&/^  [a-z][a-z-]*:$/{f=0} f' "$file"
}

# File line number of a 2-space-indented job header (empty if absent).
job_line() {
  local file="$1" job="$2"
  awk -v job="^  $job:" '$0 ~ job {print NR; exit}' "$file"
}

# Extract a top-level YAML block: from `^key:` to the next top-level key.
top_block() {
  local file="$1" key="$2"
  awk -v key="^$key:" '$0 ~ key {f=1;next} f&&/^[a-z][a-z-]*:/{f=0} f' "$file"
}

# Extract one step block from within an already-extracted job block: starts
# at the 6-space `- name:` header, ends at the next 6-space `- ` item (or end
# of input). Literal substring match (index, not regex) so step names with
# parens/braces cannot break the pattern.
step_block() {
  local content="$1" name="$2"
  awk -v name="- name: $name" 'index($0, name) {f=1;next} f&&/^      - /{f=0} f' <<< "$content"
}

# ---------------------------------------------------------------------------
# Phase 0 — file presence (early exit: every later clause needs the file)
# ---------------------------------------------------------------------------

echo "== Phase 0: file presence =="

if [ ! -f "$RELEASE_FILE" ]; then
  ko "$RELEASE_FILE exists"
  echo ""
  echo "========================================="
  echo "  Results: $PASS passed, $FAIL failed"
  echo "========================================="
  exit 1
fi
ok "$RELEASE_FILE exists"

# ---------------------------------------------------------------------------
# Phase 1 — trigger pins (on-block extraction)
# ---------------------------------------------------------------------------

echo "== Phase 1: trigger pins =="

ON_BLOCK="$(top_block "$RELEASE_FILE" on || true)"

if grep -qF 'workflow_dispatch:' <<< "$ON_BLOCK"; then
  ok "workflow_dispatch trigger present (on-block pin)"
else
  ko "workflow_dispatch trigger present — missing from on: block"
fi

# One anchored pattern, not two independent greps: `tags:` and `v*` found
# separately could come from different keys (e.g. `branches:` + a comment).
# Anchored: the v* pattern must sit inside the tags: filter value itself
# (quoted or unquoted, flow list or plain).
if grep -qE 'tags:\s*\[?"?v\*' <<< "$ON_BLOCK"; then
  ok "tag trigger present: tags: v* (on-block pin)"
else
  ko "tag trigger present: tags: v* — no v* pattern inside the tags: filter in on: block"
fi

# ---------------------------------------------------------------------------
# Phase 2 — build job: matrix + toolchain + binary + tarball contract
# ---------------------------------------------------------------------------

echo "== Phase 2: build job pins =="

BUILD_BLOCK="$(job_block "$RELEASE_FILE" build || true)"
BUILD_LINE="$(job_line "$RELEASE_FILE" build || true)"
BUILD_BLOCK_CODE="$(grep -vE '^[[:space:]]*#' <<< "$BUILD_BLOCK" || true)"

# Matrix — all 4 targets (AC-2's uname→target mapping depends on each one).
TARGETS_OK=0
for target in x86_64-unknown-linux-gnu aarch64-unknown-linux-gnu x86_64-apple-darwin aarch64-apple-darwin; do
  grep -qF "$target" <<< "$BUILD_BLOCK" && TARGETS_OK=$((TARGETS_OK + 1))
done
if [ "$TARGETS_OK" -eq 4 ]; then
  ok "matrix covers 4 targets: linux x86_64/aarch64 + macOS x86_64/aarch64 (4/4)"
else
  ko "matrix covers 4 targets — only $TARGETS_OK/4 target triples found in build job"
fi

# Build command — release + locked + the workspace cli package + cross-target.
if grep -qF 'cargo build --release --locked' <<< "$BUILD_BLOCK_CODE" \
    && grep -qF -e '-p blendtutor-cli' <<< "$BUILD_BLOCK_CODE" \
    && grep -qF -e '--target ${{ matrix.target }}' <<< "$BUILD_BLOCK_CODE"; then
  ok "build runs cargo build --release --locked -p blendtutor-cli --target <matrix.target>"
else
  ko "build runs cargo build --release --locked -p blendtutor-cli --target <matrix.target> — missing"
fi

# Toolchain pinned via rust-toolchain.toml (repo convention: the rustup show
# pattern; the file is the single source of truth — no action or version here).
if grep -qF 'rustup show active-toolchain || rustup toolchain install' <<< "$BUILD_BLOCK_CODE"; then
  ok "toolchain pinned via rust-toolchain.toml (rustup show active-toolchain pattern)"
else
  ko "toolchain pinned via rust-toolchain.toml — 'rustup show active-toolchain || rustup toolchain install' missing from build job"
fi

if grep -qF 'dtolnay/rust-toolchain' "$RELEASE_FILE"; then
  ko "no dtolnay/rust-toolchain action (rust-toolchain.toml is the single source of truth)"
else
  ok "no dtolnay/rust-toolchain action (rust-toolchain.toml is the single source of truth)"
fi

# Binary staged from the target-triple dir (host/target mixup would pack the
# wrong architecture's binary into the tarball).
if grep -qF 'target/${{ matrix.target }}/release/blendtutor' <<< "$BUILD_BLOCK_CODE"; then
  ok "binary staged from target/<matrix.target>/release/blendtutor"
else
  ko "binary staged from target/<matrix.target>/release/blendtutor — missing"
fi

# Tar root layout (pinned HERE per the release.yml header contract): the
# tarball contains the bare `blendtutor` binary at its root — install.sh
# (AC-2) extracts the member by that exact path.
if grep -qF -e '-C dist blendtutor' <<< "$BUILD_BLOCK_CODE"; then
  ok "tar root layout: bare blendtutor binary at tarball root (-C dist blendtutor)"
else
  ko "tar root layout: bare blendtutor binary at tarball root — '-C dist blendtutor' missing from build job"
fi

# Artifact-name contract (consumed by AC-2 — pinned HERE): the tarball literal.
if grep -qF 'blendtutor-${{ github.ref_name }}-${{ matrix.target }}.tar.gz' <<< "$BUILD_BLOCK_CODE"; then
  ok "tarball contract: blendtutor-<tag>-<target>.tar.gz (exact literal in build job)"
else
  ko "tarball contract: blendtutor-<tag>-<target>.tar.gz — exact literal missing from build job"
fi

if grep -qF 'actions/upload-artifact' <<< "$BUILD_BLOCK_CODE"; then
  ok "build job uploads tarball artifact (actions/upload-artifact)"
else
  ko "build job uploads tarball artifact — actions/upload-artifact missing"
fi

# ---------------------------------------------------------------------------
# Phase 3 — release job: order, needs, gate, checksums, release name = tag
# ---------------------------------------------------------------------------

echo "== Phase 3: release job pins =="

RELEASE_BLOCK="$(job_block "$RELEASE_FILE" release || true)"
RELEASE_LINE="$(job_line "$RELEASE_FILE" release || true)"
RELEASE_BLOCK_CODE="$(grep -vE '^[[:space:]]*#' <<< "$RELEASE_BLOCK" || true)"

# The gh-release-create step block (upload args live here — a job-level grep
# cannot tell the Generate step's output filename from the create step's
# upload arg, so the upload arg is pinned in the step block itself).
CREATE_STEP_BLOCK="$(step_block "$RELEASE_BLOCK" "Create GitHub release (name = tag)" || true)"
CREATE_STEP_CODE="$(grep -vE '^[[:space:]]*#' <<< "$CREATE_STEP_BLOCK" || true)"

# Line-order pin: release declared AFTER the build block ends.
NEXT_AFTER_BUILD="$(awk -v bl="${BUILD_LINE:-0}" 'NR>bl && /^  [a-z][a-z-]*:$/{print NR; exit}' "$RELEASE_FILE" || true)"
if [ -n "$NEXT_AFTER_BUILD" ]; then
  BUILD_END=$((NEXT_AFTER_BUILD - 1))
else
  BUILD_END="$(wc -l < "$RELEASE_FILE" | tr -d ' ')"
fi
if [ -n "$RELEASE_LINE" ] && [ -n "$BUILD_LINE" ] && [ "$RELEASE_LINE" -gt "$BUILD_END" ]; then
  ok "release job declared after build block (line $RELEASE_LINE > build end $BUILD_END)"
else
  ko "release job declared after build block (release_line=$RELEASE_LINE build_line=$BUILD_LINE build_end=$BUILD_END)"
fi

if grep -qF 'needs: build' <<< "$RELEASE_BLOCK_CODE"; then
  ok "release job needs build"
else
  ko "release job needs build — missing"
fi

# Gate: the release job runs only for v* tag refs. Without the gate, a
# workflow_dispatch from a branch would create a junk tag named after the
# branch (gh release create makes the tag if absent).
if grep -qF "startsWith(github.ref, 'refs/tags/v')" <<< "$RELEASE_BLOCK_CODE"; then
  ok "release job gated to v* tag refs (workflow_dispatch builds without publishing)"
else
  ko "release job gated to v* tag refs — startsWith(github.ref, 'refs/tags/v') missing"
fi

if grep -qF 'actions/download-artifact' <<< "$RELEASE_BLOCK_CODE"; then
  ok "release job downloads build artifacts (actions/download-artifact)"
else
  ko "release job downloads build artifacts — actions/download-artifact missing"
fi

# Repo context: gh resolves the repo from GH_REPO env or git remotes under
# cwd — it ignores GITHUB_REPOSITORY entirely. The release job cd's into
# dist and runs gh release create; on a fresh runner without a checkout (or
# GH_REPO env) that fails "could not determine git repo" and the first v*
# tag push publishes nothing. docs.yml verify-live checks out before gh
# usage — repo convention.
if grep -qF 'actions/checkout' <<< "$RELEASE_BLOCK_CODE" \
    || grep -qF 'GH_REPO:' <<< "$RELEASE_BLOCK_CODE"; then
  ok "release job has repo context (actions/checkout or GH_REPO env) for gh release create"
else
  ko "release job has repo context — actions/checkout or GH_REPO env missing from release job (gh cannot resolve the repo)"
fi

# Checksums contract (consumed by AC-2 — pinned HERE). Generation (sha256sum)
# is pinned at job level; the upload arg is pinned in the gh-release-create
# step block — the Generate step writing the file does not put it on the
# release, so the literal must appear in the create step itself.
if grep -qF 'sha256sum' <<< "$RELEASE_BLOCK_CODE" \
    && grep -qF 'blendtutor-${{ github.ref_name }}-sha256sums.txt' <<< "$CREATE_STEP_CODE"; then
  ok "checksums contract: sha256sum generated + blendtutor-<tag>-sha256sums.txt uploaded (literal in gh-release-create step)"
else
  ko "checksums contract: sha256sum → blendtutor-<tag>-sha256sums.txt — literal missing from gh-release-create step"
fi

# Release upload args: the create step must upload the tarballs too —
# deleting the tarball glob would silently ship a checksums-only release.
if grep -qF 'blendtutor-*.tar.gz' <<< "$CREATE_STEP_CODE"; then
  ok "release uploads tarballs (blendtutor-*.tar.gz arg in gh-release-create step)"
else
  ko "release uploads tarballs — blendtutor-*.tar.gz arg missing from gh-release-create step"
fi

# Release name = tag (gh release create with the tag as name AND title).
if grep -qF 'gh release create' <<< "$RELEASE_BLOCK_CODE" \
    && grep -qF -e '--title "${{ github.ref_name }}"' <<< "$RELEASE_BLOCK_CODE"; then
  ok "release name = tag (gh release create --title <tag>)"
else
  ko "release name = tag — 'gh release create' or '--title <tag>' missing from release job"
fi

# ---------------------------------------------------------------------------
# Phase 4 — doctrine: least-privilege permissions + error-swallow bans
# ---------------------------------------------------------------------------

echo "== Phase 4: doctrine pins =="

PERMS_BLOCK="$(top_block "$RELEASE_FILE" permissions || true)"

if grep -qF 'contents: read' <<< "$PERMS_BLOCK"; then
  ok "workflow-level permissions: contents: read (least privilege)"
else
  ko "workflow-level permissions: contents: read — missing from top-level permissions block"
fi

if grep -qF 'contents: write' <<< "$RELEASE_BLOCK_CODE"; then
  ok "release job scoped contents: write (job-level, creates the release)"
else
  ko "release job scoped contents: write — missing from release job block"
fi

if grep -qF 'contents: write' <<< "$BUILD_BLOCK_CODE"; then
  ko "build job does NOT hold contents: write (least privilege)"
else
  ok "build job does NOT hold contents: write (least privilege)"
fi

# Error-swallow bans — file-wide by spec ("no continue-on-error/|| true
# anywhere"), over comment-stripped content so prose cannot false-positive.
WHOLE_CODE="$(grep -vE '^[[:space:]]*#' "$RELEASE_FILE" || true)"

if grep -qF 'continue-on-error' <<< "$WHOLE_CODE"; then
  ko "no continue-on-error anywhere in release.yml"
else
  ok "no continue-on-error anywhere in release.yml"
fi

if grep -qF '|| true' <<< "$WHOLE_CODE"; then
  ko "no || true anywhere in release.yml"
else
  ok "no || true anywhere in release.yml"
fi

if grep -qF 'if: always()' <<< "$WHOLE_CODE"; then
  ko "no if: always() anywhere in release.yml"
else
  ok "no if: always() anywhere in release.yml"
fi

# ---------------------------------------------------------------------------
# Phase 5 — ci.yml wiring (check job runs this test)
# ---------------------------------------------------------------------------

echo "== Phase 5: ci.yml wiring =="

CHECK_BLOCK="$(job_block "$CI_FILE" check || true)"
if grep -qF 'scripts/tests/test_release_yml.sh' <<< "$CHECK_BLOCK"; then
  ok "ci.yml check job runs test_release_yml.sh"
else
  ko "ci.yml check job runs test_release_yml.sh — not found in check job block"
fi

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

echo ""
echo "========================================="
echo "  Results: $PASS passed, $FAIL failed"
echo "========================================="

if [ "$FAIL" -gt 0 ]; then
  exit 1
fi

echo "All tests passed."
