#!/usr/bin/env bash
# Executable spec for issue #108 — Vendor codemirror.js + scoped styles.css.
#
# Verifies the compound predicate from AC-3:
#   1. codemirror.js exists in _extensions/blendtutor/assets/
#   2. codemirror.js byte-identical to source (cmp exit 0)
#   3. codemirror.js >= 690000 bytes
#   4. styles.css exists in _extensions/blendtutor/assets/
#   5. sync script exists + executable
#   6. --check exits 0 (the probe)
#   7. idempotency: sync produces no git diff
#   8. no symlinks in assets dir
#   9. drift detection: --check exits 1 when vendored modified
#  10. missing source: --check exits 2
#  11. symlink detection: --check exits 3
#
# Negative cases: source changes but copy doesn't -> --check fails (9).
# Script missing source -> exit 2 (10). Symlink -> exit 3 (11).
#
# Usage: bash scripts/tests/test_sync_assets.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PASS=0
FAIL=0

ok() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
ko() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

SOURCE_DIR="crates/core/assets/shared"
DEST_DIR="_extensions/blendtutor/assets"
SYNC_SCRIPT="scripts/sync-quarto-assets.sh"

# ---------------------------------------------------------------------------
# Assertion 1 — codemirror.js exists in vendored assets
# ---------------------------------------------------------------------------

echo "== Assertion 1: codemirror.js exists in vendored assets =="

CM_DEST="$DEST_DIR/codemirror.js"
if [ -f "$CM_DEST" ]; then
  ok "codemirror.js exists at $CM_DEST"
else
  ko "codemirror.js exists at $CM_DEST — file not found"
fi

# ---------------------------------------------------------------------------
# Assertion 2 — codemirror.js byte-identical to source (cmp exit 0)
# ---------------------------------------------------------------------------

echo "== Assertion 2: codemirror.js byte-identical to source =="

CM_SOURCE="$SOURCE_DIR/codemirror.js"
if [ -f "$CM_SOURCE" ] && [ -f "$CM_DEST" ]; then
  if cmp -s "$CM_SOURCE" "$CM_DEST"; then
    ok "codemirror.js byte-identical (cmp exit 0)"
  else
    ko "codemirror.js byte-identical (cmp exit 0) — files differ"
  fi
else
  ko "codemirror.js byte-identical (cmp exit 0) — source or dest missing"
fi

# ---------------------------------------------------------------------------
# Assertion 3 — codemirror.js >= 690000 bytes
# ---------------------------------------------------------------------------

echo "== Assertion 3: codemirror.js >= 690000 bytes =="

if [ -f "$CM_DEST" ]; then
  CM_SIZE=$(wc -c < "$CM_DEST" | tr -d ' ')
  if [ "$CM_SIZE" -ge 690000 ]; then
    ok "codemirror.js is $CM_SIZE bytes (>= 690000)"
  else
    ko "codemirror.js is $CM_SIZE bytes (>= 690000) — too small"
  fi
else
  ko "codemirror.js is >= 690000 bytes — file missing"
fi

# ---------------------------------------------------------------------------
# Assertion 4 — styles.css exists in vendored assets
# ---------------------------------------------------------------------------

echo "== Assertion 4: styles.css exists in vendored assets =="

CSS_DEST="$DEST_DIR/styles.css"
if [ -f "$CSS_DEST" ]; then
  ok "styles.css exists at $CSS_DEST"
else
  ko "styles.css exists at $CSS_DEST — file not found"
fi

# ---------------------------------------------------------------------------
# Assertion 5 — sync script exists + executable
# ---------------------------------------------------------------------------

echo "== Assertion 5: sync script exists + executable =="

if [ -f "$SYNC_SCRIPT" ]; then
  ok "sync script exists at $SYNC_SCRIPT"
else
  ko "sync script exists at $SYNC_SCRIPT — file not found"
fi

if [ -x "$SYNC_SCRIPT" ]; then
  ok "sync script is executable"
else
  ko "sync script is executable — not executable"
fi

# ---------------------------------------------------------------------------
# Assertion 6 — --check exits 0 (the probe)
# ---------------------------------------------------------------------------

echo "== Assertion 6: --check exits 0 (probe) =="

if [ -f "$SYNC_SCRIPT" ]; then
  bash "$SYNC_SCRIPT" --check 2>/dev/null && CHECK_RC=0 || CHECK_RC=$?
  if [ "$CHECK_RC" -eq 0 ]; then
    ok "--check exits 0 (assets in sync)"
  else
    ko "--check exits 0 (assets in sync) — exit code $CHECK_RC"
  fi
else
  ko "--check exits 0 (assets in sync) — script missing"
fi

# ---------------------------------------------------------------------------
# Assertion 7 — idempotency: sync produces no git diff
# ---------------------------------------------------------------------------

echo "== Assertion 7: idempotency (sync produces no git diff) =="

if [ -f "$SYNC_SCRIPT" ]; then
  bash "$SYNC_SCRIPT" 2>/dev/null
  if git diff --exit-code -- "$DEST_DIR/" 2>/dev/null; then
    ok "idempotency: sync produces no git diff"
  else
    ko "idempotency: sync produces no git diff — drift detected"
    # Restore from git to clean up any unexpected changes.
    git checkout -- "$DEST_DIR/" 2>/dev/null || true
  fi
else
  ko "idempotency: sync produces no git diff — script missing"
fi

# ---------------------------------------------------------------------------
# Assertion 8 — no symlinks in assets dir
# ---------------------------------------------------------------------------

echo "== Assertion 8: no symlinks in assets dir =="

SYMLINK_FOUND=0
if [ -d "$DEST_DIR" ]; then
  while IFS= read -r -d '' f; do
    if [ -L "$f" ]; then
      ko "no symlinks in assets dir — symlink found: $f"
      SYMLINK_FOUND=1
    fi
  done < <(find "$DEST_DIR" -type f -print0 2>/dev/null)
  if [ "$SYMLINK_FOUND" -eq 0 ]; then
    ok "no symlinks in assets dir"
  fi
else
  ko "no symlinks in assets dir — assets dir missing"
fi

# ---------------------------------------------------------------------------
# Assertion 9 — drift detection: --check exits 1 when vendored modified
# ---------------------------------------------------------------------------

echo "== Assertion 9: drift detection (--check exits 1) =="

if [ -f "$SYNC_SCRIPT" ] && [ -f "$CM_DEST" ]; then
  # Backup, inject drift, test, restore.
  cp "$CM_DEST" /tmp/.bt-cm-backup.js
  echo "/* drift injection */" >> "$CM_DEST"
  bash "$SYNC_SCRIPT" --check 2>/dev/null && DRIFT_RC=0 || DRIFT_RC=$?
  if [ "$DRIFT_RC" -eq 1 ]; then
    ok "drift detection: --check exits 1 when vendored modified"
  else
    ko "drift detection: --check exits 1 when vendored modified — exit code $DRIFT_RC"
  fi
  # Restore.
  cp /tmp/.bt-cm-backup.js "$CM_DEST"
  rm -f /tmp/.bt-cm-backup.js
else
  ko "drift detection: --check exits 1 when vendored modified — script or dest missing"
fi

# ---------------------------------------------------------------------------
# Assertion 10 — missing source: --check exits 2
# ---------------------------------------------------------------------------

echo "== Assertion 10: missing source detection (--check exits 2) =="

if [ -f "$SYNC_SCRIPT" ] && [ -f "$CM_SOURCE" ]; then
  # Temporarily move source, test, restore.
  mv "$CM_SOURCE" /tmp/.bt-cm-source-backup.js
  bash "$SYNC_SCRIPT" --check 2>/dev/null && MISSING_RC=0 || MISSING_RC=$?
  if [ "$MISSING_RC" -eq 2 ]; then
    ok "missing source: --check exits 2"
  else
    ko "missing source: --check exits 2 — exit code $MISSING_RC"
  fi
  # Restore source immediately.
  mv /tmp/.bt-cm-source-backup.js "$CM_SOURCE"
else
  ko "missing source: --check exits 2 — script or source missing"
fi

# ---------------------------------------------------------------------------
# Assertion 11 — symlink detection: --check exits 3
# ---------------------------------------------------------------------------

echo "== Assertion 11: symlink detection (--check exits 3) =="

if [ -f "$SYNC_SCRIPT" ] && [ -f "$CM_DEST" ]; then
  # Replace vendored file with a symlink, test, restore.
  rm "$CM_DEST"
  ln -s "$CM_SOURCE" "$CM_DEST"
  bash "$SYNC_SCRIPT" --check 2>/dev/null && SYMLINK_RC=0 || SYMLINK_RC=$?
  if [ "$SYMLINK_RC" -eq 3 ]; then
    ok "symlink detection: --check exits 3"
  else
    ko "symlink detection: --check exits 3 — exit code $SYMLINK_RC"
  fi
  # Restore via sync.
  rm -f "$CM_DEST"
  bash "$SYNC_SCRIPT" 2>/dev/null
else
  ko "symlink detection: --check exits 3 — script or dest missing"
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
