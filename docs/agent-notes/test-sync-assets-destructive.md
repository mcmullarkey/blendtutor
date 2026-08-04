# Agent note: test_sync_assets.sh is destructive to uncommitted asset changes

**Lesson from fix-demo-visible-exercises (PR #149):** `scripts/tests/test_sync_assets.sh`
assertion 7 (idempotency) runs `bash scripts/sync-quarto-assets.sh` then checks
`git diff --exit-code -- _extensions/blendtutor/assets/`. On drift it executes:

```bash
git checkout -- "$DEST_DIR/" 2>/dev/null || true
```

which RESTORES `_extensions/blendtutor/assets/` from git HEAD — wiping ANY
uncommitted changes there (e.g. a new `exercise-runtime.js` edit).

**Consequences:** the failure path silently reverted `_extensions/blendtutor/assets/exercise-runtime.js`
twice during PR #149, requiring re-application from the vendored copy
`demo-book/_extensions/mcmullarkey/blendtutor/assets/`.

**Rule:** do NOT run `test_sync_assets.sh` with uncommitted changes in
`_extensions/blendtutor/assets/`. Commit asset changes first, or re-apply
afterward. The test passes only when the committed state matches what sync
regenerates (it is a committed-parity check, not a sandboxed check).

Sibling files (both must stay byte-identical when edited):
- `_extensions/blendtutor/assets/exercise-runtime.js` ↔ `demo-book/_extensions/mcmullarkey/blendtutor/assets/exercise-runtime.js`
- `_extensions/blendtutor/assets/styles.css` ↔ `demo-book/_extensions/mcmullarkey/blendtutor/assets/styles.css` (scoped output of `crates/core/assets/shared/styles.css` via sync-quarto-assets.sh)
- `_extensions/blendtutor/blendtutor.lua` ↔ `demo-book/_extensions/mcmullarkey/blendtutor/blendtutor.lua` (manual copy — no parity test)
