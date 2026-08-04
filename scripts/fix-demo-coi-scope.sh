#!/usr/bin/env bash
# Fix the COI service-worker scope for a standalone Quarto render (issue #150).
#
# WHY THIS SCRIPT EXISTS — register-default-scope mechanics (empirically
# verified in _extensions/blendtutor/assets/coi-serviceworker.js:103):
#
#   n.serviceWorker.register(window.document.currentScript.src).then(...)
#
# register() is called with a SINGLE argument — no {scope} option — so the
# browser assigns the DEFAULT scope: the directory of the service-worker
# script URL. The blendtutor filter's raw emission (resolve_asset_path,
# blendtutor.lua:95-128) points the coi <script> at
#
#   ../_extensions/blendtutor/assets/coi-serviceworker.js
#
# (repo-root-relative reference). Served on GitHub Pages that URL resolves to
# /_extensions/blendtutor/assets/coi-serviceworker.js → its default scope is
# /_extensions/blendtutor/assets/ — which NEVER covers /index.html. webR's
# SharedArrayBuffer then fails despite a green "coi script present" grep
# (the AC-5 book trap re-surfaced in standalone mode).
#
# FIX: the service worker must sit at the page root, because the page-root
# script URL yields the page-root default scope. This post-process:
#   1. rewrites every coi <script src> in the rendered HTML from the subdir
#      path to exactly ./coi-serviceworker.js (page-root relative), and
#   2. copies the vendored shim (_extensions/blendtutor/assets/
#      coi-serviceworker.js) to <dir>/coi-serviceworker.js.
#
# The blendtutor filter stays UNTOUCHED — the SW-scope seam is owned by this
# post-process (module responsibility, §4).
#
# IDEMPOTENT: rewriting src="./coi-serviceworker.js" to itself and copying the
# same shim over an identical file are no-ops. Running twice is safe.
#
# Usage: bash scripts/fix-demo-coi-scope.sh <render-dir>
#   e.g. bash scripts/fix-demo-coi-scope.sh demo-standalone
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

if [ "$#" -ne 1 ]; then
  echo "usage: $0 <render-dir>" >&2
  exit 2
fi

DIR="$1"
HTML_FILE="$DIR/index.html"
SHIM_SRC="_extensions/blendtutor/assets/coi-serviceworker.js"
SHIM_DST="$DIR/coi-serviceworker.js"

if [ ! -f "$HTML_FILE" ]; then
  echo "error: rendered HTML not found: $HTML_FILE" >&2
  exit 1
fi

if [ ! -f "$SHIM_SRC" ]; then
  echo "error: vendored shim not found: $SHIM_SRC" >&2
  exit 1
fi

# 1. Rewrite the coi script src to the page-root-relative path. The raw filter
#    emission is the subdir path (../_extensions/...); rewrite ANY coi src
#    pointing at a coi-serviceworker.js outside the page root. sed in-place
#    keeps the file writable while preserving all other bytes (idempotent:
#    ./coi-serviceworker.js does not match the pattern).
sed -i '' \
  -e 's#src="[^"]*/coi-serviceworker.js"#src="./coi-serviceworker.js"#g' \
  "$HTML_FILE" 2>/dev/null || \
sed -i \
  -e 's#src="[^"]*/coi-serviceworker.js"#src="./coi-serviceworker.js"#g' \
  "$HTML_FILE"

# 2. Copy the vendored shim to the page root so the page-root script URL
#    yields the page-root default scope. Idempotent: cp over an identical
#    file is a no-op.
cp "$SHIM_SRC" "$SHIM_DST"

echo "fixed COI scope for $DIR: src -> ./coi-serviceworker.js, shim -> $SHIM_DST"
