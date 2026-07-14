#!/usr/bin/env bash
# Build the combined documentation site locally and assert it satisfies the
# docs slice (#3) and the example-sites slice (#76): mdBook narrative + rustdoc
# API + example course sites (webr + pyodide) merged into one Pages artifact.
#
# This is the slice's executable spec — the same predicates CI enforces,
# runnable by hand:
#   scripts/check-docs.sh
#
# It mirrors .github/workflows/docs.yml's build + assemble steps. CI keeps those
# commands inline (rather than calling this script) so the workflow reads
# standalone and the deploy pipeline is greppable; this script is the local
# counterpart that also asserts the output predicates the workflow trusts.
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

book_out="docs/book/book"

# AC2 — rustdoc API docs build with zero warnings. RUSTDOCFLAGS="-D warnings"
# turns any rustdoc warning (e.g. a broken intra-doc link) into a hard failure;
# undocumented public items are caught by the crate's own #![deny(missing_docs)].
echo "docs: building API reference (rustdoc, -D warnings) …"
RUSTDOCFLAGS="-D warnings" cargo doc --no-deps
test -f target/doc/blendtutor_core/index.html \
  || { echo "docs: missing target/doc/blendtutor_core/index.html" >&2; exit 1; }

# AC1 — mdBook narrative site builds with a working search index.
echo "docs: building narrative site (mdBook) …"
mdbook build docs/book

# mdBook 0.5 content-hashes static asset filenames for cache-busting, so the
# search artifacts ship as searchindex-<hash>.js / searcher-<hash>.js, not bare
# names. Match by glob: the AC's intent is a working client-side search index,
# and disabling search emits neither file, so this still fails closed.
require_nonempty_glob() {
  local label="$1" pattern="$2" first
  first="$(find "$book_out" -maxdepth 1 -name "$pattern" -size +0c -print -quit)"
  [ -n "$first" ] \
    || { echo "docs: $label missing/empty ($book_out/$pattern)" >&2; exit 1; }
}
require_nonempty_glob "search index" 'searchindex*.js'
require_nonempty_glob "searcher" 'searcher*.js'

# AC3 (local proxy) — assemble one artifact dir: nest the rustdoc tree under the
# book output at /api so a single deploy serves the book at / and the API at /api.
echo "docs: assembling merged site ($book_out + /api) …"
rm -rf "$book_out/api"
cp -R target/doc "$book_out/api"
test -f "$book_out/index.html" \
  || { echo "docs: book index missing ($book_out/index.html)" >&2; exit 1; }
test -f "$book_out/api/blendtutor_core/index.html" \
  || { echo "docs: API index not nested under /api" >&2; exit 1; }

# AC-6 (#76) — build example course sites (webr + pyodide) alongside the docs.
# Mirrors the two `cargo run` steps in docs.yml so the local check exercises the
# same build path CI uses. Each site nests into the Pages artifact at
# $book_out/examples/{r,python}/, mirroring the /api nesting above.
echo "docs: building example sites (webr + pyodide) …"
cargo run --release -p blendtutor-cli -- build examples/write-less-code-r \
  --target webr -o "$book_out/examples/r"
cargo run --release -p blendtutor-cli -- build examples/write-less-code-python \
  --target pyodide -o "$book_out/examples/python"

# AC-6 — assert each example site has the required files: index.html,
# lesson-runner.js, lessons/0.json (at least one lesson built), eval-results.html.
for target_dir in "$book_out/examples/r" "$book_out/examples/python"; do
  for fname in index.html lesson-runner.js eval-results.html; do
    test -f "$target_dir/$fname" \
      || { echo "docs: missing $target_dir/$fname" >&2; exit 1; }
  done
  test -f "$target_dir/lessons/0.json" \
    || { echo "docs: missing $target_dir/lessons/0.json (no lessons built)" >&2; exit 1; }
  # Both eval-results.html must carry the validated marker (AC-5 eval reports).
  grep -q 'data-eval-status="validated"' "$target_dir/eval-results.html" \
    || { echo "docs: $target_dir/eval-results.html missing validated marker" >&2; exit 1; }
done

# AC-6 — cross-target boot check: R site boots webR (not pyodide), Python site
# boots Pyodide (not webr). Catches copy-paste duplication where both sites ship
# the same runtime adapter.
grep -qi "webr" "$book_out/examples/r/lesson-runner.js" \
  || { echo "docs: R lesson-runner.js missing 'webr' (cross-target boot check)" >&2; exit 1; }
grep -qi "pyodide" "$book_out/examples/python/lesson-runner.js" \
  || { echo "docs: Python lesson-runner.js missing 'pyodide' (cross-target boot check)" >&2; exit 1; }

# AC-6 — failure propagation: a build with a missing course path must fail.
# set -euo pipefail ensures cargo's non-zero exit propagates; this negative test
# pins that contract so a future change that swallows the exit code fails here.
echo "docs: verifying failure propagation (missing course path) …"
if cargo run --release -p blendtutor-cli -- build /nonexistent/course \
  --target webr -o /tmp/bt-check-docs-fail 2>/dev/null; then
  echo "docs: build with missing course path should have failed" >&2
  exit 1
fi

# AC3 (local proxy) — the deploy workflow wires both builds into a Pages deploy.
# The live deploy leg is CI-only; here we assert the workflow is present and
# references each required step so a missing-deploy regression fails locally.
workflow=".github/workflows/docs.yml"
test -f "$workflow" \
  || { echo "docs: deploy workflow missing ($workflow)" >&2; exit 1; }
for needle in 'mdbook build' 'cargo doc --no-deps' \
  'actions/upload-pages-artifact' 'actions/deploy-pages'; do
  grep -q "$needle" "$workflow" \
    || { echo "docs: $workflow missing required step: $needle" >&2; exit 1; }
done

# AC-6 — docs.yml contains both cargo run build commands with correct
# target/path pairing (webr→r, pyodide→python).
for needle in \
  'build examples/write-less-code-r --target webr' \
  'build examples/write-less-code-python --target pyodide'; do
  grep -q "$needle" "$workflow" \
    || { echo "docs: $workflow missing build command: $needle" >&2; exit 1; }
done

# AC-6 — README.md links to both example sites.
for needle in 'examples/r/' 'examples/python/'; do
  grep -q "$needle" README.md \
    || { echo "docs: README.md missing link to $needle" >&2; exit 1; }
done

# AC-6 — SUMMARY.md links to the examples page.
grep -q 'examples' docs/book/src/SUMMARY.md \
  || { echo "docs: SUMMARY.md missing examples page" >&2; exit 1; }

# AC-6 — built mdBook HTML contains rendered links (not just source). mdBook
# renders docs/book/src/examples.md → docs/book/book/examples.html; the rendered
# HTML must contain the example-site links, not just the markdown source.
test -f "$book_out/examples.html" \
  || { echo "docs: built mdBook missing examples.html (page not in SUMMARY.md?)" >&2; exit 1; }
grep -q 'examples/r/' "$book_out/examples.html" \
  || { echo "docs: built mdBook examples.html missing rendered link to examples/r/" >&2; exit 1; }
grep -q 'examples/python/' "$book_out/examples.html" \
  || { echo "docs: built mdBook examples.html missing rendered link to examples/python/" >&2; exit 1; }

echo "docs: OK — merged site at $book_out (book at /, API at /api, examples at /examples/{r,python})"
