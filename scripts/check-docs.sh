#!/usr/bin/env bash
# Build the combined documentation site locally and assert it satisfies the
# docs slice (#3): mdBook narrative + rustdoc API merged into one Pages artifact.
#
# This is the slice's executable spec — the same predicates CI enforces, runnable
# by hand:
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

echo "docs: OK — merged site at $book_out (book at /, API at /api)"
