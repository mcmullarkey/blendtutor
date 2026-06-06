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
test -s "$book_out/searchindex.js" \
  || { echo "docs: search index missing/empty ($book_out/searchindex.js)" >&2; exit 1; }
test -s "$book_out/searcher.js" \
  || { echo "docs: searcher missing/empty ($book_out/searcher.js)" >&2; exit 1; }

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
