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

# AC-2 (#152) — render demo-book + demo-standalone into the Pages artifact at
# /demo-book/ + /demo/, mirroring the /api + /examples/{r,python} nesting.
# Mirrors the docs.yml build steps (module-responsibility contract): demo-book
# renders into demo-book/_output/ (its _quarto.yml pins output-dir: _output),
# so the copy DOT-COPIES (trailing /.) to avoid a demo-book/_output/ layer;
# demo-standalone renders beside its source, so the copy is SELECTIVE
# (index.html + index_files/ + coi-serviceworker.js only — no .qmd sources,
# _quarto.yml, or _extensions/ leak). The COI post-process runs after render,
# before copy, so the assembled page serves ./coi-serviceworker.js at page root
# (SW scope covers the page, else R exercises are silent-dead on Pages).
echo "docs: rendering demos (quarto) and assembling /demo-book/ + /demo/ …"
quarto render demo-book --to html
quarto render demo-standalone --to html
bash scripts/fix-demo-coi-scope.sh demo-standalone
mkdir -p "$book_out/demo-book"
cp -R demo-book/_output/. "$book_out/demo-book/"
mkdir -p "$book_out/demo"
cp demo-standalone/index.html "$book_out/demo/"
cp -R demo-standalone/index_files "$book_out/demo/"
cp demo-standalone/coi-serviceworker.js "$book_out/demo/"
touch "$book_out/.nojekyll"

# AC-2 — assert the assembled layout (clause 9):
#   * demo-book/index.html exists at /demo-book/ root, NOT under an _output/
#     layer (dot-copy flattened the render output)
#   * demo/index.html exists with coi src EXACTLY ./coi-serviceworker.js and no
#     _extensions/ substring in the coi script tag (post-process applied AFTER
#     copy would keep the subdir src → SW scope = assets dir → webR dead)
#   * demo/coi-serviceworker.js exists, non-empty, byte-identical to the
#     vendored shim
#   * .nojekyll at artifact ROOT
test -f "$book_out/demo-book/index.html" \
  || { echo "docs: demo-book/index.html missing ($book_out/demo-book/)" >&2; exit 1; }
if [ -e "$book_out/demo-book/_output" ]; then
  echo "docs: demo-book assembled under an _output/ layer (bare cp, not dot-copy)" >&2
  exit 1
fi
test -f "$book_out/demo/index.html" \
  || { echo "docs: demo/index.html missing ($book_out/demo/)" >&2; exit 1; }
grep -qF 'src="./coi-serviceworker.js"' "$book_out/demo/index.html" \
  || { echo "docs: demo/index.html coi src is not exactly ./coi-serviceworker.js (SW scope trap)" >&2; exit 1; }
COI_TAG=$(grep -oE '<script[^>]*coi-serviceworker\.js[^>]*>' "$book_out/demo/index.html")
if grep -qF '_extensions/' <<< "$COI_TAG"; then
  echo "docs: demo/index.html coi script tag leaks _extensions/ (source-tree path in artifact)" >&2
  exit 1
fi
test -f "$book_out/demo/coi-serviceworker.js" \
  || { echo "docs: demo/coi-serviceworker.js missing" >&2; exit 1; }
test -s "$book_out/demo/coi-serviceworker.js" \
  || { echo "docs: demo/coi-serviceworker.js is empty" >&2; exit 1; }
DEMO_SHIM_SUM=$(cksum "$book_out/demo/coi-serviceworker.js" | cut -d' ' -f1)
DEMO_SHIM_SRC_SUM=$(cksum "_extensions/blendtutor/assets/coi-serviceworker.js" | cut -d' ' -f1)
if [ "$DEMO_SHIM_SUM" != "$DEMO_SHIM_SRC_SUM" ]; then
  echo "docs: demo/coi-serviceworker.js not byte-identical to vendored shim" >&2
  exit 1
fi
test -f "$book_out/.nojekyll" \
  || { echo "docs: .nojekyll missing at artifact root ($book_out/)" >&2; exit 1; }

# AC-2 — clause 10: the existing artifact survives assembly (no rm -rf clobber).
for fname in "$book_out/index.html" \
             "$book_out/api/blendtutor_core/index.html" \
             "$book_out/examples/r/index.html" \
             "$book_out/examples/python/index.html"; do
  test -f "$fname" \
    || { echo "docs: existing artifact clobbered by demo assembly — missing $fname" >&2; exit 1; }
done

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

# AC-2 (#152) — docs.yml contains the demo deploy steps (quarto setup, both
# renders, COI post-process, dot-copy demo-book, root .nojekyll) so the local
# mirror cannot silently diverge from CI.
for needle in \
  'quarto-dev/quarto-actions/setup@v2' \
  'quarto render demo-book' \
  'quarto render demo-standalone' \
  'scripts/fix-demo-coi-scope.sh demo-standalone' \
  'demo-book/_output/.' \
  'docs/book/book/demo' \
  'docs/book/book/.nojekyll'; do
  grep -q "$needle" "$workflow" \
    || { echo "docs: $workflow missing demo deploy step: $needle" >&2; exit 1; }
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

echo "docs: OK — merged site at $book_out (book at /, API at /api, examples at /examples/{r,python}, demos at /demo-book/ + /demo/)"
