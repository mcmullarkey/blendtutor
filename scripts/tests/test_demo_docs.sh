#!/usr/bin/env bash
# Executable spec for issue #157 — AC-5: README live demo URLs, what-works-where
# capability mapping, and the COI book-mode limitation (extend-in-place).
#
# Verifies the 11-clause predicate from .opencode/plans/github-pages-deploy/AC-5.md
# against README.md. Source-content pins only — NO network calls (live-URL
# runtime validity belongs to the AC-3/AC-4 rodney probes; a curl/HTTP-HEAD
# probe here would 404 before the AC-2 Pages deploy is live).
#
#   c1.  Both live URLs as exact literals with trailing slash, inside the demo
#        section (`### Demo book` → `## License`)
#   c2.  Book capabilities: Python-interactive claim + literal `static fallback`
#   c3.  Standalone capabilities: interactive R (webR) + interactive Python
#   c4.  COI book-mode limitation survives + names `type: book`
#   c5.  Book explicitly does NOT run R (regex; generic COI-doesn't-function
#        insufficient)
#   c6.  Pyodide accuracy guard, whole README (pyodide needs no COI)
#   c7.  No stale /examples/ conflation inside the demo section
#   c8.  Extend-don't-duplicate: 'COI does not function in Quarto' == 1 AND
#        'Book-mode limitation' == 1 (whole README)
#   c9.  Region pin: both live URLs at line >= 288 and < 342 (whole README)
#   c10. ADR-0015 pointer in README + file exists
#   c11. Distribution-doc pins survive (test_quarto_distribution.sh README
#        group): python3 -m http.server 8000 present; 'COI configuration'
#        absent; PANDOC_SCRIPT_FILE absent; `type: book` present; demo-book/
#        dir exists
#
# Usage: bash scripts/tests/test_demo_docs.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PASS=0
FAIL=0

ok() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
ko() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

README="README.md"
DEMO_BOOK_DIR="demo-book"
ADR_FILE="docs/adr/0015-opt-in-coi-cross-origin.md"

# Demo section scope: `### Demo book` through `## License` (exclusive).
DEMO_SECTION="$(awk '/^### Demo book/,/^## License/' "$README")"

# ---------------------------------------------------------------------------
# c1: Both live URLs exact literals, trailing slash pinned (demo section)
# ---------------------------------------------------------------------------
echo "== c1: live demo URLs =="
if printf '%s' "$DEMO_SECTION" | grep -qF 'https://mcmullarkey.github.io/blendtutor/demo-book/'; then
  ok "live demo-book URL literal present (trailing slash)"
else
  ko "live demo-book URL literal missing from demo section"
fi
if printf '%s' "$DEMO_SECTION" | grep -qF 'https://mcmullarkey.github.io/blendtutor/demo/'; then
  ok "live demo URL literal present (trailing slash)"
else
  ko "live demo URL literal missing from demo section"
fi

# ---------------------------------------------------------------------------
# c2: Book capabilities — Python interactive + static fallback (demo section)
# ---------------------------------------------------------------------------
echo "== c2: book capability mapping =="
if printf '%s' "$DEMO_SECTION" | grep -qiE 'python.*interactive|interactive.*python|fully interactive'; then
  ok "book states Python exercises run interactively"
else
  ko "book capability — no Python-interactive claim in demo section"
fi
if printf '%s' "$DEMO_SECTION" | grep -qF 'static fallback'; then
  ok "book states literal 'static fallback'"
else
  ko "book capability — literal 'static fallback' missing from demo section"
fi

# ---------------------------------------------------------------------------
# c3: Standalone capabilities — interactive R (webR) + interactive Python
# ---------------------------------------------------------------------------
echo "== c3: standalone capability mapping =="
if printf '%s' "$DEMO_SECTION" | grep -qiE 'r .*interactive.*webr|interactive.*r.*webr|r exercises? run interactively via webr'; then
  ok "standalone states interactive R via webR"
else
  ko "standalone capability — interactive R (webR) claim missing"
fi
if printf '%s' "$DEMO_SECTION" | grep -qiE 'python.*interactive|interactive.*python|python exercises? run interactively'; then
  ok "standalone states interactive Python"
else
  ko "standalone capability — interactive Python claim missing"
fi

# ---------------------------------------------------------------------------
# c4: COI book-mode limitation survives + names `type: book` (demo section)
# ---------------------------------------------------------------------------
echo "== c4: COI book-mode limitation =="
if printf '%s' "$DEMO_SECTION" | grep -qF 'type: book'; then
  ok "COI limitation names Quarto type: book"
else
  ko "COI limitation — 'type: book' not in demo section"
fi
if printf '%s' "$DEMO_SECTION" | grep -qE 'COI does not (function|take effect|work)'; then
  ok "COI limitation states COI does not function/take effect/work"
else
  ko "COI limitation — no COI-does-not-function claim in demo section"
fi

# ---------------------------------------------------------------------------
# c5: Book explicitly does NOT run R (demo section) — generic
#     COI-doesn't-function is insufficient
# ---------------------------------------------------------------------------
echo "== c5: R does not run in book =="
if printf '%s' "$DEMO_SECTION" | grep -E 'R exercises.*(don.?t|do not|cannot|not).*(run|execute)|R exercises.*unavailable|editors mount but execution' >/dev/null; then
  ok "book states R does not run / editors mount but execution unavailable"
else
  ko "book does NOT state R-does-not-run — generic COI-doesn't-function insufficient"
fi

# ---------------------------------------------------------------------------
# c6: Pyodide accuracy guard — whole README
# ---------------------------------------------------------------------------
echo "== c6: pyodide no-COI accuracy guard =="
if grep -qE 'pyodide.*(do not|doesn.?t|no).*COI|Pyodide-only.*do not need COI' "$README"; then
  ok "README states pyodide needs no COI"
else
  ko "pyodide accuracy — no pyodide-no-COI statement anywhere in README"
fi

# ---------------------------------------------------------------------------
# c7: No stale /examples/ conflation (demo section) — lines 143-146 examples
#     sites are Rust-binary deployments, a different thing
# ---------------------------------------------------------------------------
echo "== c7: no stale /examples/ conflation =="
if ! printf '%s' "$DEMO_SECTION" | grep -qF 'mcmullarkey.github.io/blendtutor/examples/'; then
  ok "demo section free of stale /examples/ site URLs"
else
  ko "demo section conflates stale /examples/ sites with the demos"
fi

# ---------------------------------------------------------------------------
# c8: Extend-don't-duplicate — count pins (whole README)
# ---------------------------------------------------------------------------
echo "== c8: extend-don't-duplicate count pins =="
count_coi_phrase="$(grep -cF 'COI does not function in Quarto' "$README" || true)"
if [ "$count_coi_phrase" -eq 1 ]; then
  ok "'COI does not function in Quarto' appears exactly once"
else
  ko "'COI does not function in Quarto' count != 1 (got $count_coi_phrase — duplicated or deleted)"
fi
count_book_heading="$(grep -c 'Book-mode limitation' "$README" || true)"
if [ "$count_book_heading" -eq 1 ]; then
  ok "'Book-mode limitation' appears exactly once"
else
  ko "'Book-mode limitation' count != 1 (got $count_book_heading — duplicated or deleted)"
fi

# ---------------------------------------------------------------------------
# c9: Region pin — both live URLs at line >= 288 and < 342 (whole README)
# ---------------------------------------------------------------------------
echo "== c9: demo section region pin =="
for url in 'https://mcmullarkey.github.io/blendtutor/demo-book/' 'https://mcmullarkey.github.io/blendtutor/demo/'; do
  line="$(grep -nF "$url" "$README" | cut -d: -f1 | head -n1 || true)"
  if [ -n "$line" ] && [ "$line" -ge 288 ] && [ "$line" -lt 342 ]; then
    ok "URL at line $line (288 <= line < 342): $url"
  else
    ko "URL line pin failed for $url (got: ${line:-missing})"
  fi
done

# ---------------------------------------------------------------------------
# c10: ADR-0015 pointer + file exists
# ---------------------------------------------------------------------------
echo "== c10: ADR-0015 pointer =="
if grep -qF 'docs/adr/0015-opt-in-coi-cross-origin.md' "$README"; then
  ok "README links docs/adr/0015-opt-in-coi-cross-origin.md"
else
  ko "README missing ADR-0015 pointer (docs/adr/0015-opt-in-coi-cross-origin.md)"
fi
if [ -f "$ADR_FILE" ]; then
  ok "ADR file exists ($ADR_FILE)"
else
  ko "ADR file missing: $ADR_FILE"
fi

# ---------------------------------------------------------------------------
# c11: Distribution-doc pins survive (test_quarto_distribution.sh README group)
# ---------------------------------------------------------------------------
echo "== c11: distribution-doc pins survive =="
if grep -qF 'python3 -m http.server 8000' "$README"; then
  ok "serve-over-HTTP instruction present (python3 -m http.server 8000)"
else
  ko "distribution pin — 'python3 -m http.server 8000' missing"
fi
if ! grep -qF 'COI configuration' "$README"; then
  ok "no overclaiming 'COI configuration' phrase"
else
  ko "distribution pin — 'COI configuration' present (overclaim)"
fi
if ! grep -qF 'PANDOC_SCRIPT_FILE' "$README"; then
  ok "no stale mechanism phrase 'PANDOC_SCRIPT_FILE'"
else
  ko "distribution pin — 'PANDOC_SCRIPT_FILE' present (stale mechanism)"
fi
if grep -qF 'type: book' "$README"; then
  ok "COI caveat names Quarto type: book (README-wide)"
else
  ko "distribution pin — 'type: book' not found"
fi
if [ -d "$DEMO_BOOK_DIR" ]; then
  ok "demo-book/ directory exists (relative link target)"
else
  ko "distribution pin — demo-book/ directory missing"
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
