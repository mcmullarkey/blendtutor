#!/usr/bin/env bash
# Executable spec for issue #156 — verify-live post-deploy alarm job in
# docs.yml (AC-3 probe vs live URL, rodney COI wrapper).
#
# Verifies the 16-clause compound predicate from AC-4:
#   Phase 1 — workflow structural pins (clauses 1-13): docs.yml gains a
#     verify-live job AFTER the deploy block (line-order pin, clause 1) that
#     needs deploy (2); deploy declares job-level outputs: page_url from
#     steps.deployment.outputs (3 — step outputs do NOT cross job boundaries);
#     DEPLOYED_URL = needs.deploy.outputs.page_url + /demo/ suffix (4);
#     probe runs pages-live.js in LIVE mode (5); no continue-on-error (6);
#     no exit-code swallow on the run line (7); no if: always() (8);
#     actions/checkout present (9); astral-sh/setup-uv present (10);
#     actions/setup-node present (11); deploy does NOT need verify-live
#     (12 — alarm, not gate, inversion check). ALL docs.yml assertions use
#     awk job-block extraction, NEVER file-wide grep (13). ci.yml
#     quarto-render job runs this test (wiring, AC-2 clause-11 pattern).
#   Phase 2 — wrapper unit test (clause 14): scripts/rodney-chrome.sh exists +
#     executable, resolution order $REAL_CHROME → macOS Chrome →
#     /usr/bin/google-chrome → Chromium.orig; stub Chrome via
#     REAL_CHROME=/bin/echo and assert the wrapper strips
#     --single-process / --disable-site-isolation-trials / the whole
#     --disable-features=... arg while keeping --no-sandbox +
#     --disable-dev-shm-usage and the trailing about:blank.
#   Phase 3 — env + pin (clauses 15-16): pages-live.js wires ROD_CHROME_BIN
#     to the wrapper itself (harness-level setting satisfies clause 15 —
#     post-AC-3-fix at 9a50c01; job-level explicit set is optional
#     belt-and-suspenders); rodney pinned via uvx --from rodney==0.4.0.
#   Phase 4: none — live probe execution is the workflow's job at deploy
#     time; the first real push to main confirms it (manual, one-time).
#
# Negative cases (from the spec):
#   - verify-live job omitted → clause 1
#   - probe wired into deploy job (no separate alarm job) → clauses 1 + 12
#   - needs: build instead of needs: deploy → runs pre/concurrent with deploy,
#     page_url empty, probe 404s → clause 2
#   - deploy outputs: block missing → needs.deploy.outputs.page_url empty →
#     DEPLOYED_URL = demo/ relative → wrong root → clause 3
#   - DEPLOYED_URL = page_url verbatim (no /demo/) → mdBook root, no
#     .cm-editor → verify-live ALWAYS red → clause 4
#   - DEPLOYED_URL hardcoded instead of needs-output → breaks org rename /
#     branch preview → clause 4
#   - probe invoked as pages-live.js local → local static server, never
#     touches live URL → silent-pass → clause 5
#   - continue-on-error: true → alarm silenced (most dangerous sneaky-pass)
#     → clause 6
#   - uv run node ... || true → exit swallowed → clause 7
#   - if: always() → runs on failed/skipped deploy → false alarm every time
#     → clause 8
#   - no actions/checkout → probe file absent → infra failure masks demo
#     health → clause 9
#   - file-wide grep -q verify-live → passes if job commented out or needs:
#     deploy lands in the wrong job → clause 13 (this test uses awk
#     extraction everywhere)
#   - wrapper missing/not executable, OR ROD_CHROME_BIN unwired → rodney
#     0.4.0 launches Chrome with poison flags → crossOriginIsolated never
#     true → verify-live PERMANENTLY red from deploy #1 → clauses 14 + 15.
#     Sub-case: wrapper strips only the exact site-per-process value → the
#     combined --disable-features=... arg partially survives → Phase 2
#     full-arg-strip assertion kills it.
#
# Usage: bash scripts/tests/test_verify_live_wiring.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

PASS=0
FAIL=0

ok() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
ko() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

DOCS_FILE=".github/workflows/docs.yml"
CI_FILE=".github/workflows/ci.yml"
WRAPPER="scripts/rodney-chrome.sh"
PROBE="rodney-probes/pages-live.js"

# ---------------------------------------------------------------------------
# Helpers — job-block extraction + job header line numbers
# ---------------------------------------------------------------------------

# Extract one job block from a workflow: starts at the 2-space-indented job
# header (the header line itself is skipped so the range cannot self-close),
# ends at the next 2-space-indented job header. Mirrors the AC-1 cycle-2
# pattern — a file-wide grep is insufficient because a step that regressed to
# the wrong job (e.g. deploy) would still pass it (clause 13).
job_block() {
  local file="$1" job="$2"
  awk -v job="^  $job:" '$0 ~ job {f=1;next} f&&/^  [a-z][a-z-]*:$/{f=0} f' "$file"
}

# File line number of a 2-space-indented job header (empty if absent).
job_line() {
  local file="$1" job="$2"
  awk -v job="^  $job:" '$0 ~ job {print NR; exit}' "$file"
}

# ---------------------------------------------------------------------------
# Phase 1 — workflow structural pins (clauses 1-13)
# ---------------------------------------------------------------------------

echo "== Phase 1: structural pins (docs.yml verify-live job) =="

DEPLOY_BLOCK="$(job_block "$DOCS_FILE" deploy || true)"
VERIFY_BLOCK="$(job_block "$DOCS_FILE" verify-live || true)"
DEPLOY_LINE="$(job_line "$DOCS_FILE" deploy || true)"
VERIFY_LINE="$(job_line "$DOCS_FILE" verify-live || true)"

# Clause 1 — verify-live is a top-level job key declared AFTER the deploy
# block ends (block start line > deploy block end line).
NEXT_AFTER_DEPLOY="$(awk -v dl="${DEPLOY_LINE:-0}" 'NR>dl && /^  [a-z][a-z-]*:$/{print NR; exit}' "$DOCS_FILE" || true)"
if [ -n "$NEXT_AFTER_DEPLOY" ]; then
  DEPLOY_END=$((NEXT_AFTER_DEPLOY - 1))
else
  DEPLOY_END="$(wc -l < "$DOCS_FILE" | tr -d ' ')"
fi
if [ -n "$VERIFY_LINE" ] && [ -n "$DEPLOY_LINE" ] && [ "$VERIFY_LINE" -gt "$DEPLOY_END" ]; then
  ok "verify-live top-level job declared after deploy block (line $VERIFY_LINE > deploy end $DEPLOY_END)"
else
  ko "verify-live top-level job declared after deploy block (verify_line=$VERIFY_LINE deploy_line=$DEPLOY_LINE deploy_end=$DEPLOY_END)"
fi

# Clause 2 — needs: deploy INSIDE the verify-live block (awk-extracted, not
# file-wide grep).
if grep -qF 'needs: deploy' <<< "$VERIFY_BLOCK"; then
  ok "verify-live needs deploy (job-block pin)"
else
  ko "verify-live needs deploy (job-block pin) — missing"
fi

# Clause 3 — deploy job declares job-level outputs: page_url from
# steps.deployment.outputs (step outputs do NOT cross job boundaries).
if grep -qF 'outputs:' <<< "$DEPLOY_BLOCK" \
    && grep -qF 'page_url: ${{ steps.deployment.outputs.page_url }}' <<< "$DEPLOY_BLOCK"; then
  ok "deploy job declares outputs: page_url from steps.deployment.outputs (cross-job interface)"
else
  ko "deploy job declares outputs: page_url from steps.deployment.outputs — missing"
fi

# Clause 4 — DEPLOYED_URL references needs.deploy.outputs.page_url AND
# includes the demo path suffix (page_url yields site ROOT; harness expects
# demo dir root).
if grep -qF 'needs.deploy.outputs.page_url' <<< "$VERIFY_BLOCK" \
    && grep -qF 'demo/' <<< "$VERIFY_BLOCK"; then
  ok "DEPLOYED_URL = needs.deploy.outputs.page_url + /demo/ suffix"
else
  ko "DEPLOYED_URL = needs.deploy.outputs.page_url + /demo/ suffix — missing needs.deploy output ref or demo/ suffix"
fi

# Clause 5 — probe invocation in LIVE mode (NOT local).
if grep -qF 'pages-live.js live' <<< "$VERIFY_BLOCK"; then
  ok "probe runs pages-live.js in live mode"
else
  ko "probe runs pages-live.js in live mode — missing"
fi

# Clause 6 — no continue-on-error anywhere in the verify-live job block
# (a true value would silence the alarm).
if grep -qF 'continue-on-error' <<< "$VERIFY_BLOCK"; then
  ko "no continue-on-error in verify-live job (silent-alarm trap)"
else
  ok "no continue-on-error in verify-live job"
fi

# Clause 7 — probe run line swallows no exit codes.
SWALLOW=""
for pat in '|| true' '|| exit 0' '|| :' '; true' '&& true'; do
  if grep -qF "$pat" <<< "$VERIFY_BLOCK"; then
    SWALLOW="$SWALLOW '$pat'"
  fi
done
if [ -z "$SWALLOW" ]; then
  ok "probe run line swallows no exit codes (no || true / || exit 0 / || : / ; true / && true)"
else
  ko "probe run line swallows no exit codes — found:$SWALLOW"
fi

# Clause 8 — no if: always() (runs only when deploy succeeds; always() would
# false-alarm on failed/skipped deploys).
if grep -qF 'if: always()' <<< "$VERIFY_BLOCK"; then
  ko "no if: always() on verify-live job (runs only when deploy succeeds)"
else
  ok "no if: always() on verify-live job"
fi

# Clause 9 — actions/checkout present (deploy does NOT checkout; the probe
# file must exist on the runner).
if grep -qF 'actions/checkout' <<< "$VERIFY_BLOCK"; then
  ok "actions/checkout in verify-live job (probe file must exist on runner)"
else
  ko "actions/checkout in verify-live job — missing"
fi

# Clause 10 — astral-sh/setup-uv present (uvx rodney requires uv).
if grep -qF 'astral-sh/setup-uv' <<< "$VERIFY_BLOCK"; then
  ok "astral-sh/setup-uv in verify-live job (uvx rodney requires uv)"
else
  ko "astral-sh/setup-uv in verify-live job — missing"
fi

# Clause 11 — actions/setup-node present (defensive pin over the
# ubuntu-latest default node).
if grep -qF 'actions/setup-node' <<< "$VERIFY_BLOCK"; then
  ok "actions/setup-node in verify-live job (node for probe)"
else
  ko "actions/setup-node in verify-live job — missing"
fi

# Clause 12 — deploy job block does NOT declare needs: verify-live
# (post-deploy alarm, NOT a gate — inversion check).
if grep -qF 'needs: verify-live' <<< "$DEPLOY_BLOCK"; then
  ko "deploy does NOT need verify-live (post-deploy alarm, not a gate)"
else
  ok "deploy does NOT need verify-live (post-deploy alarm, not a gate)"
fi

# Clause 13 — all docs.yml assertions above use awk job-block extraction
# (this file), NOT file-wide grep. Satisfied by construction of this test.

echo "== Phase 1: structural pins (ci.yml quarto-render job) =="

QR_BLOCK="$(job_block "$CI_FILE" quarto-render || true)"
if grep -qF 'scripts/tests/test_verify_live_wiring.sh' <<< "$QR_BLOCK"; then
  ok "CI quarto-render job runs test_verify_live_wiring.sh"
else
  ko "CI quarto-render job runs test_verify_live_wiring.sh — not found in quarto-render job block"
fi

# ---------------------------------------------------------------------------
# Phase 2 — rodney COI wrapper (clause 14)
# ---------------------------------------------------------------------------

echo "== Phase 2: rodney COI wrapper (clause 14) =="

# Static: wrapper committed + executable.
if [ -f "$WRAPPER" ] && [ -x "$WRAPPER" ]; then
  ok "scripts/rodney-chrome.sh committed and executable"
else
  ko "scripts/rodney-chrome.sh committed and executable — missing or not executable"
fi

# Static: resolution order $REAL_CHROME → macOS Chrome →
# /usr/bin/google-chrome → Chromium.orig fallback.
RESOLVE_OK=0
for needle in 'REAL_CHROME' '/Applications/Google Chrome.app' '/usr/bin/google-chrome' 'Chromium.orig'; do
  grep -qF "$needle" "$WRAPPER" && RESOLVE_OK=$((RESOLVE_OK + 1))
done
if [ "$RESOLVE_OK" -eq 4 ]; then
  ok "wrapper resolution order: \$REAL_CHROME → macOS Chrome → /usr/bin/google-chrome → Chromium.orig (4/4)"
else
  ko "wrapper resolution order — only $RESOLVE_OK/4 resolution strings found in $WRAPPER"
fi

# Behavioral (no browser): stub Chrome via REAL_CHROME=/bin/echo; the wrapper
# must strip the 3 COI-breaking flag classes and exec echo with the rest.
WRAPPER_OUT="$(REAL_CHROME=/bin/echo bash "$WRAPPER" \
  --no-sandbox --single-process --disable-site-isolation-trials \
  --disable-features=site-per-process --disable-dev-shm-usage about:blank 2>&1)"

LEAKED=""
for flag in '--single-process' '--disable-site-isolation-trials' '--disable-features'; do
  if grep -qF -- "$flag" <<< "$WRAPPER_OUT"; then
    LEAKED="$LEAKED '$flag'"
  fi
done
if [ -z "$LEAKED" ]; then
  ok "wrapper strips --single-process / --disable-site-isolation-trials / whole --disable-features=* arg (argv: $WRAPPER_OUT)"
else
  ko "wrapper strips the 3 COI-breaking flag classes — leaked:$LEAKED (argv: $WRAPPER_OUT)"
fi

KEPT_OK=1
for needle in '--no-sandbox' '--disable-dev-shm-usage'; do
  grep -qF -- "$needle" <<< "$WRAPPER_OUT" || KEPT_OK=0
done
if [ "$KEPT_OK" -eq 1 ] && [[ "$WRAPPER_OUT" == *"about:blank" ]] \
    && [[ "${WRAPPER_OUT##* }" == "about:blank" ]]; then
  ok "wrapper keeps --no-sandbox --disable-dev-shm-usage + trailing about:blank (argv: $WRAPPER_OUT)"
else
  ko "wrapper keeps --no-sandbox --disable-dev-shm-usage + trailing about:blank (argv: $WRAPPER_OUT)"
fi

# ---------------------------------------------------------------------------
# Phase 3 — ROD_CHROME_BIN wiring + rodney pin (clauses 15-16)
# ---------------------------------------------------------------------------

echo "== Phase 3: ROD_CHROME_BIN wiring + rodney pin (clauses 15-16) =="

# Clause 15 — ROD_CHROME_BIN wired to the wrapper. The harness sets it
# itself (post-AC-3-fix at 9a50c01: pages-live.js sets ROD_CHROME_BIN to
# scripts/rodney-chrome.sh unless the caller overrode it) — harness-level
# setting satisfies this clause; job-level explicit set is optional
# belt-and-suspenders. Documented: grep of the HARNESS, not the job env.
if grep -qF 'ROD_CHROME_BIN' "$PROBE" \
    && grep -qF 'rodney-chrome.sh' "$PROBE"; then
  ok "ROD_CHROME_BIN wired to wrapper (harness sets it: pages-live.js → scripts/rodney-chrome.sh)"
else
  ko "ROD_CHROME_BIN wired to wrapper — pages-live.js must set ROD_CHROME_BIN to the wrapper"
fi

# Clause 16 — rodney pinned: uvx --from rodney==0.4.0 (prevents silent drift
# to a future rodney that changes flags/behavior).
if grep -qF -e '--from' "$PROBE" && grep -qF 'rodney==0.4.0' "$PROBE"; then
  ok "rodney pinned uvx --from rodney==0.4.0"
else
  ko "rodney pinned uvx --from rodney==0.4.0 — --from or rodney==0.4.0 missing in pages-live.js"
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
