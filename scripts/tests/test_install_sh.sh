#!/usr/bin/env bash
# Stub-based BDD test for scripts/install.sh (issue #224).
#
# Verifies the AC-2 predicate — a uv-style `curl | sh` installer whose asset
# naming byte-matches the release.yml contract pinned by test_release_yml.sh:
#
#   1. Happy path (linux x86_64, file mode): exit 0; binary installed to the
#      DEFAULT dir ($HOME/.local/bin) and executable; curl fetched exactly
#      (a) the latest-release API URL, (b) the tarball URL byte-matching
#      blendtutor-<tag>-<target>.tar.gz, (c) blendtutor-<tag>-sha256sums.txt.
#   2. curl|sh contract: `sh < install.sh` (stdin-piped, $0=sh) also installs
#      — no $0 assumptions, no stdin consumption by any child command.
#   3. Fail-closed: a tarball whose hash differs from the checksums file →
#      nonzero exit + message naming the checksum.
#   4. Unsupported OS (uname -s) / arch (uname -m) → nonzero + message.
#   5. uname mapping covers ALL 4 release targets, incl. macOS `arm64` →
#      aarch64-apple-darwin (Darwin reports arm64, not aarch64).
#   6. BLENDTUTOR_INSTALL_DIR override honored (binary lands there).
#   7. API fetch failure / tarball download failure / checksums download
#      failure → nonzero (fail-closed, no || true).
#   8. Tarball absent from the checksums file → fail closed.
#   9. PATH hint: printed when the install dir is not on PATH; absent when
#      it is — including when the dir is given with a trailing slash and the
#      PATH entry has none (no false hint).
#  10. Hygiene pins: `#!/bin/sh` shebang, `set -eu`, no `$0` anywhere (the
#      curl|sh contract), `mcmullarkey/blendtutor` repo literal present.
#  11. Environment fail arms: HOME unset/empty → nonzero + message naming
#      BLENDTUTOR_INSTALL_DIR; no sha256 tool on PATH → nonzero + "no sha256
#      tool" message (fail-closed before any install).
#  12. Recovery arms: shasum-only PATH (sha256sum absent, shasum present) →
#      install succeeds via the sha256_of fallback; HOME empty but
#      BLENDTUTOR_INSTALL_DIR set → installs to the override dir.
#  13. Self-wiring: ci.yml's check job invokes this test (a deleted step
#      fails this suite instead of silently dropping coverage — sibling
#      precedent: test_release_yml.sh Phase 5).
#
# Stub pattern follows scripts/tests/test_smevals_runner.sh: fake curl/uname
# via PATH injection + counter file proving invocation. The stub curl serves
# a canned release layout (a REAL tar.gz with the bare `blendtutor` member at
# its root, per the pinned tar-root contract) — zero network.
#
# Usage: bash scripts/tests/test_install_sh.sh
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

INSTALL_SH="scripts/install.sh"
PASS=0
FAIL=0

ok() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
ko() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

assert_eq() {
  local label="$1" expected="$2" actual="$3"
  if [ "$expected" = "$actual" ]; then
    ok "$label (got: $actual)"
  else
    ko "$label — expected [$expected], got [$actual]"
  fi
}

# Portable sha256 (macOS lacks GNU sha256sum; CI ubuntu has it).
sha256_of() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | awk '{print $1}'
  else
    shasum -a 256 "$1" | awk '{print $1}'
  fi
}

# ---------------------------------------------------------------------------
# Phase 0 — file presence (early exit: every later clause needs the file)
# ---------------------------------------------------------------------------

echo "== Phase 0: file presence =="

if [ ! -f "$INSTALL_SH" ]; then
  ko "$INSTALL_SH exists"
  echo ""
  echo "========================================="
  echo "  Results: $PASS passed, $FAIL failed"
  echo "========================================="
  exit 1
fi
ok "$INSTALL_SH exists"

# Self-wiring pin (sibling precedent: test_release_yml.sh Phase 5) — this
# suite is invoked by ci.yml's check job; if that step is ever deleted, this
# clause fails instead of the coverage silently dropping out of CI.
CI_FILE=".github/workflows/ci.yml"
if [ -f "$CI_FILE" ] && grep -qF 'bash scripts/tests/test_install_sh.sh' "$CI_FILE"; then
  ok "wired into ci.yml (check job runs this test)"
else
  ko "wired into ci.yml — 'bash scripts/tests/test_install_sh.sh' not found in $CI_FILE"
fi

# ---------------------------------------------------------------------------
# Setup: temp dir + stub curl/uname on PATH + canned release layout.
# ---------------------------------------------------------------------------

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

STUB_DIR="$TMPDIR/bin"
mkdir -p "$STUB_DIR"

# Counter file — proves the stub curl was invoked with the contracted URLs
# (predicate 1: no canned assumptions about what install.sh fetches).
COUNTER="$TMPDIR/curl-calls.txt"
: > "$COUNTER"

# Canned release layout — mirrors the release.yml contract: a tar.gz holding
# the bare `blendtutor` member at its root, plus a sha256sums file listing
# the tarball under ALL 4 target triples (one layout serves every
# uname-mapping scenario). Tag canned as v0.1.0.
LAYOUT="$TMPDIR/layout"
mkdir -p "$LAYOUT"
printf '#!/bin/sh\necho "blendtutor-stub-binary v0.1.0"\n' > "$LAYOUT/blendtutor"
chmod +x "$LAYOUT/blendtutor"
tar -czf "$LAYOUT/canned.tar.gz" -C "$LAYOUT" blendtutor
TARBALL_SHA=$(sha256_of "$LAYOUT/canned.tar.gz")

SUMS="$LAYOUT/blendtutor-v0.1.0-sha256sums.txt"
: > "$SUMS"
for t in x86_64-unknown-linux-gnu aarch64-unknown-linux-gnu x86_64-apple-darwin aarch64-apple-darwin; do
  printf '%s  blendtutor-v0.1.0-%s.tar.gz\n' "$TARBALL_SHA" "$t" >> "$SUMS"
done

# A checksums file that does NOT list the fetched tarball (predicate 8:
# fail closed when the release ships no entry for our platform).
printf '%s  blendtutor-v9.9.9-x86_64-unknown-linux-gnu.tar.gz\n' "$TARBALL_SHA" \
  > "$LAYOUT/partial-sums.txt"

# A VALID tarball with different bytes (predicate 3: checksum MISMATCH, not
# a tar-parse error — isolates the fail-closed verify arm).
mkdir -p "$LAYOUT/corruptsrc"
printf '#!/bin/sh\necho "TAMPERED"\n' > "$LAYOUT/corruptsrc/blendtutor"
tar -czf "$LAYOUT/corrupt.tar.gz" -C "$LAYOUT/corruptsrc" blendtutor

# Contract URLs (deliberate contract pins — these byte-match the
# release.yml asset-name contract pinned by test_release_yml.sh; the canned
# tag is v0.1.0).
API_URL="https://api.github.com/repos/mcmullarkey/blendtutor/releases/latest"
TARBALL_URL="https://github.com/mcmullarkey/blendtutor/releases/download/v0.1.0/blendtutor-v0.1.0-x86_64-unknown-linux-gnu.tar.gz"
SUMS_URL="https://github.com/mcmullarkey/blendtutor/releases/download/v0.1.0/blendtutor-v0.1.0-sha256sums.txt"

cat > "$STUB_DIR/curl" <<'STUB'
#!/usr/bin/env bash
# Stub curl — serves the canned release layout by URL pattern, records every
# invocation, and honors `-o <file>` (install.sh fetches to files). Modes:
#   ok (default) | api-fail | download-fail | sums-fail | sums-missing | corrupt
set -euo pipefail

COUNTER="${INSTALL_TEST_COUNTER:?INSTALL_TEST_COUNTER not set}"
LAYOUT="${INSTALL_TEST_LAYOUT:?INSTALL_TEST_LAYOUT not set}"

url=""
out="/dev/stdout"
prev=""
for arg in "$@"; do
  if [ "$prev" = "-o" ]; then
    out="$arg"
  fi
  case "$arg" in
    http*) [ -z "$url" ] && url="$arg" ;;
  esac
  prev="$arg"
done
if [ -z "$url" ]; then
  echo "stub-curl: no URL in args: $*" >&2
  exit 1
fi

echo "$url" >> "$COUNTER"

emit() {
  if [ "$out" = "/dev/stdout" ]; then
    cat
  else
    cat > "$out"
  fi
}

mode="${STUB_CURL_MODE:-ok}"

case "$url" in
  *"api.github.com/repos/mcmullarkey/blendtutor/releases/latest")
    if [ "$mode" = "api-fail" ]; then
      echo "stub-curl: simulated api failure" >&2
      exit 7
    fi
    printf '{"tag_name": "v0.1.0", "name": "v0.1.0", "assets": []}\n' | emit
    ;;
  *"releases/download/v0.1.0/"*)
    case "$url" in
      *-sha256sums.txt)
        if [ "$mode" = "sums-fail" ]; then
          echo "stub-curl: simulated checksums download failure" >&2
          exit 22
        fi
        if [ "$mode" = "sums-missing" ]; then
          cat "$LAYOUT/partial-sums.txt" | emit
        else
          cat "$LAYOUT/blendtutor-v0.1.0-sha256sums.txt" | emit
        fi
        ;;
      *.tar.gz)
        if [ "$mode" = "download-fail" ]; then
          echo "stub-curl: simulated tarball download failure" >&2
          exit 22
        fi
        if [ "$mode" = "corrupt" ]; then
          cat "$LAYOUT/corrupt.tar.gz" | emit
        else
          cat "$LAYOUT/canned.tar.gz" | emit
        fi
        ;;
      *)
        echo "stub-curl: unexpected download path: $url" >&2
        exit 1
        ;;
    esac
    ;;
  *)
    echo "stub-curl: unexpected URL: $url" >&2
    exit 1
    ;;
esac
STUB
chmod +x "$STUB_DIR/curl"

cat > "$STUB_DIR/uname" <<'STUB'
#!/usr/bin/env bash
# Stub uname — serves INSTALL_TEST_UNAME_S / INSTALL_TEST_UNAME_M
# (defaults mimic the CI runner: Linux x86_64).
set -euo pipefail
case "${1:-}" in
  -m) printf '%s\n' "${INSTALL_TEST_UNAME_M:-x86_64}" ;;
  *)  printf '%s\n' "${INSTALL_TEST_UNAME_S:-Linux}" ;;
esac
STUB
chmod +x "$STUB_DIR/uname"

TEST_HOME="$TMPDIR/home"
mkdir -p "$TEST_HOME"

OUT="$TMPDIR/out.txt"
ERR="$TMPDIR/err.txt"

# run_install <out> <err> [VAR=val ...] — clean-env run of install.sh in
# FILE mode (sh <path>). Sets INSTALL_STATUS. Stub curl/uname win PATH;
# real coreutils come after. STUB_CURL_MODE / INSTALL_TEST_* / env overrides
# pass through "$@".
run_install() {
  local out="$1" err="$2"
  shift 2
  INSTALL_STATUS=0
  env -i \
    PATH="$STUB_DIR:/usr/bin:/bin" \
    INSTALL_TEST_COUNTER="$COUNTER" \
    INSTALL_TEST_LAYOUT="$LAYOUT" \
    HOME="$TEST_HOME" \
    "$@" \
    sh "$INSTALL_SH" >"$out" 2>"$err" || INSTALL_STATUS=$?
}

# run_install_stdin <out> <err> [VAR=val ...] — same, but the script is
# PIPED VIA STDIN (`sh` with stdin = the script), the actual `curl | sh`
# execution shape: $0 is "sh" and any stdin-reading command would eat the
# remaining script bytes and truncate it.
run_install_stdin() {
  local out="$1" err="$2"
  shift 2
  INSTALL_STATUS=0
  env -i \
    PATH="$STUB_DIR:/usr/bin:/bin" \
    INSTALL_TEST_COUNTER="$COUNTER" \
    INSTALL_TEST_LAYOUT="$LAYOUT" \
    HOME="$TEST_HOME" \
    "$@" \
    sh >"$out" 2>"$err" <"$INSTALL_SH" || INSTALL_STATUS=$?
}

# ---------------------------------------------------------------------------
# Predicate 1 — happy path: default dir, executable binary, contract URLs.
# ---------------------------------------------------------------------------

echo "== Predicate 1: happy path (linux x86_64, default install dir) =="

: > "$COUNTER"
run_install "$OUT" "$ERR"
assert_eq "install.sh exits 0" "0" "$INSTALL_STATUS"

DEFAULT_BIN="$TEST_HOME/.local/bin/blendtutor"
if [ -x "$DEFAULT_BIN" ]; then
  ok "binary installed executable at \$HOME/.local/bin/blendtutor"
else
  ko "binary installed executable at \$HOME/.local/bin/blendtutor — got: $(ls -la "$TEST_HOME/.local/bin" 2>&1 || echo dir-missing)"
fi

assert_eq "installed binary runs (bare member extracted)" \
  "blendtutor-stub-binary v0.1.0" "$("$DEFAULT_BIN" 2>/dev/null || echo RUN-FAILED)"

assert_eq "curl called exactly 3 times (api + tarball + sums)" \
  "3" "$(wc -l < "$COUNTER" | tr -d ' ')"

if grep -qF "$API_URL" "$COUNTER" \
    && grep -qF "$TARBALL_URL" "$COUNTER" \
    && grep -qF "$SUMS_URL" "$COUNTER"; then
  ok "fetched contract URLs: latest API + blendtutor-<tag>-<target>.tar.gz + blendtutor-<tag>-sha256sums.txt"
else
  ko "fetched contract URLs — counter: $(cat "$COUNTER")"
fi

# ---------------------------------------------------------------------------
# Predicate 2 — curl|sh contract: stdin-piped execution installs too.
# ---------------------------------------------------------------------------

echo "== Predicate 2: curl|sh (stdin-piped, \$0=sh) =="

: > "$COUNTER"
run_install_stdin "$OUT" "$ERR"
assert_eq "stdin-piped install.sh exits 0" "0" "$INSTALL_STATUS"

STDIN_BIN="$TEST_HOME/.local/bin/blendtutor"
if [ -x "$STDIN_BIN" ]; then
  ok "stdin-piped run installed the binary (no \$0 assumptions, no stdin eating)"
else
  ko "stdin-piped run installed the binary — script truncated or \$0-dependent"
fi

# ---------------------------------------------------------------------------
# Predicate 3 — fail-closed on checksum mismatch.
# ---------------------------------------------------------------------------

echo "== Predicate 3: checksum mismatch fails closed =="

: > "$COUNTER"
# Own install dir: the happy path (P1) already populated the default dir, so
# the nothing-installed assertion must watch a fresh dir.
CORRUPT_DIR="$TMPDIR/corrupt-inst"
run_install "$OUT" "$ERR" "STUB_CURL_MODE=corrupt" "BLENDTUTOR_INSTALL_DIR=$CORRUPT_DIR"
if [ "$INSTALL_STATUS" -ne 0 ]; then
  ok "corrupt tarball → nonzero exit"
else
  ko "corrupt tarball → nonzero exit — sneaky-pass install!"
fi
if grep -qi 'checksum' "$ERR"; then
  ok "mismatch message names the checksum"
else
  ko "mismatch message names the checksum — got: $(cat "$ERR")"
fi
if [ ! -e "$CORRUPT_DIR/blendtutor" ]; then
  ok "nothing installed on checksum mismatch"
else
  ko "nothing installed on checksum mismatch — binary exists after failed verify"
fi

# ---------------------------------------------------------------------------
# Predicate 4 — unsupported OS / arch fail with a message.
# ---------------------------------------------------------------------------

echo "== Predicate 4: unsupported OS/arch =="

run_install "$OUT" "$ERR" "INSTALL_TEST_UNAME_S=Plan9"
if [ "$INSTALL_STATUS" -ne 0 ] && grep -qi 'unsupported' "$ERR"; then
  ok "unsupported OS (Plan9) → nonzero + message"
else
  ko "unsupported OS (Plan9) → nonzero + message — status: ${INSTALL_STATUS:-?}, err: $(cat "$ERR")"
fi

run_install "$OUT" "$ERR" "INSTALL_TEST_UNAME_M=sparc"
if [ "$INSTALL_STATUS" -ne 0 ] && grep -qi 'unsupported' "$ERR"; then
  ok "unsupported arch (sparc) → nonzero + message"
else
  ko "unsupported arch (sparc) → nonzero + message — status: ${INSTALL_STATUS:-?}, err: $(cat "$ERR")"
fi

# ---------------------------------------------------------------------------
# Predicate 5 — uname mapping covers all 4 release targets.
# ---------------------------------------------------------------------------

echo "== Predicate 5: uname → target-triple mapping (4 targets) =="

i=0
for combo in \
  "Linux:x86_64:x86_64-unknown-linux-gnu" \
  "Linux:aarch64:aarch64-unknown-linux-gnu" \
  "Darwin:x86_64:x86_64-apple-darwin" \
  "Darwin:arm64:aarch64-apple-darwin"; do
  i=$((i + 1))
  s="${combo%%:*}"
  rest="${combo#*:}"
  m="${rest%%:*}"
  t="${rest##*:}"
  : > "$COUNTER"
  run_install "$OUT" "$ERR" \
    "INSTALL_TEST_UNAME_S=$s" "INSTALL_TEST_UNAME_M=$m" \
    "BLENDTUTOR_INSTALL_DIR=$TMPDIR/inst-$i"
  assert_eq "mapping $s/$m → exit 0" "0" "$INSTALL_STATUS"
  if grep -qF "releases/download/v0.1.0/blendtutor-v0.1.0-$t.tar.gz" "$COUNTER"; then
    ok "mapping $s/$m → $t tarball fetched"
  else
    ko "mapping $s/$m → $t tarball fetched — counter: $(cat "$COUNTER")"
  fi
done

# ---------------------------------------------------------------------------
# Predicate 6 — BLENDTUTOR_INSTALL_DIR override.
# ---------------------------------------------------------------------------

echo "== Predicate 6: BLENDTUTOR_INSTALL_DIR override =="

: > "$COUNTER"
OVERRIDE_DIR="$TMPDIR/custom-tools"
run_install "$OUT" "$ERR" "BLENDTUTOR_INSTALL_DIR=$OVERRIDE_DIR"
assert_eq "override install exits 0" "0" "$INSTALL_STATUS"
if [ -x "$OVERRIDE_DIR/blendtutor" ]; then
  ok "binary installed at BLENDTUTOR_INSTALL_DIR"
else
  ko "binary installed at BLENDTUTOR_INSTALL_DIR — got: $(ls -la "$OVERRIDE_DIR" 2>&1 || echo dir-missing)"
fi

# ---------------------------------------------------------------------------
# Predicate 7 — fetch failures fail closed.
# ---------------------------------------------------------------------------

echo "== Predicate 7: fetch failures fail closed =="

for arm in api-fail download-fail sums-fail; do
  : > "$COUNTER"
  run_install "$OUT" "$ERR" "STUB_CURL_MODE=$arm"
  if [ "$INSTALL_STATUS" -ne 0 ]; then
    ok "$arm → nonzero exit"
  else
    ko "$arm → nonzero exit — error swallowed!"
  fi
  if [ -s "$ERR" ]; then
    ok "$arm → message on stderr"
  else
    ko "$arm → message on stderr — silent failure"
  fi
done

# ---------------------------------------------------------------------------
# Predicate 8 — tarball absent from checksums file fails closed.
# ---------------------------------------------------------------------------

echo "== Predicate 8: tarball not in checksums file =="

: > "$COUNTER"
run_install "$OUT" "$ERR" "STUB_CURL_MODE=sums-missing"
if [ "$INSTALL_STATUS" -ne 0 ]; then
  ok "unlisted tarball → nonzero exit"
else
  ko "unlisted tarball → nonzero exit — installed without verification!"
fi
if grep -qi 'not listed\|checksum' "$ERR"; then
  ok "unlisted-tarball message names the verification failure"
else
  ko "unlisted-tarball message names the verification failure — got: $(cat "$ERR")"
fi

# ---------------------------------------------------------------------------
# Predicate 9 — PATH hint on/off.
# ---------------------------------------------------------------------------

echo "== Predicate 9: PATH hint =="

: > "$COUNTER"
HINT_DIR="$TMPDIR/hint-bin"
run_install "$OUT" "$ERR" "BLENDTUTOR_INSTALL_DIR=$HINT_DIR"
assert_eq "hint scenario exits 0" "0" "$INSTALL_STATUS"
if grep -qF "not on your PATH" "$OUT" "$ERR"; then
  ok "PATH hint printed when install dir not on PATH"
else
  ko "PATH hint printed when install dir not on PATH — out: $(cat "$OUT"), err: $(cat "$ERR")"
fi

: > "$COUNTER"
HINT_DIR2="$TMPDIR/hint-bin2"
run_install "$OUT" "$ERR" \
  "BLENDTUTOR_INSTALL_DIR=$HINT_DIR2" \
  "PATH=$STUB_DIR:$HINT_DIR2:/usr/bin:/bin"
assert_eq "on-PATH scenario exits 0" "0" "$INSTALL_STATUS"
if grep -qF "not on your PATH" "$OUT" "$ERR"; then
  ko "no PATH hint when install dir IS on PATH — false hint: $(cat "$OUT") $(cat "$ERR")"
else
  ok "no PATH hint when install dir IS on PATH"
fi

# Trailing-slash install dir: BLENDTUTOR_INSTALL_DIR=/dir/ must match a PATH
# entry of /dir (the case match normalizes the trailing slash — a false
# "not on your PATH" hint here is the bug this clause pins).
: > "$COUNTER"
HINT_DIR3="$TMPDIR/hint-bin3"
run_install "$OUT" "$ERR" \
  "BLENDTUTOR_INSTALL_DIR=$HINT_DIR3/" \
  "PATH=$STUB_DIR:$HINT_DIR3:/usr/bin:/bin"
assert_eq "trailing-slash scenario exits 0" "0" "$INSTALL_STATUS"
if [ -x "$HINT_DIR3/blendtutor" ]; then
  ok "trailing-slash install dir still installs the binary"
else
  ko "trailing-slash install dir still installs the binary — got: $(ls -la "$HINT_DIR3" 2>&1 || echo dir-missing)"
fi
if grep -qF "not on your PATH" "$OUT" "$ERR"; then
  ko "no false PATH hint when trailing-slash dir IS on PATH — got: $(cat "$OUT") $(cat "$ERR")"
else
  ok "no false PATH hint when trailing-slash install dir IS on PATH"
fi

# ---------------------------------------------------------------------------
# Predicate 10 — hygiene pins (shebang, set -eu, no $0, repo literal).
# ---------------------------------------------------------------------------

echo "== Predicate 10: hygiene pins =="

assert_eq "shebang is #!/bin/sh (POSIX sh)" "#!/bin/sh" "$(sed -n '1p' "$INSTALL_SH")"

if grep -qE '^set -eu$' "$INSTALL_SH"; then
  ok "contains set -eu (fail-closed hygiene)"
else
  ko "contains set -eu — missing"
fi

if grep -qF '$0' "$INSTALL_SH"; then
  ko "no \$0 anywhere (curl|sh contract — script is stdin-piped, \$0 is sh)"
else
  ok "no \$0 anywhere (curl|sh contract)"
fi

if grep -qF 'mcmullarkey/blendtutor' "$INSTALL_SH"; then
  ok "repo literal mcmullarkey/blendtutor present"
else
  ko "repo literal mcmullarkey/blendtutor present — missing"
fi

# ---------------------------------------------------------------------------
# Predicate 11 — environment fail arms (fail-closed on a poisoned env).
# ---------------------------------------------------------------------------

echo "== Predicate 11: environment fail arms =="

# Arm 1 — HOME unset (empty): neither BLENDTUTOR_INSTALL_DIR nor HOME
# resolves, so install.sh must fail naming the override var. The run_install
# wrapper sets HOME="$TEST_HOME" before "$@", so the "HOME=" override wins
# (env applies later assignments last) and the elif arm sees an empty value.
: > "$COUNTER"
run_install "$OUT" "$ERR" "HOME="
if [ "$INSTALL_STATUS" -ne 0 ]; then
  ok "HOME unset/empty → nonzero exit"
else
  ko "HOME unset/empty → nonzero exit — installed with unknown dir!"
fi
if grep -qF 'BLENDTUTOR_INSTALL_DIR' "$ERR"; then
  ok "HOME-unset/empty message names BLENDTUTOR_INSTALL_DIR"
else
  ko "HOME-unset/empty message names BLENDTUTOR_INSTALL_DIR — got: $(cat "$ERR")"
fi

# Arm 2 — no sha256 tool: PATH has the stubs plus a stripped coreutils dir
# (everything from /usr/bin and /bin EXCEPT sha256sum/shasum), so every
# pre-verify step succeeds and the failure isolates sha256_of's no-tool arm.
# (PATH=$STUB_DIR alone would die earlier at mktemp — wrong arm.)
NOTOOL_BIN="$TMPDIR/notool-bin"
mkdir -p "$NOTOOL_BIN"
for src_dir in /usr/bin /bin; do
  [ -d "$src_dir" ] || continue
  for src in "$src_dir"/*; do
    [ -e "$src" ] || continue
    b=${src##*/}
    case "$b" in sha256sum|shasum) continue ;; esac
    ln -sf "$src" "$NOTOOL_BIN/$b" 2>/dev/null || :
  done
done

: > "$COUNTER"
NOTOOL_INST="$TMPDIR/notool-inst"
run_install "$OUT" "$ERR" "PATH=$STUB_DIR:$NOTOOL_BIN" "BLENDTUTOR_INSTALL_DIR=$NOTOOL_INST"
if [ "$INSTALL_STATUS" -ne 0 ]; then
  ok "no sha256 tool → nonzero exit"
else
  ko "no sha256 tool → nonzero exit — installed an unverified tarball!"
fi
if grep -qi 'sha256 tool' "$ERR"; then
  ok "no-sha256-tool message names the missing tool"
else
  ko "no-sha256-tool message names the missing tool — got: $(cat "$ERR")"
fi
if [ ! -e "$NOTOOL_INST/blendtutor" ]; then
  ok "nothing installed without a sha256 tool"
else
  ko "nothing installed without a sha256 tool — binary exists after failed verify"
fi

# ---------------------------------------------------------------------------
# Predicate 12 — recovery arms (degraded env still installs).
# ---------------------------------------------------------------------------

echo "== Predicate 12: recovery arms =="

# Arm 1 — shasum fallback: PATH has shasum but NOT sha256sum (same stripped-
# coreutils symlink pattern as the no-tool arm, minus the shasum exclusion),
# so sha256_of must fall back to shasum and the install succeeds. On macOS
# hosts sha256sum is absent system-wide, so this doubles as the native path;
# on Linux CI it exercises the actual fallback branch.
SHASUM_ONLY_BIN="$TMPDIR/shasum-only-bin"
mkdir -p "$SHASUM_ONLY_BIN"
for src_dir in /usr/bin /bin; do
  [ -d "$src_dir" ] || continue
  for src in "$src_dir"/*; do
    [ -e "$src" ] || continue
    b=${src##*/}
    case "$b" in sha256sum) continue ;; esac
    ln -sf "$src" "$SHASUM_ONLY_BIN/$b" 2>/dev/null || :
  done
done

: > "$COUNTER"
SHASUM_INST="$TMPDIR/shasum-inst"
run_install "$OUT" "$ERR" "PATH=$STUB_DIR:$SHASUM_ONLY_BIN" "BLENDTUTOR_INSTALL_DIR=$SHASUM_INST"
assert_eq "shasum-only PATH (no sha256sum) exits 0 (fallback)" "0" "$INSTALL_STATUS"
if [ -x "$SHASUM_INST/blendtutor" ]; then
  ok "binary installed via shasum fallback"
else
  ko "binary installed via shasum fallback — got: $(ls -la "$SHASUM_INST" 2>&1 || echo dir-missing)"
fi

# Arm 2 — HOME empty but BLENDTUTOR_INSTALL_DIR set: the advertised recovery
# path (install.sh checks the override var BEFORE HOME). The "HOME=" override
# wins over the wrapper's HOME="$TEST_HOME" (env applies later assignments
# last).
: > "$COUNTER"
NOHOME_INST="$TMPDIR/nohome-inst"
run_install "$OUT" "$ERR" "HOME=" "BLENDTUTOR_INSTALL_DIR=$NOHOME_INST"
assert_eq "HOME empty + BLENDTUTOR_INSTALL_DIR set exits 0" "0" "$INSTALL_STATUS"
if [ -x "$NOHOME_INST/blendtutor" ]; then
  ok "binary installed to override dir despite empty HOME"
else
  ko "binary installed to override dir despite empty HOME — got: $(ls -la "$NOHOME_INST" 2>&1 || echo dir-missing)"
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
