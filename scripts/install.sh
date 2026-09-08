#!/bin/sh
# uv-style installer for the blendtutor CLI (issue #224).
#
#   curl -LsSf https://raw.githubusercontent.com/mcmullarkey/blendtutor/main/scripts/install.sh | sh
#
# Works via `curl | sh` (stdin-piped): POSIX sh, no script-path assumptions
# (the shell's script-name variable is never referenced — the script is
# stdin, not a file), and no command reads stdin (a stdin-reading child would
# eat the remaining script bytes). All fetches go to files via `curl -o`.
#
# Asset contract — pinned by .github/workflows/release.yml and structurally
# enforced by scripts/tests/test_release_yml.sh (consumed HERE):
#   blendtutor-<tag>-<target>.tar.gz   — bare `blendtutor` binary at tar root
#   blendtutor-<tag>-sha256sums.txt    — sha256 of every release tarball
#
# Fail-closed doctrine: any fetch, parse, or checksum failure exits nonzero
# with a message — nothing is installed on a failed verification, and there
# is no `|| true` / continue-on-error anywhere.
#
# Install dir: $HOME/.local/bin by default, overridable via
# BLENDTUTOR_INSTALL_DIR (mirrors uv's UV_INSTALL_DIR).
#
# Tested by scripts/tests/test_install_sh.sh (stub curl/uname via PATH
# injection — zero network), wired into ci.yml's check job.

set -eu

REPO="mcmullarkey/blendtutor"

warn() { printf '%s\n' "$1" >&2; }
fail() { warn "error: $1"; exit 1; }

# --- install dir (BLENDTUTOR_INSTALL_DIR overrides ~/.local/bin) ------------

if [ -n "${BLENDTUTOR_INSTALL_DIR:-}" ]; then
  INSTALL_DIR="$BLENDTUTOR_INSTALL_DIR"
elif [ -n "${HOME:-}" ]; then
  INSTALL_DIR="$HOME/.local/bin"
else
  fail "cannot determine install dir: set BLENDTUTOR_INSTALL_DIR (HOME is unset)"
fi

# Strip a trailing slash so the PATH-hint case match doesn't false-positive
# (BLENDTUTOR_INSTALL_DIR=/opt/tools/ must match PATH entry /opt/tools) and
# installed-path messages don't double-slash.
INSTALL_DIR="${INSTALL_DIR%/}"

# --- OS/arch → release target triple (release.yml 4-target matrix) ----------

os_name=$(uname -s)
case "$os_name" in
  Linux) os=linux ;;
  Darwin) os=darwin ;;
  *) fail "unsupported operating system: $os_name (supported: Linux, macOS)" ;;
esac

arch_name=$(uname -m)
case "$arch_name" in
  x86_64|amd64) arch=x86_64 ;;
  aarch64|arm64) arch=aarch64 ;;
  *) fail "unsupported architecture: $arch_name (supported: x86_64, aarch64/arm64)" ;;
esac

case "$os-$arch" in
  linux-x86_64) target=x86_64-unknown-linux-gnu ;;
  linux-aarch64) target=aarch64-unknown-linux-gnu ;;
  darwin-x86_64) target=x86_64-apple-darwin ;;
  darwin-aarch64) target=aarch64-apple-darwin ;;
  *) fail "no release target for $os_name/$arch_name" ;;
esac

# --- scratch dir -------------------------------------------------------------

SCRATCH=$(mktemp -d) || fail "mktemp -d failed"
trap 'rm -rf "$SCRATCH"' EXIT INT TERM

# --- resolve the latest release tag ------------------------------------------

api_url="https://api.github.com/repos/$REPO/releases/latest"
curl -fsSL "$api_url" -o "$SCRATCH/release.json" \
  || fail "failed to fetch latest release info from $api_url"
tag=$(sed -n 's/.*"tag_name" *: *"\([^"]*\)".*/\1/p' "$SCRATCH/release.json" | head -n 1)
[ -n "$tag" ] || fail "could not parse tag_name from $api_url response"

# --- download tarball + checksums --------------------------------------------

base_url="https://github.com/$REPO/releases/download/$tag"
tarball="blendtutor-$tag-$target.tar.gz"
sums_file="blendtutor-$tag-sha256sums.txt"

curl -fsSL "$base_url/$tarball" -o "$SCRATCH/$tarball" \
  || fail "failed to download $base_url/$tarball"
curl -fsSL "$base_url/$sums_file" -o "$SCRATCH/$sums_file" \
  || fail "failed to download $base_url/$sums_file"

# --- verify checksum (fail-closed, before any extraction) --------------------

sha256_of() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | awk '{print $1}'
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$1" | awk '{print $1}'
  else
    return 1
  fi
}

expected_hash=$(grep -F "$tarball" "$SCRATCH/$sums_file" \
  | awk -v t="$tarball" '$2 == t {print $1}' | head -n 1)
[ -n "$expected_hash" ] \
  || fail "$tarball not listed in $sums_file — refusing to install an unverified tarball"

actual_hash=$(sha256_of "$SCRATCH/$tarball") \
  || fail "no sha256 tool available (need sha256sum or shasum)"

if [ "$actual_hash" != "$expected_hash" ]; then
  warn "checksum mismatch for $tarball:"
  warn "  expected: $expected_hash"
  warn "  actual:   $actual_hash"
  fail "checksum verification failed — refusing to install a corrupted tarball"
fi

# --- extract + install --------------------------------------------------------
# Tar-root contract (pinned in release.yml): the archive holds the bare
# `blendtutor` binary at its root — extract that member by its exact path.

tar -xzf "$SCRATCH/$tarball" -C "$SCRATCH" blendtutor \
  || fail "failed to extract blendtutor from $tarball (expected bare blendtutor member at tarball root)"

mkdir -p "$INSTALL_DIR" || fail "cannot create install dir $INSTALL_DIR"
mv "$SCRATCH/blendtutor" "$INSTALL_DIR/blendtutor" || fail "cannot move binary into $INSTALL_DIR"
chmod +x "$INSTALL_DIR/blendtutor" || fail "cannot chmod $INSTALL_DIR/blendtutor"

# --- PATH hint ----------------------------------------------------------------

case ":${PATH:-}:" in
  *":$INSTALL_DIR:"*) ;;
  *)
    warn ""
    warn "note: $INSTALL_DIR is not on your PATH."
    warn "add it to your shell profile, e.g.:"
    warn "  export PATH=\"$INSTALL_DIR:\$PATH\""
    ;;
esac

printf '%s\n' "blendtutor $tag installed to $INSTALL_DIR/blendtutor"
