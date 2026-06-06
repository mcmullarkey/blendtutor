#!/usr/bin/env bash
# Install repo-managed git hooks (issue #24, ADR-0002). Idempotent — re-run
# after pulling new hooks.
#
# Symlinks each file in .githooks/ into the repo's hooks directory so the
# version-controlled source stays the live hook. We deliberately do NOT set
# core.hooksPath: that would replace .git/hooks wholesale and disable other
# locally-installed hooks (e.g. the roborev post-commit gate). Symlinking
# coexists with them.
set -euo pipefail

root="$(git rev-parse --show-toplevel)"
src="$root/.githooks"
dst="$(git rev-parse --git-path hooks)" # absolute or repo-relative hooks dir
mkdir -p "$dst"

for hook in "$src"/*; do
  # Only executable files are hooks — skip docs or stray files so they are not
  # symlinked in as a broken hook.
  [ -x "$hook" ] || continue
  name="$(basename "$hook")"
  # Absolute target so the link always resolves: `git rev-parse --git-path
  # hooks` can return a hooks dir whose depth below the repo root varies
  # (gitfile or submodule layouts, a relocated common dir, or invocation from a
  # subdirectory), where a fixed-depth ../../ relative target would dangle. Each
  # clone runs this locally, so the machine-specific path is fine; re-run after
  # moving the repo.
  target="$src/$name"
  dest="$dst/$name"
  # Manage only our own link. If something is already there and it is not our
  # link — a real file, or a symlink pointing elsewhere (incl. a dangling one) —
  # leave it untouched with a warning. Our own link is refreshed (idempotent).
  if { [ -e "$dest" ] || [ -L "$dest" ]; } && [ "$(readlink "$dest" 2>/dev/null || true)" != "$target" ]; then
    echo "install-hooks: $dest already exists and is not our link — leaving it;" \
      "remove it to manage $name via .githooks" >&2
    continue
  fi
  ln -sf "$target" "$dest"
  echo "installed $name -> .githooks/$name"
done
