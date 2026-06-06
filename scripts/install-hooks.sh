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
  [ -e "$hook" ] || continue
  name="$(basename "$hook")"
  # Absolute target: correct regardless of where the hooks dir sits — a
  # worktree's hooks live under .git/worktrees/<name>/, not two levels below the
  # root, so a fixed-depth relative link would dangle there. Each clone runs
  # this locally, so the machine-specific path is fine; re-run after moving the
  # repo.
  ln -sf "$src/$name" "$dst/$name"
  echo "installed $name -> .githooks/$name"
done
