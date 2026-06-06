# ADR-0002: Git hook mechanism — committed `.githooks/` linked into `.git/hooks`

- Status: Accepted
- Date: 2026-06-06

## Context

Issue #24 adds a blocking **pre-push** gate that runs `cargo mutants --in-diff`
on a branch's changed `core` lines (see ADR-0001 for the `core`/`cli` split).
For a gate to be useful it must be shared: every clone should get the same hook,
and it should be reviewable like any other code. Git's default hook directory,
`.git/hooks/`, is per-clone and not version-controlled, so a hook dropped there
is invisible to the rest of the team and drifts silently.

A second constraint is specific to this repo: a machine-local `post-commit` hook
(the roborev review gate) already lives in `.git/hooks/`. The mechanism we pick
must not disable it.

## Options

1. **`git config core.hooksPath .githooks`** — point git at a committed
   directory. Simplest config. But `core.hooksPath` *replaces* `.git/hooks`
   entirely: git then runs hooks **only** from the configured path, so the
   existing per-developer `post-commit` (roborev) hook would silently stop
   firing unless it too were committed into `.githooks/` — which would force
   roborev on every contributor, who may not use it.
2. **Committed `.githooks/` + a symlink-install script** — hook *sources* are
   version-controlled in `.githooks/`; `scripts/install-hooks.sh` symlinks each
   into `.git/hooks/`. The links stay live (editing the source updates the
   active hook), and unmanaged hooks already in `.git/hooks/` (roborev's
   `post-commit`) keep working alongside them.
3. **Hook manager (lefthook / pre-commit / cargo-husky)** — feature-rich;
   `cargo-husky` is Rust-native and installs hooks on first build. Adds a
   dependency and, for cargo-husky, a build-time side effect, for a single hook.

## Decision

Option 2. Commit hook sources under `.githooks/`, and provide an idempotent
`scripts/install-hooks.sh` that symlinks them into `.git/hooks/`. Setup is a
one-time `bash scripts/install-hooks.sh` per clone, documented in the README.

Rejected `core.hooksPath` because it would disable the existing roborev
`post-commit` hook (the crux above); rejected a hook manager to avoid a
dependency and build-time side effect for one hook.

## Consequences

- The roborev `post-commit` hook and the new `pre-push` hook coexist — the
  reason to pay the symlink-install cost over the simpler `core.hooksPath`.
- Hook logic is version-controlled and reviewed like any code; the live hook is
  a symlink, so source edits take effect with no re-install.
- Setup is not automatic on clone — it needs the one-time install command. CI
  does not run it (CI gates are separate; #2). Documented in the README.
- Symlink install assumes a platform with symlinks (macOS/Linux); a Windows
  contributor would need the copy fallback. Acceptable for the current team.
- The thin hook delegates to `scripts/check-mutants-diff.sh`, so the gate logic
  stays testable without git's push machinery (ADR-0001 boundary discipline).
