---
ac: 2
depends_on: AC-1
risk: medium
status: spec
---

### AC-2 — install.sh
- Spec mode: spec-resolved
- Key files: `scripts/install.sh` (NEW), `scripts/tests/test_install_sh.sh` (NEW), `README.md` (install section — final wording lands in AC-3; AC-2 makes only the minimal section edit)
- Invariants/constraints:
  - uv installer contract: `curl -LsSf https://github.com/mcmullarkey/blendtutor/releases/...` — script must work via `curl | sh` (stdin-piped, no `$0` assumptions, POSIX-ish sh).
  - Asset naming must byte-match AC-1's pinned contract.
  - Fail-closed: checksum mismatch → exit nonzero with message (repo doctrine).
  - Install dir default `~/.local/bin`, overridable via env (uv uses `UV_INSTALL_DIR`; mirror as `BLENDTUTOR_INSTALL_DIR`).
- Prior art: astral.sh/uv/install.sh; repo shell-BDD precedent `scripts/tests/test_smevals_runner.sh` (stub-based, env-injection).
- Verification: code
- Test seam: stub-based shell BDD — fake `curl`/`uname` via PATH injection (mirror `test_smevals_runner.sh` pattern). Runs in ci.yml like other script tests → test must also add its ci.yml invocation (hot conflict with CI file — note for batch scheduling).
- Literal grep: `mcmullarkey/blendtutor` base URL literal — grep across `scripts/ tests/ rodney-probes/` at spec-compile to catch existing pins (quarto add tests use the same org/repo string).

### Progress
- (none yet)

### Decision Log
- (none yet)

### Surprises & Discoveries
- (none yet)

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open
