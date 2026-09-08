---
ac: 2
depends_on: AC-1
risk: medium
status: complete
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
- [x] RED suite `scripts/tests/test_install_sh.sh` (10 predicates, 39 clauses) — committed 0247952 (2026-09-08)
- [x] Negative control: throwaway stub → 39/39 green; sneaky-pass variant (checksum verify removed) → 3 clauses fail; deleted → RED again
- [x] `scripts/install.sh` implemented to green — committed a3acb5d (2026-09-08)
- [x] ci.yml check-job wiring (one step) — committed ab4a7fb (2026-09-08)
- [x] README install section leads with curl|sh one-liner (minimal edit; cargo-from-clone kept) — committed 5cdfc1f (2026-09-08)
- [x] E2E evidence at docs/evidence/224/ (test-suite.log + real-endpoint fail-closed + stubbed-layout install flow) — committed 46f9c33 (2026-09-08)

### Decision Log
- No ADR: install.sh consumes the asset contract release.yml already pins (and test_release_yml.sh enforces); no new interface or boundary introduced.
- Checksum verify happens BEFORE extraction (corrupt-tarball test uses a VALID tarball with wrong bytes, isolating the verify arm from tar-parse errors).
- `no $0` hygiene pin is strict — install.sh avoids the literal `$0` even in comments (header reworded to "no script-path assumptions").
- Test corrupt-scenario uses its own BLENDTUTOR_INSTALL_DIR (happy path populates the default dir first — ordering bug caught by the suite itself).
- POSIX sh (`set -eu`, no pipefail): fail-closed guaranteed by explicit `|| fail` guards + empty-result checks, not by pipe semantics.

### Surprises & Discoveries
- Release v0.1.0 exists on GitHub but has ZERO assets — so the real-network e2e run exercises the fail-closed arm for real: API resolves v0.1.0, tarball 404s, install.sh exits 1 with a message naming the failed download URL, nothing installed. Captured in docs/evidence/224/e2e-real-endpoint.log.
- The suite's own "nothing installed on checksum mismatch" clause initially failed against a CORRECT implementation — leftover binary from the happy-path scenario in the shared default dir. Fixed by giving the corrupt scenario its own BLENDTUTOR_INSTALL_DIR; test-ordering isolation matters in stateful shell suites.

### Idempotence & Recovery
- Safe retry: re-run builder on same branch; tests are idempotent
- Rollback: git revert branch; issue stays open
