---
ac: 3
depends_on: AC-2
risk: high
status: complete
---

## AC-3: BYOK-builder embedded API key (--embed-key)

### Executable Spec
- **predicate:** `blendtutor build --target webr <course> -o <out> --password <pw> --embed-key fireworks:fw_test123` produces a site where:
  1. Build succeeds (exit 0)
  2. No plaintext API key (`fw_test123`) in any emitted file
  3. `--embed-key` without `--password` = hard error (exit != 0, no output dir)
  4. Provider/key prefix mismatch (e.g. `fireworks:sk-ant-xxx`) = hard error
  5. stderr contains warning keywords: "unique"/"single-use" + "spend limit"/"budget"/"cost" + "WARNING"/"⚠"/"CAUTION"
  6. feedback.js contains `applyEmbeddedKey` function that reads `window.__btEmbeddedKey`, calls `storeKey` + `storeProvider`, clears the global
  7. `handleSubmit` is unchanged (key pre-loaded into sessionStorage, phase 1 skipped naturally)
  8. Existing negative scan tests (`sk-ant`, `fw_`) still pass — key inside encrypted base64
  9. Cross-target byte-identity: feedback.js identical across webr + pyodide
- **probe:** `cargo test -p blendtutor-cli --test build embed_key`
- **negative:** `--embed-key` without `--password` = hard error; provider mismatch = hard error; plaintext key in any file = fail
- **verification:** code
- **fixture status:** extends encrypt_site_files (AC-2), extends decrypt-shell.html, extends feedback.js
- **rubric anchor:** §1.2, §1.3.1, §2.1, §3.2, §4.1, §4.2, §5.1

### Design Intent
- §1: EmbeddedKey struct {provider, key}. encrypt_site_files takes Option<&EmbeddedKey>.
- §2: plan_site UNCHANGED. encrypt_site_files extended: when embed_key is Some, index.html payload is JSON {"html":"...","embeddedKey":{...}}.
- §3: Wire format: JSON (not string splitting). Standard base64 (alphabet excludes _ and -, so fw_/sk-ant never in ciphertext).
- §4: decrypt-shell.html: after decrypt, try JSON.parse; if embeddedKey present, set window.__btEmbeddedKey, use html field.
- §5: feedback.js applyEmbeddedKey: reads global, stores in sessionStorage, clears global. Called before submitButton.addEventListener. handleSubmit UNCHANGED.

### User Decisions
- Wire format: JSON {"html":"...","embeddedKey":{"provider":"...","key":"..."}} — NOT string splitting
- Standard base64 (not base64url) — alphabet excludes _ and -, so fw_/sk-ant never in ciphertext
- applyEmbeddedKey clears window.__btEmbeddedKey after reading
- handleSubmit UNCHANGED — key pre-loaded into sessionStorage, phase 1 skipped naturally
- CLI flag only (no manifest fields)
- --embed-key format: provider:key (split on first colon)
- Provider validation: fireworks (key must start fw_), anthropic (key must start sk-ant-)

### Progress
- [x] Create AC-3.md plan file
- [x] Write red integration test (8 tests, all red initially — --embed-key not recognized)
- [x] Add --embed-key CLI flag to main.rs
- [x] Implement parse_embed_key + validation + warnings in build.rs
- [x] Add EmbeddedKey type + extend encrypt_site_files in site/mod.rs
- [x] Update decrypt-shell.html to extract embeddedKey from JSON payload
- [x] Add applyEmbeddedKey() to feedback.js
- [x] Write all tests (8 integration tests in build.rs)
- [x] Run cargo test (245 passed) + clippy (clean) + fmt --check (clean)
- [x] Write ADR-0013
- [x] Produce E2E evidence at docs/evidence/99/ (build-output.log + test-suite.log)
- [x] Commit + push

### Decision Log
- 2026-07-16 — Wire format is JSON {"html":"...","embeddedKey":{...}}, not string splitting (user decision)
- 2026-07-16 — Standard base64 (not base64url) so fw_/sk-ant never appear in ciphertext (user decision)
- 2026-07-16 — applyEmbeddedKey clears window.__btEmbeddedKey after reading (user decision)
- 2026-07-16 — handleSubmit UNCHANGED — key pre-loaded into sessionStorage (user decision)

### Surprises & Discoveries
- The `encrypt_site_files` signature change (added `Option<&EmbeddedKey>`) required updating 2 existing call sites in site/mod.rs tests (lines ~2869 and ~3122) to pass `None`. These were the only call sites besides build.rs. The 55+ `plan_site` tests were unaffected because `plan_site` itself is unchanged.
- The decrypt shell's `const html` declaration needed to change to `let html` because the embedded-key path reassigns it after JSON.parse extraction. This is the only change to the decrypt flow — the rest (DOMParser, script re-injection, fetch monkeypatch) is untouched.
- The `eval-results.html` page does NOT receive the embedded key — it doesn't load feedback.js, so there's no feedback flow to skip. It stays as plain encrypted HTML. The decrypt shell handles both cases via JSON.parse fallback (try/catch).
- The pre-push hook runs `cargo mutants --in-diff` which takes hours. User instructed to push with --no-verify. All format/lint/test checks were run manually before pushing — the --no-verify only skips the mutation testing gate, not format/lint.

### Idempotence & Recovery
- Safe retry: re-run cargo test
- Rollback: revert encrypt_site_files signature, decrypt-shell.html, feedback.js changes
