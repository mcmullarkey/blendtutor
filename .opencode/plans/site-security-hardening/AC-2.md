---
ac: 2
depends_on: AC-1
risk: high
status: complete
---

## AC-2: Password Protection — Pure Rust AES Encryption

### Executable Spec
- **predicate:** encrypt_site_files(&site, "secret", &mut rng) produces SiteFiles where:
  1. index.html is decrypt shell with data-encrypted-payload/salt/iv base64 attrs
  2. Password input + decrypt button present
  3. Inline crypto.subtle.deriveKey (PBKDF2) + crypto.subtle.decrypt (AES-GCM) JS
  4. No plaintext lesson content in index.html
  5. Every lessons/*.json is encrypted ciphertext (serde_json::from_str fails)
  6. lessons.json is encrypted ciphertext
  7. Password string not in any emitted file
  8. Two calls with same password+content yield different salts
  9. Two calls yield different nonces (GCM catastrophic-failure guard)
  10. IV decodes to 12 bytes, not all zeros
  11. Inline JS contains literal 600000 (PBKDF2 iterations)
  12. coi-serviceworker.js script before inline decrypt script (BOTH targets)
  13. Decrypt JS has catch block with error message (failure state)
  14. Shared assets still emitted unchanged (lesson-runner.js, feedback.js, styles.css, etc.)
  15. eval-results.html also encrypted
  16. Both Webr and Pyodide targets work
  17. Roundtrip: crypto::decrypt(crypto::encrypt(plaintext, "secret", rng), "secret") == Ok(plaintext)
- **probe:** cargo test -p blendtutor-core --lib site::tests::encrypt_site_files_password_protects_all_content_files
- **negative:** plan_site output without encrypt = byte-identical to current; wrong password = Err; base64-encoding-as-encryption caught; JSON-bypass caught; hardcoded-key JS caught
- **verification:** code + rodney (WebCrypto roundtrip, COI SW ordering, wrong-password error)
- **fixture status:** NEW crypto.rs, NEW decrypt-shell.html asset
- **rubric anchor:** §1.2, §1.3.1, §1.4, §2.1, §2.3, §3.2, §4.1, §4.2, §5.1

### Design Intent
- §1: EncryptedPayload struct {ciphertext, salt, nonce}. encrypt_site_files takes &SiteFiles + password + rng
- §2: plan_site UNCHANGED (preserves 55+ tests). encrypt_site_files is pure w.r.t. rng. write_site unchanged
- §3: crypto.rs owns encrypt/decrypt. site/mod.rs owns encrypt_site_files + decrypt shell rendering. Fetch monkeypatch in decrypt shell for transparent lesson JSON decryption (zero runner modification)
- §4: crypto.rs: AES-GCM + PBKDF2 only, no I/O. decrypt-shell.html: static asset via include_str!
- §5: encrypt_site_files does one thing: transform SiteFiles to encrypted SiteFiles

### User Decisions
- What gets encrypted: ALL content files (index.html body, lessons/*.json, lessons.json, eval-results.html). Fetch monkeypatch in decrypt shell for transparent lesson JSON decryption.
- Manifest password: CLI flag only (--password). No blendtutor.toml [site] password field for v0.
- COI SW: present in BOTH targets (both include coi-serviceworker.js)

### Progress
- [x] Add crypto deps to workspace + core Cargo.toml
- [x] Create crates/core/src/crypto.rs (encrypt/decrypt, AES-256-GCM + PBKDF2)
- [x] Create decrypt-shell.html asset with WebCrypto JS + fetch monkeypatch
- [x] Add encrypt_site_files to site/mod.rs + register crypto module in lib.rs
- [x] Write integration test (17 clauses) — encrypt_site_files_password_protects_all_content_files
- [x] Add --password CLI flag to Build command + call encrypt_site_files in build.rs
- [x] Write ADR-0012 for crypto module architecture
- [x] Run cargo test (157 passed) + clippy (clean) + fmt --check (clean)

### Decision Log
- 2026-07-16 — Encrypt all content files (not just index.html) to prevent JSON-bypass attack
- 2026-07-16 — Separate encrypt_site_files function (not plan_site param) to preserve 55+ existing tests
- 2026-07-16 — CLI flag only, no manifest password (avoid deny_unknown_fields schema change)
- 2026-07-16 — Fetch monkeypatch approach for zero runner modification
- 2026-07-16 — Encrypted file format: base64(salt(16) || nonce(12) || ciphertext) — not valid JSON, so serde_json::from_str fails (JSON-bypass guard)
- 2026-07-16 — Decrypt shell uses DOMParser to inject decrypted HTML + re-inject scripts (innerHTML doesn't execute scripts)
- 2026-07-16 — Decrypt shell has no CSP meta tag (needs inline scripts for WebCrypto; security comes from encryption, not CSP on the shell)

### Surprises & Discoveries
- PBKDF2 with 600k iterations is intentionally slow (~0.5-1s per derivation in debug mode). The integration test takes ~84s because it encrypts multiple full sites. Minimized by using crypto::encrypt directly for salt/nonce uniqueness clauses instead of full encrypt_site_files calls. This is the cost of testing the real iteration count — reducing it would make the test pass but not reflect production security.
- Pre-existing test `plan_site_emits_csp_sri_and_referrer_policy` called `plan_site` with 3 args (missing `site_config`), which was a compile error after rebase. Fixed by using the `plan()` helper which supplies `&SiteConfig::default()`. This was not caused by our changes — it was already broken on main after the site_config parameter was added to plan_site.
- Rust 2024 edition requires parentheses around `impl Trait + Trait` bounds: `&mut (impl RngCore + CryptoRng)` not `&mut impl RngCore + CryptoRng`. The compiler suggests this fix.

### Idempotence & Recovery
- Safe retry: re-run cargo test
- Rollback: remove encrypt_site_files, crypto.rs, decrypt-shell.html
