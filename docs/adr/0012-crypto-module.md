# ADR-0012: Password protection — pure Rust AES-256-GCM encryption

- Status: Accepted
- Date: 2026-07-16

## Context

Issue #97 adds password protection for built static sites. A course author who
builds a site with `--password <pw>` gets a site where all content files
(index.html, lessons/*.json, lessons.json, eval-results.html) are encrypted
with AES-256-GCM + PBKDF2. The learner enters the password in a decrypt shell
that uses WebCrypto to decrypt the page client-side — no server, no runtime
decryption, no plaintext content at rest.

The threat model: a static site deployed to GitHub Pages is publicly readable.
Without encryption, lesson content (prompts, solutions, checks) is visible to
anyone who inspects the network tab. With encryption, only someone with the
password can read the content. This is not access control (the decrypt shell +
encrypted files are public) — it is content confidentiality at rest.

## Options

1. **Server-side decryption (SSR).** A backend holds the password and decrypts
   on each request. Strongest security (password never ships), but breaks the
   "static site, no server" deployment model (ADR-0008). Requires hosting,
   secrets management, and a runtime — the opposite of the current
   GitHub-Pages deploy.

2. **Client-side decryption with a hardcoded key in JS.** The decrypt shell
   contains the encryption key as a JS literal. Simple, but the key is visible
   in the page source — anyone who inspects the JS can decrypt the content.
   This is obfuscation, not encryption.

3. **Client-side decryption with PBKDF2-derived key (WebCrypto).** The decrypt
   shell takes a user-entered password, derives a key via PBKDF2 (600k
   iterations, SHA-256), and decrypts via AES-GCM. The password is never baked
   into any file. The encrypted content is ciphertext (not base64-as-encryption).
   The key derivation is slow enough to resist brute-force, and GCM
   authentication catches wrong passwords. Pure Rust encrypts at build time;
   WebCrypto decrypts at learner time — same algorithm, same parameters.

## Decision

Option 3.

- **Crypto module (§3.2, §4.1).** `core::crypto` owns `encrypt(plaintext,
  password, rng) -> EncryptedPayload` and `decrypt(payload, password) ->
  Result<String, CryptoError>`. Pure: no I/O, no filesystem. The rng is injected
  so the transform is deterministic given the rng's output (testable). AES-256-GCM
  with PBKDF2-HMAC-SHA256, 600k iterations, 16-byte salt, 12-byte nonce — all
  fresh per call (GCM nonce reuse is catastrophic, §1.3.1).

- **EncryptedPayload (§1.2).** A struct `{ciphertext, salt, nonce}` — a
  represented state, not a bag of bytes. `to_base64()` encodes as
  `salt || nonce || ciphertext` for the file format (not valid JSON, so
  `serde_json::from_str` fails — the JSON-bypass guard).

- **encrypt_site_files (§5.1).** A separate post-processing step:
  `encrypt_site_files(&SiteFiles, password, rng) -> SiteFiles`. `plan_site` is
  unchanged — the 55+ existing tests are unaffected. Content files (index.html,
  lessons/*.json, lessons.json, eval-results.html) are encrypted; infrastructure
  files (lesson-runner.js, feedback.js, styles.css, etc.) pass through unchanged.

- **Decrypt shell (§4.1).** A static asset (`assets/shared/decrypt-shell.html`)
  with placeholders `{{payload}}`, `{{salt}}`, `{{iv}}`, `{{iterations}}`.
  Provides: password input, decrypt button, error element, inline WebCrypto JS
  (PBKDF2 `deriveKey` + AES-GCM `decrypt`), body injection, script re-injection,
  and a `fetch` monkeypatch for transparent lesson-JSON decryption. The COI
  service worker loads before the inline decrypt script (both targets).

- **Fetch monkeypatch.** Intercepts `fetch()` for `lessons/*.json` and
  `lessons.json`, decrypts the base64 `salt || nonce || ciphertext` response,
  and returns plaintext JSON. Zero runner modification — the lesson runner's
  `fetch()` calls work unchanged.

- **CLI flag (§2.3).** `--password <pw>` on `blendtutor build`. When present,
  `encrypt_site_files` runs after `plan_site` and before `write_site`. When
  absent, the site is unencrypted (current behavior). No manifest password
  field — avoids a `deny_unknown_fields` schema change.

## Consequences

- The decrypt shell needs inline scripts, so it cannot carry the strict CSP
  (no `'unsafe-inline'` for script-src) the original index.html has. The
  decrypt shell's CSP is the browser default (no CSP meta tag). The security
  comes from the encryption, not from CSP on the decrypt shell. The original
  CSP is part of the encrypted payload and is not active until after decryption
  — at which point the page-level CSP (from the decrypt shell) is what the
  browser enforces. This is a known limitation of client-side decryption.

- PBKDF2 with 600k iterations is intentionally slow (~0.5-1s per derivation).
  The build encrypts each content file separately, so a course with N lessons
  takes ~(N+3) derivations. The test suite is slow in debug mode (~84s for the
  integration test) — this is the cost of testing the real iteration count.

- The password is passed as a CLI argument, visible in the process list. A
  future improvement could read from an environment variable or stdin. For v0,
  this is acceptable — the author runs the build locally and deploys the
  encrypted output.

- `rand_core`, `aes-gcm`, `pbkdf2`, `sha2`, and `base64` become runtime
  dependencies of `blendtutor-core`. The `rand_core` `std` feature enables
  `OsRng` for the CLI's cryptographic randomness source.
