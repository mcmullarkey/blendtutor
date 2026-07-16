# ADR-0013: BYOK-builder embedded API key (--embed-key)

- Status: Accepted
- Date: 2026-07-16

## Context

Issue #99 adds the ability for a course author to embed an API key directly
into a password-protected built site. The learner enters the site password,
the page decrypts, and the embedded key is pre-loaded into sessionStorage —
so the learner never sees a key-entry prompt. The feedback flow
(`handleSubmit`) is unchanged: it reads the key from sessionStorage as
before, but the key is already there.

The threat model: the embedded key is inside the AES-256-GCM-encrypted
payload (ADR-0012). Anyone with the site password can decrypt and extract
the key. This is acceptable for a course author who wants a friction-free
learner experience (no key entry) and controls the password distribution.
The CLI warning reminds the author to use a dedicated, single-use key with
a tight spend limit.

## Options

1. **Plaintext key in a shipped JS file.** The key is baked into feedback.js
   or a separate config file as a literal. Simple, but the key is visible to
   anyone who inspects the site source — no encryption, no protection. This
   is the existing negative-scan guard (`fw_` / `sk-ant` must never ship).

2. **Key in a separate encrypted file.** A dedicated `embedded-key.json`
   encrypted with the same password. The decrypt shell fetches and decrypts
   it separately. More moving parts (an extra fetch, an extra decrypt path)
   for no security gain over embedding in the existing index.html payload.

3. **Key inside the encrypted index.html payload (JSON wire format).** The
   index.html plaintext becomes a JSON object
   `{"html":"<page>","embeddedKey":{"provider":"...","key":"..."}}`. The
   decrypt shell decrypts, tries `JSON.parse`, extracts the key (sets
   `window.__btEmbeddedKey`), and uses the `html` field as the page content.
   When no key is embedded, the payload is plain HTML — `JSON.parse` fails
   and the decrypt shell falls back to treating the plaintext as HTML. Zero
   extra files, zero extra fetches, and the key is inside the same
   AES-256-GCM ciphertext as the page content.

## Decision

Option 3.

- **EmbeddedKey (§1.2).** A struct `{provider, key}` — a represented state,
  not a loose string. Serialized as `embeddedKey` (camelCase) in the JSON
  payload so the JS decrypt shell reads it with the same key name.

- **Wire format (§3.2).** JSON `{"html":"<page>","embeddedKey":{...}}` — not
  string splitting. The `html` field carries the original page content; the
  `embeddedKey` field carries the key. Standard base64 (not base64url) is
  used for the ciphertext — the alphabet (`A-Za-z0-9+/=`) excludes `_` and
  `-`, so the `fw_` / `sk-ant-` key prefixes can never appear in the encoded
  output. This preserves the existing negative-scan guards.

- **encrypt_site_files (§5.1).** Extended with an `Option<&EmbeddedKey>`
  parameter. When `Some`, the index.html plaintext is the JSON payload; when
  `None`, it is plain HTML (current behavior). `eval-results.html` does not
  load feedback.js, so it does not receive the embedded key — it stays as
  plain encrypted HTML. `plan_site` is unchanged.

- **Decrypt shell (§4.1).** After decrypting, the shell tries `JSON.parse`
  on the plaintext. If it succeeds and has `embeddedKey` + `html`, it sets
  `window.__btEmbeddedKey` and uses `html` as the page content. If
  `JSON.parse` fails (plain HTML payload), it falls back to treating the
  plaintext as HTML. The same shell handles both cases — no fork.

- **feedback.js applyEmbeddedKey (§4.1).** A new function called at init,
  before `submitButton.addEventListener`. It reads
  `window.__btEmbeddedKey`, calls `storeKey` + `storeProvider` (pre-loading
  the key into sessionStorage), and clears the global
  (`window.__btEmbeddedKey = undefined`). `handleSubmit` is UNCHANGED — it
  reads the key from sessionStorage as before; the key is already there, so
  the key-entry prompt (phase 1) is skipped naturally.

- **CLI flag (§2.3).** `--embed-key provider:key` on `blendtutor build`.
  Requires `--password` (hard error if missing — the key is encrypted into
  the payload, so without a password there is nothing to encrypt with).
  Validates the provider (`fireworks` or `anthropic`) and the key prefix
  (`fw_` for fireworks, `sk-ant-` for anthropic) — a mismatch is a hard
  error. Emits a CLI warning to stderr about the key being unique / single-use
  and the need to set a spend limit / budget to bound cost.

## Consequences

- The embedded key is inside the AES-256-GCM ciphertext. Anyone with the
  site password can extract it. The CLI warning reminds the author to use a
  dedicated, single-use key with a tight spend limit. This is a deliberate
  trade-off: friction-free learner experience (no key entry) in exchange for
  the author controlling password distribution.

- The decrypt shell now tries `JSON.parse` on every decrypted payload. For
  plain HTML payloads (no embedded key), `JSON.parse` throws and the catch
  block falls back to treating the plaintext as HTML — zero behavior change
  for password-only sites. The try/catch is the same pattern as the fetch
  monkeypatch (ADR-0012).

- The `encrypt_site_files` signature changed (added `Option<&EmbeddedKey>`).
  All existing call sites pass `None` — the 55+ `plan_site` tests and the
  AC-2 password protection tests are unaffected.

- The base64 alphabet guarantee (`_` and `-` excluded) means the existing
  negative-scan tests (`fw_` / `sk-ant` must never ship) pass unchanged
  even when a key is embedded — the key is inside the ciphertext, and the
  ciphertext's base64 encoding can never contain the key's prefix
  characters.
