//! Password-based encryption for built sites: AES-256-GCM + PBKDF2.
//!
//! Pure (§2.1): no I/O, no filesystem. [`encrypt`] takes plaintext + password +
//! an injected rng and returns an [`EncryptedPayload`]; [`decrypt`] takes a
//! payload + password and returns the plaintext. The rng is injected so the
//! whole transform is deterministic given the rng's output (testable, no global
//! randomness source).
//!
//! The browser-side decryption uses the same algorithm via WebCrypto
//! (`crypto.subtle.deriveKey` + `crypto.subtle.decrypt`), so a payload encrypted
//! in Rust decrypts in the browser and vice versa (ADR-0012).

use std::error::Error;
use std::fmt;

use aes_gcm::aead::{Aead, KeyInit};
use aes_gcm::{Aes256Gcm, Key, Nonce};
use base64::Engine;
use pbkdf2::pbkdf2_hmac;
use rand_core::{CryptoRng, RngCore};
use sha2::Sha256;

/// PBKDF2 iteration count — 600,000 with HMAC-SHA-256 (OWASP 2023 recommendation
/// for PBKDF2-SHA256). The same literal appears in the decrypt shell's inline JS
/// so the browser-side WebCrypto derivation matches.
pub const PBKDF2_ITERATIONS: u32 = 600_000;

/// Salt length in bytes — 128 bits, per NIST SP 800-132.
const SALT_LEN: usize = 16;

/// GCM nonce (IV) length in bytes — 96 bits, the GCM standard length
/// (NIST SP 800-38D).
const NONCE_LEN: usize = 12;

/// AES-256 key length in bytes — 256 bits.
const KEY_LEN: usize = 32;

/// An encrypted payload: the ciphertext plus the salt and nonce needed to
/// decrypt it (§1.2 — a represented state, not a bag of bytes).
///
/// The salt and nonce are generated fresh per encryption call (§1.3.1 — GCM
/// nonce reuse is catastrophic), so two encryptions of the same plaintext with
/// the same password yield different payloads.
#[derive(Debug, Clone)]
pub struct EncryptedPayload {
    /// The AES-256-GCM ciphertext (includes the GCM authentication tag).
    pub ciphertext: Vec<u8>,
    /// The PBKDF2 salt — 16 random bytes, unique per encryption.
    pub salt: [u8; SALT_LEN],
    /// The GCM nonce — 12 random bytes, unique per encryption.
    pub nonce: [u8; NONCE_LEN],
}

impl EncryptedPayload {
    /// Encode the payload as base64 of `salt || nonce || ciphertext` — a single
    /// self-contained string the browser fetch monkeypatch can decode and decrypt.
    ///
    /// The format is: first 16 bytes = salt, next 12 bytes = nonce, rest =
    /// ciphertext (including GCM tag). This is NOT valid JSON, so
    /// `serde_json::from_str` fails on it (the JSON-bypass guard).
    pub fn to_base64(&self) -> String {
        let mut bytes = Vec::with_capacity(SALT_LEN + NONCE_LEN + self.ciphertext.len());
        bytes.extend_from_slice(&self.salt);
        bytes.extend_from_slice(&self.nonce);
        bytes.extend_from_slice(&self.ciphertext);
        base64::engine::general_purpose::STANDARD.encode(&bytes)
    }

    /// Decode a base64 payload (`salt || nonce || ciphertext`) back into an
    /// [`EncryptedPayload`].
    pub fn from_base64(s: &str) -> Result<Self, CryptoError> {
        let bytes = base64::engine::general_purpose::STANDARD
            .decode(s)
            .map_err(|_| CryptoError::InvalidPayload)?;
        if bytes.len() < SALT_LEN + NONCE_LEN {
            return Err(CryptoError::InvalidPayload);
        }
        let mut salt = [0u8; SALT_LEN];
        let mut nonce = [0u8; NONCE_LEN];
        salt.copy_from_slice(&bytes[..SALT_LEN]);
        nonce.copy_from_slice(&bytes[SALT_LEN..SALT_LEN + NONCE_LEN]);
        let ciphertext = bytes[SALT_LEN + NONCE_LEN..].to_vec();
        Ok(EncryptedPayload {
            ciphertext,
            salt,
            nonce,
        })
    }
}

/// Derive a 256-bit AES key from a password and salt via PBKDF2-HMAC-SHA256.
fn derive_key(password: &str, salt: &[u8]) -> [u8; KEY_LEN] {
    let mut key = [0u8; KEY_LEN];
    pbkdf2_hmac::<Sha256>(password.as_bytes(), salt, PBKDF2_ITERATIONS, &mut key);
    key
}

/// Encrypt `plaintext` with `password`, using `rng` to generate a fresh salt
/// and nonce (§2.1 — pure w.r.t. rng: the rng is the only source of
/// nondeterminism).
///
/// Returns an [`EncryptedPayload`] carrying the ciphertext, salt, and nonce.
/// Two calls with the same plaintext and password yield different payloads
/// (different salt and nonce), so GCM nonce reuse is impossible.
pub fn encrypt(
    plaintext: &str,
    password: &str,
    rng: &mut (impl RngCore + CryptoRng),
) -> EncryptedPayload {
    let mut salt = [0u8; SALT_LEN];
    let mut nonce = [0u8; NONCE_LEN];
    rng.fill_bytes(&mut salt);
    rng.fill_bytes(&mut nonce);

    let key = derive_key(password, &salt);
    let cipher = Aes256Gcm::new(Key::<Aes256Gcm>::from_slice(&key));
    let ciphertext = cipher
        .encrypt(Nonce::from_slice(&nonce), plaintext.as_bytes())
        .expect("AES-GCM encryption is infallible for valid inputs");

    EncryptedPayload {
        ciphertext,
        salt,
        nonce,
    }
}

/// Decrypt an [`EncryptedPayload`] with `password`.
///
/// Returns `Err(CryptoError::DecryptionFailed)` if the password is wrong or the
/// payload is corrupt — GCM authentication catches both (the tag fails to verify).
pub fn decrypt(payload: &EncryptedPayload, password: &str) -> Result<String, CryptoError> {
    let key = derive_key(password, &payload.salt);
    let cipher = Aes256Gcm::new(Key::<Aes256Gcm>::from_slice(&key));
    let plaintext = cipher
        .decrypt(
            Nonce::from_slice(&payload.nonce),
            payload.ciphertext.as_ref(),
        )
        .map_err(|_| CryptoError::DecryptionFailed)?;
    String::from_utf8(plaintext).map_err(|_| CryptoError::DecryptionFailed)
}

/// Why decryption failed — the password was wrong, the payload is corrupt, or
/// the base64 encoding is malformed.
#[derive(Debug)]
pub enum CryptoError {
    /// The GCM authentication tag did not verify — wrong password or corrupt
    /// ciphertext. GCM does not distinguish the two (by design).
    DecryptionFailed,
    /// The base64 payload is malformed or too short to contain salt + nonce.
    InvalidPayload,
}

impl fmt::Display for CryptoError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CryptoError::DecryptionFailed => {
                write!(f, "decryption failed: wrong password or corrupt data")
            }
            CryptoError::InvalidPayload => {
                write!(f, "invalid encrypted payload: malformed or too short")
            }
        }
    }
}

impl Error for CryptoError {}

#[cfg(test)]
mod tests {
    use super::*;
    use rand_core::OsRng;

    #[test]
    fn roundtrip_encrypt_decrypt_recovers_plaintext() {
        let mut rng = OsRng;
        let payload = encrypt("hello world", "secret", &mut rng);
        let plaintext = decrypt(&payload, "secret").expect("correct password decrypts");
        assert_eq!(plaintext, "hello world");
    }

    #[test]
    fn wrong_password_fails_decryption() {
        let mut rng = OsRng;
        let payload = encrypt("hello world", "secret", &mut rng);
        assert!(matches!(
            decrypt(&payload, "wrong"),
            Err(CryptoError::DecryptionFailed)
        ));
    }

    #[test]
    fn two_encryptions_yield_different_salts() {
        let mut rng = OsRng;
        let a = encrypt("same plaintext", "same password", &mut rng);
        let b = encrypt("same plaintext", "same password", &mut rng);
        assert_ne!(a.salt, b.salt, "two calls must yield different salts");
    }

    #[test]
    fn two_encryptions_yield_different_nonces() {
        let mut rng = OsRng;
        let a = encrypt("same plaintext", "same password", &mut rng);
        let b = encrypt("same plaintext", "same password", &mut rng);
        assert_ne!(
            a.nonce, b.nonce,
            "two calls must yield different nonces (GCM catastrophic-failure guard)"
        );
    }

    #[test]
    fn nonce_is_twelve_bytes_not_all_zeros() {
        let mut rng = OsRng;
        let payload = encrypt("test", "pw", &mut rng);
        assert_eq!(payload.nonce.len(), 12, "IV must decode to 12 bytes");
        assert!(
            payload.nonce.iter().any(|&b| b != 0),
            "IV must not be all zeros"
        );
    }

    #[test]
    fn base64_roundtrip_preserves_payload() {
        let mut rng = OsRng;
        let payload = encrypt("base64 test", "pw", &mut rng);
        let encoded = payload.to_base64();
        let decoded = EncryptedPayload::from_base64(&encoded).expect("decodes");
        assert_eq!(decoded.salt, payload.salt);
        assert_eq!(decoded.nonce, payload.nonce);
        assert_eq!(decoded.ciphertext, payload.ciphertext);
        let plaintext = decrypt(&decoded, "pw").expect("roundtrip decrypts");
        assert_eq!(plaintext, "base64 test");
    }

    #[test]
    fn from_base64_rejects_short_input() {
        assert!(matches!(
            EncryptedPayload::from_base64("dG9vIHNob3J0"),
            Err(CryptoError::InvalidPayload)
        ));
    }

    /// A key derived from file A's salt cannot decrypt file B's ciphertext,
    /// even when both files share the same password. This is the crypto-level
    /// root cause of the fetch-monkeypatch bug: each content file gets its own
    /// fresh salt from `encrypt`, so the browser must derive a per-file key
    /// using THAT file's salt — not reuse a key derived from index.html's salt.
    #[test]
    fn per_file_salt_prevents_cross_file_decryption() {
        let mut rng = OsRng;
        let payload_a = encrypt("index page html", "shared-pw", &mut rng);
        let payload_b = encrypt("lesson json content", "shared-pw", &mut rng);

        // Sanity: each file has its own salt.
        assert_ne!(
            payload_a.salt, payload_b.salt,
            "two encrypt calls must yield different salts"
        );

        // Construct a Frankenstein payload: B's ciphertext + B's nonce, but
        // A's salt. This simulates the bug — deriving a key from A's salt
        // (index.html) and trying to decrypt B (a lesson JSON).
        let frankenstein = EncryptedPayload {
            ciphertext: payload_b.ciphertext.clone(),
            salt: payload_a.salt, // wrong salt!
            nonce: payload_b.nonce,
        };
        assert!(
            decrypt(&frankenstein, "shared-pw").is_err(),
            "a key derived from file A's salt must NOT decrypt file B's ciphertext \
             — the browser fetch monkeypatch must derive a per-file key"
        );

        // Control: B decrypts fine with its own salt (correct password).
        assert!(
            decrypt(&payload_b, "shared-pw").is_ok(),
            "file B must decrypt with its own salt + correct password"
        );
    }
}
