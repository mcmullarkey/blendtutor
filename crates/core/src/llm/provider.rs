//! Provider selection for LLM feedback.
//!
//! [`ProviderChoice`] is the closed set of LLM backends (ADR-0006). It carries
//! only the metadata the boundary needs — which key env var to read, the default
//! base URL, and a default model — not a rig client, because the two providers
//! build different rig client types; constructing them lives in
//! [`request_feedback`](super::request_feedback), one match arm each (§3.4). The
//! `match`es here are exhaustive with no wildcard, so a new provider forces a new
//! arm rather than silently falling through.

/// The LLM backend used to grade a submission.
///
/// A closed enum, not a string, so an unknown provider is unrepresentable (§1.2).
/// `Fireworks` is the default, mirroring the R package.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ProviderChoice {
    /// Fireworks (OpenAI-compatible chat completions). Reads `FIREWORKS_API_KEY`.
    #[default]
    Fireworks,
    /// Anthropic (native messages API). Reads `ANTHROPIC_API_KEY`.
    Anthropic,
}

impl ProviderChoice {
    /// The environment variable holding this provider's API key.
    ///
    /// The single source of truth for the key var, so the boundary guard and any
    /// "set your key" message name the same variable.
    pub fn key_var(self) -> &'static str {
        match self {
            ProviderChoice::Fireworks => "FIREWORKS_API_KEY",
            ProviderChoice::Anthropic => "ANTHROPIC_API_KEY",
        }
    }

    /// The provider's default API base URL.
    ///
    /// Fireworks is OpenAI-compatible, reached at its inference root; rig appends
    /// the `/chat/completions` path. Overridable per request to point at a mock
    /// server in tests (ADR-0006).
    pub fn default_base_url(self) -> &'static str {
        match self {
            ProviderChoice::Fireworks => "https://api.fireworks.ai/inference/v1",
            ProviderChoice::Anthropic => "https://api.anthropic.com",
        }
    }

    /// The default model id for this provider.
    ///
    /// Fireworks mirrors the browser BYOK fallback (`deepseek-v4-flash` in
    /// `assets/shared/feedback.js`); Anthropic uses a current Claude model. Model
    /// selection is not yet configurable — a later slice can lift these to a
    /// setting.
    pub fn default_model(self) -> &'static str {
        match self {
            ProviderChoice::Fireworks => "accounts/fireworks/models/deepseek-v4-flash",
            ProviderChoice::Anthropic => "claude-sonnet-4-5",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fireworks_is_the_default_provider() {
        assert_eq!(ProviderChoice::default(), ProviderChoice::Fireworks);
    }

    #[test]
    fn each_provider_names_its_own_key_var() {
        assert_eq!(ProviderChoice::Fireworks.key_var(), "FIREWORKS_API_KEY");
        assert_eq!(ProviderChoice::Anthropic.key_var(), "ANTHROPIC_API_KEY");
    }

    #[test]
    fn each_provider_targets_its_own_api_host() {
        // The base URL is the request root; rig appends the path. The integration
        // tests always override it with a mock, so pin the real defaults here.
        assert!(
            ProviderChoice::Fireworks
                .default_base_url()
                .contains("api.fireworks.ai"),
            "Fireworks targets its inference host, got {}",
            ProviderChoice::Fireworks.default_base_url()
        );
        assert!(
            ProviderChoice::Anthropic
                .default_base_url()
                .contains("api.anthropic.com"),
            "Anthropic targets its native host, got {}",
            ProviderChoice::Anthropic.default_base_url()
        );
    }

    #[test]
    fn each_provider_has_a_recognizable_default_model() {
        // Pinned by exact equality so a wrong-but-similar model id (prefix typo,
        // stale bump) fails rather than sneaking through a substring match. The
        // mock ignores the model field, so nothing else exercises this value —
        // this assertion is load-bearing. Anthropic stays a substring because its
        // CLI default (`claude-sonnet-4-5`) differs from the browser BYOK default
        // (`claude-opus-4-8`); only the Claude family is invariant across both.
        assert_eq!(
            ProviderChoice::Fireworks.default_model(),
            "accounts/fireworks/models/deepseek-v4-flash",
            "the Fireworks default model id must match the browser BYOK fallback",
        );
        assert!(
            ProviderChoice::Anthropic.default_model().contains("claude"),
            "the Anthropic model id is a Claude model, got {}",
            ProviderChoice::Anthropic.default_model()
        );
    }
}
