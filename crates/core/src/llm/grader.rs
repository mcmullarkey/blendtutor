//! The grader configuration: which provider and which model grade a submission
//! (ADR-0027).
//!
//! Holds [`Grader`] and [`ModelId`]. A grader used to be just a
//! [`ProviderChoice`] with a compile-time model, so a model that vanished from
//! the provider broke every graded path. Making the model part of a value also
//! lets two graders be compared side by side without shared state.
//!
//! Pure data. Building a client and sending the request stay in
//! [`request_feedback`](super::request_feedback).

use std::fmt;
use std::str::FromStr;

use super::ProviderChoice;

/// A provider model id, e.g. `accounts/fireworks/models/deepseek-v4p1-flash-1017`.
/// Never empty or blank, so a request cannot go out naming no model (§1.2).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModelId(String);

impl ModelId {
    /// The id as sent to the provider.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Why a string is not a [`ModelId`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmptyModelId;

impl fmt::Display for EmptyModelId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("model id must not be empty")
    }
}

impl std::error::Error for EmptyModelId {}

impl FromStr for ModelId {
    type Err = EmptyModelId;

    /// Parse a model id, trimming surrounding whitespace and refusing a blank one.
    fn from_str(raw: &str) -> Result<Self, Self::Err> {
        let trimmed = raw.trim();
        if trimmed.is_empty() {
            return Err(EmptyModelId);
        }
        Ok(Self(trimmed.to_string()))
    }
}

/// Who grades: a provider and the model to ask.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Grader {
    provider: ProviderChoice,
    model: ModelId,
}

impl Grader {
    /// The provider with its default model.
    pub fn default_for(provider: ProviderChoice) -> Self {
        Self {
            provider,
            model: ModelId(provider.default_model().to_string()),
        }
    }

    /// The same provider, asking `model` instead.
    pub fn with_model(self, model: ModelId) -> Self {
        Self { model, ..self }
    }

    /// The provider to call.
    pub fn provider(&self) -> ProviderChoice {
        self.provider
    }

    /// The model to ask.
    pub fn model(&self) -> &ModelId {
        &self.model
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_blank_model_id_is_refused() {
        assert_eq!("".parse::<ModelId>(), Err(EmptyModelId));
        assert_eq!("   ".parse::<ModelId>(), Err(EmptyModelId));
        assert_eq!(EmptyModelId.to_string(), "model id must not be empty");
    }

    #[test]
    fn a_model_id_is_trimmed() {
        let id: ModelId = "  accounts/x/models/y \n".parse().expect("parses");
        assert_eq!(id.as_str(), "accounts/x/models/y");
    }

    #[test]
    fn the_default_grader_asks_the_provider_default_model() {
        let grader = Grader::default_for(ProviderChoice::Fireworks);
        assert_eq!(grader.provider(), ProviderChoice::Fireworks);
        assert_eq!(
            grader.model().as_str(),
            ProviderChoice::Fireworks.default_model()
        );
    }

    #[test]
    fn with_model_overrides_only_the_model() {
        let grader = Grader::default_for(ProviderChoice::Anthropic)
            .with_model("claude-x".parse().expect("parses"));
        assert_eq!(grader.provider(), ProviderChoice::Anthropic);
        assert_eq!(grader.model().as_str(), "claude-x");
    }
}
