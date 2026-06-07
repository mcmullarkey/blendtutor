//! The LLM feedback layer: a pure prompt builder and the rig-core provider client
//! that turns a model's structured tool call into a typed verdict.
//!
//! Three concerns, one submodule each (§4.1): `prompt` builds the
//! injection-hardened prompt (pure, §2.1); `provider` is the closed set of LLM
//! backends; `feedback` runs the request and maps the result to a [`Verdict`]
//! (the only effectful part, §2.2). The split keeps the pure prompt core separate
//! from the effectful request (§3.1). This layer owns prompt-build, provider-client
//! construction, and the DTO → domain mapping; it does **not** run learner code or
//! score evals — that is the [`runner`](crate::runner) and [`grade`](crate::grade)
//! layers' job. See ADR-0006.
//!
//! The public names are re-exported here, so callers use `core::llm::build_prompt`,
//! `core::llm::OPEN_CODE`, etc., without depending on the internal split.

mod feedback;
mod prompt;
mod provider;

pub use feedback::{FeedbackError, Verdict, request_feedback};
pub use prompt::{
    CHECKS_LABEL, CLOSE_CODE, ExecResults, OPEN_CODE, OUTPUT_LABEL, Prompt, Submission,
    build_prompt,
};
pub use provider::ProviderChoice;
