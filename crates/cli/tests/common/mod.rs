//! Shared helpers for the `blendtutor run` integration tests.
//!
//! `run` executes learner code through the real interpreter and asks the LLM
//! provider for a verdict. The interpreter is real (so these tests skip-with-
//! notice when `Rscript` is absent, per the Slice-1 convention). The provider is
//! a `wiremock` server bound to a real localhost port and pointed at via the
//! `BLENDTUTOR_PROVIDER_URL` base-URL override (ADR-0006) — the spawned binary
//! connects to it exactly as it would a real provider, so the request/extract/
//! verdict path is exercised end to end without a real API.
//!
//! `assert_cmd::Command` is synchronous, so a test spawns the binary inside
//! `tokio::task::spawn_blocking`: that frees the test's async runtime to keep
//! serving the wiremock mock while the child's HTTP request is in flight (a
//! direct blocking call on a single-threaded runtime would deadlock).

use assert_cmd::Command;
use serde_json::json;
use wiremock::matchers::method;
use wiremock::{Mock, MockServer, ResponseTemplate};

/// True (after printing a notice) when `Rscript` is not on `PATH`, so a
/// real-interpreter test can `return` early instead of failing on machines
/// without R installed (mirrors `core/tests/runner.rs`).
pub fn rscript_absent() -> bool {
    let present = std::process::Command::new("Rscript")
        .arg("--version")
        .output()
        .map(|out| out.status.success())
        .unwrap_or(false);
    if !present {
        eprintln!("SKIP: Rscript absent — skipping real-interpreter run test");
    }
    !present
}

/// Mount a 200 returning a real OpenAI chat-completions envelope whose single
/// tool call carries `arguments` as a JSON-**encoded string** — the shape rig's
/// openai client parses. The tool is named `submit`, the name rig's `Extractor`
/// forces. Mirrors `core/tests/llm.rs::mount_tool_call` so the CLI exercises the
/// same wire contract the provider layer is unit-tested against.
pub async fn mount_tool_call(server: &MockServer, arguments: &str) {
    let body = json!({
        "id": "chatcmpl-test",
        "object": "chat.completion",
        "created": 0,
        "model": "test-model",
        "choices": [{
            "index": 0,
            "message": {
                "role": "assistant",
                "content": null,
                "tool_calls": [{
                    "id": "call_1",
                    "type": "function",
                    "function": { "name": "submit", "arguments": arguments }
                }]
            },
            "finish_reason": "tool_calls"
        }],
        "usage": { "prompt_tokens": 1, "completion_tokens": 1, "total_tokens": 2 }
    });
    Mock::given(method("POST"))
        .respond_with(ResponseTemplate::new(200).set_body_json(body))
        .mount(server)
        .await;
}

/// The ported example lesson (R, no checks — LLM-graded), under `core`'s fixtures.
pub const LESSON: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/lessons/add_two_numbers.yaml"
);

/// A correct `add_two` submission (valid R that runs cleanly).
pub const CORRECT_CODE: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/fixtures/runs/add_two_numbers_correct.R"
);

/// A wrong-but-runnable `add_two` submission (valid R, wrong arithmetic).
pub const WRONG_CODE: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/fixtures/runs/add_two_numbers_wrong.R"
);

/// Run the `blendtutor` binary with `args`, supplying a dummy Fireworks key and
/// pointing the provider seam at `provider_url`, and capture its output.
///
/// Synchronous (`assert_cmd`); call inside `spawn_blocking` so the wiremock
/// server on the test runtime keeps serving while this blocks. The key is set on
/// the **child** (never the test process), so tests need no serialization.
pub fn blendtutor_output(args: Vec<String>, provider_url: String) -> std::process::Output {
    Command::cargo_bin("blendtutor")
        .expect("binary `blendtutor` should be built")
        .args(&args)
        .env("FIREWORKS_API_KEY", "test-key")
        .env("BLENDTUTOR_PROVIDER_URL", provider_url)
        .output()
        .expect("running `blendtutor run` should produce output")
}
