"""E2E evidence stub for issue #229 — a minimal OpenAI-compatible provider.

Serves the `submit` tool-call envelope the CLI's rig client parses, returning a
distinct verdict per submission token (alpha/beta/gamma), mirroring the
wiremock mounts in crates/cli/tests/eval.rs. Run:

    uv run python docs/evidence/229/provider_stub.py <port>
"""

import json
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer

VERDICTS = {
    "alpha": (True, "alpha looks right"),
    "beta": (False, "beta is off"),
    "gamma": (False, "gamma is off"),
}


def envelope(is_correct: bool, feedback: str) -> bytes:
    arguments = json.dumps({"is_correct": is_correct, "feedback_message": feedback})
    body = {
        "id": "chatcmpl-stub",
        "object": "chat.completion",
        "created": 0,
        "model": "stub-model",
        "choices": [
            {
                "index": 0,
                "message": {
                    "role": "assistant",
                    "content": None,
                    "tool_calls": [
                        {
                            "id": "call_1",
                            "type": "function",
                            "function": {"name": "submit", "arguments": arguments},
                        }
                    ],
                },
                "finish_reason": "tool_calls",
            }
        ],
        "usage": {"prompt_tokens": 1, "completion_tokens": 1, "total_tokens": 2},
    }
    return json.dumps(body).encode()


class Handler(BaseHTTPRequestHandler):
    def do_POST(self) -> None:
        length = int(self.headers.get("content-length", 0))
        request_body = self.rfile.read(length).decode()
        verdict = (False, "no scripted verdict for this submission")
        for token, (is_correct, feedback) in VERDICTS.items():
            if token in request_body:
                verdict = (is_correct, feedback)
                break
        self.send_response(200)
        self.send_header("content-type", "application/json")
        self.end_headers()
        self.wfile.write(envelope(*verdict))

    def log_message(self, *args) -> None:
        pass


if __name__ == "__main__":
    HTTPServer(("127.0.0.1", int(sys.argv[1])), Handler).serve_forever()
