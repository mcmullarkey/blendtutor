#!/usr/bin/env python3
"""Executable spec for issue #197 — LLM-judge checker (judge_feedback.py).

Verifies the 15-clause compound predicate from AC-4 (smevals-eval-report):
  P1  (env-not-argv)   — checker invoked with zero argv; reads
                         $SMEVALS_RUN_DIR/output.txt via the ABSOLUTE env path.
                         The test runs with cwd != run dir and plants a decoy
                         output.txt in cwd: the graded message must come from
                         the run dir, never the decoy (no cwd-relative reads).
  P2  (normalization)  — score == mean_of_5_dimensions / 5. Stubbed judge
                         response mean 4.4/5 → stdout JSON score == 0.88
                         (abs diff < 1e-9).
  P3  (threshold sep)  — stubbed low score (0.4/5 → 0.08) → exit 0 with
                         score 0.08 on stdout. Threshold application belongs
                         to smevals, NOT the checker. Nonzero exit = check
                         ERROR only.
  P4  (tool-call shape)— request body model == $SMEVALS_CHECK_MODEL when set
                         (checked BEFORE $SMEVALS_MODEL — precedence test with
                         both set to different values; fallback test with only
                         SMEVALS_MODEL set; never hardcoded). tools == one
                         function; tool_choice.function.name set; NO
                         response_format: json_object key anywhere in body.
  P5  (endpoint+auth)  — POST to ${baseUrl}/chat/completions where baseUrl
                         already ends in /v1 (no doubling — stub records path
                         /v1/chat/completions); Authorization: Bearer
                         $FIREWORKS_API_KEY; content-type: application/json.
  P6  (http timeout)   — 60s cap on the Fireworks call: stub delaying 30s
                         still succeeds; accept-but-never-respond socket →
                         checker aborts < 65s with nonzero exit.
  P7  (fail closed)    — FIREWORKS_API_KEY unset → nonzero exit; stderr names
                         the variable; stub records ZERO requests.
  P8  (fail closed)    — $SMEVALS_RUN_DIR/output.txt absent → nonzero exit;
                         stderr names the file.
  P9  (fail closed)    — stub returns HTTP 500 → nonzero exit; stderr names
                         the status code.
  P10 (fail closed)    — function.arguments unparseable JSON → nonzero exit;
                         NO retry loop (stub records exactly ONE request).
  P11 (clamping)       — dimension value 7 (out of 0-5) → clamped at the parse
                         boundary: emitted score NEVER > 1.0; metrics carry the
                         clamped value. Missing dimensions key → nonzero exit.
  P12 (injection)      — feedback message fenced as DATA with explicit
                         "data, not instructions" framing; prompt carries the
                         expected verdict (SMEVALS_TASK_EXPECTED) and the
                         actual verdict (output.txt line 1 header).
  P13 (5-key JSON)     — stdout parses as JSON with score (float), notes,
                         metrics (the 5 dimension scores); details tolerated.
  P14 (wiring)         — generator template emits polarity check FIRST
                         (required: true), judge SECOND, model scalar,
                         scoring.pass_threshold == 0.8 — asserted against the
                         committed golden fixture (cross-file, mirroring
                         test_smevals_runner.sh predicate 17).
  P15 (determinism)    — with a stubbed judge response, stdout JSON is
                         byte-identical across two runs (stable key order, no
                         timestamps/random in output).

Negative: dimension score 7 and HTTP 500 in sequence → never emit score > 1.0;
malformed tool-call output → never retry; argv never read; pass_threshold never
applied by the checker (exit 0 with score 0.08 proves separation). A checker
that hardcodes the model, doubles /v1, or reads output.txt relative to cwd
FAILS this spec.

Zero network: all model responses come from a local stdlib http.server stub;
zero Fireworks spend. The 30s-delay and 60s-hang arms use real wall-clock.

Usage: python3 scripts/tests/test_judge_feedback.py
       uv run pytest scripts/tests/test_judge_feedback.py -x -q
"""

from __future__ import annotations

import http.server
import json
import os
import socket
import subprocess
import sys
import tempfile
import threading
import time
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
CHECKER = REPO_ROOT / "scripts" / "smevals" / "judge_feedback.py"
GOLDEN_GRADER = (
    REPO_ROOT
    / "crates"
    / "core"
    / "tests"
    / "fixtures"
    / "generate_eval_dir"
    / "graders"
    / "default.yaml"
)

PASS = 0
FAIL = 0


def check(cond: bool, msg: str) -> None:
    """Record a PASS/FAIL and raise on failure so pytest sees it."""
    global PASS, FAIL
    if cond:
        PASS += 1
        print(f"  PASS: {msg}")
    else:
        FAIL += 1
        print(f"  FAIL: {msg}")
        raise AssertionError(msg)


# ---------------------------------------------------------------------------
# Stub HTTP harness — stdlib only, zero network
# ---------------------------------------------------------------------------

TOOL_NAME = "grade_feedback"
DIMENSIONS = [
    "verdict_rationale_correctness",
    "actionability",
    "references_check_results",
    "no_solution_leak",
    "no_hallucinated_errors",
]
# Mean 4.4/5 → 0.88 (predicate 2).
DIMS_HIGH = {d: 4 for d in DIMENSIONS}
DIMS_HIGH["references_check_results"] = 5
DIMS_HIGH["no_solution_leak"] = 5
# Mean 0.4/5 → 0.08 (predicate 3).
DIMS_LOW = {d: 0 for d in DIMENSIONS}
DIMS_LOW["references_check_results"] = 1
DIMS_LOW["no_solution_leak"] = 1

OUTPUT_OK = "verdict: correct\nThe solution is correct and well-reasoned.\nSecond line."
DECOY_OUTPUT = "verdict: incorrect\nDECOY MESSAGE"


def tool_call_response(arguments) -> dict:
    """An OpenAI-compatible chat completion carrying one forced tool call."""
    return {
        "choices": [
            {
                "message": {
                    "role": "assistant",
                    "content": None,
                    "tool_calls": [
                        {
                            "id": "call_1",
                            "type": "function",
                            "function": {
                                "name": TOOL_NAME,
                                "arguments": json.dumps(arguments),
                            },
                        }
                    ],
                }
            }
        ]
    }


class _StubHandler(http.server.BaseHTTPRequestHandler):
    """Records path/headers/body per request; serves the configured response."""

    def do_POST(self):
        s = self.server
        length = int(self.headers.get("Content-Length") or 0)
        raw = self.rfile.read(length) if length else b""
        s.records.append(
            {
                "path": self.path,
                "headers": dict(self.headers),
                "body": json.loads(raw.decode("utf-8") or "{}"),
            }
        )
        if s.delay_seconds:
            time.sleep(s.delay_seconds)
        if s.status == 200:
            if isinstance(s.response, bytes):
                payload = s.response  # raw body passthrough (malformed-JSON arm)
            else:
                payload = json.dumps(s.response).encode("utf-8")
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(payload)))
            self.end_headers()
            self.wfile.write(payload)
        else:
            self.send_response(s.status)
            self.end_headers()

    def log_message(self, *args):  # silence per-request logging
        pass


class StubServer:
    """Local HTTP stub: records requests; serves a canned tool-call response."""

    def __init__(self, response: dict, status: int = 200, delay: float = 0):
        self.server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), _StubHandler)
        self.server.response = response
        self.server.status = status
        self.server.delay_seconds = delay
        self.server.records = []
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()

    @property
    def port(self) -> int:
        return self.server.server_address[1]

    def url(self) -> str:
        # baseUrl that already ends in /v1 (mirrors provider.rs:43) — the
        # checker must append only /chat/completions, never double /v1.
        return f"http://127.0.0.1:{self.port}/v1"

    def requests(self) -> list:
        return list(self.server.records)

    def stop(self) -> None:
        self.server.shutdown()
        self.server.server_close()


class HangServer:
    """Accepts connections but never responds — drives the 60s timeout arm."""

    def __init__(self):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.sock.bind(("127.0.0.1", 0))
        self.sock.listen(8)
        self.sock.settimeout(0.2)
        self.conns = []
        self._stop = threading.Event()
        self.thread = threading.Thread(target=self._serve, daemon=True)
        self.thread.start()

    def _serve(self) -> None:
        while not self._stop.is_set():
            try:
                conn, _ = self.sock.accept()
            except socket.timeout:
                continue
            self.conns.append(conn)  # accepted, never responded to

    def url(self) -> str:
        return f"http://127.0.0.1:{self.sock.getsockname()[1]}/v1"

    def stop(self) -> None:
        self._stop.set()
        for conn in self.conns:
            conn.close()
        self.sock.close()


def base_env(stub_url: str, **overrides) -> dict:
    """A minimal valid checker env: run dir set by run_checker; all vars injected."""
    env = {
        "FIREWORKS_API_KEY": "test-key-123",
        "FIREWORKS_BASE_URL": stub_url,
        "SMEVALS_TASK_EXPECTED": "correct",
        "SMEVALS_CHECK_MODEL": "accounts/fireworks/models/deepseek-v4-flash-0731",
    }
    for name, value in overrides.items():
        if value is None:
            env.pop(name, None)  # None = explicitly unset
        else:
            env[name] = value
    return env


def run_checker(
    env: dict,
    output_txt: str | None = OUTPUT_OK,
    cwd_decoy: str | None = None,
    timeout: float = 75,
):
    """Run the checker: cwd is a temp 'grade workspace' distinct from the run
    dir; only SMEVALS_RUN_DIR points at the run dir (the real smevals contract
    — cli.py runs checkers with cwd = grade workspace)."""
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        run_dir = root / "run"
        run_dir.mkdir()
        if output_txt is not None:
            (run_dir / "output.txt").write_text(output_txt)
        work = root / "work"
        work.mkdir()
        if cwd_decoy is not None:
            (work / "output.txt").write_text(cwd_decoy)
        full_env = dict(os.environ)
        for name in (
            "FIREWORKS_API_KEY",
            "FIREWORKS_BASE_URL",
            "SMEVALS_TASK_EXPECTED",
            "SMEVALS_CHECK_MODEL",
            "SMEVALS_MODEL",
        ):
            full_env.pop(name, None)
        full_env["SMEVALS_RUN_DIR"] = str(run_dir.resolve())
        full_env.update(env)
        t0 = time.monotonic()
        result = subprocess.run(
            [sys.executable, str(CHECKER)],  # zero argv — script path only
            env=full_env,
            cwd=str(work.resolve()),
            capture_output=True,
            text=True,
            timeout=timeout,
        )
        elapsed = time.monotonic() - t0
        return result, elapsed


def grade_json(result: subprocess.CompletedProcess) -> dict:
    """Parse the checker's stdout as the smevals grader JSON contract."""
    return json.loads(result.stdout)


# ---------------------------------------------------------------------------
# The 15 predicate arms
# ---------------------------------------------------------------------------


def test_p1_env_not_argv_absolute_run_dir() -> None:
    print("  -- P1: zero argv; output.txt via absolute SMEVALS_RUN_DIR path --")
    stub = StubServer(tool_call_response(DIMS_HIGH))
    try:
        # cwd gets a DECOY output.txt: a cwd-relative read would grade the
        # decoy message; only an absolute env-path read grades the run dir's.
        result, _ = run_checker(
            base_env(stub.url()), output_txt=OUTPUT_OK, cwd_decoy=DECOY_OUTPUT
        )
        check(result.returncode == 0, f"exit 0 (got: {result.returncode})")
        check(
            stub.requests()[0]["body"]["messages"][0]["content"].find("DECOY MESSAGE")
            == -1,
            "decoy cwd output.txt never graded",
        )
        check(
            "well-reasoned" in stub.requests()[0]["body"]["messages"][0]["content"],
            "run-dir output.txt graded via absolute env path",
        )
    finally:
        stub.stop()


def test_p2_score_normalization() -> None:
    print("  -- P2: score == mean_of_5_dimensions / 5 (4.4/5 → 0.88) --")
    stub = StubServer(tool_call_response(DIMS_HIGH))
    try:
        result, _ = run_checker(base_env(stub.url()))
        check(result.returncode == 0, f"exit 0 (got: {result.returncode})")
        score = grade_json(result)["score"]
        check(
            isinstance(score, float),
            f"score is a float (got: {type(score).__name__})",
        )
        check(
            abs(score - 0.88) < 1e-9,
            f"score == 0.88 (got: {score})",
        )
    finally:
        stub.stop()


def test_p3_exit_code_threshold_separation() -> None:
    print("  -- P3: low score 0.4/5 → exit 0 with score 0.08 (no threshold) --")
    stub = StubServer(tool_call_response(DIMS_LOW))
    try:
        result, _ = run_checker(base_env(stub.url()))
        check(result.returncode == 0, f"low score still exits 0 (got: {result.returncode})")
        score = grade_json(result)["score"]
        check(
            abs(score - 0.08) < 1e-9,
            f"score == 0.08 (got: {score})",
        )
    finally:
        stub.stop()


def test_p4_tool_call_shape_and_model_precedence() -> None:
    print("  -- P4: tool-call shape; model from CHECK_MODEL then MODEL --")
    stub = StubServer(tool_call_response(DIMS_HIGH))
    try:
        result, _ = run_checker(
            base_env(
                stub.url(),
                SMEVALS_CHECK_MODEL="check-model-pin",
                SMEVALS_MODEL="fallback-model",
            )
        )
        check(result.returncode == 0, f"exit 0 (got: {result.returncode})")
        body = stub.requests()[0]["body"]
        check(
            body["model"] == "check-model-pin",
            f"SMEVALS_CHECK_MODEL wins precedence (got: {body['model']})",
        )
        check(
            isinstance(body["tools"], list) and len(body["tools"]) == 1,
            f"tools == one function (got: {len(body['tools'])} tools)",
        )
        fn = body["tools"][0]["function"]
        check(body["tools"][0]["type"] == "function", "tool type == function")
        check(fn["name"] == TOOL_NAME, f"function name == {TOOL_NAME}")
        check(
            body["tool_choice"] == {"type": "function", "function": {"name": TOOL_NAME}},
            "tool_choice.function.name forces the judge tool",
        )
        params = fn["parameters"]
        check(params["type"] == "object", "parameters type == object")
        check(
            set(params["properties"].keys()) == set(DIMENSIONS),
            "parameters carry the 5 rubric dimensions",
        )
        check(
            sorted(params["required"]) == sorted(DIMENSIONS),
            "all 5 dimensions required",
        )
        check(
            "response_format" not in body,
            "NO response_format key in request body",
        )
        # Precedence fallback: only SMEVALS_MODEL set.
        result2, _ = run_checker(
            base_env(stub.url(), SMEVALS_CHECK_MODEL=None, SMEVALS_MODEL="fallback-model")
        )
        check(result2.returncode == 0, "fallback arm exits 0")
        check(
            stub.requests()[1]["body"]["model"] == "fallback-model",
            f"SMEVALS_MODEL fallback (got: {stub.requests()[1]['body']['model']})",
        )
    finally:
        stub.stop()


def test_p5_endpoint_and_auth() -> None:
    print("  -- P5: ${baseUrl}/chat/completions (no /v1 doubling) + auth --")
    stub = StubServer(tool_call_response(DIMS_HIGH))
    try:
        result, _ = run_checker(base_env(stub.url()))
        check(result.returncode == 0, f"exit 0 (got: {result.returncode})")
        record = stub.requests()[0]
        check(
            record["path"] == "/v1/chat/completions",
            f"POST to /v1/chat/completions, no doubling (got: {record['path']})",
        )
        check(
            record["headers"].get("Authorization") == "Bearer test-key-123",
            "Authorization: Bearer $FIREWORKS_API_KEY",
        )
        check(
            record["headers"].get("Content-Type") == "application/json",
            "content-type: application/json",
        )
    finally:
        stub.stop()


def test_p6_http_timeout() -> None:
    print("  -- P6: 60s cap — 30s delay succeeds; hang aborts < 65s nonzero --")
    slow = StubServer(tool_call_response(DIMS_HIGH), delay=30)
    try:
        t0 = time.monotonic()
        result, elapsed = run_checker(base_env(slow.url()))
        check(result.returncode == 0, f"30s-delayed response still succeeds (got: {result.returncode})")
        check(
            time.monotonic() - t0 >= 29,
            "the checker actually waited for the delayed response",
        )
    finally:
        slow.stop()

    hang = HangServer()
    try:
        result, elapsed = run_checker(base_env(hang.url()))
        check(result.returncode != 0, "never-responding server → nonzero exit")
        check(
            elapsed < 65,
            f"aborts within the 60s cap, wall < 65s (got: {elapsed:.1f}s)",
        )
    finally:
        hang.stop()


def test_p7_missing_key_fail_closed() -> None:
    print("  -- P7: FIREWORKS_API_KEY unset → nonzero, names the var, no HTTP --")
    stub = StubServer(tool_call_response(DIMS_HIGH))
    try:
        result, _ = run_checker(base_env(stub.url(), FIREWORKS_API_KEY=None))
        check(result.returncode != 0, f"nonzero exit (got: {result.returncode})")
        check(
            "FIREWORKS_API_KEY" in result.stderr,
            f"stderr names the variable (got: {result.stderr.strip()})",
        )
        check(
            len(stub.requests()) == 0,
            "no HTTP request issued without a key",
        )
    finally:
        stub.stop()


def test_p8_missing_artifact_fail_closed() -> None:
    print("  -- P8: $SMEVALS_RUN_DIR/output.txt absent → nonzero, names file --")
    stub = StubServer(tool_call_response(DIMS_HIGH))
    try:
        result, _ = run_checker(base_env(stub.url()), output_txt=None)
        check(result.returncode != 0, f"nonzero exit (got: {result.returncode})")
        check(
            "output.txt" in result.stderr,
            f"stderr names the file (got: {result.stderr.strip()})",
        )
        check(
            len(stub.requests()) == 0,
            "no HTTP request issued without the artifact",
        )
    finally:
        stub.stop()


def test_p9_http_500_fail_closed() -> None:
    print("  -- P9: HTTP 500 → nonzero, stderr names status code --")
    stub = StubServer(tool_call_response(DIMS_HIGH), status=500)
    try:
        result, _ = run_checker(base_env(stub.url()))
        check(result.returncode != 0, f"nonzero exit (got: {result.returncode})")
        check(
            "500" in result.stderr,
            f"stderr names status code (got: {result.stderr.strip()})",
        )
    finally:
        stub.stop()


def test_p10_malformed_tool_call_args_no_retry() -> None:
    print("  -- P10: unparseable arguments → nonzero, exactly ONE request --")
    stub = StubServer(tool_call_response("{not json"))
    try:
        result, _ = run_checker(base_env(stub.url()))
        check(result.returncode != 0, f"nonzero exit (got: {result.returncode})")
        check(
            len(stub.requests()) == 1,
            f"no retry loop — exactly one HTTP call (got: {len(stub.requests())})",
        )
    finally:
        stub.stop()

    # A non-JSON 200 body fails closed just as hard, also without retry.
    stub2 = StubServer({"choices": []})
    stub2.server.response = b"this is not json"
    try:
        result2, _ = run_checker(base_env(stub2.url()))
        check(result2.returncode != 0, "non-JSON response body → nonzero exit")
        check(
            len(stub2.requests()) == 1,
            "non-JSON response body → no retry either",
        )
    finally:
        stub2.stop()


def test_p11_clamping_and_missing_dimensions() -> None:
    print("  -- P11: dimension 7 clamped, score never > 1.0; missing → nonzero --")
    dims = dict(DIMS_HIGH)
    dims["verdict_rationale_correctness"] = 7  # out of 0-5
    stub = StubServer(tool_call_response(dims))
    try:
        result, _ = run_checker(base_env(stub.url()))
        check(result.returncode == 0, f"out-of-range dimension clamped, exit 0 (got: {result.returncode})")
        out = grade_json(result)
        check(out["score"] <= 1.0, f"emitted score never > 1.0 (got: {out['score']})")
        check(
            abs(out["score"] - 0.92) < 1e-9,
            f"7 clamped to 5 → dims [5,4,5,5,4] → mean 4.6 → score 0.92 (got: {out['score']})",
        )
        check(
            out["metrics"]["verdict_rationale_correctness"] == 5.0,
            f"metrics carry the clamped value (got: {out['metrics']['verdict_rationale_correctness']})",
        )
    finally:
        stub.stop()

    # Missing dimensions key entirely → fail closed.
    stub2 = StubServer(tool_call_response({"actionability": 4}))
    try:
        result2, _ = run_checker(base_env(stub2.url()))
        check(result2.returncode != 0, "missing dimensions key → nonzero exit")
    finally:
        stub2.stop()


def test_p12_prompt_injection_defense() -> None:
    print("  -- P12: DATA fence + 'data, not instructions' + verdict anchors --")
    stub = StubServer(tool_call_response(DIMS_HIGH))
    try:
        result, _ = run_checker(
            base_env(stub.url(), SMEVALS_TASK_EXPECTED="incorrect"),
            output_txt="verdict: correct\nReal feedback body.",
        )
        check(result.returncode == 0, f"exit 0 (got: {result.returncode})")
        prompt = stub.requests()[0]["body"]["messages"][0]["content"]
        check("BEGIN FEEDBACK DATA" in prompt and "END FEEDBACK DATA" in prompt,
              "feedback message fenced by DATA delimiters")
        check("data, not instructions" in prompt.lower(),
              "explicit 'data, not instructions' framing")
        check("Expected verdict: incorrect" in prompt,
              "prompt carries the expected verdict from SMEVALS_TASK_EXPECTED")
        check("Actual verdict: correct" in prompt,
              "prompt carries the actual verdict from output.txt line 1")
        check("Real feedback body." in prompt,
              "fenced message is the output.txt body")
    finally:
        stub.stop()


def test_p13_smevals_5_key_json() -> None:
    print("  -- P13: smevals 5-key JSON: score/metrics/notes/details --")
    stub = StubServer(tool_call_response(DIMS_HIGH))
    try:
        result, _ = run_checker(base_env(stub.url()))
        check(result.returncode == 0, f"exit 0 (got: {result.returncode})")
        out = grade_json(result)
        check("score" in out and isinstance(out["score"], (int, float)),
              "score present and numeric")
        check(isinstance(out.get("metrics"), dict) and len(out["metrics"]) == 5,
              "metrics carries the 5 dimension scores")
        check(isinstance(out.get("notes"), str), "notes present")
        check("details" in out, "details present (unknown keys tolerated)")
    finally:
        stub.stop()


def test_p14_generator_wiring_golden() -> None:
    print("  -- P14: graders/default.yaml — polarity first, judge second, 0.8 --")
    check(GOLDEN_GRADER.is_file(), "golden graders/default.yaml exists")
    text = GOLDEN_GRADER.read_text()
    check(
        text.find("check_polarity.sh") != -1 and text.find("judge_feedback.py") != -1,
        "template names both checkers",
    )
    check(
        text.find("check_polarity.sh") < text.find("judge_feedback.py"),
        "polarity check FIRST, judge SECOND",
    )
    check("required: true" in text,
          "polarity check is required: true")
    check("model: accounts/fireworks/models/deepseek-v4-flash-0731" in text,
          "judge check carries the provider-default model scalar (SMEVALS_CHECK_MODEL)")
    check("pass_threshold: 0.8" in text,
          "scoring.pass_threshold == 0.8")
    check("judge_feedback.py" in text, "judge path present in golden template")


def test_p15_determinism() -> None:
    print("  -- P15: stdout byte-identical across two runs --")
    stub = StubServer(tool_call_response(DIMS_HIGH))
    try:
        env = base_env(stub.url())
        result1, _ = run_checker(env)
        result2, _ = run_checker(env)
        check(result1.returncode == 0 and result2.returncode == 0, "both runs exit 0")
        check(
            result1.stdout == result2.stdout,
            "stdout JSON byte-identical (stable key order, no timestamps)",
        )
    finally:
        stub.stop()


# ---------------------------------------------------------------------------
# Main (python3 direct invocation; pytest collects the test_* functions)
# ---------------------------------------------------------------------------


def main() -> int:
    tests = [
        test_p1_env_not_argv_absolute_run_dir,
        test_p2_score_normalization,
        test_p3_exit_code_threshold_separation,
        test_p4_tool_call_shape_and_model_precedence,
        test_p5_endpoint_and_auth,
        test_p6_http_timeout,
        test_p7_missing_key_fail_closed,
        test_p8_missing_artifact_fail_closed,
        test_p9_http_500_fail_closed,
        test_p10_malformed_tool_call_args_no_retry,
        test_p11_clamping_and_missing_dimensions,
        test_p12_prompt_injection_defense,
        test_p13_smevals_5_key_json,
        test_p14_generator_wiring_golden,
        test_p15_determinism,
    ]
    print("=== AC-4 LLM-judge checker — test_judge_feedback.py ===\n")
    for t in tests:
        print(f"-- {t.__name__} --")
        try:
            t()
        except AssertionError:
            pass  # already counted by check()
        print()
    print(f"=== Results: {PASS} passed, {FAIL} failed ===")
    return 1 if FAIL else 0


if __name__ == "__main__":
    sys.exit(main())
