#!/usr/bin/env python3
"""Verify filter output: assert 9-key SiteLesson JSON contract.

Reads an HTML file, extracts all bt-exercise widget JSON payloads,
and asserts the contract from AC-2's executable spec.

Usage: python3 verify_filter_output.py <html-file>
"""

from __future__ import annotations

import json
import re
import sys
from typing import Any


# The 9 SiteLesson keys that MUST be present in every widget JSON.
REQUIRED_KEYS: set[str] = {
    "id",
    "title",
    "prompt",
    "code_template",
    "checks",
    "packages",
    "solution",
    "hints",
    "gotchas",
}

# Keys that MUST NOT appear — llm_evaluation_prompt is server/CLI only
# and must never be shipped to the browser (ADR-0008).
FORBIDDEN_KEYS: set[str] = {"llm_evaluation_prompt"}


def extract_widgets(html: str) -> list[dict[str, Any]]:
    """Extract all bt-exercise widget JSON payloads from HTML.

    The filter emits:
        <div class="bt-exercise">
        <script type="application/json">{...}</script>
        </div>

    Returns a list of parsed JSON dicts, one per widget.
    """
    pattern = (
        r'<div class="bt-exercise">\s*<script type="application/json">(.*?)</script>'
    )
    matches = re.findall(pattern, html, re.DOTALL)
    return [json.loads(m) for m in matches]


def assert_full_exercise(data: dict[str, Any], errors: list[str]) -> None:
    """Assert conditions for the full R exercise (index 0).

    Full exercise: prompt has <code> (HTML rendered), code_template has <-,
    checks array len 2, solution has "a + b", hints has <-, gotchas null,
    packages [].
    """
    # prompt has <code> (HTML rendered from inline code)
    if "<code>" not in data["prompt"]:
        errors.append("Full exercise: prompt missing <code> tag")

    # code_template has <-
    ct = data["code_template"]
    if ct is None or "<-" not in ct:
        errors.append(f"Full exercise: code_template missing '<-' — got: {ct!r}")

    # checks array len 2
    checks = data["checks"]
    if not isinstance(checks, list) or len(checks) != 2:
        errors.append(
            f"Full exercise: checks should be len 2, got {len(checks) if isinstance(checks, list) else 'non-list'}"
        )

    # solution has "a + b"
    if data["solution"] != "a + b":
        errors.append(
            f"Full exercise: solution should be 'a + b', got: {data['solution']!r}"
        )

    # hints has <-
    hints = data["hints"]
    if hints is None or "<-" not in hints:
        errors.append(f"Full exercise: hints missing '<-' — got: {hints!r}")

    # gotchas null
    if data["gotchas"] is not None:
        errors.append(
            f"Full exercise: gotchas should be null, got: {data['gotchas']!r}"
        )

    # packages []
    if data["packages"] != []:
        errors.append(
            f"Full exercise: packages should be [], got: {data['packages']!r}"
        )


def assert_minimal_python(data: dict[str, Any], errors: list[str]) -> None:
    """Assert conditions for the minimal Python exercise (index 1).

    Minimal Python: packages parsed from attribute, all absent fields null/[].
    """
    if data["packages"] != ["numpy", "pandas"]:
        errors.append(
            f"Minimal Python: packages should be ['numpy', 'pandas'], got: {data['packages']!r}"
        )

    if data["code_template"] is not None:
        errors.append(
            f"Minimal Python: code_template should be null, got: {data['code_template']!r}"
        )

    if data["checks"] != []:
        errors.append(f"Minimal Python: checks should be [], got: {data['checks']!r}")

    if data["solution"] is not None:
        errors.append(
            f"Minimal Python: solution should be null, got: {data['solution']!r}"
        )

    if data["hints"] is not None:
        errors.append(f"Minimal Python: hints should be null, got: {data['hints']!r}")

    if data["gotchas"] is not None:
        errors.append(
            f"Minimal Python: gotchas should be null, got: {data['gotchas']!r}"
        )


def assert_empty_exercise(data: dict[str, Any], errors: list[str]) -> None:
    """Assert conditions for the empty exercise (index 2).

    Empty exercise: all fields null/[] except id/title (auto-generated).
    """
    if data["prompt"] != "":
        errors.append(f"Empty exercise: prompt should be '', got: {data['prompt']!r}")

    if data["code_template"] is not None:
        errors.append(
            f"Empty exercise: code_template should be null, got: {data['code_template']!r}"
        )

    if data["checks"] != []:
        errors.append(f"Empty exercise: checks should be [], got: {data['checks']!r}")

    if data["packages"] != []:
        errors.append(
            f"Empty exercise: packages should be [], got: {data['packages']!r}"
        )

    if data["solution"] is not None:
        errors.append(
            f"Empty exercise: solution should be null, got: {data['solution']!r}"
        )

    if data["hints"] is not None:
        errors.append(f"Empty exercise: hints should be null, got: {data['hints']!r}")

    if data["gotchas"] is not None:
        errors.append(
            f"Empty exercise: gotchas should be null, got: {data['gotchas']!r}"
        )


def main() -> int:
    """Run all assertions and return exit code (0=pass, 1=fail)."""
    if len(sys.argv) < 2:
        print("Usage: verify_filter_output.py <html-file>", file=sys.stderr)
        return 2

    html_file = sys.argv[1]
    try:
        with open(html_file) as f:
            html = f.read()
    except OSError as e:
        print(f"ERROR: cannot read {html_file}: {e}", file=sys.stderr)
        return 2

    errors: list[str] = []

    # Extract widgets
    try:
        widgets = extract_widgets(html)
    except json.JSONDecodeError as e:
        print(f"  FAIL: JSON decode error in widget payload: {e}", file=sys.stderr)
        return 1

    # Assertion: >=3 exercises
    if len(widgets) < 3:
        errors.append(f"Expected >=3 bt-exercise widgets, found {len(widgets)}")

    # For each widget: 9 keys present, no forbidden keys, title non-empty
    for i, data in enumerate(widgets):
        keys = set(data.keys())
        missing = REQUIRED_KEYS - keys
        extra = keys - REQUIRED_KEYS
        forbidden = keys & FORBIDDEN_KEYS

        if missing:
            errors.append(f"Exercise {i}: missing keys: {sorted(missing)}")
        if extra:
            errors.append(
                f"Exercise {i}: extra keys (not in 9-key contract): {sorted(extra)}"
            )
        if forbidden:
            errors.append(f"Exercise {i}: FORBIDDEN key present: {sorted(forbidden)}")

        # title non-empty
        if not data.get("title"):
            errors.append(f"Exercise {i}: title is empty or missing")

    # Check specific exercises if we have enough
    if len(widgets) >= 1:
        assert_full_exercise(widgets[0], errors)
    if len(widgets) >= 2:
        assert_minimal_python(widgets[1], errors)
    if len(widgets) >= 3:
        assert_empty_exercise(widgets[2], errors)

    # IDs distinct
    ids = [w["id"] for w in widgets]
    if len(ids) != len(set(ids)):
        errors.append(f"IDs not distinct: {ids}")

    # llm_evaluation_prompt ABSENT in all (redundant with forbidden check, but explicit)
    for i, data in enumerate(widgets):
        if "llm_evaluation_prompt" in data:
            errors.append(
                f"Exercise {i}: llm_evaluation_prompt present (FORBIDDEN by ADR-0008)"
            )

    if errors:
        for e in errors:
            print(f"  FAIL: {e}", file=sys.stderr)
        return 1

    print(f"All assertions passed ({len(widgets)} widgets verified)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
