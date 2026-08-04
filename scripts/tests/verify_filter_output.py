#!/usr/bin/env python3
"""Verify filter output: assert 9-key SiteLesson JSON contract + data-language.

Reads an HTML file, extracts all bt-exercise widget JSON payloads and their
data-language attributes, and asserts:
  - the 9-key SiteLesson contract from AC-2's executable spec
  - every bt-exercise div carries exactly one data-language="r|python"
    (all-or-none: attribute count must equal widget count)
  - per-index language pairing matches quarto-fixture/filter.qmd order
    [r, python, r, r] (AC-1) — catches hardcoded-"r" fakes.

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

# The only languages the filter may emit (validate_language in blendtutor.lua).
ALLOWED_LANGUAGES: set[str] = {"r", "python"}

# Per-index expected language, matching quarto-fixture/filter.qmd exercise
# order (4-exercise order is load-bearing — do not reorder):
#   0: Full R exercise        -> "r"
#   1: Minimal Python exercise -> "python"
#   2: Empty exercise         -> "r"
#   3: XSS + gotchas exercise -> "r"
EXPECTED_LANGUAGES: list[str] = ["r", "python", "r", "r"]


def extract_widgets(html: str) -> list[dict[str, Any]]:
    """Extract all bt-exercise widget JSON payloads from HTML.

    The filter emits:
        <div class="bt-exercise" data-language="r|python">
        <script type="application/json">{...}</script>
        </div>

    The div opener is matched attr-tolerantly (`[^>]*`), so the JSON is
    found regardless of which attributes the filter emits on the div. The
    gap between the div opener and the payload script is matched lazily
    (`[\s\S]*?`) because the filter emits a static fallback block
    (`<div class="bt-exercise-static">…`) BEFORE the payload script
    (fix-demo-visible-exercises — progressive enhancement: static content
    server-rendered, JS upgrades it). The payload script is the only
    `script[type=application/json]` in the div, so the lazy match stops at
    the right script.

    Returns a list of parsed JSON dicts, one per widget.
    """
    pattern = (
        r'<div class="bt-exercise"[^>]*>[\s\S]*?'
        r'<script type="application/json">(.*?)</script>'
    )
    matches = re.findall(pattern, html, re.DOTALL)
    return [json.loads(m) for m in matches]


def extract_widget_languages(html: str) -> list[str | None]:
    """Extract the data-language attribute from each bt-exercise div opener.

    Returns one entry per widget div in document order. A div carrying zero
    (or more than one) data-language attributes yields None — the all-or-none
    contract requires exactly one per div.
    """
    langs: list[str | None] = []
    for div in re.findall(r'<div class="bt-exercise"[^>]*>', html):
        attrs = re.findall(r'\bdata-language="([^"]*)"', div)
        langs.append(attrs[0] if len(attrs) == 1 else None)
    return langs


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


def assert_xss_gotchas_exercise(data: dict[str, Any], errors: list[str]) -> None:
    """Assert conditions for the XSS + gotchas exercise (index 3).

    Tests three review fixes:
    - </script> in code_template (XSS escape — if not escaped, HTML parser
      closes <script> early, truncating JSON and causing json.loads to fail)
    - gotchas is not null (Div with both .hints and .gotchas classes —
      elseif chain previously dropped gotchas)
    - hints is not null (same div, .hints class still parsed)
    """
    ct = data["code_template"]
    if ct is None or "</script>" not in ct:
        errors.append(f"XSS exercise: code_template missing '</script>' — got: {ct!r}")

    if data["gotchas"] is None:
        errors.append(
            "XSS exercise: gotchas should not be null "
            "(Div with both .hints and .gotchas classes)"
        )

    if data["hints"] is None:
        errors.append("XSS exercise: hints should not be null")


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


def assert_static_fallback(
    html: str, widgets: list[dict[str, Any]], errors: list[str]
) -> None:
    """Assert every bt-exercise div carries a static fallback block BEFORE the
    payload script (fix-demo-visible-exercises, Part 1).

    The filter emits, inside each div.bt-exercise and BEFORE the
    <script type="application/json"> payload:
      <div class="bt-exercise-static">
        <h3 class="bt-static-title">…</h3>
        <div class="bt-static-prompt">…prompt HTML…</div>
        <pre class="bt-static-code"><code>…HTML-escaped code_template…</code></pre>
        <details class="bt-static-hints"><summary>Hints</summary>…</details>
        <details class="bt-static-gotchas"><summary>Gotchas</summary>…</details>
      </div>

    So the exercise is VISIBLE even when JS never runs (file:// CORS-blocked
    ES modules, JS disabled). The runtime removes the block when it mounts.
    The payload script must stay untouched (the runtime reads it via
    script[type=application/json]).
    """
    div_pat = re.compile(r'<div class="bt-exercise"[^>]*>')
    script_pat = re.compile(r'<script type="application/json">')
    static_pat = re.compile(r'<div class="bt-exercise-static">')
    # Quarto post-processes raw h3 headings by appending classes (e.g.
    # "anchored") — match the class prefix, not the exact class list.
    title_pat = re.compile(r'<h3 class="bt-static-title')
    prompt_pat = re.compile(r'<div class="bt-static-prompt">')
    code_pat = re.compile(r'<pre class="bt-static-code"><code>')
    hints_pat = re.compile(
        r'<details class="bt-static-hints"><summary>Hints</summary>'
    )
    gotchas_pat = re.compile(
        r'<details class="bt-static-gotchas"><summary>Gotchas</summary>'
    )

    for i, m in enumerate(div_pat.finditer(html), start=1):
        rest = html[m.end() :]
        sm = script_pat.search(rest)
        if not sm:
            errors.append(
                f"Static fallback (exercise {i}): payload script missing"
            )
            continue
        between = rest[: sm.start()]  # div content BEFORE the payload script
        if not static_pat.search(between):
            errors.append(
                f"Static fallback (exercise {i}): missing .bt-exercise-static "
                "block before payload script"
            )
            continue
        if not title_pat.search(between):
            errors.append(
                f"Static fallback (exercise {i}): missing .bt-static-title"
            )
        data = widgets[i - 1] if i - 1 < len(widgets) else None
        if data is None:
            continue
        if data.get("prompt"):
            if not prompt_pat.search(between):
                errors.append(
                    f"Static fallback (exercise {i}): prompt present in "
                    "payload but missing .bt-static-prompt"
                )
        if data.get("code_template"):
            if not code_pat.search(between):
                errors.append(
                    f"Static fallback (exercise {i}): code_template present in "
                    "payload but missing <pre class=bt-static-code><code>"
                )
        if data.get("hints"):
            if not hints_pat.search(between):
                errors.append(
                    f"Static fallback (exercise {i}): hints present in payload "
                    "but missing <details class=bt-static-hints>"
                )
        if data.get("gotchas"):
            if not gotchas_pat.search(between):
                errors.append(
                    f"Static fallback (exercise {i}): gotchas present in "
                    "payload but missing <details class=bt-static-gotchas>"
                )
        ct = data.get("code_template") or ""
        if "</script>" in ct:
            if "&lt;/script&gt;" not in between:
                errors.append(
                    f"Static fallback (exercise {i}): code_template </script> "
                    "must be HTML-escaped in static block (&lt;/script&gt;)"
                )
            if "</script>" in between:
                errors.append(
                    f"Static fallback (exercise {i}): raw </script> leaks "
                    "into the static block"
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

    # Assertion: >=4 exercises (filter.qmd has 4 — order load-bearing)
    if len(widgets) < 4:
        errors.append(f"Expected >=4 bt-exercise widgets, found {len(widgets)}")

    # Static fallback block: every exercise div carries .bt-exercise-static
    # BEFORE the payload script (fix-demo-visible-exercises Part 1).
    assert_static_fallback(html, widgets, errors)

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
    if len(widgets) >= 4:
        assert_xss_gotchas_exercise(widgets[3], errors)

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

    # data-language: all-or-none + per-index pairing (AC-1 clauses 2-3)
    langs = extract_widget_languages(html)
    if len(langs) != len(widgets):
        errors.append(
            f"data-language attrs ({len(langs)}) must equal widget count "
            f"({len(widgets)}) — every div must carry exactly one (all-or-none)"
        )
    for i, lang in enumerate(langs):
        if lang not in ALLOWED_LANGUAGES:
            errors.append(
                f"Exercise {i}: data-language must be 'r' or 'python', got: {lang!r}"
            )
        elif i < len(EXPECTED_LANGUAGES) and lang != EXPECTED_LANGUAGES[i]:
            errors.append(
                f"Exercise {i}: data-language should be '{EXPECTED_LANGUAGES[i]}' "
                f"(filter.qmd order), got: {lang!r}"
            )

    if errors:
        for e in errors:
            print(f"  FAIL: {e}", file=sys.stderr)
        return 1

    print(f"All assertions passed ({len(widgets)} widgets verified)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
