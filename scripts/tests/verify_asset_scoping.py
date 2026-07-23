#!/usr/bin/env python3
"""Verify CSS scoping of vendored styles.css under .bt-exercise.

Tests the CSS-specific clauses of the AC-3 compound predicate:
  1. No @import in scoped CSS
  2. --bt-* custom properties preserved (count matches source)
  3. :root blocks not scoped (kept global)
  4. @media block preserved
  5. @keyframes preserved
  6. body{} transformed to .bt-exercise{}
  7. Page-layout props stripped from .bt-exercise{}
     (max-width, margin, padding, min-height, display, flex-direction)
  8. Typography props retained in .bt-exercise{}
     (font-family, font-size, line-height, color, background)
  9. All selectors scoped under .bt-exercise
     (except :root, @media, @keyframes)
  10. No unscoped page-level IDs (#xxx without .bt-exercise prefix)
  11. @media inner selectors scoped (except :root)

Usage: python3 scripts/tests/verify_asset_scoping.py
"""
from __future__ import annotations

import re
import sys
from pathlib import Path
from typing import NamedTuple

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
SOURCE_CSS = REPO_ROOT / "crates" / "core" / "assets" / "shared" / "styles.css"
SCOPED_CSS = REPO_ROOT / "_extensions" / "blendtutor" / "assets" / "styles.css"

SCOPE_PREFIX = ".bt-exercise"
PAGE_LAYOUT_PROPS = frozenset({
    "max-width", "margin", "padding", "min-height", "display", "flex-direction",
})
TYPOGRAPHY_PROPS = frozenset({
    "font-family", "font-size", "line-height", "color", "background",
})

PASS = 0
FAIL = 0


def ok(msg: str) -> None:
    global PASS
    PASS += 1
    print(f"  PASS: {msg}")


def ko(msg: str) -> None:
    global FAIL
    FAIL += 1
    print(f"  FAIL: {msg}")


class Rule(NamedTuple):
    """A CSS rule: selector(s) + declaration body."""
    selector: str
    body: str


class AtRule(NamedTuple):
    """A CSS at-rule with a body block: @media, @keyframes, etc."""
    keyword: str  # e.g. '@media', '@keyframes'
    params: str   # e.g. '(prefers-color-scheme: dark)', 'bt-pulse'
    body: str


def parse_css(css: str) -> list:
    """Parse CSS into a list of items.

    Returns a list where each element is one of:
      - ('comment', text)
      - ('rule', Rule)
      - ('atrule', AtRule)
    """
    items: list = []
    i = 0
    n = len(css)

    while i < n:
        # Skip whitespace between blocks.
        if css[i].isspace():
            i += 1
            continue

        # Comment.
        if css[i : i + 2] == "/*":
            end = css.find("*/", i + 2)
            if end == -1:
                end = n
            else:
                end += 2
            items.append(("comment", css[i:end]))
            i = end
            continue

        # Find the opening brace.
        brace = css.find("{", i)
        if brace == -1:
            remaining = css[i:].strip()
            if remaining:
                items.append(("text", remaining))
            break

        selector = css[i:brace].strip()

        # Find matching close brace (brace-counting for nesting).
        depth = 1
        j = brace + 1
        while j < n and depth > 0:
            if css[j] == "{":
                depth += 1
            elif css[j] == "}":
                depth -= 1
            j += 1
        body = css[brace + 1 : j - 1]

        if selector.startswith("@"):
            parts = selector.split(None, 1)
            at_kw = parts[0]
            params = parts[1] if len(parts) > 1 else ""
            items.append(("atrule", AtRule(at_kw, params, body)))
        else:
            items.append(("rule", Rule(selector, body)))

        i = j

    return items


def extract_declarations(body: str) -> dict[str, str]:
    """Extract property: value pairs from a rule body."""
    decls: dict[str, str] = {}
    for line in body.split("\n"):
        stripped = line.strip()
        if not stripped or stripped.startswith("/*") or stripped.startswith("*"):
            continue
        m = re.match(r"([\w-]+)\s*:\s*(.+?)\s*;?\s*$", stripped)
        if m:
            decls[m.group(1)] = m.group(2)
    return decls


def split_selectors(selector: str) -> list[str]:
    """Split comma-separated selectors, respecting quotes and brackets."""
    parts: list[str] = []
    current: list[str] = []
    depth = 0
    in_string = False
    string_char = ""

    for char in selector:
        if in_string:
            current.append(char)
            if char == string_char:
                in_string = False
        elif char in ('"', "'"):
            in_string = True
            string_char = char
            current.append(char)
        elif char == "[":
            depth += 1
            current.append(char)
        elif char == "]":
            depth -= 1
            current.append(char)
        elif char == "," and depth == 0:
            parts.append("".join(current))
            current = []
        else:
            current.append(char)

    if current:
        parts.append("".join(current))

    return [p.strip() for p in parts if p.strip()]


def is_scoped(selector: str) -> bool:
    """Check if a selector is scoped under .bt-exercise."""
    return selector.strip().startswith(SCOPE_PREFIX)


def main() -> int:
    # ------------------------------------------------------------------
    # Load files
    # ------------------------------------------------------------------

    if not SCOPED_CSS.exists():
        print(f"ERROR: scoped CSS not found: {SCOPED_CSS}")
        return 1
    if not SOURCE_CSS.exists():
        print(f"ERROR: source CSS not found: {SOURCE_CSS}")
        return 1

    scoped_text = SCOPED_CSS.read_text()
    source_text = SOURCE_CSS.read_text()

    scoped_items = parse_css(scoped_text)
    source_items = parse_css(source_text)

    # ------------------------------------------------------------------
    # Test 1 — No @import in scoped CSS
    # ------------------------------------------------------------------

    print("== Test 1: no @import in scoped CSS ==")

    has_import = any(
        item[0] == "atrule" and item[1].keyword == "@import"
        for item in scoped_items
    )
    if has_import:
        ko("no @import in scoped CSS — @import found")
    else:
        ok("no @import in scoped CSS")

    # ------------------------------------------------------------------
    # Test 2 — --bt-* custom properties preserved
    # ------------------------------------------------------------------

    print("== Test 2: --bt-* custom properties preserved ==")

    def count_bt_vars(items: list) -> set[str]:
        """Extract all --bt-* property names from CSS items."""
        bt_vars: set[str] = set()
        for item in items:
            if item[0] == "rule":
                decls = extract_declarations(item[1].body)
                for prop in decls:
                    if prop.startswith("--bt-"):
                        bt_vars.add(prop)
            elif item[0] == "atrule":
                inner = parse_css(item[1].body)
                bt_vars.update(count_bt_vars(inner))
        return bt_vars

    source_bt_vars = count_bt_vars(source_items)
    scoped_bt_vars = count_bt_vars(scoped_items)

    missing = source_bt_vars - scoped_bt_vars
    if missing:
        ko(f"--bt-* preserved — missing: {sorted(missing)}")
    else:
        ok(f"--bt-* preserved ({len(scoped_bt_vars)} custom properties)")

    # ------------------------------------------------------------------
    # Test 3 — :root blocks not scoped (kept global)
    # ------------------------------------------------------------------

    print("== Test 3: :root blocks not scoped ==")

    root_rules = [
        item[1] for item in scoped_items
        if item[0] == "rule" and item[1].selector.strip() == ":root"
    ]
    if root_rules:
        ok(f":root blocks not scoped ({len(root_rules)} found)")
    else:
        ko(":root blocks not scoped — no :root rule found")

    for root_rule in root_rules:
        if is_scoped(root_rule.selector):
            ko(f":root blocks not scoped — :root rule is scoped: {root_rule.selector}")
        else:
            ok(":root blocks not scoped — :root selector is global")

    # ------------------------------------------------------------------
    # Test 4 — @media block preserved
    # ------------------------------------------------------------------

    print("== Test 4: @media block preserved ==")

    media_rules = [
        item[1] for item in scoped_items
        if item[0] == "atrule" and item[1].keyword == "@media"
    ]
    if media_rules:
        ok(f"@media block preserved ({len(media_rules)} found)")
    else:
        ko("@media block preserved — no @media rule found")

    # ------------------------------------------------------------------
    # Test 5 — @keyframes preserved
    # ------------------------------------------------------------------

    print("== Test 5: @keyframes preserved ==")

    keyframes_rules = [
        item[1] for item in scoped_items
        if item[0] == "atrule" and item[1].keyword == "@keyframes"
    ]
    if keyframes_rules:
        ok(f"@keyframes preserved ({len(keyframes_rules)} found)")
    else:
        ko("@keyframes preserved — no @keyframes rule found")

    # ------------------------------------------------------------------
    # Test 6 — body{} transformed to .bt-exercise{}
    # ------------------------------------------------------------------

    print("== Test 6: body{} transformed to .bt-exercise{} ==")

    bt_exercise_rules = [
        item[1] for item in scoped_items
        if item[0] == "rule" and item[1].selector.strip() == SCOPE_PREFIX
    ]
    if bt_exercise_rules:
        ok("body{} transformed to .bt-exercise{}")
    else:
        ko("body{} transformed to .bt-exercise{} — no .bt-exercise rule found")

    # ------------------------------------------------------------------
    # Test 7 — Page-layout props stripped from .bt-exercise{}
    # ------------------------------------------------------------------

    print("== Test 7: page-layout props stripped from .bt-exercise{} ==")

    if bt_exercise_rules:
        decls = extract_declarations(bt_exercise_rules[0].body)
        stripped_found = [p for p in PAGE_LAYOUT_PROPS if p in decls]
        if stripped_found:
            ko(f"page-layout props stripped — found: {stripped_found}")
        else:
            ok("page-layout props stripped (none found in .bt-exercise{})")
    else:
        ko("page-layout props stripped — .bt-exercise rule missing")

    # ------------------------------------------------------------------
    # Test 8 — Typography props retained in .bt-exercise{}
    # ------------------------------------------------------------------

    print("== Test 8: typography props retained in .bt-exercise{} ==")

    if bt_exercise_rules:
        decls = extract_declarations(bt_exercise_rules[0].body)
        missing_typo = [p for p in TYPOGRAPHY_PROPS if p not in decls]
        if missing_typo:
            ko(f"typography props retained — missing: {missing_typo}")
        else:
            ok(f"typography props retained ({len(TYPOGRAPHY_PROPS)} props)")
    else:
        ko("typography props retained — .bt-exercise rule missing")

    # ------------------------------------------------------------------
    # Test 9 — All selectors scoped under .bt-exercise
    #          (except :root, @media, @keyframes)
    # ------------------------------------------------------------------

    print("== Test 9: all selectors scoped under .bt-exercise ==")

    unscoped_selectors: list[str] = []
    for item in scoped_items:
        if item[0] != "rule":
            continue
        selector = item[1].selector.strip()
        if selector == ":root":
            continue
        # Check each comma-separated part.
        parts = split_selectors(selector)
        for part in parts:
            if not part.startswith(SCOPE_PREFIX):
                unscoped_selectors.append(part)

    if unscoped_selectors:
        ko(f"all selectors scoped — unscoped: {unscoped_selectors[:5]}")
    else:
        ok("all selectors scoped under .bt-exercise (except :root)")

    # ------------------------------------------------------------------
    # Test 10 — No unscoped page-level IDs
    # ------------------------------------------------------------------

    print("== Test 10: no unscoped page-level IDs ==")

    unscoped_ids: list[str] = []
    for item in scoped_items:
        if item[0] != "rule":
            continue
        selector = item[1].selector.strip()
        if selector == ":root":
            continue
        parts = split_selectors(selector)
        for part in parts:
            # Look for #id selectors not preceded by .bt-exercise.
            if re.search(r"#[\w-]+", part) and not part.startswith(SCOPE_PREFIX):
                unscoped_ids.append(part)

    if unscoped_ids:
        ko(f"no unscoped page-level IDs — found: {unscoped_ids[:5]}")
    else:
        ok("no unscoped page-level IDs")

    # ------------------------------------------------------------------
    # Test 11 — @media inner selectors scoped (except :root)
    # ------------------------------------------------------------------

    print("== Test 11: @media inner selectors scoped (except :root) ==")

    unscoped_media: list[str] = []
    for item in scoped_items:
        if item[0] != "atrule" or item[1].keyword != "@media":
            continue
        inner_items = parse_css(item[1].body)
        for inner in inner_items:
            if inner[0] != "rule":
                continue
            inner_sel = inner[1].selector.strip()
            if inner_sel == ":root":
                continue
            parts = split_selectors(inner_sel)
            for part in parts:
                if not part.startswith(SCOPE_PREFIX):
                    unscoped_media.append(part)

    if unscoped_media:
        ko(f"@media inner selectors scoped — unscoped: {unscoped_media[:5]}")
    else:
        ok("@media inner selectors scoped (except :root)")

    # ------------------------------------------------------------------
    # Summary
    # ------------------------------------------------------------------

    print()
    print("=========================================")
    print(f"  Results: {PASS} passed, {FAIL} failed")
    print("=========================================")

    if FAIL > 0:
        return 1

    print("All tests passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
