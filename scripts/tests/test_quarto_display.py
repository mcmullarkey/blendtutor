#!/usr/bin/env python3
"""Executable spec: Quarto widget display polish (ADR-0021).

Renders an embedded R fixture (no checks, like an LLM-graded pseudocode
lesson) in a temp project with the in-repo extension installed by name,
serves it over HTTP, drives headless Chrome through rodney, and asserts:

  D1 prompt      -- after the runtime mounts the editor, the exercise prompt
                    is still visible (the static fallback held it).
  D2 idle chrome -- before anything runs, the status badge and the output box
                    are not rendered.
  D3 key form    -- Get feedback with no stored key shows an input labeled
                    "Fireworks API key" with an fw_ placeholder and a
                    "stored only in this browser" explanation.
  D4 theme       -- the extension ships quarto-theme.css keyed on Quarto's
                    body.quarto-light / body.quarto-dark classes, loaded by the
                    filter, with token values equal to the shared stylesheet's
                    light and dark palettes (drift guard), so an OS dark
                    preference cannot darken a light book.

Negative: the runtime removing the prompt with the static block; idle badge
and empty output shown for an exercise that has not run; an unlabeled key
input; widget colors switching on prefers-color-scheme alone.

Usage: uv run --no-project python scripts/tests/test_quarto_display.py
"""

from __future__ import annotations

import re
import shutil
import socket
import subprocess
import sys
import tempfile
import time
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
EXT_DIR = REPO_ROOT / "_extensions" / "blendtutor"
THEME_CSS = EXT_DIR / "assets" / "quarto-theme.css"
LUA_FILTER = EXT_DIR / "blendtutor.lua"
SHARED_CSS = REPO_ROOT / "crates" / "core" / "assets" / "shared" / "styles.css"
PROMPT_MARKER = "DISPLAY-PROMPT-MARKER"

FIXTURE_QMD = f"""---
title: Display fixture
filters:
  - mcmullarkey/blendtutor
---

::: {{.blendtutor language="r"}}
{PROMPT_MARKER}: write pseudocode as comments.

```r
# Your comments here
```
:::
"""

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


def check(cond: bool, msg: str) -> None:
    if cond:
        ok(msg)
    else:
        ko(msg)


# ---------------------------------------------------------------------------
# D4: theme stylesheet keyed on Quarto's page class (static)
# ---------------------------------------------------------------------------


def color_tokens(block: str) -> dict[str, str]:
    """Map every --bt-color-* custom property in a CSS block to its value,
    lowercased so equal hex colors compare equal regardless of case."""
    return {
        m.group(1): m.group(2).strip().lower()
        for m in re.finditer(r"(--bt-color-[\w-]+)\s*:\s*([^;]+);", block)
    }


def block_after(css: str, opener: str) -> str:
    """The brace-balanced body of the first block whose header matches `opener`."""
    match = re.search(opener, css)
    if not match:
        return ""
    start = css.index("{", match.start()) + 1
    depth = 1
    for i in range(start, len(css)):
        if css[i] == "{":
            depth += 1
        elif css[i] == "}":
            depth -= 1
            if depth == 0:
                return css[start:i]
    return ""


def check_theme_stylesheet() -> None:
    print("== D4: theme follows the Quarto page class ==")
    if not THEME_CSS.exists():
        ko("quarto-theme.css exists in the extension assets")
        return
    ok("quarto-theme.css exists in the extension assets")
    theme = THEME_CSS.read_text()
    shared = SHARED_CSS.read_text()
    light_expected = color_tokens(block_after(shared, r"(?m)^:root\s*\{"))
    # (?m)^ anchors to real at-rules: the stylesheet header comment also
    # mentions "@media (prefers-color-scheme: dark)" and ":root".
    dark_media = block_after(shared, r"(?m)^@media \(prefers-color-scheme: dark\)\s*\{")
    dark_expected = color_tokens(block_after(dark_media, r"(?m)^\s*:root\s*\{"))
    # (?![\w-]) keeps a future body.quarto-light-dim from matching.
    light_actual = color_tokens(block_after(theme, r"body\.quarto-light(?![\w-])\s*\{"))
    dark_actual = color_tokens(block_after(theme, r"body\.quarto-dark(?![\w-])\s*\{"))
    check(bool(light_expected) and light_actual == light_expected,
          "body.quarto-light re-declares every shared light color token")
    check(bool(dark_expected) and dark_actual == dark_expected,
          "body.quarto-dark declares the shared dark color tokens")
    check("assets/quarto-theme.css" in LUA_FILTER.read_text(),
          "the filter's html dependency ships quarto-theme.css")


# ---------------------------------------------------------------------------
# Render + serve + browser (D1-D3)
# ---------------------------------------------------------------------------


def render_fixture(workdir: Path) -> Path | None:
    shutil.copytree(EXT_DIR, workdir / "_extensions" / "mcmullarkey" / "blendtutor")
    (workdir / "display.qmd").write_text(FIXTURE_QMD)
    result = subprocess.run(
        ["quarto", "render", "display.qmd", "--to", "html"],
        cwd=workdir, capture_output=True, text=True, timeout=300, check=False,
    )
    html = workdir / "display.html"
    if result.returncode != 0 or not html.exists():
        ko(f"render display fixture -- exit {result.returncode}: {result.stderr[-400:]}")
        return None
    ok("render display fixture")
    check("quarto-theme.css" in html.read_text(), "rendered page links quarto-theme.css")
    return html


def free_port() -> int:
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


def rodney(*args: str, timeout: int = 60) -> str:
    result = subprocess.run(
        ["uvx", "rodney", *args], capture_output=True, text=True, timeout=timeout, check=False,
    )
    return result.stdout.strip()


def js(expr: str) -> str:
    out = rodney("js", expr)
    return out.splitlines()[-1].strip() if out else ""


def wait_for(expr: str, seconds: int) -> bool:
    deadline = time.monotonic() + seconds
    while time.monotonic() < deadline:
        if js(expr) == "true":
            return True
        time.sleep(1)
    return False


def check_browser(url: str) -> None:
    print("== D1-D3: rendered widget in a real browser ==")
    rodney("start")
    try:
        rodney("open", url)
        js("localStorage.clear()")
        mounted = wait_for("!!document.querySelector('.bt-exercise .bt-editor')", 90)
        check(mounted, "runtime mounts the editor")
        if not mounted:
            return
        check(
            js(f"document.querySelector('.bt-exercise').innerText.includes('{PROMPT_MARKER}')") == "true",
            "D1: the prompt stays visible after the editor mounts",
        )
        check(
            js("(() => { const w = document.querySelector('.bt-exercise'); const s = w.querySelector('.bt-status'); const o = w.querySelector('.bt-output'); return !!s && !!o && s.getClientRects().length === 0 && o.getClientRects().length === 0; })()") == "true",
            "D2: status badge and output box are not rendered before a run",
        )
        rodney("click", ".bt-exercise .bt-feedback-btn")
        has_input = wait_for("!!document.querySelector('.bt-exercise [data-byok=key-input]')", 15)
        check(has_input, "Get feedback without a key mounts the inline key form")
        if not has_input:
            return
        check(
            js("(() => { const i = document.querySelector('.bt-exercise [data-byok=key-input]'); return [...i.labels].some((l) => l.textContent.includes('Fireworks API key')); })()") == "true",
            "D3: the key input is labeled 'Fireworks API key'",
        )
        check(
            js("document.querySelector('.bt-exercise [data-byok=key-input]').placeholder.startsWith('fw_')") == "true",
            "D3: the key input has an fw_ placeholder",
        )
        check(
            js("document.querySelector('.bt-exercise [data-byok=feedback]').innerText.includes('stored only in this browser')") == "true",
            "D3: the inline form explains the key is stored only in this browser",
        )
    finally:
        rodney("stop")


def main() -> int:
    check_theme_stylesheet()
    if not shutil.which("quarto") or not shutil.which("uvx"):
        ko("quarto and uvx are required for the browser checks")
    else:
        with tempfile.TemporaryDirectory(prefix="bt-display-") as tmp:
            html = render_fixture(Path(tmp))
            if html is not None:
                port = free_port()
                server = subprocess.Popen(
                    [sys.executable, "-m", "http.server", str(port), "--bind", "127.0.0.1", "--directory", tmp],
                    stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                )
                try:
                    time.sleep(1)
                    check_browser(f"http://127.0.0.1:{port}/display.html")
                finally:
                    server.terminate()
    print(f"=== Results: {PASS} passed, {FAIL} failed ===")
    return 1 if FAIL else 0


if __name__ == "__main__":
    sys.exit(main())
