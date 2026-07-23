#!/usr/bin/env bash
# sync-quarto-assets.sh — sync shared assets from crates/core/assets/shared/
# to _extensions/blendtutor/assets/ with CSS scoping under .bt-exercise.
#
# WHAT:  Copy codemirror.js byte-identical; derive scoped styles.css from source.
# WHERE: crates/core/assets/shared/ → _extensions/blendtutor/assets/
# NOT:   Runtime loading, coi-serviceworker.js (AC-9), lesson-runner-core.js,
#        feedback.js. Source files are READ-ONLY — never modify.
#
# Modes:
#   sync (default):  Write vendored files. Exit 0 on success.
#   --check:         Regenerate to temp, compare with committed. Exit 0=match,
#                    1=drift, 2=missing source, 3=symlink detected.
#
# Exit codes: 0=ok/match, 1=drift/error, 2=missing source, 3=symlink detected.
#
# File list is extensible — AC-9 will add coi-serviceworker.js entry.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

SOURCE_DIR="$REPO_ROOT/crates/core/assets/shared"
DEST_DIR="$REPO_ROOT/_extensions/blendtutor/assets"

# Asset file list — extensible (AC-9 adds coi-serviceworker.js).
# Format: "source_name:dest_name:mode"
#   mode=copy   → byte-identical copy (cmp verified)
#   mode=scope  → CSS scoping derivation under .bt-exercise
ASSET_FILES=(
  "codemirror.js:codemirror.js:copy"
  "styles.css:styles.css:scope"
  "coi-serviceworker.js:coi-serviceworker.js:copy"
)

# ---------------------------------------------------------------------------
# scope_css — transform source CSS into scoped CSS under .bt-exercise.
#
# Rules:
#   - :root blocks: kept global (not scoped)
#   - body{}: transformed to .bt-exercise{}, page-layout props stripped
#     (max-width, margin, padding, min-height, display, flex-direction)
#   - Other rules: selectors prefixed with .bt-exercise (descendant combinator)
#   - @media: wrapper preserved, inner selectors scoped (except :root)
#   - @keyframes: kept global (not scoped)
#   - Comma-separated selectors: each individually prefixed
#   - Comments: preserved
# ---------------------------------------------------------------------------
scope_css() {
  local src="$1" dst="$2"
  python3 - "$src" "$dst" <<'PYEOF'
import re
import sys

SCOPE = ".bt-exercise"
PAGE_LAYOUT_PROPS = frozenset({
    "max-width", "margin", "padding", "min-height", "display", "flex-direction",
})


def parse_blocks(css):
    """Parse CSS into a list of items.

    Each item is one of:
      - ("comment", text)
      - ("rule", selector, body)
      - ("atrule", keyword, params, body)
      - ("text", text)
    """
    items = []
    i = 0
    n = len(css)

    while i < n:
        if css[i].isspace():
            i += 1
            continue

        if css[i : i + 2] == "/*":
            end = css.find("*/", i + 2)
            if end == -1:
                end = n
            else:
                end += 2
            items.append(("comment", css[i:end]))
            i = end
            continue

        brace = css.find("{", i)
        if brace == -1:
            remaining = css[i:].strip()
            if remaining:
                items.append(("text", remaining))
            break

        selector = css[i:brace].strip()

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
            items.append(("atrule", at_kw, params, body))
        else:
            items.append(("rule", selector, body))

        i = j

    return items


def split_selectors(selector):
    """Split comma-separated selectors, respecting quotes and brackets."""
    parts = []
    current = []
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


def scope_selector(selector):
    """Prefix a selector with .bt-exercise descendant combinator.

    Exceptions (kept global, not scoped):
      - body: transformed to .bt-exercise (handled separately)
      - :root: kept as :root (design tokens are global)
    """
    selector = selector.strip()
    if selector == "body":
        return SCOPE
    if selector == ":root":
        return selector
    parts = split_selectors(selector)
    scoped = [f"{SCOPE} {p}" for p in parts]
    return ",\n".join(scoped)


def format_declarations(body, indent, strip_page_layout=False):
    """Format CSS declarations with consistent indentation."""
    lines = body.strip().split("\n")
    result = []
    for line in lines:
        stripped = line.strip()
        if not stripped:
            continue
        if strip_page_layout:
            m = re.match(r"([\w-]+)\s*:", stripped)
            if m and m.group(1) in PAGE_LAYOUT_PROPS:
                continue
        result.append(f"{indent}{stripped}")
    return "\n".join(result)


def format_rule(selector, body, indent=""):
    """Format a scoped CSS rule."""
    scoped = scope_selector(selector)
    strip_pl = selector.strip() == "body"
    body_text = format_declarations(body, indent + "  ", strip_pl)
    return f"{indent}{scoped} {{\n{body_text}\n{indent}}}"


def format_atrule(keyword, params, body, indent=""):
    """Format an at-rule (@media, @keyframes)."""
    if keyword == "@media":
        inner_items = parse_blocks(body)
        inner_parts = []
        for item in inner_items:
            if item[0] == "comment":
                inner_parts.append(f"{indent}  {item[1].strip()}")
            elif item[0] == "rule":
                sel = item[1].strip()
                if sel == ":root":
                    body_text = format_declarations(item[2], indent + "    ")
                    inner_parts.append(
                        f"{indent}  {sel} {{\n{body_text}\n{indent}  }}"
                    )
                else:
                    inner_parts.append(format_rule(sel, item[2], indent + "  "))
            elif item[0] == "atrule":
                inner_parts.append(
                    format_atrule(item[1], item[2], item[3], indent + "  ")
                )
            elif item[0] == "text":
                inner_parts.append(f"{indent}  {item[1]}")
        inner_css = "\n\n".join(s for s in inner_parts if s)
        return f"{indent}{keyword} {params} {{\n{inner_css}\n{indent}}}"
    else:
        # @keyframes and other at-rules: keep global, preserve body
        body_text = format_declarations(body, indent + "  ")
        return f"{indent}{keyword} {params} {{\n{body_text}\n{indent}}}"


def scope_css_text(source_css):
    """Transform source CSS into scoped CSS under .bt-exercise."""
    header = (
        "/* AUTO-GENERATED by scripts/sync-quarto-assets.sh — DO NOT EDIT.\n"
        " * Source: crates/core/assets/shared/styles.css\n"
        ' * Scoped under .bt-exercise for Quarto extension isolation.\n'
        " * Run sync-quarto-assets.sh to regenerate. */"
    )

    items = parse_blocks(source_css)
    output_parts = [header]

    for item in items:
        if item[0] == "comment":
            output_parts.append(item[1].strip())
        elif item[0] == "rule":
            output_parts.append(format_rule(item[1], item[2]))
        elif item[0] == "atrule":
            output_parts.append(format_atrule(item[1], item[2], item[3]))
        elif item[0] == "text":
            output_parts.append(item[1])

    return "\n\n".join(output_parts) + "\n"


def main():
    src = sys.argv[1]
    dst = sys.argv[2]

    with open(src, "r") as f:
        source_css = f.read()

    scoped = scope_css_text(source_css)

    with open(dst, "w") as f:
        f.write(scoped)


if __name__ == "__main__":
    main()
PYEOF
}

# ---------------------------------------------------------------------------
# sync_assets — copy/generate all vendored assets.
# ---------------------------------------------------------------------------
sync_assets() {
  # Verify source directory exists.
  if [ ! -d "$SOURCE_DIR" ]; then
    echo "ERROR: source directory not found: $SOURCE_DIR" >&2
    exit 2
  fi

  # Create destination directory.
  mkdir -p "$DEST_DIR"

  for entry in "${ASSET_FILES[@]}"; do
    IFS=':' read -r src_name dest_name mode <<< "$entry"
    src_path="$SOURCE_DIR/$src_name"
    dest_path="$DEST_DIR/$dest_name"

    # Check source exists.
    if [ ! -f "$src_path" ]; then
      echo "ERROR: source file not found: $src_path" >&2
      exit 2
    fi

    # Check for symlink in destination (refuse to overwrite).
    if [ -L "$dest_path" ]; then
      echo "ERROR: symlink detected at destination: $dest_path" >&2
      exit 3
    fi

    case "$mode" in
      copy)
        cp "$src_path" "$dest_path"
        ;;
      scope)
        scope_css "$src_path" "$dest_path"
        ;;
      *)
        echo "ERROR: unknown mode '$mode' for $src_name" >&2
        exit 1
        ;;
    esac

    echo "  synced: $src_name → $dest_name ($mode)"
  done
}

# ---------------------------------------------------------------------------
# check_parity — regenerate to temp, compare with committed.
# Exit 0=match, 1=drift, 2=missing source, 3=symlink detected.
# ---------------------------------------------------------------------------
check_parity() {
  # Verify source directory exists.
  if [ ! -d "$SOURCE_DIR" ]; then
    echo "ERROR: source directory not found: $SOURCE_DIR" >&2
    exit 2
  fi

  tmpdir="$(mktemp -d)"
  trap 'rm -rf "$tmpdir"' EXIT

  drift_found=0

  for entry in "${ASSET_FILES[@]}"; do
    IFS=':' read -r src_name dest_name mode <<< "$entry"
    src_path="$SOURCE_DIR/$src_name"
    dest_path="$DEST_DIR/$dest_name"
    tmp_path="$tmpdir/$dest_name"

    # Check source exists.
    if [ ! -f "$src_path" ]; then
      echo "ERROR: source file not found: $src_path" >&2
      exit 2
    fi

    # Check for symlink in destination.
    if [ -L "$dest_path" ]; then
      echo "ERROR: symlink detected at destination: $dest_path" >&2
      exit 3
    fi

    # Generate expected output to temp.
    case "$mode" in
      copy)
        cp "$src_path" "$tmp_path"
        ;;
      scope)
        scope_css "$src_path" "$tmp_path"
        ;;
      *)
        echo "ERROR: unknown mode '$mode' for $src_name" >&2
        exit 1
        ;;
    esac

    # Compare temp with committed.
    if [ ! -f "$dest_path" ]; then
      echo "DRIFT: $dest_name — vendored file missing" >&2
      drift_found=1
    elif ! cmp -s "$tmp_path" "$dest_path"; then
      echo "DRIFT: $dest_name — vendored file differs from source" >&2
      drift_found=1
    else
      echo "  ok: $dest_name matches source"
    fi
  done

  if [ "$drift_found" -ne 0 ]; then
    exit 1
  fi

  echo "All assets in sync."
}

# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------
main() {
  case "${1:-sync}" in
    --check)
      check_parity
      ;;
    sync|"")
      sync_assets
      ;;
    -h|--help)
      echo "Usage: $0 [--check|sync]"
      echo "  sync (default): Write vendored assets from source."
      echo "  --check:        Verify vendored assets match source (read-only)."
      echo ""
      echo "Exit codes: 0=ok/match, 1=drift, 2=missing source, 3=symlink"
      ;;
    *)
      echo "ERROR: unknown argument '$1'" >&2
      echo "Usage: $0 [--check|sync]" >&2
      exit 1
      ;;
  esac
}

main "$@"
