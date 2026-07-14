#!/usr/bin/env bash
# Course-level eval orchestrator: iterate blendtutor.toml lessons, run
# `blendtutor eval --format json` per lesson, aggregate sum(matched)/sum(total)
# across ALL lessons into <course-dir>/eval-report.json.
#
# Usage: scripts/eval-course.sh <course-dir>
#
# Aggregation formula: sum(matched)/sum(total) across ALL lessons' .cases[] —
# NOT average(.accuracy[]) across lessons. Lessons have unequal case counts,
# so averaging per-lesson accuracy would weight a 1-case lesson the same as a
# 50-case lesson.
#
# Fail-fast: any non-zero eval exit aborts the run — no partial report is
# written. An empty course (zero lessons) or zero total cases produces
# {"accuracy": 0.0}, not NaN or a crash.
#
# TOML parsing: uses python3 with a comment-aware inline parser. The parser
# strips TOML comments (# ...) outside of double-quoted strings, so inline
# comments in blendtutor.toml do not break path extraction.
set -euo pipefail

# ---------------------------------------------------------------------------
# Argument validation — exactly one <course-dir> argument.
# ---------------------------------------------------------------------------

if [ $# -ne 1 ]; then
  echo "usage: $0 <course-dir>" >&2
  exit 1
fi

course_dir="$1"
manifest="$course_dir/blendtutor.toml"

if [ ! -f "$manifest" ]; then
  echo "eval-course: manifest not found: $manifest" >&2
  exit 1
fi

# ---------------------------------------------------------------------------
# Dependency check — jq is required for JSON aggregation.
# ---------------------------------------------------------------------------

if ! command -v jq > /dev/null 2>&1; then
  echo "eval-course: jq is required but not found on PATH" >&2
  exit 1
fi

if ! command -v python3 > /dev/null 2>&1; then
  echo "eval-course: python3 is required for TOML parsing" >&2
  exit 1
fi

# ---------------------------------------------------------------------------
# Extract ALL lesson paths from blendtutor.toml.
#
# Uses python3 with a comment-aware inline parser — strips TOML comments
# outside of double-quoted strings, then extracts `path = "..."` values from
# [[lessons]] blocks. This handles inline comments that would break naive
# rg+sed approaches.
# ---------------------------------------------------------------------------

lesson_paths=$(python3 -c '
import re
import sys

def strip_comment(line):
    """Remove a TOML comment (# ...) that is not inside a double-quoted string."""
    in_string = False
    for i, c in enumerate(line):
        if c == "\"":
            in_string = not in_string
        elif c == "#" and not in_string:
            return line[:i]
    return line

def extract_lesson_paths(toml_text):
    """Extract all path values from [[lessons]] blocks in TOML text."""
    paths = []
    in_lessons = False
    for raw_line in toml_text.splitlines():
        line = strip_comment(raw_line).strip()
        if line == "[[lessons]]":
            in_lessons = True
            continue
        # Any other table header exits the lessons block.
        if line.startswith("["):
            in_lessons = False
            continue
        if in_lessons:
            m = re.match(r"^path\s*=\s*\"([^\"]*)\"", line)
            if m:
                paths.append(m.group(1))
    return paths

with open(sys.argv[1]) as f:
    for p in extract_lesson_paths(f.read()):
        print(p)
' "$manifest")

# ---------------------------------------------------------------------------
# Run blendtutor eval per lesson, collecting JSON outputs.
#
# Fail-fast: any non-zero eval exit aborts immediately — no partial report.
# The temp file holds one JSON document per line (one per lesson).
# ---------------------------------------------------------------------------

temp_jsons=$(mktemp)
trap 'rm -f "$temp_jsons"' EXIT

lesson_count=0
while IFS= read -r path; do
  [ -z "$path" ] && continue
  lesson_count=$((lesson_count + 1))
  lesson_file="$course_dir/$path"

  if ! json_output=$(blendtutor eval "$lesson_file" --format json 2>&1); then
    echo "eval-course: eval failed for $lesson_file" >&2
    echo "$json_output" >&2
    exit 1
  fi

  echo "$json_output" >> "$temp_jsons"
done <<< "$lesson_paths"

# ---------------------------------------------------------------------------
# Aggregate: sum(matched) / sum(total) across ALL lessons.
#
# Uses jq -s (slurp) to collect all JSON documents into an array, then:
#   total = sum of .cases | length across all lessons
#   matched = sum of .cases | map(select(.matched)) | length across all lessons
#   accuracy = matched / total (or 0.0 if total == 0)
#
# This is NOT average(.accuracy[]) — which would weight lessons equally
# regardless of case count. The unequal-case-count fixture catches that cheat.
# ---------------------------------------------------------------------------

if [ "$lesson_count" -eq 0 ] || [ ! -s "$temp_jsons" ]; then
  # Zero lessons or zero outputs → 0.0 (not NaN, not crash).
  accuracy="0.0"
else
  accuracy=$(jq -s '
    (map(.cases | length) | add // 0) as $total |
    (map(.cases | map(select(.matched)) | length) | add // 0) as $matched |
    (if $total == 0 then 0.0 else ($matched / $total) end)
  ' "$temp_jsons")
fi

# ---------------------------------------------------------------------------
# Write <course-dir>/eval-report.json atomically.
#
# The report contains at least {"accuracy": <f64 in [0.0, 1.0]>}, which is
# the shape blendtutor build reads via eval_summary_from_report_json.
# ---------------------------------------------------------------------------

report_path="$course_dir/eval-report.json"
temp_report=$(mktemp)

jq -n --argjson acc "$accuracy" '{"accuracy": $acc}' > "$temp_report"
mv "$temp_report" "$report_path"

echo "eval-course: wrote $report_path (accuracy: $accuracy, lessons: $lesson_count)"
