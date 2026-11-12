"""Unit tests for the pure parts of protegi.py (no provider, no binary).

Run: uv run --with pytest --with httpx pytest scripts/apo
"""

import importlib.util
import sys
from pathlib import Path

spec = importlib.util.spec_from_file_location("protegi", Path(__file__).with_name("protegi.py"))
protegi = importlib.util.module_from_spec(spec)
sys.modules["protegi"] = protegi  # dataclasses resolve the module by name
spec.loader.exec_module(protegi)


def test_score_counts_judge_against_human_and_skips_unjudged_ids():
    s = protegi.score({"a": True, "b": True, "c": False}, {"a": True, "b": False, "c": False, "z": True})
    assert (s.tp, s.fn, s.tn, s.fp) == (1, 0, 1, 1)
    assert s.balanced == (1.0 + 0.5) / 2


def test_a_yes_sayer_scores_half_balanced_accuracy():
    s = protegi.score({"a": True, "b": True}, {"a": True, "b": False})
    assert s.balanced == 0.5


def test_errors_are_the_disagreeing_ids():
    assert protegi.errors({"a": True, "b": False}, {"a": False, "b": False, "c": True}) == ["a"]


def test_parse_definition_takes_the_tagged_body_or_none():
    assert protegi.parse_definition("x <definition>\n New def. \n</definition> y") == "New def."
    assert protegi.parse_definition("no tags here") is None
    assert protegi.parse_definition("<definition>  </definition>") is None


def test_parse_reasons_reads_a_json_list_or_falls_back_to_the_text():
    assert protegi.parse_reasons('Sure: ["one", "two", "three"]', 2) == ["one", "two"]
    assert protegi.parse_reasons("not json", 2) == ["not json"]


def test_definition_id_ignores_surrounding_whitespace():
    assert protegi.definition_id(" a \n") == protegi.definition_id("a")
