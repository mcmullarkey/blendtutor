# /// script
# requires-python = ">=3.11"
# dependencies = ["httpx>=0.27"]
# ///
"""ProTeGi-style prompt optimization of the actionability definition (ADR-0036).

Pryzant et al. 2023, "Automatic Prompt Optimization with 'Gradient Descent' and
Beam Search" (arXiv:2305.03495). Each round, for each definition in the beam:

1. score it on the dev items with `blendtutor judge-feedback` (actionability
   only, 1 sample) and `blendtutor judge-agreement` against the human labels;
2. take a minibatch of items it got wrong and ask an LLM for "gradients":
   reasons the definition misjudged them, given the labeler's notes;
3. ask the LLM to edit the definition to fix each reason (the step against the
   gradient);
4. keep the top `beam` definitions by balanced accuracy, (TPR + TNR) / 2.

Deviation from the paper: every candidate is scored on the full dev set (30
items) instead of a bandit over minibatches, because the set is small.

Only dev items and dev labels are read. The test split is never touched here;
it is scored once, separately, as ADR-0036 pre-registers.

Run: uv run scripts/apo/protegi.py --help
"""

from __future__ import annotations

import argparse
import concurrent.futures
import hashlib
import json
import os
import random
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path

import httpx

FIREWORKS_URL = "https://api.fireworks.ai/inference/v1/chat/completions"


# ---------------------------------------------------------------- pure parts


@dataclass
class Score:
    tp: int
    fn: int
    tn: int
    fp: int

    @property
    def tpr(self) -> float:
        return self.tp / (self.tp + self.fn) if self.tp + self.fn else 0.0

    @property
    def tnr(self) -> float:
        return self.tn / (self.tn + self.fp) if self.tn + self.fp else 0.0

    @property
    def balanced(self) -> float:
        return (self.tpr + self.tnr) / 2


def score(predictions: dict[str, bool], labels: dict[str, bool]) -> Score:
    """Confusion counts for the ids both sides have (judge pass vs human pass)."""
    s = Score(0, 0, 0, 0)
    for item_id, human in labels.items():
        if item_id not in predictions:
            continue
        judge = predictions[item_id]
        if human and judge:
            s.tp += 1
        elif human:
            s.fn += 1
        elif judge:
            s.fp += 1
        else:
            s.tn += 1
    return s


def errors(predictions: dict[str, bool], labels: dict[str, bool]) -> list[str]:
    """Ids where the judge and the human disagree, in sorted order."""
    return sorted(i for i, h in labels.items() if i in predictions and predictions[i] != h)


def definition_id(text: str) -> str:
    return hashlib.sha256(text.strip().encode()).hexdigest()[:10]


GRADIENT_PROMPT = """You are improving the definition an LLM judge uses to decide whether a \
tutor's feedback on a programming exercise is ACTIONABLE. A human expert labeled each \
example; the judge, using the definition below, disagreed with them on these examples.

Current definition:
<definition>
{definition}
</definition>

Examples the judge got wrong:
{examples}

Give {k} distinct reasons why the definition led the judge to the wrong verdict on these \
examples. Focus on what the definition says or fails to say, not on the examples' details. \
Answer as a JSON list of {k} strings and nothing else."""

EDIT_PROMPT = """You are improving the definition an LLM judge uses to decide whether a \
tutor's feedback on a programming exercise is ACTIONABLE.

Current definition:
<definition>
{definition}
</definition>

A problem with it:
<problem>
{reason}
</problem>

Rewrite the definition to fix this problem. Keep it general: it must apply to any \
exercise, so do not mention specific exercises, functions, or numbers from examples. \
Keep it under 150 words. Answer with the new definition only, inside <definition> tags."""


def format_examples(ids: list[str], items: dict[str, dict], labels: dict[str, dict],
                    judged: dict[str, dict]) -> str:
    blocks = []
    for n, item_id in enumerate(ids, 1):
        item, label, j = items[item_id], labels[item_id], judged[item_id]
        human = "ACTIONABLE" if label["actionability"] == "pass" else "NOT ACTIONABLE"
        judge = "ACTIONABLE" if j["pass"] else "NOT ACTIONABLE"
        blocks.append(
            f"Example {n}\nFeedback: {item['feedback']}\n"
            f"Human label: {human}. Human note: {label.get('note') or '(none)'}\n"
            f"Judge verdict: {judge}. Judge explanation: {j['explanations'][0]}"
        )
    return "\n\n".join(blocks)


def parse_reasons(text: str, k: int) -> list[str]:
    start, end = text.find("["), text.rfind("]")
    try:
        reasons = json.loads(text[start : end + 1])
        return [str(r) for r in reasons][:k]
    except (ValueError, json.JSONDecodeError):
        return [text.strip()]


def parse_definition(text: str) -> str | None:
    start, end = text.find("<definition>"), text.find("</definition>")
    if start < 0 or end < start:
        return None
    body = text[start + len("<definition>") : end].strip()
    return body or None


# ---------------------------------------------------------------- effects


def chat(model: str, prompt: str, temperature: float = 0.7) -> str:
    response = httpx.post(
        FIREWORKS_URL,
        headers={"Authorization": f"Bearer {os.environ['FIREWORKS_API_KEY']}"},
        json={"model": model, "temperature": temperature, "max_tokens": 2000,
              "messages": [{"role": "user", "content": prompt}]},
        timeout=180,
    )
    response.raise_for_status()
    return response.json()["choices"][0]["message"]["content"]


def judge(binary: str, model: str, items_file: Path, definition: str, work: Path,
          samples: int) -> dict[str, dict]:
    """Judge actionability on every item by `definition`; checkpointed per definition."""
    did = definition_id(definition)
    definition_file = work / f"{did}.txt"
    definition_file.write_text(definition.strip() + "\n")
    checkpoint = work / f"{did}-s{samples}.jsonl"
    result = subprocess.run(
        [binary, "judge-feedback", str(items_file), "--criterion", "actionability",
         "--actionability-definition", str(definition_file), "--judge-model", model,
         "--samples", str(samples), "--checkpoint", str(checkpoint), "--format", "json"],
        capture_output=True, text=True, check=False,
    )
    if result.returncode != 0:
        raise RuntimeError(f"judge-feedback failed for {did}: {result.stderr[-2000:]}")
    report = json.loads(result.stdout)
    return {i["id"]: i["judge"]["actionability"] for i in report["items"]}


@dataclass
class Candidate:
    text: str
    parent: str | None
    reason: str | None
    round: int
    judged: dict[str, dict] = field(default_factory=dict)
    score: Score | None = None

    @property
    def id(self) -> str:
        return definition_id(self.text)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--items", type=Path, required=True, help="dev items JSON")
    ap.add_argument("--labels", type=Path, required=True, help="human labels JSON")
    ap.add_argument("--start", type=Path, required=True, help="starting definition")
    ap.add_argument("--out", type=Path, required=True, help="candidates log (JSON lines)")
    ap.add_argument("--work", type=Path, required=True, help="scratch dir for checkpoints")
    ap.add_argument("--binary", default="./target/debug/blendtutor")
    ap.add_argument("--judge-model", default="accounts/fireworks/models/kimi-k3p1-pro")
    ap.add_argument("--optimizer-model", default="accounts/fireworks/models/kimi-k3p1-pro")
    ap.add_argument("--rounds", type=int, default=3)
    ap.add_argument("--beam", type=int, default=2)
    ap.add_argument("--gradients", type=int, default=2, help="reasons per parent")
    ap.add_argument("--minibatch", type=int, default=4, help="errors shown per gradient")
    ap.add_argument("--samples", type=int, default=1, help="judge samples during search")
    ap.add_argument("--seed", type=int, default=20261112)
    args = ap.parse_args()

    rng = random.Random(args.seed)
    args.work.mkdir(parents=True, exist_ok=True)
    items = {i["id"]: i for i in json.loads(args.items.read_text())}
    all_labels = {l["id"]: l for l in json.loads(args.labels.read_text())}
    labels = {i: all_labels[i] for i in items}  # dev labels only
    human = {i: l["actionability"] == "pass" for i, l in labels.items()}

    def evaluate(c: Candidate) -> Candidate:
        c.judged = judge(args.binary, args.judge_model, args.items, c.text, args.work,
                         args.samples)
        c.score = score({i: j["pass"] for i, j in c.judged.items()}, human)
        return c

    def log(c: Candidate) -> None:
        s = c.score
        with args.out.open("a") as f:
            f.write(json.dumps({
                "id": c.id, "round": c.round, "parent": c.parent, "reason": c.reason,
                "tp": s.tp, "fn": s.fn, "tn": s.tn, "fp": s.fp,
                "tpr": round(s.tpr, 4), "tnr": round(s.tnr, 4),
                "balanced": round(s.balanced, 4), "text": c.text,
            }) + "\n")
        print(f"round {c.round} {c.id} TPR {s.tp}/{s.tp + s.fn} TNR {s.tn}/{s.tn + s.fp} "
              f"balanced {s.balanced:.3f}", flush=True)

    start = evaluate(Candidate(args.start.read_text().strip(), None, None, 0))
    log(start)
    seen = {start.id: start}
    beam = [start]
    for rnd in range(1, args.rounds + 1):
        children: list[Candidate] = []
        for parent in beam:
            wrong = errors({i: j["pass"] for i, j in parent.judged.items()}, human)
            if not wrong:
                continue
            batch = rng.sample(wrong, min(args.minibatch, len(wrong)))
            reasons = parse_reasons(chat(args.optimizer_model, GRADIENT_PROMPT.format(
                definition=parent.text, k=args.gradients,
                examples=format_examples(batch, items, labels, parent.judged))), args.gradients)
            for reason in reasons:
                text = parse_definition(chat(args.optimizer_model, EDIT_PROMPT.format(
                    definition=parent.text, reason=reason)))
                if text and definition_id(text) not in seen:
                    children.append(Candidate(text, parent.id, reason, rnd))
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
            for child in pool.map(evaluate, children):
                seen[child.id] = child
                log(child)
        beam = sorted(seen.values(), key=lambda c: c.score.balanced, reverse=True)[: args.beam]
        print(f"round {rnd} beam: {[(c.id, round(c.score.balanced, 3)) for c in beam]}",
              flush=True)

    best = beam[0]
    (args.out.parent / "apo-best.txt").write_text(best.text + "\n")
    print(f"best {best.id} balanced {best.score.balanced:.3f}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
