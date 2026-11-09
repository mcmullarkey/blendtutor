# Taking eval measurement seriously

*A measurement-science approach to blendtutor’s evals*

## blendtutor basics

blendtutor lets instructors build lessons for R + Python learners with real-time feedback. Instructors can embed those lessons in static websites or any Quarto artifact. Learners get personalized feedback without instructors having to spread themselves beyond thin.

blendtutor already had “batteries included” evals, so instructors could feel more confident that the feedback matched their standards before deploying the lessons. I deliberately left those evals un-opinionated at first, for two reasons:

- I wanted to make sure I had a full, end-to-end tool to present at Posit conf
- I didn’t want to make doing evals so cumbersome no one would use the tool

## A better default

However, as I spent more time digging into agentic evals, I decided the open-ended approach just wasn’t cutting it. LLM evals are a social science measurement problem, and expecting people to re-create that expertise while using blendtutor sets them up to fail.

My first steps in shifting the blendtutor eval approach prioritize content validity, retest reliability, and meta-evaluation. Let’s get nitty-gritty!

## Content validity

I wanted to give people some kind of grounding while creating their lesson evals, rather than having them stare at a blank YAML file. I decided to focus on potential misconceptions learners could have about each lesson. This approach fits with expert instructors who can anticipate where learners’ gaps in understanding might emerge.

Each eval item now [ties back to a misconception](https://mcmullarkey.github.io/blendtutor/whole-game.html#misconception-coverage) learners could potentially harbor about the material. I’ve also included [tooling](https://mcmullarkey.github.io/blendtutor/whole-game.html#find-misconceptions-the-blueprint-is-missing) to help instructors improve content validity by suggesting potential misconceptions not yet covered by any eval examples.

## Retest reliability

There’s lots of work to do to improve blendtutor’s LLM judge setup. And none of that work will matter if we can’t get (mostly) consistent evals from run-to-run. If the scores on evals can differ drastically with *nothing* about the prompt or setup changing, then we can’t reliably track improvements or regressions.

I’ve added [tooling](https://mcmullarkey.github.io/blendtutor/whole-game.html#check-that-a-run-is-repeatable) that allows us to track retest reliability for a lesson’s eval. Establishing this baseline allows future work on LLM judges to proceed with more confidence. For example, in a test lesson the verdicts matched across three runs on 30 of 31 items (95% CI 84%-99%).

## Meta-evaluation

A fair question to have during the content validity section: “How do you know you’re giving good suggestions about misconception coverage?” Let’s tackle this deceptively thorny question from two angles.

First, we can take some example lessons with misconception coverage we believe is good. Right now that’s based on one human’s judgment, which is a validity issue itself, but I think it’s better to make assumptions explicit vs. hiding them. Then, we can see how well our `suggest-misconceptions` tooling recovers misconceptions if we [explicitly leave one out of the lesson](adr/0032-suggest-misconceptions-content-validity.md#validation-lesson-03-deepseek-v4p1-flash-1017-2026-11-08).

Second, we can decide what kinds of misconceptions we want the tooling to catch. For example, I think the most important misconceptions are going to be conceptual vs. mechanical. Modern agentic LLMs can help with mechanical coding mistakes quite well at baseline, whereas they might struggle a bit more to completely cover all potential misconceptions learners could hold.

With this combination, we can tune the misconception coverage toward finding conceptual gaps (and confirm it succeeds on the held out misconceptions) while being less bothered if it misses on more mechanical misconceptions we can catch with [more deterministic tooling](https://mcmullarkey.github.io/blendtutor/whole-game.html#audit-the-labels-first-free).

## What’s next?

blendtutor’s evals system still has plenty of room to improve. The LLM judges are still almost completely untapped as a measurement science puzzle. We could integrate IRT across more continuous measurements of key concepts. We could take a further step back and even better define the grading pipeline + its stages and tackle each stage as a construct validity problem. And if I tried to do that all at once for this project this blog post would still be in drafts and no code would get pushed ever. So, I’m going to see how these improvements go and calibrate from there.

If you’re interested in thinking about evals as a measurement science problem I recommend checking out [this paper](https://arxiv.org/abs/2411.10939) + [this practical set of tips](https://hamel.dev/blog/posts/evals-faq/).
