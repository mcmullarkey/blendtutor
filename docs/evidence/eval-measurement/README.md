# Browser verification: feedback_guidance and the model pin (rodney)

Proof-of-done for the UI-touching changes on `feat/eval-measurement`: the
browser prompt builders now send `feedback_guidance` (ADR-0030), and the
Fireworks default pin moved to `deepseek-v4p1-flash-1017` (ADR-0016
amendment). Remove this directory before merging to main.

Method: headless Chrome via `uvx rodney`. A fake key is seeded and `fetch` is
replaced with a recorder (a stubbed model roster and a canned verdict), so no
real provider is called. On lesson 03 (`examples/write-less-code-r`), submit,
then read the recorded chat-completions request.

| Surface | Built from | Recorded request |
|---|---|---|
| Static site (`blendtutor build --target webr`) | `crates/core/assets/shared/feedback.js` | `{"url":"https://api.fireworks.ai/inference/v1/chat/completions","model":"accounts/fireworks/models/deepseek-v4p1-flash-1017","hasGuidance":true,"criteriaBeforeGuidance":true,"guidanceBeforeCode":true}` |
| Quarto widget (`export-quarto --document` + `quarto render`) | `_extensions/blendtutor/assets/exercise-feedback.js` | `{"url":"https://api.fireworks.ai/inference/v1/chat/completions","model":"accounts/fireworks/models/deepseek-v4p1-flash-1017","hasGuidance":true,"criteriaBeforeGuidance":true,"guidanceBeforeCode":true}` |

`hasGuidance` checks for the exact section
`Feedback guidance:\nGuide the learner to the fix without giving a working solution.`
in the prompt text. The two ordering flags confirm it sits after
`Success criteria:` and before the fenced student code.

- `site-lesson03-submitted.png`: static site, lesson 03, stubbed verdict rendered.
- `quarto-lesson03-feedback.png`: Quarto widget, lesson 03, stubbed verdict rendered.
