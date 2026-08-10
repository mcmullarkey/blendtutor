---
feature: byok-api-key
slice: followup-179
issue: 179
status: complete
---

# Follow-up #179 — Emit maxFeedbackPerSession default in blendtutor.lua

## Context

Discovered during AC-8 rodney probe review (PR #178). `build_key_page_config_script`
emits ONLY `window.__btConfig.keyPageUrl` — never `maxFeedbackPerSession`.
exercise-feedback.js:377 reads `(window.__btConfig && window.__btConfig.maxFeedbackPerSession) || 0`
→ undefined → 0 → `feedbackCount() >= 0` ALWAYS true → rate-limit reached
instantly → "Get feedback" silently disabled on any deployed Quarto book
(incl. demo-book). The AC-8 probe papers over it by injecting
`maxFeedbackPerSession = 100` (rodney-probes/feedback-probe.js:414).

## Acceptance Criteria (from issue #179)

- [ ] blendtutor.lua emits `maxFeedbackPerSession` default via merge pattern
      (`window.__btConfig = window.__btConfig || {}` then
      `window.__btConfig.maxFeedbackPerSession = window.__btConfig.maxFeedbackPerSession ?? <default>`)
- [ ] Rendered demo-book pages (`demo-book/_output/*.html`) contain
      `maxFeedbackPerSession` alongside `keyPageUrl`
- [ ] feedback-probe.js manual injection removed; probe asserts real render
- [ ] All existing tests pass (bootstrap, asset_deployment, feedback, distribution)
      + rodney probes green (CI rodney job)

## Decision Log

- **Default value = 20** (2026-08-07): crates parity. crates/core/src/course.rs
  `default_max_feedback()` returns 20, shared by serde default + `SiteConfig::default()`;
  config.js emission (crates/core/src/site/mod.rs:321) renders that value. The
  Quarto path mirrors the same default so both render paths agree.
- **`??` (nullish) chosen** over `||`: a user-set `maxFeedbackPerSession` of 0
  (deliberate disable) must NOT be overridden; `??` only fills undefined/null.
- **No ADR**: config default emission, no new interface/boundary (issue explicitly
  says ADR-0025 unlikely needed).
- **Do NOT touch crates** (crates path already emits config.js) and **do NOT touch
  exercise-feedback.js** (`|| 0` fallback intentional — fix is emission).

## Progress

- [x] Issue read + spec parsed (predicate/probe/negative/verification valid) — 2026-08-07
- [x] Default value decision (20, crates parity) — 2026-08-07
- [x] RED: bootstrap clause 14 + feedback.py lua emission assertions fail — 2026-08-07
      (python: 64 pass / 1 fail on old lua; rendered-path red confirmed via
      stash-render: grep maxFeedbackPerSession on old-lua render = absent)
- [x] GREEN: blendtutor.lua emission — 2026-08-07 (feedback.py 65/65; direct
      mixed-lang render carries `?? 20`)
- [x] Probe: remove injection, P10 guard asserts real render — 2026-08-07
      (node --check clean; commit 693715a)
- [x] Demo-book vendored sync (cp + cmp identical, sha256 match) — 2026-08-07
      (commit c5f47d9)
- [x] E2E demo-book render: ALL 4 _output/*.html carry maxFeedbackPerSession — 2026-08-07
      (render-proof.log)
- [x] Regression suites green — 2026-08-07 (feedback 65/65, bootstrap 73/73,
      asset-deployment 68/68, distribution 89/89 → test-suite.log)
- [x] Assemble evidence at docs/evidence/179/ — 2026-08-07 (commit 76be7fa)
- [x] Push + PR — 2026-08-07

## Surprises & Discoveries

- 2026-08-07: The vendored demo-book blendtutor.lua was already byte-identical to
  the source (`cmp` clean pre-change) — the marker-based distribution test
  (test_quarto_distribution.sh) does NOT check byte parity (build-time `cmp -s`
  is the guard per its comment); the sync is manual per issue done-condition.
- 2026-08-07: test_quarto_bootstrap.sh auto-detects quarto vs pandoc; local
  machine has quarto 1.10.18 so the rendered-output clause runs locally.
- 2026-08-07: No pyproject.toml — repo Python tests run via `python3 scripts/...`
  directly (CI convention); the builder generic pytest/ty/ruff gate does not apply.
- 2026-08-07: CONCURRENCY — a parallel Director session (issue #170 / AC-9,
  ADR-0016 work) shares this working tree: it checked out
  170-docs-byok-fireworks-adr-0016 and committed twice while my uncommitted
  edits sat in the tree (git reflog HEAD@{2}). Mitigation: commit promptly in
  small atomic commits; re-verify branch (`git branch --show-current`) before
  each commit. No file collision (170's commits are docs-only).
- 2026-08-07: Initial bootstrap test locally is SLOW (~1 min/fixture render,
  ~15 fixtures) — ran in background via nohup; the script auto-detects quarto
  vs pandoc. CI runs it in its own step; local wall-time is not a hang.
- 2026-08-07: test_quarto_bootstrap.sh C22 bare-clobber check on RENDERED
  content is safe (comments never reach output), but a whole-file source grep
  in test_quarto_feedback.py matched the docstring prose `window.__btConfig =
  {...}` — fixed by restricting the negative to the build_key_page_config_script
  function body (find(start)/find(end)).
