---
ac: 8
depends_on: 4, 5, 6, 7
risk: medium
status: complete
---

**Predicate:** Rodney coverage passes CI-gated end-to-end with no sneaky-pass surfaces. All of:
- **P1 exit-code gate:** `rodney-probes/key-page-probe.js` AND `rodney-probes/feedback-probe.js` call `process.exit(1)` (non-zero) when writeReport verdict is PROBES_FAIL. Fixes feedback-probe.js:537 defect (currently exits 0 on failure; pages-live.js:701 is the reference pattern).
- **P2 CI-wired, PR-gating:** `.github/workflows/ci.yml` contains a NEW job (e.g. `rodney-probes`) triggered on `pull_request` that runs BOTH probes; probe steps have NO `continue-on-error`, NO `|| true`, NO `if: always()`. Job includes setup-uv, setup-node, Chrome, and `ROD_CHROME_BIN=scripts/rodney-chrome.sh`.
- **P3 real rendered pages only:** neither probe calls `generateProbeHtml` or substitutes synthetic DOM (feedback-probe.js:188-216 sneaky-pass removed); all navigation targets files under the served `demo-book/_output/` root.
- **P4 storage swap complete:** `grep -c 'sessionStorage' rodney-probes/feedback-probe.js` == 0 (ALL THREE refs :254, :259, :381 converted to localStorage; :259 `freshFixture()` included).
- **P5 cross-page persistence:** key saved via UI on `api-key.html` (page 1) → real navigation (`location.href`, blank-then-href per :235-241) to `r-exercises.html` (page 2, same origin) → localStorage key present AND feedback UI proceeds past no-key state. NOT eval pre-seed, NOT new tab.
- **P6 save flow UI+effect:** input `type=password` + `autocomplete=off`; status line visible per `getComputedStyle` (not display:none/off-screen); localStorage slot populated; input value cleared after save; fetch spy observed GET `/models` against stub.
- **P7 clear flow counter reset:** after clear, key slot AND `bt_feedback_count` both null in localStorage.
- **P8 invalid-key 401:** stub `/_config/auth` toggle forces 401 on `/models`; key page shows invalid-key error AND key NOT stored (matches AC-2 classifyValidation 401 → invalid-key).
- **P9 network error optimistic save:** stub unreachable → key still stored (optimistic save per AC-2) AND network-error status shown.
- **P10 verdict end-to-end through real stub:** with localStorage key present, clicking `[data-byok="submit"]` drives request through localhost stub `/chat/completions`; verdict renders into `[data-byok="verdict"]` via textContent only — XSS payload in stub response renders as literal text (no element injection). No fetch-spy substitution for this clause.
- **P11 no-key link end-to-end:** with empty localStorage, exercise page renders `[data-byok="no-key"]` link (target=_blank rel=noopener, href = keyPageUrl) AND ZERO `/chat/completions` fetches observed.
- **P12 EVIDENCE_DIR parameterized:** both probes read `EVIDENCE_DIR` env with default `docs/evidence/<issue>`; no hardcoded `docs/evidence/112` (feedback-probe.js:25 defect fixed).
- **P13 Chrome wrapper:** `scripts/rodney-chrome.sh` used via `ROD_CHROME_BIN` for the cross-page COI leg; strips poison flags (`--single-process`, `--disable-site-isolation-trials`, `--disable-features=*`).

**Probe:**
```bash
# CI wiring greps
grep -q 'rodney-probes' .github/workflows/ci.yml && \
! grep -A20 'rodney-probes' .github/workflows/ci.yml | grep -qE 'continue-on-error|\|\| true|if: always\(\)' && \
grep -q 'pull_request' .github/workflows/ci.yml && \
grep -q 'ROD_CHROME_BIN' .github/workflows/ci.yml
# source scans
test "$(grep -c 'sessionStorage' rodney-probes/feedback-probe.js)" = "0" && \
! grep -q 'generateProbeHtml' rodney-probes/key-page-probe.js rodney-probes/feedback-probe.js && \
! grep -q 'docs/evidence/112' rodney-probes/feedback-probe.js
# render + run (mirrors ci.yml:201 render step)
quarto render demo-book && \
EVIDENCE_DIR=docs/evidence/<issue> uv run node rodney-probes/key-page-probe.js && \
EVIDENCE_DIR=docs/evidence/<issue> uv run node rodney-probes/feedback-probe.js
# both exit 0; both write probe-report.json verdict PROBES_PASS covering P5-P11
```

**Negative:**
- Probe asserts selector presence only (element exists but display:none/off-screen) → must use getComputedStyle for visibility clauses (P6, P11).
- Probe passes while feedback-probe.js still reads sessionStorage (:254/:259/:381 unconverted) → P4 grep gate.
- Happy-path only: no invalid-key 401, no network-error optimistic save → P8/P9 mandatory.
- Same-page localStorage read without navigation hop, or eval pre-seed / new-tab fake cross-page → P5 requires real `location.href` navigation same-origin.
- Exit-0-on-failure (report says PROBES_FAIL, CI green) → P1 non-zero exit + P2 no-swallow wiring.
- generateProbeHtml DOM substitution passes against fake page → P3 real rendered `_output/` only.
- Verdict clause satisfied by fetch-spy without real stub round-trip → P10 requires stub `/chat/completions` + XSS literal check.
- CI job present but decorative (continue-on-error/|| true/if: always(), or only in deploy-gated docs.yml) → P2 greps.
- Hardcoded evidence path → P12.
- Chrome missing/poisoned flags in CI runner → P13 wrapper + builder verification instruction.
- Stub 401 implemented as `?key=invalid` hack diverging from one-server config seam → P8 requires `/_config/auth` toggle (consistent with `/_config/delay`).

**Verification:** rodney (P5-P11, P13 via probe execution) + code (P1, P3, P4, P12 source scans; P2 CI-wiring greps)

**Fixture status:** `rodney-probes/key-page-probe.js` NEW · `rodney-probes/feedback-probe.js` EDIT (:25, :254, :259, :381, :188-216, :537) · `scripts/rodney-chrome.sh` NEW · `.github/workflows/ci.yml` EDIT (new PR-gating job) · stub server EDIT (add `/_config/auth` toggle) · serves `demo-book/_output/` (AC-7 render arm output — not new fixture)

**Rubric anchor:** §3 (boundary cuts — probe harness vs production code; real-stub boundary vs fetch-spy fake), §2 (pure/effectful — probe report/writeReport separated from browser-driving shell)

**Design Intent:**
- **Types/interfaces (§1):** probe-report.json schema {verdict: PROBES_PASS|PROBES_FAIL, clauses: [...]} is the contract consumed by CI; EVIDENCE_DIR + ROD_CHROME_BIN env interface.
- **Pure/effectful (§2):** clause assertions pure predicates over observed state; servers/navigation/writeReport effectful shell at edges (:38-97, :162-169, :534-568).
- **Boundary cuts (§3):** probes exercise production code only through real HTTP + real rendered pages; stub is the single fake boundary (`/_config/delay`, `/_config/auth`); no in-process substitution.
- **Module responsibility (§4):** key-page-probe.js owns key-page clauses (P6-P9); feedback-probe.js owns feedback clauses (P5, P10, P11); ci.yml owns gating; neither probe duplicates the other's clause set.
- **Function discipline (§5):** one clause = one assertion function, independently reported in probe-report.json; writeReport + exit-code gate single responsibility.

**Technical Context:** Harness mirrors feedback-probe.js verified architecture: COI static server :8080 + stub :8081 (:38-97), rodney wrapper via execFileSync uvx (:162-169), navigateToFixture blank-then-location.href (:235-241 — required for P5 cross-page hop), installSpies (:218-233), writeReport (:534-568). Server root = `demo-book/_output/` (flat per AC-7; provides both api-key.html + r-exercises.html for P5). rodney 0.4.0 has no addInitScript — hence P5 forbids eval pre-seed. Stub extended with `/_config/auth` toggle for 401 (P8) alongside existing `/_config/delay`. CI: ci.yml quarto-render job (:53) lacks Chrome/uv and ci.yml:201 already renders demo-book — new `rodney-probes` job in ci.yml reuses render pattern, adds setup-uv, setup-node, Chrome, ROD_CHROME_BIN=scripts/rodney-chrome.sh. docs.yml rejected as target (deploy-gated, :172 runs pages-live.js only; not a PR gate). Builder instruction (not spec clause): verify Chrome + rodney launch in the GitHub runner per github-pages-deploy retro tool-behavior research requirement — rodney-chrome.sh must strip `--single-process` and related poison flags. Exit-code pattern copy pages-live.js:701.

**Dependencies:** depends-on: 4 (no-key link), 5 (submit/verdict/pinned model), 6 (provider select removed — probe asserts no provider UI), 7 (rendered `_output/` api-key.html — fixture source) | blocks: none | conflict set: `rodney-probes/key-page-probe.js` (NEW), `rodney-probes/feedback-probe.js` (EDIT), `.github/workflows/ci.yml` (EDIT — decomposition missed it), `scripts/rodney-chrome.sh` (NEW), stub server (EDIT) | notes: lands LAST in feature; transitively depends on AC-3 vendored sync done-condition (book must render correctly); builder must verify Chrome/rodney in CI runner before declaring done.

**Clarifications resolved:**
- Workflow target: NEW PR-gating `rodney-probes` job in ci.yml. Rationale: docs.yml is deploy-gated (not a PR gate); existing ci.yml jobs lack Chrome — new job isolates browser deps without destabilizing Python-test job.
- :259 scope: AC-8 owns ALL THREE sessionStorage refs (:254, :259, :381), enforced as `grep -c == 0`. Rationale: count-based gate can't miss a ref the way enumeration did in AC-1.md.
- Stub 401 mechanism: `/_config/auth` toggle on existing stub. Rationale: one stub server, consistent with `/_config/delay` seam.
- Fixture source: serve `demo-book/_output/` for all clauses. Rationale: P5 cross-page hop needs two real same-origin pages (api-key.html + r-exercises.html) which only exist in the book render. Adds AC-7 + AC-3 transitive dependency — confirmed.
- Invalid-key semantics: stub 401 → invalid-key error shown + key NOT stored. Matches AC-2 classifyValidation contract.
- P12 EVIDENCE_DIR: env-parameterized, default `docs/evidence/<issue>`, both probes.

**needs-clarification:** NONE

### Progress
- [x] AC-8 spec resolved — 2026-08-06
- [x] implementation — P1-P13 all green (key-page-probe + feedback-probe rewritten, CI job wired, both probes PASS with exit 0, evidence at docs/evidence/169/) — 2026-08-07

### Decision Log
- 2026-08-06 — NEW PR-gating rodney-probes job in ci.yml (docs.yml rejected — deploy-gated); all 3 sessionStorage refs in feedback-probe.js; /_config/auth stub toggle; demo-book/_output served root; EVIDENCE_DIR env-parameterized; exit-code gate + CI-wiring are the non-negotiable core.
- 2026-08-07 — **P8 "key NOT stored" is a spec ERROR about AC-2.** The merged AC-2 implementation stores the key optimistically BEFORE validation (key-page.js handleSave → storeKey → validateAndReport), and AC-2's own test asserts it: scripts/tests/test_quarto_key_page.py:369 `"advisory: key stored even when validation rejects"`. The AC-8 speculator misread the AC-2 contract. Probe asserts the REAL merged contract: 401 → invalid-key error shown + GET /models fired + key STORED (advisory). Implementing the literal "key NOT stored" would permanently red the CI gate against real production code. Deviating as documented; flag to Director + reviewers.
- 2026-08-07 — rodney browser profile persists localStorage across runs; both probes clear localStorage on the blank page (same origin) at startup so P6/P11 preconditions ("no stored key") hold on re-runs. Also rodney caches stale demo-book JS — probes call `clear-cache` after start (observed: stale model-picker UI after a re-render without it).
- 2026-08-07 — demo-book/_output was STALE (rendered before AC-5 merged); re-rendered locally + the CI rodney-probes job renders fresh. Probes also render-if-missing.

### Surprises & Discoveries
- **demo-book/_output was stale** — the committed render predated AC-5's model-picker collapse, so r-exercises.html still mounted the old `[data-byok=model-picker]` UI. The smoke test initially "passed" against it, then the verdict path differed from spec. Re-render (quarto render demo-book) fixed it; the CI job renders fresh; local probes render-if-missing. Lesson: rodney probes against demo-book MUST NOT trust an existing _output — staleness silently changes the UI under test (P3 real-pages gate depends on a FRESH render).
- **rodney Chrome persists localStorage across runs** — the same profile dir is reused, so a prior probe run's stored key leaked into P6's "no stored key" precondition (P6 failed on re-run: key-set state rendered Clear instead of the form). Fixed by clearing localStorage on the same-origin blank page at startup. Re-runs are now idempotent.
- **rodney Chrome HTTP-caches page assets** — after a re-render, the browser served the OLD exercise-feedback.js (model-picker still present) until `rodney clear-cache`. The harness clears cache after start; without it, a stale render leak can silently flip which UI the probe exercises.
- **SW shim does not self-reload under the plain static server** — pages-live.js waits for the coi-serviceworker reload cycle; the demo-book probes serve demo-book/_output/ WITHOUT COOP/COEP headers so the SW shim never claims the page (controller stays null, no reload wipe). The P13 wrapper is still wired (ROD_CHROME_BIN) for the cross-page leg, but no reload-settle polling is needed in these probes — the page stays stable across navigation.

### Idempotence & Recovery
- Safe retry: re-run the probe probe command chain after interrupted edit.
- Rollback: git revert the PR.