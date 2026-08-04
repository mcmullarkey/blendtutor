"use strict";
/**
 * Structural tests for the effectful harness pages-live.js (issue #153 —
 * AC-3). The harness drives rodney (builder/unit tests cannot execute it:
 * rodney is vision-probe's domain), so the review-cycle-2 contract is pinned
 * structurally by matching CODE PATTERNS in main()'s probe orchestration:
 *
 *   1. P1 (mounts/registry) runs UNCONDITIONALLY — probeP1Mounts() must be
 *      called BEFORE the `if (coiFailed)` branch; only P3/P4 skip on the P2
 *      timeout. (Review finding: P1 was in the else branch, so a coi_failure
 *      report carried zero P1 records — destroying mount diagnostics.)
 *   2. The cm6_fallback finding is spec'd "not hard fail" (AC-3 line 12) —
 *      the fallback record must use status "pass", NOT "fail"; a "fail"
 *      record would pollute failedCount and make the non-fatal
 *      cm6_fallback_noted verdict unreachable under exec_failure-first
 *      priority.
 *   3. (Review-cycle-2 cleanup) The cm6 fallback PREDICATE is the spec
 *      literal `cmCount === "0" && taCount === "2"` — a partial mount
 *      (e.g. 1 .cm-editor + 2 textarea) must fall through to the hard-fail
 *      assert, NOT be misclassified non-fatal.
 *   4. (Review-cycle-2 cleanup) P2 records `timings.coi` immediately after
 *      the elapsed capture (before the timeout check), so coi_failure
 *      timeouts still surface the -1 timing in the report.
 */

const { test } = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");

const HARNESS_SRC = fs.readFileSync(
  path.join(__dirname, "pages-live.js"),
  "utf8",
);

// Pattern anchors in main() — assert they exist so a structural change to
// the orchestration fails loudly instead of passing vacuously.
const ANCHORS = {
  p2Call: "probeP2Coi();",
  coiBranch: "if (coiFailed) {",
  p1Call: "probeP1Mounts();",
  p3Skip: 'record("P3 R exec", "skip"',
  p4Skip: 'record("P4 Python exec", "skip"',
  p3Exec: "probeP3RExec();",
  p4Exec: "probeP4PyExec();",
  cm6FallbackRecord: '"P1 mounts: textarea fallback (CM6 unavailable)"',
  cm6FallbackPredicate: 'cmCount === "0" && taCount === "2"',
  timingsCoi: "timings.coi = elapsed;",
};

test("main() probe orchestration contains all expected anchors", () => {
  for (const [name, pattern] of Object.entries(ANCHORS)) {
    assert.ok(
      HARNESS_SRC.includes(pattern),
      `expected code pattern '${pattern}' (${name}) in pages-live.js`,
    );
  }
});

test("P1 mounts probe runs BEFORE the coiFailed branch (unconditional)", () => {
  const p2Idx = HARNESS_SRC.indexOf(ANCHORS.p2Call);
  const coiIdx = HARNESS_SRC.indexOf(ANCHORS.coiBranch);
  const p1Idx = HARNESS_SRC.indexOf(ANCHORS.p1Call);
  assert.ok(
    p1Idx > p2Idx && p1Idx < coiIdx,
    "probeP1Mounts() must be called after probeP2Coi() and BEFORE the " +
      "if (coiFailed) branch — P1 is unconditional; only P3/P4 skip on " +
      "coi_failure",
  );
});

test("only P3/P4 skip inside the coiFailed branch", () => {
  const coiIdx = HARNESS_SRC.indexOf(ANCHORS.coiBranch);
  const p3ExecIdx = HARNESS_SRC.indexOf(ANCHORS.p3Exec);
  const p4ExecIdx = HARNESS_SRC.indexOf(ANCHORS.p4Exec);
  const p3SkipIdx = HARNESS_SRC.indexOf(ANCHORS.p3Skip);
  const p4SkipIdx = HARNESS_SRC.indexOf(ANCHORS.p4Skip);
  // Exec probes must come after the branch (they are its else body).
  assert.ok(
    p3ExecIdx > coiIdx && p4ExecIdx > coiIdx,
    "P3/P4 exec probes must be inside the coiFailed else branch",
  );
  // Skip records must be inside the branch, before the exec probes.
  assert.ok(
    p3SkipIdx > coiIdx && p4SkipIdx > coiIdx,
    "P3/P4 skip records must be inside the coiFailed branch",
  );
  assert.ok(
    p3SkipIdx < p3ExecIdx && p4SkipIdx < p4ExecIdx,
    "skip records must precede exec probes within the branch",
  );
});

test("cm6_fallback finding is recorded as pass, NOT fail (spec: not hard fail)", () => {
  const recordIdx = HARNESS_SRC.indexOf(ANCHORS.cm6FallbackRecord);
  // The record status literal is the second argument, right after the name:
  //   record("P1 mounts: textarea fallback (CM6 unavailable)", "pass", ...)
  const statusSlice = HARNESS_SRC.slice(
    recordIdx + ANCHORS.cm6FallbackRecord.length,
    recordIdx + ANCHORS.cm6FallbackRecord.length + 30,
  );
  const passIdx = statusSlice.indexOf('"pass"');
  const failIdx = statusSlice.indexOf('"fail"');
  assert.ok(
    passIdx !== -1,
    `cm6 fallback record must use status "pass" — got: ${statusSlice.trim()}`,
  );
  assert.ok(
    failIdx === -1 || failIdx > passIdx,
    `cm6 fallback record must NOT use status "fail" — got: ${statusSlice.trim()}`,
  );
});
