// exercise-feedback.js — per-exercise BYOK LLM feedback (AC-7).
//
// WHAT:  Per-exercise feedback UI + shared localStorage key + ?provider= override.
// WHERE: _extensions/blendtutor/assets/exercise-feedback.js
// NOT:   NOT execution (exercise-runtime.js), NOT filter (blendtutor.lua),
//        NOT CodeMirror editor, NOT prompt construction beyond the pure builder.
//
// FORK of feedback.js (crates/core/assets/shared/feedback.js) — kills 3 singletons:
//   1. document.getElementById("feedback") (line 385) → per-exercise feedback
//      container created inside each div.bt-exercise.
//   2. window.__bt.getSubmission() (line 396) → entry.getSubmission() from the
//      AC-4 per-exercise registry (window.__btExercises).
//   3. Module-level submit-button wiring (line 690) → mountFeedback(entry),
//      called per-exercise by mountAllFeedback(registry).
//
// Pure layer ported UNCHANGED from feedback.js:
//   neutralize, buildPrompt, parseModels, modelRoster, feedbackRequest,
//   toVerdict, fireworksRequest, fireworksToVerdict, providerBaseUrl.
//   These are byte-identical to the Rust core::llm prompt constants (ADR-0006).
//
// Shared localStorage (key entered once, reused across exercises):
//   readKey/storeKey use provider-scoped slots (fireworks_api_key,
//   anthropic_api_key) — NOT exercise-scoped. The key entered for exercise 1
//   is reused for exercise 2 without re-prompting (clause 1 + clause 3).
//
// Per-exercise scoping (clause 2):
//   Each exercise gets its own feedback button + container inside its
//   div.bt-exercise. No singleton #feedback. The verdict, pending, and error
//   UI render into the per-exercise container, so two exercises never
//   overwrite each other's feedback.
//
// Concurrent guard (clause 8):
//   Per-exercise _feedbackRunning flag. A second submit on the SAME exercise
//   while the first is in flight is rejected. Different exercises can fetch
//   feedback concurrently (they have independent containers + guards).
//
// No module-level effectful code (§2.1):
//   The old feedback.js ran applyEmbeddedKey() and wired the submit button at
//   module load. This module exports mountAllFeedback(registry) — the caller
//   invokes it AFTER the runtime boots. Pure functions are importable in
//   Node.js without side effects, so they are testable without a browser.

// Issue #186: the no-key state mounts the key-management form (key-page.js
// mountKeyPage) INLINE into the feedback container — no navigation link. The
// import is a cycle (key-page.js imports the storage/provider contract from
// this module) — safe: both modules use imported bindings only at call time,
// never at module-eval time (ES live bindings resolve by then).
import { mountKeyPage } from "./key-page.js";

// --- prompt structure (the single source, ported from feedback.js) -------------
//
// Byte-identical to the Rust core::llm prompt constants (pinned by the
// integration test). The learner-side prompt structure cannot drift from
// the author-side one (ADR-0006).
export const OPEN_CODE = "<<<STUDENT_CODE_BEGIN>>>";
export const CLOSE_CODE = "<<<STUDENT_CODE_END>>>";
export const OUTPUT_LABEL = "<<<CAPTURED_OUTPUT>>>";
export const CHECKS_LABEL = "<<<CHECK_RESULTS>>>";
const NEUTRALIZED = "[neutralized-delimiter]";

// --- provider map + key handling (localStorage, persistent) ----------------------

// The closed set of providers. Each entry carries the persistent localStorage
// slot for the learner's key (provider-scoped, NOT exercise-scoped — the key is
// entered once and reused across exercises), the base URL, the fallback model,
// and the FeedbackBackend factory.
export const PROVIDERS = {
  fireworks: {
    label: "Fireworks",
    keySlot: "fireworks_api_key",
    baseUrl: "https://api.fireworks.ai/inference/v1",
    fallbackModel: "accounts/fireworks/models/deepseek-v4-flash-0731",
    factory: byokFireworks,
  },
  anthropic: {
    label: "Anthropic",
    keySlot: "anthropic_api_key",
    baseUrl: "https://api.anthropic.com",
    fallbackModel: "claude-opus-4-8",
    factory: byokAnthropic,
  },
};
export const DEFAULT_PROVIDER = "fireworks";

// Per-provider disclosure texts — literal strings so the static scan can
// verify BOTH are present. Documentation of the privacy promise shown to
// learners; the no-key state mounts the key-page form inline (issue #186)
// without repeating the disclosures, but the copy stays pinned in source.
const PROVIDER_DISCLOSURES = {
  fireworks: "Your Fireworks API key is stored in this browser (localStorage) " +
    "and sent only to Fireworks to fetch feedback — never to this site's " +
    "server or any third party.",
  anthropic: "Your Anthropic API key is stored in this browser (localStorage) " +
    "and sent only to Anthropic to fetch feedback — never to this site's " +
    "server or any third party.",
};

const TOOL_NAME = "respond_with_feedback";
const MODEL = "claude-opus-4-8";
// Pinned learner-facing model (AC-5): the model picker is collapsed, so the
// Fireworks request body always carries this exact id. Static-asserted at
// both uses (this const + PROVIDERS.fireworks.fallbackModel).
const FIREWORKS_MODEL = "accounts/fireworks/models/deepseek-v4-flash-0731";

// --- prompt assembly (pure, ported from feedback.js) -----------------------------

// Strip every structural token out of untrusted text, so the only fences/labels
// in the final prompt are the ones we emit. The student submission is untrusted
// input bound for an LLM; this is the injection defense (mirrors Rust neutralize).
export function neutralize(text) {
  return String(text ?? "")
    .replaceAll(OPEN_CODE, NEUTRALIZED)
    .replaceAll(CLOSE_CODE, NEUTRALIZED)
    .replaceAll(OUTPUT_LABEL, NEUTRALIZED)
    .replaceAll(CHECKS_LABEL, NEUTRALIZED);
}

// Render the LLM feedback prompt for a submission. Pure: a fixed structure — the
// task, a single fenced copy of the submission, captured output, and the lesson's
// checks — every interpolated value neutralized so the fences and labels appear
// exactly once even when the submission forges them (ADR-0006 §2.1–§2.3).
export function buildPrompt({ task, code, output, checks }) {
  const renderedChecks = (checks ?? []).map((check) => neutralize(check)).join("\n");
  return [
    "You are evaluating student code for a programming exercise.",
    "",
    "Task:",
    neutralize(task).replace(/\s+$/, ""),
    "",
    OPEN_CODE,
    neutralize(code).replace(/\n+$/, ""),
    CLOSE_CODE,
    "",
    OUTPUT_LABEL,
    neutralize(output).replace(/\n+$/, ""),
    "",
    CHECKS_LABEL,
    renderedChecks,
  ].join("\n");
}

// --- key handling (localStorage, persistent, SHARED across exercises) -------------
//
// The key slot is provider-scoped (fireworks_api_key / anthropic_api_key), NOT
// exercise-scoped. This is the contract that makes "key entered once, reused
// across exercises" work: exercise 1 stores the key, exercise 2 reads it from
// the same slot without re-prompting (clause 1 + clause 3).

export function readKey(providerId) {
  try {
    return window.localStorage.getItem(PROVIDERS[providerId].keySlot);
  } catch (_error) {
    // localStorage unavailable (private mode / file://) → degrade to "no key"
    // so the submit flow renders the key prompt instead of crashing.
    return null;
  }
}

export function storeKey(key, providerId) {
  window.localStorage.setItem(PROVIDERS[providerId].keySlot, key);
}

// Scoped removal: the provider's key AND the feedback counter. Deliberately NOT
// localStorage.clear() — unrelated app state (byok_provider, anthropic_api_key,
// quarto-reader-mode, quarto-persistent-tabsets-data) survives. Removing the
// counter is what un-locks a rate-capped learner (the counter is persistent now).
export function clearKey(providerId) {
  window.localStorage.removeItem(PROVIDERS[providerId].keySlot);
  window.localStorage.removeItem(FEEDBACK_COUNT_KEY);
}

export function readProvider() {
  const stored = window.localStorage.getItem("byok_provider");
  return stored && Object.hasOwn(PROVIDERS, stored) ? stored : DEFAULT_PROVIDER;
}

export function storeProvider(providerId) {
  window.localStorage.setItem("byok_provider", providerId);
}

// The provider base URL. A `?provider=` override is the test seam (point rodney
// at a local stub), honored ONLY when it resolves to a local host — so a crafted
// production link (`?provider=https://attacker.example`) can never redirect a real
// learner's key off the intended provider. This enforces the disclosure ("sent
// only to {provider}") in code (§1.5). Falls back to PROVIDERS.anthropic.baseUrl
// for an unknown provider id (defense in depth).
export function providerBaseUrl(providerId) {
  const override = new URLSearchParams(window.location.search).get("provider");
  if (override) {
    try {
      const url = new URL(override);
      const isLocal = url.hostname === "localhost" || url.hostname === "127.0.0.1";
      // Reject embedded credentials: a credentialed authority never names a clean
      // local stub, so fall through to the default rather than let a crafted
      // authority look honored.
      if (isLocal && !url.username && !url.password) {
        return override;
      }
    } catch (_error) {
      // A malformed override is ignored — fall through to the default.
    }
  }
  const provider = PROVIDERS[providerId];
  return provider ? provider.baseUrl : PROVIDERS.anthropic.baseUrl;
}

// --- model discovery (pure roster helpers, ported from feedback.js) ---------

// Pure: extract the model-id list from a /v1/models response. Both Anthropic
// and Fireworks return {data:[{id}]} — extracted once, provider-agnostic (§1.1).
export function parseModels(json) {
  const data = (json && json.data) || [];
  return data
    .map((entry) => entry && entry.id)
    .filter((id) => typeof id === "string");
}

// Pure: the roster for a live model list — the list itself when non-empty,
// else a roster of just the provider-specific pinned model (§5.1). Retained
// as part of the exported pure layer (Node-tested); the learner-facing
// picker that rendered this roster is gone (AC-5 pins the model).
export function modelRoster(models, provider) {
  const fallback = provider === "fireworks" ? FIREWORKS_MODEL : MODEL;
  return models.length ? models : [fallback];
}

// --- the byok-anthropic backend (ported from feedback.js) ------------------------

// Pure: build the Anthropic Messages API request body for `prompt` with the
// chosen `model`. The model is an explicit argument, not a captured constant.
export function feedbackRequest(prompt, model) {
  return {
    model,
    max_tokens: 1024,
    tool_choice: { type: "tool", name: TOOL_NAME },
    tools: [
      {
        name: TOOL_NAME,
        description: "Respond with a verdict on the student's submission.",
        input_schema: {
          type: "object",
          properties: {
            is_correct: {
              type: "boolean",
              description: "Whether the submission satisfies the exercise.",
            },
            feedback_message: {
              type: "string",
              description: "The feedback message for the learner.",
            },
          },
          required: ["is_correct", "feedback_message"],
        },
      },
    ],
    messages: [{ role: "user", content: prompt }],
  };
}

// Pure: map the Anthropic tool-call response to a Verdict (mirrors Rust
// Feedback → Verdict boundary).
export function toVerdict(data) {
  const block = (data.content ?? []).find(
    (b) => b.type === "tool_use" && b.name === TOOL_NAME,
  );
  if (!block || !block.input) {
    throw new Error("the provider returned no feedback tool call");
  }
  return {
    correct: Boolean(block.input.is_correct),
    message: String(block.input.feedback_message ?? ""),
  };
}

// The v1 FeedbackBackend: calls Anthropic directly from the browser with the
// learner's key. The key rides x-api-key; the direct-browser-access opt-in is
// required. This is the whole place a key is read and sent — isolated from
// render/exec (§4.2).
function byokAnthropic({ baseUrl, apiKey }) {
  return {
    name: "byok-anthropic",
    async getFeedback(prompt, model) {
      const response = await fetch(`${baseUrl}/v1/messages`, {
        method: "POST",
        headers: {
          "content-type": "application/json",
          "x-api-key": apiKey,
          "anthropic-version": "2023-06-01",
          "anthropic-dangerous-direct-browser-access": "true",
        },
        body: JSON.stringify(feedbackRequest(prompt, model)),
      });
      if (!response.ok) {
        throw new Error(`the provider returned HTTP ${response.status}`);
      }
      return toVerdict(await response.json());
    },
  };
}

// --- the byok-fireworks backend (ported from feedback.js) -----------------------

// Pure: build the Fireworks (OpenAI-compatible) chat completions request body.
export function fireworksRequest(prompt, model) {
  return {
    model,
    messages: [{ role: "user", content: prompt }],
    tools: [
      {
        "type": "function",
        "function": {
          "name": TOOL_NAME,
          "description": "Respond with a verdict on the student's submission.",
          "parameters": {
            "type": "object",
            "properties": {
              "is_correct": { "type": "boolean" },
              "feedback_message": { "type": "string" },
            },
            "required": ["is_correct", "feedback_message"],
          },
        },
      },
    ],
    "tool_choice": { "type": "function", "function": { "name": TOOL_NAME } },
  };
}

// Pure: map the Fireworks (OpenAI-compatible) tool-call response to a Verdict.
// OpenAI returns function.arguments as a JSON string, so JSON.parse is mandatory.
export function fireworksToVerdict(data) {
  const choice = (data.choices ?? [])[0];
  const toolCall = (choice?.message?.tool_calls ?? [])
    .find((c) => c?.function?.name === TOOL_NAME);
  if (!toolCall?.function?.arguments) {
    throw new Error("the provider returned no feedback tool call");
  }
  const args = JSON.parse(toolCall.function.arguments);
  return {
    correct: Boolean(args.is_correct),
    message: String(args.feedback_message ?? ""),
  };
}

// The v2 FeedbackBackend: calls Fireworks (OpenAI-compatible) directly from the
// browser with the learner's key. The key rides Authorization: Bearer.
function byokFireworks({ baseUrl, apiKey }) {
  return {
    name: "byok-fireworks",
    async getFeedback(prompt, model) {
      const response = await fetch(`${baseUrl}/chat/completions`, {
        method: "POST",
        headers: {
          "content-type": "application/json",
          "Authorization": "Bearer " + apiKey,
        },
        body: JSON.stringify(fireworksRequest(prompt, model)),
      });
      if (!response.ok) {
        throw new Error(`the provider returned HTTP ${response.status}`);
      }
      return fireworksToVerdict(await response.json());
    },
  };
}

// --- rate limiting (localStorage counter, ported from feedback.js) ---------------

const FEEDBACK_COUNT_KEY = "bt_feedback_count";

// Read the current feedback request count, defaulting to 0 when absent or
// non-integer. The parseInt + || 0 guard ensures a corrupt value never yields
// NaN (which would silently disable limiting — the negative case from the spec).
export function feedbackCount() {
  return parseInt(localStorage.getItem(FEEDBACK_COUNT_KEY)) || 0;
}

// Whether the learner has reached the per-browser feedback request limit. The
// counter lives in localStorage, so the cap is persistent across tabs and
// reloads; clearKey() resets it. The maxFeedbackPerSession config key keeps its
// name (smallest correct move) but now caps PER-BROWSER, not per-session.
export function rateLimitReached() {
  const max =
    (window.__btConfig && window.__btConfig.maxFeedbackPerSession) || 0;
  return feedbackCount() >= max;
}

// Increment the per-browser feedback request counter. Called AFTER the try/catch
// in handleSubmitForExercise — both successful and failed requests count.
export function incrementFeedbackCount() {
  localStorage.setItem(FEEDBACK_COUNT_KEY, String(feedbackCount() + 1));
}

// --- the per-exercise submit flow (effectful shell, REWRITTEN) -------------------
//
// The old feedback.js used a singleton #feedback container and window.__bt.
// This module uses per-exercise containers (created in mountFeedback) and the
// AC-4 registry entry's getSubmission(). The key handling is shared
// (localStorage), so the key entered for exercise 1 is reused for exercise 2.

// Read the current lesson + the learner's code from the per-exercise registry
// entry — no reach into runner internals (§3.4). The code comes from
// entry.getSubmission() (the CM6 editor doc, or the fallback textarea). The
// output comes from the per-exercise output element (entry.outputEl, set by
// exercise-runtime.js wireExercise). Captured output rides along if the learner
// ran the checks first; the prompt structure tolerates it empty.
function currentSubmissionForExercise(entry) {
  const lesson = entry.payload;
  const outputEl = entry.element.querySelector(".bt-output");
  return {
    task: lesson ? lesson.prompt : "",
    code: entry.getSubmission ? entry.getSubmission() : "",
    output: outputEl ? outputEl.textContent : "",
    checks: lesson && lesson.checks ? lesson.checks : [],
  };
}

// --- the no-key state (issue #186): INLINE key form, no navigation link ----
//
// The learner clicks "Get feedback" with no stored key → the feedback
// container mounts the key-management form (key-page.js mountKeyPage) INLINE
// instead of rendering a link to a separate key page (the user-reported fix:
// the old link navigated away to a confusing page and dropped the learner's
// context). The container itself is the mount target — key-page.js renders
// its password input + Save button into it.
//
// Save-continuation: onSaved re-invokes handleSubmitForExercise once the key
// is stored, so the same click path proceeds to the rate-limit check + fetch
// without a second user action. The callback only fires on a successful
// storeKey (optimistic contract — key stored = proceed); a rejected or
// unreachable advisory validation is copy on the form's status line and must
// NOT block continuation.
//
// keyPageUrl() is retained for the standalone api-key chapter (the demo-book
// index + the lua bt-key-page emission still point at it) and stays exported
// (Node-tested by key-page-url.test.js).

// Pure-ish: the key-page URL for the standalone api-key chapter. Reads
// window.__btConfig at CALL time, rejects javascript:/data: schemes (defense
// in depth against a crafted config), falls back to the default page. Total:
// never throws, never returns "". Exported so it is Node-testable without a
// browser.
export function keyPageUrl() {
  const config = typeof window !== "undefined" ? window.__btConfig : null;
  let candidate = config && config.keyPageUrl;
  if (typeof candidate !== "string") {
    return "api-key.html";
  }
  candidate = candidate.trim();
  if (!candidate) {
    return "api-key.html";
  }
  const scheme = /^[a-z][a-z0-9+.-]*:/i.exec(candidate);
  if (scheme) {
    const name = scheme[0].toLowerCase();
    if (name === "javascript:" || name === "data:") {
      return "api-key.html";
    }
  }
  return candidate;
}

// Render the verdict. textContent only — the message is model output (untrusted),
// so it is never parsed as HTML.
function renderVerdict(container, verdict) {
  const box = document.createElement("div");
  box.dataset.byok = "verdict";
  box.dataset.correct = String(verdict.correct);

  const label = document.createElement("strong");
  label.textContent = verdict.correct ? "Correct" : "Not yet";
  const message = document.createElement("p");
  message.textContent = verdict.message;

  box.append(label, message);
  container.replaceChildren(box);
}

function renderPending(container) {
  const note = document.createElement("p");
  note.dataset.byok = "pending";
  note.textContent = "Asking for feedback…";
  container.replaceChildren(note);
}

function renderError(container, error) {
  const note = document.createElement("p");
  note.dataset.byok = "error";
  note.textContent = "Could not fetch feedback: " + error.message;
  container.replaceChildren(note);
}

function renderLimitReached(container) {
  const note = document.createElement("p");
  note.dataset.byok = "limit-reached";
  note.textContent =
    "Feedback request limit reached for this browser. Clear your saved API key to reset.";
  container.replaceChildren(note);
}

// Orchestrate one submit for a single exercise, in three named states:
//   no key            → mount the key form INLINE (key-page.js mountKeyPage)
//                        with an onSaved save-continuation that re-runs this
//                        handler once the key is stored (issue #186);
//   rate-limited      → render the limit-reached message (no fetch);
//   key present       → build the prompt (task + student code + captured
//                        .bt-output + checks) and send feedback through the
//                        provider's backend with the pinned model (AC-5).
//
// The provider + key are read from SHARED localStorage (entered once, reused).
// The submission is read from the per-exercise registry entry. The verdict
// renders into the per-exercise container (no cross-exercise bleed).
//
// Model is pinned per provider (AC-5): the learner-facing model picker is
// collapsed, so there is no second click phase between the key check and the
// rate-limit check — one click goes straight to the fetch with the pinned
// model (handleSubmitForExercise is a single path, §5).
//
// Concurrent guard (clause 8): entry._feedbackRunning prevents overlapping
// requests on the SAME exercise. Different exercises have independent guards.
async function handleSubmitForExercise(entry) {
  const container = entry.feedbackContainer;
  if (!container) {
    return;
  }
  // Per-exercise concurrent guard — reject a second submit on the same
  // exercise while the first is in flight (clause 8).
  if (entry._feedbackRunning) {
    console.warn(
      `[blendtutor] Exercise "${entry.id}" feedback already in flight, ignoring concurrent request.`,
    );
    return;
  }
  const providerId = readProvider();
  const apiKey = readKey(providerId);
  if (!apiKey) {
    // Inline key form (issue #186): mount key-page.js INTO the feedback
    // container. The onSaved continuation re-runs this handler once the key
    // is stored — the key check then passes and the flow proceeds to the
    // rate-limit check + fetch without a second user action.
    mountKeyPage(container, { onSaved: () => handleSubmitForExercise(entry) });
    return;
  }
  // Rate-limit check BEFORE the fetch — a capped learner never fires a
  // request (arm 9 ordering).
  if (rateLimitReached()) {
    renderLimitReached(container);
    return;
  }
  entry._feedbackRunning = true;
  try {
    const baseUrl = providerBaseUrl(providerId);
    const model = PROVIDERS[providerId].fallbackModel;
    const submission = currentSubmissionForExercise(entry);
    const prompt = buildPrompt(submission);
    const backend = PROVIDERS[providerId].factory({ baseUrl, apiKey });
    renderPending(container);
    try {
      renderVerdict(container, await backend.getFeedback(prompt, model));
    } catch (error) {
      renderError(container, error);
    }
    incrementFeedbackCount();
  } finally {
    entry._feedbackRunning = false;
  }
}

// --- embedded key (build-time --embed-key, ported) ------------------------------
//
// Apply an embedded API key (if present). The decrypt shell sets
// window.__btEmbeddedKey; applyEmbeddedKey stores it in localStorage and
// clears the global, so handleSubmitForExercise's key-entry phase is skipped
// naturally. Exported, NOT called at module load — the caller invokes it before
// mountAllFeedback (§2.1: no module-level effectful code).
export function applyEmbeddedKey() {
  const embedded = window.__btEmbeddedKey;
  if (embedded && embedded.provider && embedded.key) {
    storeProvider(embedded.provider);
    storeKey(embedded.key, embedded.provider);
    window.__btEmbeddedKey = undefined;
  }
}

// --- per-exercise mount (the REWRITTEN effectful shell) -------------------------
//
// mountFeedback creates a feedback button + container INSIDE the exercise's
// div.bt-exercise. Each exercise gets its own UI — no singleton #feedback.
// The button triggers handleSubmitForExercise(entry), which reads the
// submission from entry.getSubmission() (the per-exercise editor) and the key
// from shared localStorage (entered once, reused).
//
// Issue #182 (toolbar placement): when the runtime has exposed entry.controls
// (the Check/Show-solution row created by exercise-runtime.js
// wireExercise), the Get feedback button is PREPENDED INTO that row so it
// occupies the former Run button's slot — FIRST, before Check and Show
// solution (the user's stated fix: "replace the Run button", which was
// first in the toolbar). The feedback CONTAINER (verdict area,
// data-byok="feedback") stays as a direct child of the exercise element
// below the controls. When entry.controls is absent (standalone mounts,
// tests), the button falls back to a direct child of the exercise element
// above the container.
export function mountFeedback(entry) {
  // Create the feedback container inside the exercise div.
  const feedbackContainer = document.createElement("div");
  feedbackContainer.className = "bt-feedback";
  feedbackContainer.dataset.byok = "feedback";
  // Back-reference so handleSubmitForExercise can find the container from entry.
  entry.feedbackContainer = feedbackContainer;

  // Per-exercise concurrent guard (clause 8).
  entry._feedbackRunning = false;

  // The feedback button — one per exercise.
  const feedbackBtn = document.createElement("button");
  feedbackBtn.className = "bt-feedback-btn";
  feedbackBtn.dataset.byok = "submit";
  feedbackBtn.textContent = "Get feedback";
  feedbackBtn.addEventListener("click", () => {
    handleSubmitForExercise(entry);
  });

  if (entry.controls) {
    // Toolbar placement (issue #182): the button joins the Check/Show
    // solution row created by exercise-runtime.js wireExercise — PREPENDED
    // so Get feedback is first (the former Run button's slot, per the
    // user's stated fix).
    entry.controls.insertBefore(feedbackBtn, entry.controls.firstChild);
  } else {
    // Fallback for standalone mounts (no runtime controls row): the button
    // sits directly above the container inside the exercise element.
    entry.element.appendChild(feedbackBtn);
  }
  entry.element.appendChild(feedbackContainer);
}

// Mount feedback UI for every exercise in the registry. Called by the page
// AFTER start(registry, adapters) boots the runtime — each exercise already has
// its editor + Run button wired by exercise-runtime.js. This adds the feedback
// button + container per-exercise, using the shared localStorage key.
export function mountAllFeedback(registry) {
  applyEmbeddedKey();
  for (const entry of registry) {
    mountFeedback(entry);
  }
}
