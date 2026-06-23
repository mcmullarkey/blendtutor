// blendtutor in-browser LLM feedback — the FeedbackBackend seam + byok-anthropic
// + byok-fireworks impls (Slice 18/ADR-0006/0008 consequence; Slice AC1/issue #50).
//
// Where this fits: a built site runs and grades lessons locally (lesson-runner-
// core.js); *this* module is the separate concern of fetching written feedback on
// a submission from an LLM. It owns three things and nothing else (§4.1):
//   1. The `FeedbackBackend` contract — `getFeedback(prompt) -> Promise<Verdict>`,
//      the one seam a future backend (WebLLM, Slice 21) plugs into with no change
//      to lesson rendering or execution (§3.3, §3.4).
//   2. The `byok-anthropic` impl (v1) and the `byok-fireworks` impl (v2): the
//      learner brings their own key. The key is read from this tab's sessionStorage
//      and sent only to the chosen provider — header injection and key handling are
//      isolated here, never in the render/exec layers (§4.2). No key literal is ever
//      baked into a shipped file.
//   3. The submit-flow wiring: prompt for a key when absent (with the dual
//      disclosure), otherwise build the prompt, call the backend, render the
//      verdict — reading the runner's already-exposed `window.__bt` seam rather
//      than reaching into its internals.
//
// `Verdict` mirrors the Rust `core::llm::Verdict` contract (`{ correct, message }`
// ↔ `Correct{message}/Incorrect{message}`), so author-side evals and learner-side
// feedback agree on shape (§1.2, §3.2).

// --- prompt structure (the Slice-10 single source) -------------------------------
//
// These delimiters and the neutralization marker are byte-identical to the Rust
// `core::llm::prompt` constants `build_prompt` emits (pinned by the integration
// test against the exported OPEN_CODE/CLOSE_CODE/OUTPUT_LABEL/CHECKS_LABEL), so the
// learner-side prompt structure cannot drift from the author-side one.
const OPEN_CODE = "<<<STUDENT_CODE_BEGIN>>>";
const CLOSE_CODE = "<<<STUDENT_CODE_END>>>";
const OUTPUT_LABEL = "<<<CAPTURED_OUTPUT>>>";
const CHECKS_LABEL = "<<<CHECK_RESULTS>>>";
const NEUTRALIZED = "[neutralized-delimiter]";

// The tab-scoped slot the learner's key lives in — sessionStorage, so it is gone
// when the tab closes and never shared with another tab/origin.
const KEY_SLOT = "anthropic_api_key";

// The Anthropic tool the model answers through — the same contract the R package
// pinned (`respond_with_feedback(is_correct, feedback_message)`); its input maps
// 1:1 to the Rust `Feedback` DTO that becomes a `Verdict`.
const TOOL_NAME = "respond_with_feedback";

// The fallback feedback model — the picker's default and the roster it falls back
// to when the live /v1/models list is empty or unavailable. Lifted from a hardcoded
// request constant to a named fallback the model-source seam reads.
const MODEL = "claude-opus-4-8";

// The Fireworks fallback model — mirrors MODEL. The accounts/ prefix is the
// Fireworks model naming convention; this const is the single source of the
// default, never an inline literal.
const FIREWORKS_MODEL = "accounts/fireworks/models/deepseek-v4-flash";

// --- prompt assembly (pure) ------------------------------------------------------

// Strip every structural token out of untrusted text, so the only fences/labels in
// the final prompt are the ones we emit. The student submission is untrusted input
// bound for an LLM; this is the injection defense (mirrors Rust `neutralize`).
function neutralize(text) {
  return String(text ?? "")
    .replaceAll(OPEN_CODE, NEUTRALIZED)
    .replaceAll(CLOSE_CODE, NEUTRALIZED)
    .replaceAll(OUTPUT_LABEL, NEUTRALIZED)
    .replaceAll(CHECKS_LABEL, NEUTRALIZED);
}

// Render the LLM feedback prompt for a submission. Pure: a fixed structure — the
// task, a single fenced copy of the submission, captured output, and the lesson's
// checks — every interpolated value neutralized so the fences and labels appear
// exactly once even when the submission forges them.
function buildPrompt({ task, code, output, checks }) {
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

// --- key handling (sessionStorage, tab-scoped) -----------------------------------

function readKey() {
  return window.sessionStorage.getItem(KEY_SLOT);
}

function storeKey(key) {
  window.sessionStorage.setItem(KEY_SLOT, key);
}

// The provider base URL. Defaults to Anthropic. A `?provider=` override is the
// test seam (point rodney at a local stub), but it is honored *only* when it
// resolves to a local host — so a crafted production link
// (`?provider=https://attacker.example`) can never redirect a real learner's key
// off-Anthropic. This *enforces* the AC1 disclosure ("sent only to Anthropic") in
// code rather than merely asserting it: a non-local override is ignored, and the
// key still reaches only api.anthropic.com.
function providerBaseUrl() {
  const override = new URLSearchParams(window.location.search).get("provider");
  if (override) {
    try {
      const url = new URL(override);
      const isLocal = url.hostname === "localhost" || url.hostname === "127.0.0.1";
      // Reject embedded credentials (`user@host`): a credentialed authority never
      // names a clean local stub (and `fetch` rejects such URLs anyway), so fall
      // through to the default rather than let a crafted authority look honored.
      if (isLocal && !url.username && !url.password) {
        return override;
      }
    } catch (_error) {
      // A malformed override is ignored — fall through to the default.
    }
  }
  return "https://api.anthropic.com";
}

// --- model discovery (the picker source seam) ------------------------------------

// Pure: extract the model-id list from an Anthropic `/v1/models` response. Tolerates
// a missing or empty `data` array (→ []) and ignores non-string ids; the caller
// applies the fallback roster, so this never has to invent a default.
function parseModels(json) {
  const data = (json && json.data) || [];
  return data
    .map((entry) => entry && entry.id)
    .filter((id) => typeof id === "string");
}

// Pure: the roster the picker renders — the live list when it has any models, else a
// roster of just the named fallback. Keeps "never a zero-option dead select" as one
// named policy rather than a guard scattered across the picker.
function modelRoster(models) {
  return models.length ? models : [MODEL];
}

// Effectful: fetch the models this key can reach, through the SAME host-gated base
// URL as messages — a non-local `?provider=` override is ignored here too, so the
// picker opens no new key-exfil vector. Returns [] on any failure (non-OK, network,
// malformed JSON); the pure `modelRoster` turns [] into a usable roster, so a models
// outage is never a dead end. The key rides `x-api-key` with the direct-browser-
// access opt-in, exactly as the messages call.
async function listModels({ baseUrl, apiKey }) {
  try {
    const response = await fetch(`${baseUrl}/v1/models`, {
      headers: {
        "x-api-key": apiKey,
        "anthropic-version": "2023-06-01",
        "anthropic-dangerous-direct-browser-access": "true",
      },
    });
    if (!response.ok) {
      return [];
    }
    return parseModels(await response.json());
  } catch (_error) {
    return [];
  }
}

// --- the byok-anthropic backend --------------------------------------------------

// Build the Anthropic Messages API request body for `prompt` with the chosen
// `model`. Pure: the model is an explicit argument, not a captured module constant,
// so the picker's selection is the only thing that drives the request model; forcing
// the `respond_with_feedback` tool makes the model answer with the typed verdict.
function feedbackRequest(prompt, model) {
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

// Map the Anthropic tool-call response to a Verdict, mirroring the Rust
// `Feedback -> Verdict` boundary: the bool's meaning lives in `correct`.
function toVerdict(data) {
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
// learner's key. The key rides `x-api-key`; the direct-browser-access opt-in is
// required for the browser call to be allowed. This is the *whole* place a key is
// read and sent — isolated from render/exec (§4.2).
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

// --- the byok-fireworks backend --------------------------------------------------

// Build the Fireworks (OpenAI-compatible) chat completions request body for
// `prompt` with the chosen `model`. Pure: mirrors `feedbackRequest` in structure
// but uses the OpenAI tool-call shape — `parameters` (not `input_schema`) and
// `{ type: "function", function: { name } }` (not `{ type: "tool", name }`).
function fireworksRequest(prompt, model) {
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

// Map the Fireworks (OpenAI-compatible) tool-call response to a Verdict.
// OpenAI returns `function.arguments` as a JSON *string* (unlike Anthropic's
// structured `input`), so `JSON.parse` is mandatory.
function fireworksToVerdict(data) {
  const choice = (data.choices ?? [])[0];
  const toolCall = (choice?.message?.tool_calls ?? [])[0];
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
// browser with the learner's key. The key rides the `Authorization: Bearer`
// header; there are no Anthropic-specific headers. The endpoint is
// `${baseUrl}/chat/completions` (not `/v1/chat/completions` — the Fireworks base
// URL already carries `/v1`).
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

// --- the submit flow (effectful shell) -------------------------------------------

function feedbackContainer() {
  return document.getElementById("feedback");
}

// Read the current lesson + the learner's code from the runner's exposed seam and
// the editor — no reach into runner internals (§3.4). Captured output rides along
// if the learner ran the checks first; the prompt structure tolerates it empty.
function currentSubmission() {
  const bt = window.__bt;
  const lesson = bt && bt.lessons ? bt.lessons[bt.current] : undefined;
  const submissionEl = document.getElementById("submission");
  const outputEl = document.getElementById("output");
  return {
    task: lesson ? lesson.prompt : "",
    code: submissionEl ? submissionEl.value : "",
    output: outputEl ? outputEl.textContent : "",
    checks: lesson && lesson.checks ? lesson.checks : [],
  };
}

// Prompt the learner for a key, with the dual disclosure: stored only in this tab,
// sent only to Anthropic. Rendered here (not baked into index.html) so the whole
// prompt-for-key path lives in the impl, isolated from the page shell (§3.3, §4.1).
function renderKeyPrompt(container) {
  const form = document.createElement("form");
  form.dataset.byok = "key-prompt";

  const disclosure = document.createElement("p");
  disclosure.textContent =
    "Your Anthropic API key is stored only in this tab (this browser tab's " +
    "sessionStorage) and sent only to Anthropic to fetch feedback — never to this " +
    "site's server or any third party.";

  const label = document.createElement("label");
  label.textContent = "Anthropic API key: ";
  const input = document.createElement("input");
  input.type = "password";
  input.name = "anthropic-key";
  input.autocomplete = "off";
  input.placeholder = "Paste your Anthropic API key";
  label.append(input);

  const save = document.createElement("button");
  save.type = "submit";
  save.textContent = "Save key & get feedback";

  form.append(disclosure, label, save);
  form.addEventListener("submit", (event) => {
    event.preventDefault();
    const key = input.value.trim();
    if (!key) {
      return;
    }
    storeKey(key);
    handleSubmit();
  });

  container.replaceChildren(form);
}

// Render the verdict. textContent only — the message is model output (untrusted),
// so it is never parsed as HTML (the same threat model that keeps lesson titles
// off `innerHTML`).
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

// Render the model picker into the feedback container: a labeled `<select>`
// populated from the learner's live model roster, defaulting to the named fallback
// when present. The select carries a distinct `data-byok="model"` marker and lives
// under `#feedback`, so the `#feedback select` test predicate pins it without prefix-
// colliding with the page-level `#lesson-select` (§1.5). Effectful only in that it
// awaits `listModels`; the roster and the default choice are pure.
async function renderModelPicker(container, source) {
  // A loading note while the live list is in flight, so a slow models query doesn't
  // read as a dead click; the roster replaces it the moment it resolves.
  const loading = document.createElement("p");
  loading.dataset.byok = "models-loading";
  loading.textContent = "Loading models…";
  container.replaceChildren(loading);

  const roster = modelRoster(await listModels(source));

  const picker = document.createElement("div");
  picker.dataset.byok = "model-picker";

  const label = document.createElement("label");
  label.textContent = "Feedback model: ";
  const select = document.createElement("select");
  select.dataset.byok = "model";
  for (const id of roster) {
    const option = document.createElement("option");
    option.value = id;
    option.textContent = id;
    select.append(option);
  }
  // Default to the named fallback when the roster carries it, else the first model —
  // never an empty value, so submit always has a model to send.
  select.value = roster.includes(MODEL) ? MODEL : roster[0];
  label.append(select);

  const hint = document.createElement("p");
  hint.textContent = "Pick a model, then Submit for feedback.";

  picker.append(label, hint);
  container.replaceChildren(picker);
}

// Whether the picker is already showing — the gate between the two submit phases.
function modelPickerPresent(container) {
  return Boolean(container.querySelector('[data-byok="model-picker"]'));
}

// The model the learner has chosen — read straight from the picker at submit time
// (no module state), falling back to the named default if the select is somehow
// absent so the request always carries a model.
function selectedModel(container) {
  const select = container.querySelector('[data-byok="model"]');
  return select ? select.value : MODEL;
}

// Orchestrate one submit, in three named states:
//   no key       → prompt for one (dual disclosure);
//   no picker yet → render the model picker from the live roster, then stop so the
//                   learner can choose (the first submit surfaces the picker);
//   picker shown  → build the prompt and send feedback with the *chosen* model.
async function handleSubmit() {
  const container = feedbackContainer();
  if (!container) {
    return;
  }
  const apiKey = readKey();
  if (!apiKey) {
    renderKeyPrompt(container);
    return;
  }
  const baseUrl = providerBaseUrl();
  if (!modelPickerPresent(container)) {
    // No try/catch twin of the /v1/messages branch below, by design: renderModelPicker
    // cannot throw. listModels swallows every fetch/parse failure into the fallback
    // roster (modelRoster guarantees a non-empty list), and the rest is pure DOM
    // construction — there is no reachable error to surface, so the loading note is
    // always replaced. If a future provider adds throwing logic *outside* listModels,
    // add the renderError guard here then.
    await renderModelPicker(container, { baseUrl, apiKey });
    return;
  }
  const model = selectedModel(container);
  const submission = currentSubmission();
  const prompt = buildPrompt(submission);
  const backend = byokAnthropic({ baseUrl, apiKey });
  renderPending(container);
  try {
    renderVerdict(container, await backend.getFeedback(prompt, model));
  } catch (error) {
    renderError(container, error);
  }
}

const submitButton = document.querySelector("[data-action=submit]");
if (submitButton) {
  submitButton.addEventListener("click", handleSubmit);
}
