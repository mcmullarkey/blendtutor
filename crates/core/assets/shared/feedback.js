// blendtutor in-browser LLM feedback — the FeedbackBackend seam + provider-chooser
// + byok-fireworks / byok-anthropic impls (Slice 18/ADR-0006/0008/0009 consequence;
// Slices AC1/issue #50, AC2/issue #51).
//
// Where this fits: a built site runs and grades lessons locally (lesson-runner-
// core.js); *this* module is the separate concern of fetching written feedback on
// a submission from an LLM. It owns three things and nothing else (§4.1):
//   1. The `FeedbackBackend` contract — `getFeedback(prompt) -> Promise<Verdict>`,
//      the one seam a future backend (WebLLM, Slice 21) plugs into with no change
//      to lesson rendering or execution (§3.3, §3.4).
//   2. A `PROVIDERS` map carrying each provider's key slot, base URL, fallback
//      model, and factory — the chooser drives routing. The learner brings their
//      own key. The key is read from this tab's sessionStorage and sent only to
//      the chosen provider — header injection and key handling are isolated here,
//      never in the render/exec layers (§4.2). No key literal is ever baked into
//      a shipped file.
//   3. The submit-flow wiring: prompt for a provider + key when absent (with
//      per-provider disclosure), otherwise build the prompt, call the backend,
//      render the verdict — reading the runner's already-exposed `window.__bt`
//      seam rather than reaching into its internals.
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

// --- provider map + key handling (sessionStorage, tab-scoped) --------------------

// The closed set of providers the chooser renders and handleSubmit routes to.
// Each entry carries the tab-scoped sessionStorage slot for the learner's key,
// the base URL for API calls, the fallback model, and the FeedbackBackend factory
// that builds the provider-specific backend.
const PROVIDERS = {
  fireworks: {
    label: "Fireworks",
    keySlot: "fireworks_api_key",
    baseUrl: "https://api.fireworks.ai/inference/v1",
    fallbackModel: "accounts/fireworks/models/deepseek-v4-flash",
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
const DEFAULT_PROVIDER = "fireworks";

// Per-provider disclosure texts — literal strings so the build-test static scan
// can verify BOTH are present (the test lowercases the source and looks for the
// provider name). Each matches the dynamic disclosure renderKeyPrompt builds from
// PROVIDERS[id].label so the disclosure is never stale.
const PROVIDER_DISCLOSURES = {
  fireworks: "Your Fireworks API key is stored only in this tab (this browser " +
    "tab's sessionStorage) and sent only to Fireworks to fetch feedback — never " +
    "to this site's server or any third party.",
  anthropic: "Your Anthropic API key is stored only in this tab (this browser " +
    "tab's sessionStorage) and sent only to Anthropic to fetch feedback — never " +
    "to this site's server or any third party.",
};

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

function readKey(providerId) {
  return window.sessionStorage.getItem(PROVIDERS[providerId].keySlot);
}

function storeKey(key, providerId) {
  window.sessionStorage.setItem(PROVIDERS[providerId].keySlot, key);
}

function readProvider() {
  const stored = window.sessionStorage.getItem("byok_provider");
  return stored && Object.hasOwn(PROVIDERS, stored) ? stored : DEFAULT_PROVIDER;
}

function storeProvider(providerId) {
  window.sessionStorage.setItem("byok_provider", providerId);
}

// The provider base URL. Takes the selected provider id and returns the
// provider's base URL from the PROVIDERS map. A `?provider=` override is the
// test seam (point rodney at a local stub), but it is honored *only* when it
// resolves to a local host — so a crafted production link
// (`?provider=https://attacker.example`) can never redirect a real learner's key
// off the intended provider. This *enforces* the disclosure ("sent only to
// {provider}") in code rather than merely asserting it: a non-local override is
// ignored, and the key still reaches only the chosen provider's base URL. The
// `?provider=` parameter is parsed as a URL (new URL(override)) — not compared
// against provider names — so the gate cannot be bypassed by supplying a provider
// id in the query string (§1.5). Falls back to PROVIDERS.anthropic.baseUrl for an
// unknown provider id (defense in depth).
function providerBaseUrl(providerId) {
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
  // Hard default based on selected provider, falling back to Anthropic.
  const provider = PROVIDERS[providerId];
  return provider ? provider.baseUrl : PROVIDERS.anthropic.baseUrl;
}

// --- model discovery (the picker source seam) ------------------------------------

// Pure: extract the model-id list from a `/v1/models` (or `/models`) response.
// Both Anthropic and Fireworks return `{data:[{id}]}` — the shared shape is
// extracted once, provider-agnostic (§1.1, §2.2). Tolerates a missing or empty
// `data` array (→ []) and ignores non-string ids; the caller applies the
// fallback roster, so this never has to invent a default.
function parseModels(json) {
  const data = (json && json.data) || [];
  return data
    .map((entry) => entry && entry.id)
    .filter((id) => typeof id === "string");
}

// Pure: the roster the picker renders — the live list when it has any models, else a
// roster of just the provider-specific fallback. Keeps "never a zero-option dead
// select" as one named policy rather than a guard scattered across the picker.
function modelRoster(models, provider) {
  const fallback = provider === "fireworks" ? FIREWORKS_MODEL : MODEL;
  return models.length ? models : [fallback];
}

// Effectful: fetch the models this key can reach, through the SAME host-gated base
// URL as messages — a non-local `?provider=` override is ignored here too, so the
// picker opens no new key-exfil vector. Returns [] on any failure (non-OK, network,
// malformed JSON); the pure `modelRoster` turns [] into a usable roster, so a models
// outage is never a dead end. Auth headers and models path switch on `provider`:
//   - Anthropic: `x-api-key` + `anthropic-version` + `anthropic-dangerous-direct-
//     browser-access`, path `${baseUrl}/v1/models` (base has no /v1).
//   - Fireworks: `Authorization: Bearer`, path `${baseUrl}/models` (base already
//     carries /v1 — concatenating /v1/models would double the path → 404).
async function listModels({ baseUrl, apiKey, provider }) {
  try {
    const headers = provider === "fireworks"
      ? { "Authorization": "Bearer " + apiKey }
      : {
          "x-api-key": apiKey,
          "anthropic-version": "2023-06-01",
          "anthropic-dangerous-direct-browser-access": "true",
        };
    const modelsPath = provider === "fireworks" ? "/models" : "/v1/models";
    const response = await fetch(`${baseUrl}${modelsPath}`, { headers });
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
// structured `input`), so `JSON.parse` is mandatory. Filters tool calls by
// name (mirrors `toVerdict`'s `b.name === TOOL_NAME` pattern) — defense in
// depth: `tool_choice` forces the tool, but if the model ever returns a
// parallel/foreign tool call, the blind `[0]` would mis-parse it as a verdict.
function fireworksToVerdict(data) {
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
// The code comes from `window.__bt.getSubmission()` (the CM6 editor doc, or the
// fallback textarea under graceful degradation) — never `.value` on the
// submission element, which is now a <div> hosting the editor (§3.4).
function currentSubmission() {
  const bt = window.__bt;
  const lesson = bt && bt.lessons ? bt.lessons[bt.current] : undefined;
  const outputEl = document.getElementById("output");
  return {
    task: lesson ? lesson.prompt : "",
    code: bt && bt.getSubmission ? bt.getSubmission() : "",
    output: outputEl ? outputEl.textContent : "",
    checks: lesson && lesson.checks ? lesson.checks : [],
  };
}

// Prompt the learner for a provider + key, with per-provider disclosure: stored
// only in this tab, sent only to the selected provider. The provider chooser
// renders a `<select data-byok="provider">` with Fireworks pre-selected as the
// default. The key is stored in the selected provider's sessionStorage slot.
// Rendered here (not baked into index.html) so the whole prompt-for-key path
// lives in the impl, isolated from the page shell (§3.3, §4.1).
function renderKeyPrompt(container) {
  const form = document.createElement("form");
  form.dataset.byok = "key-prompt";

  const disclosure = document.createElement("p");
  disclosure.id = "byok-disclosure";

  const provLabel = document.createElement("label");
  provLabel.textContent = "Provider: ";
  const provSelect = document.createElement("select");
  provSelect.dataset.byok = "provider";
  for (const [id, provider] of Object.entries(PROVIDERS)) {
    const option = document.createElement("option");
    option.value = id;
    option.textContent = provider.label;
    if (id === DEFAULT_PROVIDER) {
      option.selected = true;
    }
    provSelect.append(option);
  }
  provLabel.append(provSelect);

  const keyLabel = document.createElement("label");
  keyLabel.textContent = "API key: ";
  const keyInput = document.createElement("input");
  keyInput.type = "password";
  keyInput.name = "provider-key";
  keyInput.autocomplete = "off";
  keyLabel.append(keyInput);

  function updateDisclosure() {
    const prov = PROVIDERS[provSelect.value];
    disclosure.textContent = PROVIDER_DISCLOSURES[provSelect.value];
    keyInput.placeholder = "Paste your " + prov.label + " API key";
    keyInput.value = "";  // never let a key typed for one provider be saved to another slot
  }
  provSelect.addEventListener("change", updateDisclosure);
  updateDisclosure();

  const save = document.createElement("button");
  save.type = "submit";
  save.textContent = "Save key & get feedback";

  form.append(disclosure, provLabel, keyLabel, save);
  form.addEventListener("submit", (event) => {
    event.preventDefault();
    const key = keyInput.value.trim();
    if (!key) {
      return;
    }
    const providerId = provSelect.value;
    storeProvider(providerId);
    storeKey(key, providerId);
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

// --- rate limiting (sessionStorage counter) -----------------------------------

// The sessionStorage key for the per-session feedback request counter. A
// distinct key (not a provider key slot) so the counter is provider-agnostic:
// feedback requests count against the limit regardless of which provider the
// learner chose.
const FEEDBACK_COUNT_KEY = "bt_feedback_count";

// Read the current feedback request count, defaulting to 0 when the key is
// absent or holds a non-integer value. The parseInt + || 0 guard ensures a
// corrupt or missing value never yields NaN (which would make every >=
// comparison false, silently disabling limiting — the negative case from the
// spec: "no parseInt guard (NaN disables limiting)").
function feedbackCount() {
  return parseInt(sessionStorage.getItem(FEEDBACK_COUNT_KEY)) || 0;
}

// Whether the learner has reached the per-session feedback request limit.
// Reads window.__btConfig.maxFeedbackPerSession (emitted by config.js); a max
// of 0 disables feedback entirely (the limit is reached before any request).
// The comparison is pure: count >= max. JS is single-threaded so the
// check+increment is atomic — no race condition between concurrent submits.
function rateLimitReached() {
  const max =
    (window.__btConfig && window.__btConfig.maxFeedbackPerSession) || 0;
  return feedbackCount() >= max;
}

// Increment the per-session feedback request counter. Called AFTER the
// try/catch in handleSubmit — both successful and failed requests count, so
// a provider outage still bounds cost (the user decision: failed requests
// count). Model discovery (listModels) does NOT call this — discovery is not
// feedback.
function incrementFeedbackCount() {
  sessionStorage.setItem(FEEDBACK_COUNT_KEY, String(feedbackCount() + 1));
}

// Render the limit-reached message. textContent only — the message is a fixed
// string (not model output), but textContent is the consistent XSS defense
// (mirrors renderVerdict precedent: untrusted text is never parsed as HTML).
function renderLimitReached(container) {
  const note = document.createElement("p");
  note.dataset.byok = "limit-reached";
  note.textContent =
    "Feedback request limit reached for this session. Reload the page to reset.";
  container.replaceChildren(note);
}

// Render the model picker into the feedback container: a labeled `<select>`
// populated from the learner's live model roster, defaulting to the named fallback
// when present. The select carries a distinct `data-byok="model"` marker and lives
// under `#feedback`, so the `#feedback select` test predicate pins it without prefix-
// colliding with the page-level `#lesson-select` (§1.5). Effectful only in that it
// awaits `listModels`; the roster and the default choice are pure. The `source`
// object carries a `provider` field that flows into `listModels` (auth + path) and
// `modelRoster` (fallback model).
async function renderModelPicker(container, source) {
  // A loading note while the live list is in flight, so a slow models query doesn't
  // read as a dead click; the roster replaces it the moment it resolves.
  const loading = document.createElement("p");
  loading.dataset.byok = "models-loading";
  loading.textContent = "Loading models…";
  container.replaceChildren(loading);

  const { baseUrl, apiKey, provider } = source;
  const roster = modelRoster(await listModels({ baseUrl, apiKey, provider }), provider);

  const picker = document.createElement("div");
  picker.dataset.byok = "model-picker";

  const fallbackModel = provider === "fireworks" ? FIREWORKS_MODEL : MODEL;
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
  select.value = roster.includes(fallbackModel) ? fallbackModel : roster[0];
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
// (no module state), falling back to the provider-specific fallback if the select
// is somehow absent so the request always carries a model the provider can serve.
function selectedModel(container, providerId) {
  const select = container.querySelector('[data-byok="model"]');
  return select ? select.value : PROVIDERS[providerId].fallbackModel;
}

// Orchestrate one submit, in four named states:
//   no key           → prompt for provider + key (per-provider disclosure);
//   key + no picker  → render the model picker from the live roster, then stop so
//                      the learner can choose (the first submit surfaces the picker);
//   picker shown     → build the prompt and send feedback through the *chosen*
//                      provider's backend, using the *chosen* model.
// The provider is read from sessionStorage (set at key-prompt time), defaulting to
// Fireworks. The key is read from the selected provider's slot. The backend is
// constructed from PROVIDERS[id].factory to enforce the dead-chooser guard.
async function handleSubmit() {
  const container = feedbackContainer();
  if (!container) {
    return;
  }
  const providerId = readProvider();
  const apiKey = readKey(providerId);
  if (!apiKey) {
    renderKeyPrompt(container);
    return;
  }
  const baseUrl = providerBaseUrl(providerId);
  if (!modelPickerPresent(container)) {
    // No try/catch twin of the /v1/messages branch below, by design: renderModelPicker
    // cannot throw. listModels swallows every fetch/parse failure into the fallback
    // roster (modelRoster guarantees a non-empty list), and the rest is pure DOM
    // construction — there is no reachable error to surface, so the loading note is
    // always replaced. If a future provider adds throwing logic *outside* listModels,
    // add the renderError guard here then.
    //
    // Rate limiting does NOT apply here: model discovery (listModels) is not a
    // feedback request, so it does not count against the per-session limit. The
    // counter is incremented only after the actual feedback try/catch below.
    await renderModelPicker(container, { baseUrl, apiKey, provider: providerId });
    return;
  }
  // The rate-limit guard sits at the top of the feedback phase — after model
  // discovery (which doesn't count) and before the actual feedback request.
  // A max of 0 (from config.js) disables feedback entirely: the limit is
  // reached before any request is sent.
  if (rateLimitReached()) {
    renderLimitReached(container);
    return;
  }
  const model = selectedModel(container, providerId);
  const submission = currentSubmission();
  const prompt = buildPrompt(submission);
  const backend = PROVIDERS[providerId].factory({ baseUrl, apiKey });
  renderPending(container);
  try {
    renderVerdict(container, await backend.getFeedback(prompt, model));
  } catch (error) {
    renderError(container, error);
  }
  // Increment AFTER the try/catch — both successful and failed requests count
  // (the user decision: a provider outage still bounds cost). Placing this
  // inside the try would skip it on error; placing it inside the catch would
  // only count failures. Here, both paths reach it.
  incrementFeedbackCount();
}

const submitButton = document.querySelector("[data-action=submit]");
if (submitButton) {
  submitButton.addEventListener("click", handleSubmit);
}
