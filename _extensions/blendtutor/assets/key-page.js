// key-page.js — mountable key-management UI (AC-2 of byok-api-key).
//
// WHAT:  Key-management UI for the BYOK workflow — password input, Save,
//        Clear, and a status line. Saving stores the key through the shared
//        exercise-feedback.js contract, then validates it against the
//        host-gated provider models endpoint (GET {baseUrl}/models).
// WHERE: Mounted into the .blendtutor-key container by the blendtutor.lua
//        page; consumes the storage + provider contract from
//        exercise-feedback.js by import only (never re-declares slot names).
// NOT:   NOT feedback submission (exercise-feedback.js), NOT execution
//        (exercise-runtime.js), NOT the model picker (exercise-feedback.js),
//        NOT provider selection.
//
// Key hygiene: the key leaves the browser only as an Authorization header on
// the host-gated validation request. It is never logged, never echoed into the
// DOM, never written into the input after save, and never pre-filled on mount.

import { readKey, storeKey, clearKey, providerBaseUrl, PROVIDERS } from "./exercise-feedback.js";

// The provider this page manages. A provider id, deliberately NOT a slot name:
// slot names and base URLs live in exercise-feedback.js (imported, never
// duplicated across the module boundary).
const PROVIDER_ID = "fireworks";

// Friendly status copy. Static text only — the key value never appears.
const STATUS_MESSAGES = {
  saved: "Key saved and verified.",
  "invalid-key": "That key was rejected by the provider. Check it and try again.",
  network: "Key saved, but the provider could not be reached to verify it.",
  empty: "Enter a key to save it.",
  cleared: "Your saved API key has been removed.",
};

// --- pure core (no I/O — Node-testable without a browser) ---------------------

// Pure: the validation endpoint for a provider — the same host-gated base URL
// the feedback path uses (so ?provider= routes validation to a local stub),
// plus the models path.
export function buildValidationUrl(providerId) {
  return providerBaseUrl(providerId) + "/models";
}

// Pure: classify a validation outcome into a discriminated result:
//   {ok:true}                       — 2xx (key accepted);
//   {ok:false, reason:"invalid-key"} — 401/403 (key rejected);
//   {ok:false, reason:"network"}     — thrown fetch (provider unreachable) or
//                                      any other non-2xx status;
//   {ok:false, reason:"empty"}       — empty key, decided BEFORE any fetch
//                                      (callers must not fetch for empty keys).
export function classifyValidation(status, threw) {
  if (threw) {
    return { ok: false, reason: "network" };
  }
  if (status >= 200 && status < 300) {
    return { ok: true };
  }
  if (status === 401 || status === 403) {
    return { ok: false, reason: "invalid-key" };
  }
  return { ok: false, reason: "network" };
}

// Pure: friendly copy for a status reason. The copy never contains the key.
export function statusMessage(reason) {
  return STATUS_MESSAGES[reason] ?? "";
}

// --- effectful shell (DOM wiring, fetch, storage) ------------------------------

// Guard against double-mounting: mounting the same container twice must not
// duplicate the submit listener (one save must issue exactly one fetch).
const mountedTargets = new WeakSet();

function setStatus(statusEl, reason) {
  statusEl.textContent = statusMessage(reason);
}

// Key-set mount state: "key is set" + Clear. No input — the stored key is
// never pre-filled into the form. Clear removes the key (and the feedback
// counter) and returns the UI to the empty input form.
function renderKeySet(container) {
  const status = document.createElement("p");
  status.dataset.byok = "key-status";
  status.textContent = "Your " + PROVIDERS[PROVIDER_ID].label + " API key is set.";

  const clear = document.createElement("button");
  clear.type = "button";
  clear.dataset.byok = "clear";
  clear.textContent = "Clear saved key";
  clear.addEventListener("click", () => {
    clearKey(PROVIDER_ID);
    renderKeyForm(container, "cleared");
  });

  container.replaceChildren(status, clear);
}

// No-key mount state: password input + Save. `initialReason` seeds the status
// line (used after Clear to report the removal).
function renderKeyForm(container, initialReason) {
  const form = document.createElement("form");
  form.dataset.byok = "key-page-form";

  const status = document.createElement("p");
  status.dataset.byok = "key-status";

  const input = document.createElement("input");
  input.type = "password";
  input.name = "byok-key";
  input.autocomplete = "off";
  input.dataset.byok = "key-input";

  const save = document.createElement("button");
  save.type = "submit";
  save.dataset.byok = "save";
  save.textContent = "Save key";

  form.append(status, input, save);
  form.addEventListener("submit", (event) => {
    event.preventDefault();
    return handleSave(input, status);
  });

  if (initialReason) {
    setStatus(status, initialReason);
  }
  container.replaceChildren(form);
}

// Save flow: empty input is a no-op (stored key survives, no fetch); otherwise
// store optimistically via the imported contract, reset the input immediately
// (no password-manager capture, no echo), then validate (advisory status).
function handleSave(input, status) {
  const value = input.value.trim();
  if (!value) {
    setStatus(status, "empty");
    return;
  }
  storeKey(value, PROVIDER_ID);
  input.value = "";
  return validateAndReport(status, value);
}

// Validate the freshly stored key against GET {baseUrl}/models, through the
// same host-gated provider base URL as the feedback path. The key rides the
// Authorization header only. 401/403 and thrown fetches are discriminated by
// classifyValidation — never collapsed.
async function validateAndReport(status, key) {
  let response = null;
  let threw = false;
  try {
    response = await fetch(buildValidationUrl(PROVIDER_ID), {
      method: "GET",
      headers: { "Authorization": "Bearer " + key },
    });
  } catch (_error) {
    threw = true;
  }
  const outcome = threw
    ? classifyValidation(null, true)
    : classifyValidation(response.status, false);
  setStatus(status, outcome.ok ? "saved" : outcome.reason);
}

// Mount the key-management UI into `target`. No-op when the target is absent
// or already mounted (idempotent — one save issues exactly one fetch).
export function mountKeyPage(target) {
  if (!target || mountedTargets.has(target)) {
    return;
  }
  mountedTargets.add(target);
  if (readKey(PROVIDER_ID)) {
    renderKeySet(target);
  } else {
    renderKeyForm(target);
  }
}
