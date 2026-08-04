#!/usr/bin/env bash
# rodney-chrome.sh — strip Chrome flags that break cross-origin isolation, then
# exec a real Chrome build. Installed into the rodney probe path via
# ROD_CHROME_BIN (see rodney-probes/pages-live.js) so rodney launches Chrome
# THROUGH this wrapper and receives rodney's full argument list.
#
# WHY (see issue #153 research note):
#   rodney 0.4.0 hardcodes `--single-process` (rodney main.go:356) and go-rod's
#   default launcher additionally passes `--disable-site-isolation-trials` +
#   `--disable-features=site-per-process`. These flags permanently break
#   crossOriginIsolated: a single-process Chromium with site isolation disabled
#   cannot opt into cross-origin isolation, so AC-3's P2
#   (crossOriginIsolated === true) can NEVER pass with the stock rodney-managed
#   Chromium. rodney offers no flag override — the only escape hatches are
#   ROD_CHROME_BIN (binary swap: this wrapper receives the full arg list) or
#   the rodney connect API. This wrapper strips the isolation-breaking flags
#   (and the entire --disable-features=... argument, any value) and execs a
#   real Chrome build, keeping every other flag (--no-sandbox, --disable-gpu,
#   --user-data-dir, URL, ...) intact.
#
# Real Chrome resolution order:
#   1. $REAL_CHROME env (explicit override — use if the standard paths differ)
#   2. macOS: /Applications/Google Chrome.app/Contents/MacOS/Google Chrome
#   3. Linux: /usr/bin/google-chrome
#   4. Fallback: the rodney-managed binary with a .orig sibling — when this
#      wrapper is installed IN PLACE of the rodney-managed Chromium (the
#      out-of-repo pattern that previously unblocked the probe), the original
#      real binary lives next to it as Chromium.orig / chrome.orig
#   Otherwise: error out with a clear message (no silent fallback to the
#   isolation-breaking binary — that would re-fail P2).
set -euo pipefail

resolve_chrome() {
  if [[ -n "${REAL_CHROME:-}" && -x "${REAL_CHROME}" ]]; then
    printf '%s\n' "${REAL_CHROME}"
    return 0
  fi

  local mac_chrome="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
  if [[ -x "${mac_chrome}" ]]; then
    printf '%s\n' "${mac_chrome}"
    return 0
  fi

  local linux_chrome="/usr/bin/google-chrome"
  if [[ -x "${linux_chrome}" ]]; then
    printf '%s\n' "${linux_chrome}"
    return 0
  fi

  # rodney-managed binary with .orig sibling (wrapper installed in place).
  local dir
  for dir in "${HOME}"/.cache/rod/browser/chromium-*/Chromium.app/Contents/MacOS \
             "${HOME}"/.cache/rod/browser/chromium-*/chrome-linux; do
    if [[ -d "${dir}" ]]; then
      if [[ -x "${dir}/Chromium.orig" ]]; then
        printf '%s\n' "${dir}/Chromium.orig"
        return 0
      fi
      if [[ -x "${dir}/chrome.orig" ]]; then
        printf '%s\n' "${dir}/chrome.orig"
        return 0
      fi
    fi
  done

  return 1
}

CHROME="$(resolve_chrome)" || {
  echo "rodney-chrome.sh: no real Chrome found. Set REAL_CHROME to a Chrome/Chromium binary path (e.g. REAL_CHROME=/path/to/chrome). Checked: \$REAL_CHROME, /Applications/Google Chrome.app/Contents/MacOS/Google Chrome, /usr/bin/google-chrome, ~/.cache/rod/browser/chromium-*/*.orig" >&2
  exit 1
}

# Filter argv: drop the isolation-breaking flags, keep everything else.
CLEAN_ARGS=()
for arg in "$@"; do
  case "${arg}" in
    --single-process) continue ;;
    --disable-site-isolation-trials) continue ;;
    --disable-features=*) continue ;;
    *) CLEAN_ARGS+=("${arg}") ;;
  esac
done

exec "${CHROME}" "${CLEAN_ARGS[@]}"
