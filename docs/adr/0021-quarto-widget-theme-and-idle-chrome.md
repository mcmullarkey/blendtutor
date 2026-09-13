# ADR-0021: Quarto widget follows the page theme; idle chrome stays hidden

- Status: Accepted
- Date: 2026-09-13

## Context

In a light Quarto book viewed on a machine set to dark mode, the exercise
widget renders as a dark panel: `styles.css` (synced from
`crates/core/assets/shared/styles.css`, ADR-0010) switches every
`--bt-color-*` token under `@media (prefers-color-scheme: dark)`, which is the
right signal for the CLI-built site (it owns the whole page) but the wrong one
inside a Quarto page, whose theme is chosen by the author and marked on
`<body>` as `quarto-light` or `quarto-dark`. The same page also showed an
"IDLE" badge and an empty output box for an exercise that had never run, lost
its prompt when the runtime removed the static fallback block, and offered an
unlabeled password input for the API key.

## Options

1. **Change the shared stylesheet** to key on a page class. Couples the CLI
   site to Quarto's class names and breaks its OS-driven dark mode.
2. **Transform the dark media block in `sync-quarto-assets.sh`.** Keeps one
   source, but hides a theme decision inside a CSS rewriter already carrying
   scoping rules, and the rewritten output is hard to read and review.
3. **Ship an extension-only `quarto-theme.css`** that re-declares the color
   tokens on `body.quarto-light` (light values) and `body.quarto-dark` (dark
   values). Custom properties declared on `body` override the `:root`
   declarations for everything inside it, whatever the OS prefers.

## Decision

Option 3. The filter's html dependency lists `assets/quarto-theme.css` after
`styles.css`. A test pins its token values to the shared stylesheet's light
`:root` and dark-media `:root` blocks, so the duplicated palette cannot drift.

In the same slice the runtime keeps the widget legible:

- the prompt element is moved out of the static fallback before it is removed;
- the status badge and output box are created `hidden` and revealed by the
  first run, so exercises without checks never show them;
- the key form labels its input ("Fireworks API key"), shows an `fw_`
  placeholder, and says the key is stored only in the browser.

## Consequences

- Light books stay light and dark books stay dark regardless of OS settings.
- CodeMirror's dark-only tweaks inside the media block (gutter border, active
  line tint) still follow the OS; they are subtle on either surface and are
  left for a follow-up if they read poorly.
- Probes that look up `.bt-status` keep working: the element exists from
  mount, only its rendering is deferred.
