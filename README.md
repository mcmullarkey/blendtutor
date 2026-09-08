# blendtutor

A single-binary CLI for building interactive coding lessons — in R or Python —
with AI-powered feedback. Instructors author exercises and grading prompts;
learners practice in the terminal or in a browser site with instant,
personalized feedback.

## Install

macOS and Linux, one command (verifies the tarball's SHA256 checksum; installs
to `~/.local/bin`, override with `BLENDTUTOR_INSTALL_DIR`):

```bash
curl -LsSf https://raw.githubusercontent.com/mcmullarkey/blendtutor/main/scripts/install.sh | sh
```

No release assets are published yet — until the next release, install from a clone:

```bash
git clone https://github.com/mcmullarkey/blendtutor.git
cd blendtutor
cargo install --path crates/cli
```

## API key

`run` and `eval` call an LLM provider; authoring commands need no key:

```bash
export FIREWORKS_API_KEY=fw_...   # or ANTHROPIC_API_KEY (CLI only; browser BYOK is Fireworks-only)
```

## The whole game

The instructor loop is **`init → new → validate → run → eval → eval-report →
build`** — walked end to end in [The whole game](https://mcmullarkey.github.io/blendtutor/whole-game.html):

```bash
blendtutor init my-course                         # scaffold course + starter lesson + eval suite
blendtutor new lesson --lang r greet              # add lessons/greet.yaml + eval_<name>.yaml sibling
blendtutor validate lessons/greet.yaml            # check a lesson (nonzero exit drops into CI)
blendtutor run lessons/greet.yaml --code sub.R    # execute the submission, get an LLM verdict
blendtutor eval lessons/greet.yaml                # score grading-prompt accuracy on the eval cases
blendtutor eval-report lessons/greet.yaml         # LLM-judged report → docs/evals/<lesson>/
blendtutor build my-course --target webr -o site  # static browser site (--target webr|pyodide)
```

## Deploy to GitHub Pages

The built `site/` is fully static — deploy to **GitHub Pages** as-is. webR
needs cross-origin isolation (`COOP`/`COEP` headers Pages cannot set), so the
build ships a vendored [`coi-serviceworker`](https://github.com/gzuidhof/coi-serviceworker)
shim that re-serves the page with the required headers (Pyodide-only sites
don't need it). Two example courses are deployed alongside the docs:

- **[R example site (webR)](https://mcmullarkey.github.io/blendtutor/examples/r/)**
- **[Python example site (Pyodide)](https://mcmullarkey.github.io/blendtutor/examples/python/)**

## Quarto extension

blendtutor also ships as a [Quarto](https://quarto.org) extension for
interactive coding exercises in `.qmd` documents — in-browser editor, instant
checks, solution reveal, AI hints, all static HTML. Requires **Quarto >= 1.4**:

```bash
quarto add mcmullarkey/blendtutor
```

Installs to `_extensions/mcmullarkey/blendtutor/` (version 0.1.0). Asset
resolution is install-path-independent — assets deploy alongside the rendered
HTML, so the extension works regardless of where `quarto add` installs it.

#### Quick start (zero hand-written bootstrap)

A complete copy-paste document — zero hand-written bootstrap. Filter by name,
`.blendtutor` div, render:

````markdown
---
title: "My exercises"
filters: [mcmullarkey/blendtutor]
---

::: {.blendtutor language="r"}
Write a function `add(a, b)` that returns the sum.

```r
add <- function(a, b) { ___ }
```
:::
````

Render, open in a browser — interactive immediately. Grade submissions with a
`{.r .checks}` block (`stopifnot(add(1, 2) == 3)`); Python: same div, `language="python"`.

#### Auto-bootstrap opt-out

The filter auto-bootstraps by default; to wire up the runtime yourself, set
`bt-auto-bootstrap: false` in the YAML header. To keep it but disable the
auto-mounted AI feedback, set `bt-feedback: false` — see
[BYOK](#byok-bring-your-own-key).

### Cross-origin isolation (COI)

webR requires `SharedArrayBuffer` → cross-origin isolation (COOP/COEP). Opt in
with `coi: true` (page YAML header) or `coi="true"` (any div); the filter
injects the same service-worker shim. Pyodide-only pages do not need COI.

> **Book-mode limitation:** COI does not function in Quarto `type: book`
> projects — the shim re-serves the page's own scope, which cannot cover the
> book's `_output/` directory. Use a standalone document for COI-enabled
> exercises (mechanics: [ADR-0015](docs/adr/0015-opt-in-coi-cross-origin.md)).

### Demo book

A complete demo book with R and Python exercises lives in
[`demo-book/`](demo-book/), rendered live at
<https://mcmullarkey.github.io/blendtutor/demo-book/> (rebuild locally with
`cd demo-book && quarto render`). It is a Quarto `type: book` project, so
COI does not take effect in the book render (limitation above).
Python exercises are fully interactive (Pyodide needs no COI) and every page ships
a static fallback. R exercises do not run in book mode — editors mount but
execution is unavailable. For runnable R, use the CLI-built example sites
([Live example sites](#deploy-to-github-pages)) —
R exercises run interactively via webR there, under the shim's isolation.
Serve the rendered book over HTTP — `file://` blocks the ES-module bootstrap
(CORS), so editors never mount and you see static exercise content only:

```bash
cd demo-book/_output && python3 -m http.server 8000
```

## BYOK (Bring Your Own Key)

Browser feedback uses the learner's own API key — no server-side key. Feedback
is **auto-mounted**: the injected bootstrap imports `exercise-feedback.js` and
calls `mountAllFeedback(registry)` after the runtime starts. The key is entered
once on the API key page (the demo book ships one) and shared via `localStorage`
— readable by any JavaScript on the page's origin, so never reuse a critical
key; it is sent only to `api.fireworks.ai`. BYOK is Fireworks-only (pinned model
`accounts/fireworks/models/deepseek-v4-flash-0731`); the CLI supports other
providers (see [API key](#api-key)). Serve over HTTP — `file://` breaks
`localStorage` sharing and blocks ES modules, so feedback never mounts.
Self-hosted CSP: add `connect-src https://api.fireworks.ai` (Pages cannot set
CSP headers; the shim covers only COOP/COEP).

## License

MIT — see [`LICENSE`](LICENSE).
