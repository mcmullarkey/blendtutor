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

Prebuilt binaries for Linux and macOS (x86_64 + aarch64) are on the
[releases page](https://github.com/mcmullarkey/blendtutor/releases); verify
against `sha256sums.txt` in the same release. To build from source instead:

```bash
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
blendtutor eval lessons/greet.yaml --write-report # persist eval-report.json at the course root for build
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

## Author a course with Claude Code

This repo ships a Claude Code skill,
[`blendtutor-course`](.claude/skills/blendtutor-course/SKILL.md). In a clone,
run `/blendtutor-course <chapter file or URL>` (or ask Claude to build a
course): it scaffolds lessons with checks and solutions, writes a minimal eval
suite, verifies the checks locally, and exports a Quarto snippet or builds a site.

## Quarto extension

Interactive exercises in `.qmd` documents (Quarto >= 1.4, extension version
0.2.0). Run this from the folder that contains `_quarto.yml`:

```bash
quarto add mcmullarkey/blendtutor
```

[The whole game](https://mcmullarkey.github.io/blendtutor/whole-game.html#quarto-extension) covers the rest:

- [Quick start](https://mcmullarkey.github.io/blendtutor/whole-game.html#quick-start-zero-hand-written-bootstrap)
- [Export a lesson](https://mcmullarkey.github.io/blendtutor/whole-game.html#export-a-lesson) with `blendtutor export-quarto`
- [Auto-bootstrap opt-out](https://mcmullarkey.github.io/blendtutor/whole-game.html#auto-bootstrap-opt-out)
- [Cross-origin isolation](https://mcmullarkey.github.io/blendtutor/whole-game.html#cross-origin-isolation-coi) and R in book projects
- [Demo book](https://mcmullarkey.github.io/blendtutor/whole-game.html#demo-book)
- [BYOK feedback](https://mcmullarkey.github.io/blendtutor/whole-game.html#byok-bring-your-own-key)

## License

MIT — see [`LICENSE`](LICENSE).
