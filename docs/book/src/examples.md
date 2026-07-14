# Example sites

Two example courses — derived from the "Write Less Code" lesson arc — are built
and deployed alongside this book on GitHub Pages. Each is a fully runnable
browser site: learners edit code in the browser, submit, and get instant AI
feedback with no install required.

## R example site (webR)

- **[R example site](./examples/r/index.html)** — five R lessons booting
  [webR](https://docs.r-wasm.org/webr/) in the browser.
- **[R eval results](./examples/r/eval-results.html)** — grading-prompt accuracy
  recorded by `blendtutor eval`.

## Python example site (Pyodide)

- **[Python example site](./examples/python/index.html)** — five Python lessons
  booting [Pyodide](https://pyodide.org/) in the browser.
- **[Python eval results](./examples/python/eval-results.html)** —
  grading-prompt accuracy recorded by `blendtutor eval`.

## How they are built

The Docs CI workflow (`.github/workflows/docs.yml`) runs `blendtutor build` for
each example course after assembling the mdBook + rustdoc artifact, nesting the
output under `/examples/{r,python}/` so a single Pages deploy serves the book,
the API reference, and both example sites. `scripts/check-docs.sh` mirrors this
locally and asserts the required files are present.
