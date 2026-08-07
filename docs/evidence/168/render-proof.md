# Issue #168 — AC-7 render-time evidence (P5)

`quarto render demo-book --to html` (quarto 1.10.18) exits 0:

- Render log: `docs/evidence/168/render.log` — `Output created: _output/index.html`, exit 0.
- `_output/api-key.html` exists (all 4 chapters rendered: index, api-key, r-exercises, python-exercises).

P5 assertions verified against rendered output:

```
$ grep -F 'class="blendtutor-key"' demo-book/_output/api-key.html
<div class="blendtutor-key">

$ grep -cF '__btConfig' demo-book/_output/api-key.html
1
$ grep -cF '__btConfig' demo-book/_output/r-exercises.html
1
$ grep -cF '__btConfig' demo-book/_output/python-exercises.html
1
```

`__btConfig` emission confirmed: `demo-book/_output/api-key.html` contains
`window.__btConfig = window.__btConfig || {};` (the AC-3 C19-C22 head script
emitted by the vendored blendtutor.lua; keyPageUrl defaults to `api-key.html`).

Nav order proof (book.chapters — P2): rendered book sidebar lists
index → API Key → R Exercises → Python Exercises
(registration order in `demo-book/_quarto.yml`, which drives nav order).

Also note: `__btConfig` on r-exercises/python-exercises pages carries the
`bt-key-page: api-key.html` YAML value (P3) — the filter reads
`doc.meta["bt-key-page"]` and emits it as `window.__btConfig.keyPageUrl`.

Full suite: `docs/evidence/168/test-suite.log` — 89 passed, 0 failed
(includes the 9 new AC-7 arms: 7 structural + 2 render).
