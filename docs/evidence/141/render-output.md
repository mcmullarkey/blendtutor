# AC-4 E2E evidence: libs-dir deployment + rewritten bootstrap specifiers

## Deployed libs dir (mixed-lang)
```
codemirror.js
exercise-runtime.js
pyodide-adapter.js
styles.css
webr-adapter.js
```

## Deployed libs dir (r-only — pyodide-adapter.js must be ABSENT)
```
codemirror.js
exercise-runtime.js
styles.css
webr-adapter.js
```

## styles.css link in rendered HTML (exactly one, libs href)
```
1
<link href="mixed-lang_files/libs/quarto-contrib/blendtutor-0.1.0/styles.css" rel="stylesheet">
```

## Auto-bootstrap import specifiers (ES-module-safe ./ prefix)
```
from "./mixed-lang_files/libs/quarto-contrib/blendtutor-0.1.0/exercise-runtime.js"
from "./mixed-lang_files/libs/quarto-contrib/blendtutor-0.1.0/webr-adapter.js"
from "./mixed-lang_files/libs/quarto-contrib/blendtutor-0.1.0/pyodide-adapter.js"
```

## No classic runtime script tag
```
0
0 (no classic runtime tag)
```
