--- blendtutor.lua ---
-- WHAT:  Pandoc filter that parses ::: {.blendtutor} divs into widget HTML
--        with embedded 9-key SiteLesson JSON (ADR-0008 contract), and
--        injects the auto-bootstrap module script that boots the exercise
--        runtime with per-language adapters (AC-3).
--        ISSUE #164 (byok-api-key AC-3) additionally:
--          * recognizes ::: {.blendtutor-key} divs as the key-page mount
--            point (pass-through HTML, has_key flag drives emission only);
--          * deploys exercise-feedback.js + key-page.js alongside the runtime
--            assets via add_html_dependency (single resources table);
--          * auto-mounts per-exercise feedback + the key page in the injected
--            bootstrap (mountAllFeedback AFTER start() resolves, mountKeyPage
--            unconditional — null is a no-op);
--          * emits window.__btConfig.keyPageUrl via a SEPARATE classic head
--            script on every has_blendtutor-or-has_key page, regardless of
--            bt-auto-bootstrap / bt-feedback opt-outs (AC-4's no-key link);
--          * reads bt-feedback: false (bool OR "false") as the granular
--            feedback opt-out (keeps start() + mountKeyPage, drops only
--            mountAllFeedback), distinct from bt-auto-bootstrap: false (total
--            opt-out — no bootstrap at all).
-- WHERE: _extensions/blendtutor/blendtutor.lua (loaded via _extension.yml contributes.filters)
-- NOT:   No code execution. The filter emits AST only; runtime JS
--        (exercise-runtime + adapters + codemirror + exercise-feedback +
--        key-page) and CSS (styles.css) deploy to the render output libs dir
--        via quarto.doc.add_html_dependency (AC-4), loaded by the browser
--        from <stem>_files/libs/quarto-contrib/blendtutor-<version>/
--        (standalone) or ./site_libs/quarto-contrib/blendtutor-<version>/
--        (book renders, AC-5); pyodide CDN and coi-serviceworker.js stay
--        include_text (external URL / SW scope).
--        This filter owns the div→widget AST transform, dual-asset
--        deployment, bootstrap + head-script injection, and opt-out
--        semantics only (§4.1) — NOT key validation logic (key-page.js).
--
-- This filter is loaded via explicit path in .qmd YAML:
--   filters: [_extensions/blendtutor/blendtutor.lua]
-- Quarto resolves the path relative to the project root and loads the Lua
-- filter directly, bypassing extension discovery entirely. This works in both
-- standalone and project modes. For distribution via `quarto add`, the
-- _extension.yml contributes.filters mechanism is used instead.
--
-- SiteLesson JSON contract (9 keys, ADR-0008):
--   id, title, prompt, code_template, checks, packages, solution, hints, gotchas
-- llm_evaluation_prompt is NEVER emitted (server/CLI concern, §3.2 leak).

-- Module-level exercise counter for auto-generated IDs (bt-exercise-<index>).
-- Reset in Pandoc() so each document starts at 0.
local exercise_count = 0

-- hasDoneSetup guard — prevents double CDN injection of pyodide.js (AC-6).
-- Set to true after the first Python exercise triggers CDN injection.
-- Reset in Pandoc() so each document gets a fresh check.
local hasDoneSetup = false

-- has_python flag — set in Div() when a language="python" exercise is found.
-- Read in Pandoc() (which runs AFTER Div()) to conditionally inject CDN.
local has_python = false

-- has_r flag — set in Div() when a language="r" exercise is found (AC-3).
-- Mirrors has_python. Read in Pandoc() to conditionally import the webR
-- adapter into the auto-bootstrap module script (adapter map keys are a
-- closed set {r, python}, §1).
local has_r = false

-- has_blendtutor flag — set in Div() when a valid blendtutor exercise is found.
-- Read in Pandoc() to conditionally inject styles.css (needed for all exercises).
-- Reset in Pandoc() so each document gets a fresh check.
local has_blendtutor = false

-- has_key flag — set in Div() when a ::: {.blendtutor-key} div is found
-- (issue #164, byok-api-key AC-3). The div itself is pure HTML pass-through
-- (pandoc renders <div class="blendtutor-key"> natively); has_key ONLY drives
-- filter emission: deploy key-page.js + exercise-feedback.js, inject the
-- bootstrap, and emit the __btConfig.keyPageUrl head script. Set BEFORE the
-- non-blendtutor early-return in Div() — a key-only page has zero blendtutor
-- divs, so the flag must be readable from a page with no exercises at all.
-- Reset in Pandoc() so each document gets a fresh check (C13).
local has_key = false

-- bt_feedback_optout — set when YAML metadata bt-feedback: false (bool OR
-- string "false") is present (issue #164, AC-3 C15). GRANULAR opt-out: the
-- bootstrap is still injected, start() still runs, mountKeyPage is still
-- called — only the mountAllFeedback import + call are suppressed. Distinct
-- from bt_auto_bootstrap_optout, which suppresses the bootstrap ENTIRELY.
-- Reset in Pandoc() so each document gets a fresh check.
local bt_feedback_optout = false

-- Pinned CDN URL for pyodide.js (classic script, not ES module).
-- loadPyodide is a global function, not an ES module export (§3.4).
local PYODIDE_CDN = "https://cdn.jsdelivr.net/pyodide/v0.27.2/full/pyodide.js"

-- COI flags (AC-9, ADR-0015) — opt-in cross-origin isolation.
-- has_coi: set in Div() when coi="true" is found on ANY div, or in Pandoc()
--          when YAML metadata coi: true is present.
-- hasCoiDone: dedup guard — prevents duplicate coi-serviceworker.js injection
--             when multiple coi="true" divs exist on the same page.
-- Both reset in Pandoc() so each document gets a fresh check (per-page isolation).
local has_coi = false
local hasCoiDone = false

-- Auto-bootstrap flags (AC-3, issue #139) — filter-injected module script.
-- hasBootstrapDone: dedup guard — prevents duplicate bootstrap injection.
--                   Mirrors hasCoiDone (one activation path per page, §5).
-- bt_auto_bootstrap_optout: set when YAML metadata bt-auto-bootstrap: false
--                   is present. Suppresses injection ENTIRELY (page-level
--                   opt-out mirrors the coi YAML read); pages that hand-write
--                   their own bootstrap (webr.qmd, feedback.qmd, ux.qmd)
--                   opt out rather than relying on the runtime's
--                   double-start guard.
-- Both reset in Pandoc() so each document gets a fresh check.
local hasBootstrapDone = false
local bt_auto_bootstrap_optout = false

-- Asset paths are derived from the filter's own location (ADR-0018), never
-- hardcoded. `quarto add mcmullarkey/blendtutor` installs to
-- _extensions/mcmullarkey/blendtutor/ (org/repo path), so a hardcoded
-- _extensions/blendtutor/ prefix 404s in installed projects.

--- Resolve an asset path relative to the filter script's own location.
-- PANDOC_SCRIPT_FILE arrives in one of two forms (verified quarto 1.10.18):
--   * explicit YAML filter path — as written, relative to the qmd directory
--     (e.g. "../_extensions/blendtutor/blendtutor.lua" from quarto-fixture/);
--   * by-name _extension.yml install — an ABSOLUTE path into the project
--     (e.g. "/abs/proj/_extensions/mcmullarkey/blendtutor/blendtutor.lua").
-- Quarto does not rewrite in-header hrefs, so an absolute script path MUST be
-- converted back to a project-relative URL or the browser 404s (the emitted
-- href would be a filesystem path, not a URL).
-- @param script_file PANDOC_SCRIPT_FILE value (absolute or relative)
-- @param filename    asset basename, e.g. "styles.css"
-- @return project-relative URL (forward slashes) to the asset
local function resolve_asset_path(script_file, filename)
  local script = script_file or "blendtutor.lua"
  -- Directory extraction handles both / and \ separators (Windows, §5).
  -- pandoc.path is POSIX-only and mis-parses backslash paths, so do it here.
  local dir = script:match("^(.*)[/\\]") or "."
  local asset = dir .. "/assets/" .. filename

  -- Absolute path (by-name install): convert to project-relative. Prefer
  -- slicing from "_extensions/" — always present for a filter install and
  -- robust when the script path uses a different root than PWD (macOS
  -- /private/tmp vs /tmp symlinks). Strip the PWD prefix as a last resort.
  if asset:match("^/") or asset:match("^[A-Za-z]:[/\\]") then
    local ext = asset:find("_extensions[/\\]")
    if ext then
      asset = asset:sub(ext)
    else
      local cwd = os.getenv("PWD") or ""
      if cwd ~= "" and asset:find(cwd, 1, true) == 1 then
        asset = asset:sub(#cwd + 1):gsub("^[/\\]", "")
      end
    end
  end

  -- Normalize Windows backslashes to forward slashes (href-safe).
  return asset:gsub("\\", "/")
end

-- Path to vendored coi-serviceworker.js (synced via sync-quarto-assets.sh,
-- mode=copy). The service worker re-serves pages with COOP/COEP headers for
-- SharedArrayBuffer. Derived from the filter's own location (ADR-0018) and
-- injected via include_text — the service-worker scope is the script URL's
-- directory, so it must stay in the source tree and NEVER deploy to a libs
-- dir (AC-4 clause 7 boundary).
local COI_SCRIPT_PATH = resolve_asset_path(PANDOC_SCRIPT_FILE, "coi-serviceworker.js")

-- ---------------------------------------------------------------------------
-- Asset deployment (AC-4, issue #141)
-- ---------------------------------------------------------------------------

-- Single source of truth for the extension version (AC-4 clause 10). Used in
-- BOTH the add_html_dependency declaration AND the emitted libs URL string;
-- must equal _extension.yml:3 version.
local BT_DEP_VERSION = "0.1.0"

--- Compute the document-relative libs URL for a deployed asset (AC-4, AC-5).
-- Quarto deploys add_html_dependency resources + stylesheets to
-- <stem>_files/libs/quarto-contrib/<name>-<version>/ for STANDALONE/default
-- renders, where <stem> is the output file's basename minus extension
-- (verified quarto.js:128068-76). The rendered HTML's <link>/import
-- specifiers are document-relative: the document lives beside its own
-- <stem>_files/ dir, so the URL uses only the basename stem — never the
-- output path's directory prefix (a nested pages/ render yields
-- index_files/..., not pages/index_files/...).
-- BOOK projects (type: book, output-dir) behave DIFFERENTLY (issue #143):
-- Quarto consolidates ALL html-dependency resources into the SHARED
-- _output/site_libs/ dir (bookProjectType.libDir = "site_libs", verified
-- quarto 1.10.18); per-page <stem>_files/ dirs are never created, so the
-- standalone form 404s in book renders (demo-book boot-broken since AC-4).
-- Discriminator (empirically verified quarto 1.10.18):
--   quarto.project.output_directory ~= quarto.project.directory
-- Books with output-dir differ (output_directory = <proj>/_output); ALL
-- standalone renders — with OR without output-dir — are equal (output_directory
-- stays the project dir even when output-dir is set). The discriminator
-- matches Quarto's ACTUAL deployment: books → site_libs, standalone → _files.
-- The result MUST start with "./": ES module import specifiers reject bare
-- relative references ("Failed to resolve module specifier ... Relative
-- references must start with /, ./, or ../") — <link href> accepts them, ES
-- modules do not. quarto.doc.output_file is an absolute path at Pandoc() time
-- (probe-verified quarto 1.10.18) — strip to the basename before the stem.
-- @param filename asset basename, e.g. "exercise-runtime.js"
-- @return document-relative ES-module-safe libs URL,
--   standalone: "./index_files/libs/quarto-contrib/blendtutor-0.1.0/exercise-runtime.js"
--   book:       "./site_libs/quarto-contrib/blendtutor-0.1.0/exercise-runtime.js"
local function libs_url(filename)
  local output_file = quarto and quarto.doc and quarto.doc.output_file or ""
  local basename = output_file:match("^.*[/\\]([^/\\]+)$") or output_file
  local stem = basename:gsub("%.html$", "")
  if quarto and quarto.project and quarto.project.output_directory
    and quarto.project.directory
    and quarto.project.output_directory ~= quarto.project.directory then
    -- Book render: shared site_libs dir, document-relative (document lives in
    -- the same output dir as site_libs/).
    return "./site_libs/quarto-contrib/blendtutor-" .. BT_DEP_VERSION .. "/" .. filename
  end
  return "./" .. stem .. "_files/libs/quarto-contrib/blendtutor-" .. BT_DEP_VERSION .. "/" .. filename
end

--- Register the Quarto HTML dependency that deploys blendtutor assets.
-- Quarto copies `resources` verbatim into the libs dir and rewrites
-- `stylesheets` into <link> tags; `scripts` would emit classic <script src>
-- tags (ES module SyntaxError) — deliberately absent. Resources mirror the
-- AC-3 conditional imports: exercise-runtime.js + codemirror.js always
-- (exercise-runtime.js:29-42 statically imports ./codemirror.js, so both must
-- ship in the SAME libs dir), webr-adapter.js iff has_r, pyodide-adapter.js
-- iff has_python. ISSUE #164 (AC-3 C1): exercise-feedback.js + key-page.js
-- ALSO always — the bootstrap statically imports mountAllFeedback from the
-- former and mountKeyPage from the latter on EVERY has_blendtutor-or-has_key
-- page, so both must ship whenever the dependency is created (a key-only page
-- has no adapters but still imports both). Effectful copy is owned by Quarto
-- core — the filter only declares (§2).
local function build_html_dependency()
  local resources = {
    "assets/exercise-runtime.js",
    "assets/codemirror.js",
    "assets/exercise-feedback.js",
    "assets/key-page.js",
  }
  if has_r then
    resources[#resources + 1] = "assets/webr-adapter.js"
  end
  if has_python then
    resources[#resources + 1] = "assets/pyodide-adapter.js"
  end
  quarto.doc.add_html_dependency({
    name = "blendtutor",
    version = BT_DEP_VERSION,
    stylesheets = { "assets/styles.css" },
    resources = resources,
  })
end

-- ---------------------------------------------------------------------------
-- JSON encoding helpers (Lua has no built-in JSON encoder)
-- ---------------------------------------------------------------------------

--- Escape a string for safe inclusion in a JSON string value.
-- @param s The raw string
-- @return The JSON-escaped string (without surrounding quotes)
local function json_escape(s)
  s = s:gsub("\\", "\\\\")
  s = s:gsub('"', '\\"')
  s = s:gsub("\n", "\\n")
  s = s:gsub("\r", "\\r")
  s = s:gsub("\t", "\\t")
  -- Escape remaining C0 control chars (U+0000-U+001F) per JSON spec.
  -- Placed after explicit \n\r\t escapes so those short forms are preserved;
  -- the gsub only matches control chars not yet replaced by steps above.
  s = s:gsub("[%c]", function(c)
    return string.format("\\u%04x", string.byte(c))
  end)
  -- Prevent </script> breakout: the HTML parser closes <script> at the first
  -- </script> sequence regardless of the type attribute (type only controls
  -- execution, not parsing). Escaping < to \u003c prevents the sequence from
  -- appearing in the JSON payload, keeping the script tag intact.
  s = s:gsub("<", "\\u003c")
  return s
end

--- Encode a string as a JSON string value (with surrounding quotes).
-- @param s The raw string
-- @return A JSON string literal
local function json_string(s)
  return '"' .. json_escape(s) .. '"'
end

--- Encode a Lua table (sequence) as a JSON array of strings.
-- @param arr A sequence (array) of strings
-- @return A JSON array literal
local function json_array(arr)
  local parts = {}
  for _, v in ipairs(arr) do
    parts[#parts + 1] = json_string(v)
  end
  return "[" .. table.concat(parts, ",") .. "]"
end

--- Encode a value as a JSON string or null.
-- @param v A string or nil
-- @return A JSON string literal or "null"
local function json_value(v)
  if v == nil then
    return "null"
  end
  return json_string(v)
end

-- ---------------------------------------------------------------------------
-- Rendering helpers (pandoc.write for AST→format conversion)
-- ---------------------------------------------------------------------------

--- Render a list of Pandoc blocks to HTML.
-- @param blocks A List of Pandoc Block elements
-- @return An HTML string (empty string if blocks is empty)
local function render_html(blocks)
  if #blocks == 0 then
    return ""
  end
  local doc = pandoc.Pandoc(blocks, pandoc.Meta{})
  return pandoc.write(doc, "html")
end

--- Render a list of Pandoc blocks to markdown (for hints/gotchas raw text).
-- @param blocks A List of Pandoc Block elements
-- @return A markdown string, or nil if blocks is empty
local function render_markdown(blocks)
  if #blocks == 0 then
    return nil
  end
  local doc = pandoc.Pandoc(blocks, pandoc.Meta{})
  local md = pandoc.write(doc, "markdown")
  -- Strip trailing whitespace/newlines added by pandoc.write
  md = md:gsub("%s+$", "")
  if md == "" then
    return nil
  end
  return md
end

-- ---------------------------------------------------------------------------
-- Validation helpers
-- ---------------------------------------------------------------------------

--- Check if the current output format is HTML.
-- @return true if FORMAT is "html", false otherwise
local function is_html_format()
  return FORMAT == "html"
end

--- Validate that a language is in the supported set {r, python}.
-- @param lang The language string from the div attribute
-- @return true if supported, false otherwise
local function validate_language(lang)
  return lang == "r" or lang == "python"
end

--- Parse a comma-separated packages attribute into an array.
-- @param attr_value The raw packages attribute (e.g. "numpy,pandas") or nil
-- @return An array of package name strings (empty if absent)
local function parse_packages(attr_value)
  if attr_value == nil or attr_value == "" then
    return {}
  end
  local packages = {}
  for pkg in attr_value:gmatch("[^,]+") do
    -- Trim leading/trailing whitespace
    pkg = pkg:gsub("^%s+", ""):gsub("%s+$", "")
    if pkg ~= "" then
      packages[#packages + 1] = pkg
    end
  end
  return packages
end

-- ---------------------------------------------------------------------------
-- Inner block parsing
-- ---------------------------------------------------------------------------

--- Parse the inner blocks of a blendtutor div into SiteLesson fields.
--
-- Parsing rules:
--   - Prose (Para/Plain) before the first CodeBlock → prompt (rendered to HTML)
--   - First CodeBlock without .checks/.solution class → code_template
--   - CodeBlocks with .checks class → checks array
--   - CodeBlock with .solution class → solution
--   - Nested Div with .hints class → hints (rendered to markdown)
--   - Nested Div with .gotchas class → gotchas (rendered to markdown)
--
-- @param blocks A List of Pandoc Block elements (the div's content)
-- @return A table with prompt, code_template, checks, solution, hints, gotchas
local function parse_inner_blocks(blocks)
  local prompt_blocks = {}
  local code_template = nil
  local checks = {}
  local solution = nil
  local hints = nil
  local gotchas = nil
  local found_code = false

  for _, block in ipairs(blocks) do
    if block.t == "CodeBlock" then
      found_code = true
      if block.classes:includes("checks") then
        checks[#checks + 1] = block.text
      elseif block.classes:includes("solution") then
        solution = block.text
      else
        -- First code block without .checks/.solution = code_template
        if code_template == nil then
          code_template = block.text
        end
      end
    elseif block.t == "Div" then
      if block.classes:includes("hints") then
        hints = render_markdown(block.content)
      end
      if block.classes:includes("gotchas") then
        gotchas = render_markdown(block.content)
      end
    elseif not found_code and (block.t == "Para" or block.t == "Plain") then
      prompt_blocks[#prompt_blocks + 1] = block
    end
  end

  return {
    prompt = render_html(prompt_blocks),
    code_template = code_template,
    checks = checks,
    solution = solution,
    hints = hints,
    gotchas = gotchas,
  }
end

-- ---------------------------------------------------------------------------
-- Payload builder
-- ---------------------------------------------------------------------------

--- Build the 9-key SiteLesson JSON payload.
-- @param index The exercise index (0-based)
-- @param parsed The parsed inner blocks table
-- @param packages The packages array
-- @return A JSON string
local function build_payload(index, parsed, packages)
  local id = "bt-exercise-" .. index
  local title = "Exercise " .. (index + 1)

  local parts = {
    '"id":' .. json_string(id),
    '"title":' .. json_string(title),
    '"prompt":' .. json_string(parsed.prompt),
    '"code_template":' .. json_value(parsed.code_template),
    '"checks":' .. json_array(parsed.checks),
    '"packages":' .. json_array(packages),
    '"solution":' .. json_value(parsed.solution),
    '"hints":' .. json_value(parsed.hints),
    '"gotchas":' .. json_value(parsed.gotchas),
  }

  return "{" .. table.concat(parts, ",") .. "}"
end

-- ---------------------------------------------------------------------------
-- Static fallback block (fix-demo-visible-exercises, Part 1)
-- ---------------------------------------------------------------------------

--- Escape a string for safe inclusion in HTML text content.
-- Used for the STATIC fallback block ONLY (code template, hints, gotchas).
-- The prompt is already HTML rendered by pandoc (trusted, server-side) and
-- is inserted raw. Escaping `<`/`>` prevents author-supplied code like
-- "</script>" from closing surrounding elements in the static markup.
-- @param s The raw string (may be nil)
-- @return The HTML-escaped string (empty string if nil)
local function html_escape(s)
  if s == nil then
    return ""
  end
  s = s:gsub("&", "&amp;")
  s = s:gsub("<", "&lt;")
  s = s:gsub(">", "&gt;")
  s = s:gsub('"', "&quot;")
  return s
end

--- Build the static fallback HTML block for an exercise div.
-- Emitted BEFORE the payload script so the exercise is VISIBLE even when JS
-- never runs (file:// CORS-blocks ES modules; JS disabled; CDN slow). The
-- runtime (exercise-runtime.js) removes this block when it mounts the
-- interactive editor — progressive enhancement: static content
-- server-rendered, JS upgrades it. The block shares the payload-building
-- source of truth: it renders from the SAME `parsed` table + auto-generated
-- title that build_payload() encodes (§4.1 — no duplicate parsing).
-- @param title The exercise title ("Exercise <N>")
-- @param parsed The parsed inner blocks table (prompt/code_template/hints/gotchas)
-- @return An HTML string (empty string only if nothing to show)
local function build_static_block(title, parsed)
  local parts = {}
  parts[#parts + 1] = '<div class="bt-exercise-static">'
  parts[#parts + 1] = '<h3 class="bt-static-title">' .. html_escape(title) .. "</h3>"
  if parsed.prompt ~= nil and parsed.prompt ~= "" then
    parts[#parts + 1] = '<div class="bt-static-prompt">' .. parsed.prompt .. "</div>"
  end
  if parsed.code_template ~= nil then
    parts[#parts + 1] = '<pre class="bt-static-code"><code>'
      .. html_escape(parsed.code_template) .. "</code></pre>"
  end
  if parsed.hints ~= nil then
    parts[#parts + 1] = '<details class="bt-static-hints"><summary>Hints</summary>'
      .. html_escape(parsed.hints) .. "</details>"
  end
  if parsed.gotchas ~= nil then
    parts[#parts + 1] = '<details class="bt-static-gotchas"><summary>Gotchas</summary>'
      .. html_escape(parsed.gotchas) .. "</details>"
  end
  parts[#parts + 1] = "</div>"
  return table.concat(parts, "\n")
end

-- ---------------------------------------------------------------------------
-- Widget emitter
-- ---------------------------------------------------------------------------

--- Emit the widget HTML as a Pandoc RawBlock.
-- @param payload The JSON string
-- @param lang The validated exercise language ("r" or "python")
-- @param title The exercise title ("Exercise <N>")
-- @param parsed The parsed inner blocks table (single source of truth for the
--   static fallback content — same table build_payload() encodes)
-- @return A pandoc.RawBlock("html", ...) element
local function emit_widget(payload, lang, title, parsed)
  local html = '<div class="bt-exercise" data-language="' .. lang .. '">\n'
    .. build_static_block(title, parsed)
    .. '<script type="application/json">' .. payload .. "</script>\n"
    .. "</div>"
  return pandoc.RawBlock("html", html)
end

-- ---------------------------------------------------------------------------
-- Main Div filter
-- ---------------------------------------------------------------------------

--- Process a Div element. If it has the "blendtutor" class, parse it into a
-- widget. Otherwise, pass through unchanged.
-- @param div A Pandoc Div element
-- @return A RawBlock with widget HTML, or nil (pass-through), or the original div (skip)
function Div(div)
  -- Check for COI activation (AC-9, ADR-0015) — on ANY div, not just blendtutor.
  -- COI is a page-level concern, separate from exercise runtime (§3).
  -- Only the exact string "true" activates; "false", "yes", "" are rejected (§1).
  if div.attributes["coi"] == "true" then
    has_coi = true
  end

  -- Check for the key-page mount div (issue #164, AC-3 C12/C13) — on ANY div.
  -- A ::: {.blendtutor-key} fenced div is a pure HTML pass-through marker:
  -- pandoc renders it as <div class="blendtutor-key"> unchanged, and has_key
  -- only drives asset/bootstrap/config emission. MUST be set BEFORE the
  -- non-blendtutor early-return below — a key-only page has zero blendtutor
  -- divs and would otherwise never set the flag (C14 dead key page).
  if div.classes:includes("blendtutor-key") then
    has_key = true
  end

  -- Non-blendtutor divs: pass through (COI/key flags already set above if present).
  if not div.classes:includes("blendtutor") then
    return nil
  end

  -- Non-HTML formats: warn + return div unchanged (no widget emission)
  if not is_html_format() then
    io.stderr:write("[blendtutor] WARNING: bt-exercise widgets only emitted for HTML output; "
      .. "skipping for format: " .. tostring(FORMAT) .. "\n")
    return div
  end

  -- Validate language attribute
  local lang = div.attributes["language"]
  if lang == nil or lang == "" then
    io.stderr:write("[blendtutor] WARNING: blendtutor div missing language attribute; skipping.\n")
    return div
  end

  if not validate_language(lang) then
    io.stderr:write('[blendtutor] WARNING: unsupported language "' .. lang
      .. '"; supported: r, python. Skipping.\n')
    return div
  end

  -- Track Python exercises for CDN injection (AC-6).
  -- Pandoc() runs AFTER Div() and reads this flag.
  if lang == "python" then
    has_python = true
  end

  -- Track R exercises for the auto-bootstrap adapter map (AC-3).
  -- Mirrors has_python (:373-375): the bootstrap only imports/registers an
  -- adapter for a language present on the page (render-time invariant, §1).
  if lang == "r" then
    has_r = true
  end

  -- Track blendtutor exercises for styles.css injection (AC-8).
  -- Pandoc() runs AFTER Div() and reads this flag.
  has_blendtutor = true

  -- Parse packages from div attribute (comma-separated)
  local packages = parse_packages(div.attributes["packages"])

  -- Parse inner blocks into SiteLesson fields
  local parsed = parse_inner_blocks(div.content)

  -- Build and emit widget
  local index = exercise_count
  exercise_count = exercise_count + 1

  local payload = build_payload(index, parsed, packages)
  -- lang validated to {r, python} above (:364-368) — attribute is the
  -- sole language carrier (payload has no language key, AC-1 §3).
  local title = "Exercise " .. (index + 1)
  -- build_payload() derives the same title internally (build_payload :403) —
  -- passing it through keeps the static block and JSON payload in lockstep
  -- (single source of truth, fix-demo-visible-exercises Part 1).
  return emit_widget(payload, lang, title, parsed)
end

-- ---------------------------------------------------------------------------
-- Auto-bootstrap module script builder (AC-3, issue #139)
-- ---------------------------------------------------------------------------

--- Build the filter-injected auto-bootstrap module script.
-- Imports the exercise runtime + per-language adapters + feedback + key-page
-- modules from the deployed libs URLs (AC-4 — add_html_dependency copies them
-- to <stem>_files/libs/quarto-contrib/blendtutor-<version>/), then calls
-- start(registry, map) with the adapter map keyed only for languages present
-- on the page. ISSUE #164 (AC-3):
--   * registry is HOISTED to one shared const — the SAME registry instance is
--     passed to start() and mountAllFeedback() (C9 — never re-built).
--   * mountAllFeedback(registry) sits inside .then( — AFTER start() resolves,
--     BEFORE .catch( (C7) — and has exactly one call site (C8).
--   * mountKeyPage(document.querySelector(".blendtutor-key")) is UNCONDITIONAL
--     (C10) — a missing div is a no-op in key-page.js (AC-2 P12), so no guard.
--   * bt_feedback_optout suppresses ONLY the mountAllFeedback import + call
--     (C16) — start() and mountKeyPage stay.
-- The webR adapter is a FACTORY (call it: createWebRAdapter()); the pyodide
-- adapter is a SINGLETON (use pyodideAdapter directly) — the emitted map
-- handles both shapes. Ends with a single .catch() error sink (§5).
-- @return The full <script type="module" data-bt-bootstrap="auto">…</script> string
local function build_bootstrap_script()
  local runtime_url = libs_url("exercise-runtime.js")
  local feedback_url = libs_url("exercise-feedback.js")
  local key_page_url = libs_url("key-page.js")
  local webr_url = libs_url("webr-adapter.js")
  local pyodide_url = libs_url("pyodide-adapter.js")
  local parts = {
    '<script type="module" data-bt-bootstrap="auto">',
    '  import { scanExercises, buildRegistry, start } from "' .. runtime_url .. '";',
  }
  if not bt_feedback_optout then
    parts[#parts + 1] = '  import { mountAllFeedback } from "' .. feedback_url .. '";'
  end
  parts[#parts + 1] = '  import { mountKeyPage } from "' .. key_page_url .. '";'
  if has_r then
    parts[#parts + 1] = '  import { createWebRAdapter } from "' .. webr_url .. '";'
  end
  if has_python then
    parts[#parts + 1] = '  import { pyodideAdapter } from "' .. pyodide_url .. '";'
  end

  parts[#parts + 1] = ''
  parts[#parts + 1] = '  const registry = buildRegistry(scanExercises());'
  parts[#parts + 1] = '  start(registry, {'
  if has_r then
    parts[#parts + 1] = '    r: createWebRAdapter(),'
  end
  if has_python then
    parts[#parts + 1] = '    python: pyodideAdapter,'
  end
  parts[#parts + 1] = '  }).then(() => {'
  if not bt_feedback_optout then
    parts[#parts + 1] = '    mountAllFeedback(registry);'
  end
  parts[#parts + 1] = '    mountKeyPage(document.querySelector(".blendtutor-key"));'
  parts[#parts + 1] = '  }).catch((err) => console.error("[blendtutor] auto-bootstrap failed", err));'
  parts[#parts + 1] = '</script>'

  return table.concat(parts, "\n")
end

--- Build the classic (non-module) __btConfig keyPageUrl head script.
-- ISSUE #164 (AC-3 C19-C22). Emitted via a SEPARATE include_text — NOT inside
-- the module bootstrap, which is unreachable on bt-auto-bootstrap:false /
-- bt-feedback:false opt-out pages (AC-4's no-key link still needs the URL).
-- MERGE pattern (C22): `window.__btConfig = window.__btConfig || {};` then a
-- property assignment — NEVER a bare `window.__btConfig = {...}`. config.js
-- (crates/core/src/site/mod.rs:321) sets maxFeedbackPerSession on the SAME
-- object; a clobber would silently break rate limiting at
-- exercise-feedback.js:376.
-- @param key_page_url The bt-key-page YAML value (string) or the default
-- @return A classic <script>…</script> string for the <head>
local function build_key_page_config_script(key_page_url)
  return '<script>window.__btConfig = window.__btConfig || {};'
    .. 'window.__btConfig.keyPageUrl = "' .. json_escape(key_page_url) .. '";</script>'
end

--- Normalize a pandoc meta value to a plain Lua string, or nil when the value
-- is not a string (boolean/number/nil/other).
-- Pandoc 3.x wraps YAML string scalars — including quoted "false" — in a
-- structured type, NOT a plain Lua string: doc.meta["k"] for `k: "false"`
-- arrives as a list of Inlines whose first element is Str "false" (empirically
-- verified quarto 1.10.18 / pandoc 3.1). Pandoc 2.x returned the bare string.
-- Both forms are normalized here so the bt-auto-bootstrap / bt-feedback
-- string-form opt-outs work across pandoc versions (§5 — one helper, no
-- duplicated type-switching at call sites).
-- @param val A pandoc meta value (boolean, string, or structured table)
-- @return The plain string, or nil
local function meta_string(val)
  if type(val) == "string" then
    return val
  end
  if type(val) == "table" then
    if type(val.text) == "string" then
      return val.text
    end
    if #val == 1 and val[1] and val[1].t == "Str" and type(val[1].text) == "string" then
      return val[1].text
    end
  end
  return nil
end

--- Reset counters and inject CDN script tag at document level.
-- Called AFTER all Div elements (pandoc calls Div first, then Pandoc).
-- If any Python exercise was found (has_python flag), injects the pyodide.js
-- CDN script tag via the hasDoneSetup guard (AC-6).
-- @param doc Pandoc document object
-- @return doc (possibly modified with CDN script tag prepended)
function Pandoc(doc)
  exercise_count = 0

  -- Check YAML metadata for coi: true (AC-9, ADR-0015).
  -- YAML boolean true activates COI at the document level (no div needed).
  -- Also accepts string "true" for robustness across YAML parsers.
  local yaml_coi = doc.meta["coi"]
  if yaml_coi == true then
    has_coi = true
  elseif type(yaml_coi) == "string" and yaml_coi == "true" then
    has_coi = true
  end

  -- Check YAML metadata for bt-auto-bootstrap: false (AC-3) — page-level
  -- opt-out that suppresses auto-bootstrap injection ENTIRELY. Mirrors the
  -- coi YAML read; also accepts string "false" for robustness (normalized via
  -- meta_string — pandoc 3 wraps quoted YAML strings in a structured type).
  -- Pages that hand-write their own bootstrap opt out here rather than relying
  -- on the runtime's double-start guard.
  local yaml_bootstrap = doc.meta["bt-auto-bootstrap"]
  if yaml_bootstrap == false or meta_string(yaml_bootstrap) == "false" then
    bt_auto_bootstrap_optout = true
  end

  -- Check YAML metadata for bt-feedback: false (issue #164, AC-3 C15) —
  -- GRANULAR opt-out: suppresses ONLY the mountAllFeedback import + call in
  -- the bootstrap. start(), mountKeyPage, asset deployment, and the
  -- __btConfig.keyPageUrl head script all stay. Accepts both boolean false
  -- and string "false" for YAML-parser robustness (C17 parity).
  local yaml_feedback = doc.meta["bt-feedback"]
  if yaml_feedback == false or meta_string(yaml_feedback) == "false" then
    bt_feedback_optout = true
  end

  -- Inject CDN script tag if Python exercises are present (AC-6).
  -- has_python is set in Div() which runs before Pandoc().
  if has_python and is_html_format() and not hasDoneSetup then
    hasDoneSetup = true
    local cdn_script = '<script src="' .. PYODIDE_CDN .. '"></script>'
    -- Try Quarto API first (injects in <head>), fall back to RawBlock.
    if quarto and quarto.doc and quarto.doc.include_text then
      quarto.doc.include_text("in-header", cdn_script)
    else
      -- Pandoc fallback: prepend to document body.
      table.insert(doc.blocks, 1, pandoc.RawBlock("html", cdn_script))
    end
  end

  -- Inject coi-serviceworker.js if COI is activated (AC-9, ADR-0015).
  -- has_coi is set in Div() (coi="true" attribute) or above (YAML coi: true).
  -- hasCoiDone guard ensures one activation path per page (§5 — no duplicates).
  if has_coi and is_html_format() and not hasCoiDone then
    hasCoiDone = true
    local coi_script = '<script src="' .. COI_SCRIPT_PATH .. '"></script>'
    -- Try Quarto API first (injects in <head>), fall back to RawBlock.
    if quarto and quarto.doc and quarto.doc.include_text then
      quarto.doc.include_text("in-header", coi_script)
    else
      -- Pandoc fallback: prepend to document body.
      table.insert(doc.blocks, 1, pandoc.RawBlock("html", coi_script))
    end
  end

  -- Deploy styles.css + runtime JS modules to the libs dir (AC-4).
  -- has_blendtutor is set in Div() which runs before Pandoc(); has_key too
  -- (issue #164, C14) — a key-only page still needs the deployed feedback +
  -- key-page modules that its bootstrap imports.
  -- Quarto copies resources + rewrites the stylesheet <link>; add_html_dependency
  -- requires quarto (no RawBlock fallback — deployment is a Quarto-core
  -- mechanism, not a filter concern).
  if (has_blendtutor or has_key) and is_html_format() then
    if quarto and quarto.doc and quarto.doc.add_html_dependency then
      build_html_dependency()
    else
      io.stderr:write("[blendtutor] WARNING: quarto.doc.add_html_dependency unavailable; "
        .. "assets not deployed to libs dir (quarto required)\n")
    end
  end

  -- Inject the auto-bootstrap module script if exercises OR a key-page div are
  -- present (AC-3 + issue #164 C14). has_blendtutor is set in Div() (any valid
  -- r/python exercise); has_key for the key-page mount div. has_r and
  -- has_python select which adapters to import. The hasBootstrapDone guard
  -- ensures exactly one bootstrap per page (mirror hasCoiDone, §5).
  -- YAML bt-auto-bootstrap: false opts out entirely (no injection branch).
  if (has_blendtutor or has_key) and is_html_format() and not bt_auto_bootstrap_optout and not hasBootstrapDone then
    hasBootstrapDone = true
    local bootstrap = build_bootstrap_script()
    -- Try Quarto API first (injects in <head>), fall back to RawBlock.
    if quarto and quarto.doc and quarto.doc.include_text then
      quarto.doc.include_text("in-header", bootstrap)
    else
      -- Pandoc fallback: prepend to document body.
      table.insert(doc.blocks, 1, pandoc.RawBlock("html", bootstrap))
    end
  end

  -- Emit window.__btConfig.keyPageUrl (issue #164, AC-3 C19-C22).
  -- SEPARATE include_text classic script (NOT the module bootstrap — opt-out
  -- pages never get the bootstrap but still need keyPageUrl for AC-4's no-key
  -- link). Present on EVERY has_blendtutor-or-has_key page REGARDLESS of
  -- bt-auto-bootstrap / bt-feedback opt-outs. Value from doc.meta["bt-key-page"]
  -- YAML (string form), default "api-key.html". Merge pattern (C22) — never a
  -- bare window.__btConfig = {...} clobber.
  if (has_blendtutor or has_key) and is_html_format() then
    -- bt-key-page YAML value normalized via meta_string (pandoc 3 wraps plain
    -- YAML strings in a structured type — the custom value would be silently
    -- ignored without it). Default "api-key.html".
    local key_page_meta = meta_string(doc.meta["bt-key-page"])
    local key_page_url = "api-key.html"
    if key_page_meta ~= nil and key_page_meta ~= "" then
      key_page_url = key_page_meta
    end
    local config_script = build_key_page_config_script(key_page_url)
    -- Try Quarto API first (injects in <head>), fall back to RawBlock.
    if quarto and quarto.doc and quarto.doc.include_text then
      quarto.doc.include_text("in-header", config_script)
    else
      -- Pandoc fallback: prepend to document body.
      table.insert(doc.blocks, 1, pandoc.RawBlock("html", config_script))
    end
  end

  -- Reset flags for next document (per-page isolation, §3).
  has_python = false
  hasDoneSetup = false
  has_coi = false
  hasCoiDone = false
  has_blendtutor = false
  has_key = false
  has_r = false
  hasBootstrapDone = false
  bt_auto_bootstrap_optout = false
  bt_feedback_optout = false

  return doc
end
