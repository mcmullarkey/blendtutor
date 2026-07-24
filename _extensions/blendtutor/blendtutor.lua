--- blendtutor.lua ---
-- WHAT:  Pandoc filter that parses ::: {.blendtutor} divs into widget HTML
--        with embedded 9-key SiteLesson JSON (ADR-0008 contract).
-- WHERE: _extensions/blendtutor/blendtutor.lua (loaded via _extension.yml contributes.filters)
-- NOT:   No code execution. The filter emits AST only; runtime JS (pyodide,
--        coi-serviceworker) and CSS (styles.css) are injected as <head>
--        tags via quarto.doc.include_text, loaded by the browser.
--        This filter owns the div→widget AST transform only (§4.1).
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

-- has_blendtutor flag — set in Div() when a valid blendtutor exercise is found.
-- Read in Pandoc() to conditionally inject styles.css (needed for all exercises).
-- Reset in Pandoc() so each document gets a fresh check.
local has_blendtutor = false

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

-- Path to vendored coi-serviceworker.js (synced via sync-quarto-assets.sh, mode=copy).
-- The service worker re-serves pages with COOP/COEP headers for SharedArrayBuffer.
local COI_SCRIPT_PATH = "_extensions/blendtutor/assets/coi-serviceworker.js"

-- Path to vendored styles.css (synced via sync-quarto-assets.sh, mode=scope).
-- Provides .bt-exercise styling: button states, cursor not-allowed, data-status.
local STYLES_CSS_PATH = "_extensions/blendtutor/assets/styles.css"

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
-- Widget emitter
-- ---------------------------------------------------------------------------

--- Emit the widget HTML as a Pandoc RawBlock.
-- @param payload The JSON string
-- @return A pandoc.RawBlock("html", ...) element
local function emit_widget(payload)
  local html = '<div class="bt-exercise">\n'
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

  -- Non-blendtutor divs: pass through (COI flag already set above if present).
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
  return emit_widget(payload)
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

  -- Inject styles.css if any blendtutor exercises are present (AC-8).
  -- has_blendtutor is set in Div() which runs before Pandoc().
  -- styles.css provides button states, cursor not-allowed, data-status styling.
  if has_blendtutor and is_html_format() then
    local css_link = '<link rel="stylesheet" href="' .. STYLES_CSS_PATH .. '">'
    -- Try Quarto API first (injects in <head>), fall back to RawBlock.
    if quarto and quarto.doc and quarto.doc.include_text then
      quarto.doc.include_text("in-header", css_link)
    else
      -- Pandoc fallback: prepend to document body.
      table.insert(doc.blocks, 1, pandoc.RawBlock("html", css_link))
    end
  end

  -- Reset flags for next document (per-page isolation, §3).
  has_python = false
  hasDoneSetup = false
  has_coi = false
  hasCoiDone = false
  has_blendtutor = false

  return doc
end
