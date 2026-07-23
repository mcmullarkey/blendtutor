--- blendtutor.lua ---
-- WHAT:  No-op Pandoc filter scaffold for the blendtutor Quarto extension.
-- WHERE: _extensions/blendtutor/blendtutor.lua (loaded via _extension.yml contributes.filters)
-- NOT:   No AST transformation, no HTML dependencies, no runtime JS injection.
--         This is the skeleton — subsequent ACs add fenced-div processing.
--
-- Quarto auto-discovers this filter via the _extensions/ directory. The .qmd
-- frontmatter must NOT declare `filters: [blendtutor]` — that bypasses the
-- extension mechanism. The extension manifest (_extension.yml) is the single
-- source of truth for filter registration.

--- No-op filter: returns the document unmodified.
-- @param doc Pandoc document object
-- @return doc unmodified
function Pandoc(doc)
  return doc
end
