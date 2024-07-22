return {
	style = "night",
	transparent = false,

	on_highlights = function(highlights, c)
		highlights.Boolean = { bold = true, fg = c.orange }
		highlights.Special = { bold = true, fg = c.magenta }

		highlights["@lsp.type.macro"] = { bold = true, fg = c.purple }
		highlights["@function.builtin"] = { bold = true, fg = c.purple }
		highlights["@variable.builtin"] = { italic = true, fg = c.red }
		highlights["@module"] = { italic = true, fg = c.blue2 }
		highlights["@lsp.typemod.namespace"] = { italic = true, fg = c.blue2 }
	end,
}
