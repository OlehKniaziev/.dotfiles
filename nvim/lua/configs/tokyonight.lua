return {
	style = "night",
	transparent = false,

	styles = {
		comments = { italic = true },
		keywords = { italic = false },
	},

	on_highlights = function(highlights, c)
		highlights.Boolean = { bold = true, fg = c.red }
		highlights.Special = { bold = true, fg = c.magenta }

		highlights["@keyword.modifier"] = { fg = c.magenta }
		highlights["@lsp.type.macro"] = { bold = true, fg = c.purple }
		highlights["@function.builtin"] = { bold = true, fg = c.orange }
		highlights["@variable.builtin"] = { italic = true, fg = c.red }
		highlights["@module"] = { italic = true, fg = c.magenta }
		highlights["@lsp.typemod.namespace"] = { italic = true, fg = c.blue2 }

		for _, value in pairs(highlights) do
			if value.undercurl then
				value.undercurl = false
				value.underline = true
			end
		end
	end,
}
