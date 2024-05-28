return {
	style = "night",
	transparent = false,

	on_highlights = function(highlights, colors)
		highlights.Boolean = { fg = colors.orange, style = { bold = true } }
		highlights.Special = vim.tbl_deep_extend("force", highlights.Special or {}, { style = { bold = true } })
		highlights.Constant = vim.tbl_deep_extend("force", highlights.Constant or {}, { style = { bold = true } })

		highlights["@function.builtin"] =
			vim.tbl_deep_extend("force", highlights["@function.builtin"] or {}, { style = { bold = true } })
		highlights["@variable.builtin"] =
			vim.tbl_deep_extend("force", highlights["@variable.builtin"] or {}, { style = { italic = true } })
	end,
}
