return {
	style = "modus_operandi",
	styles = {
		keywords = { italic = true },
	},

	---@param highlights Highlights
	---@param colors ColorScheme
	on_highlights = function(highlights, colors)
		highlights.Constant = vim.tbl_deep_extend("force", highlights.Constant, { style = { bold = true } })
		highlights.Conditional = vim.tbl_deep_extend("force", highlights.Conditional, { style = { italic = true } })
		highlights.Special = vim.tbl_deep_extend("force", highlights.Special, { style = { bold = true } })

		highlights["@function.builtin"] =
			vim.tbl_deep_extend("force", highlights["@function.builtin"] or {}, { style = { bold = true } })

		highlights["@function.builtin.c"] = { link = "@function.builtin" }
		highlights["@variable.builtin.c"] = { link = "@function.builtin" }

		highlights["@function.builtin.javascript"] = { link = "@function.builtin" }
		highlights["@variable.builtin.javascript"] = { link = "@function.builtin" }

		highlights["@keyword.function"] = { link = "Keyword" }
		highlights["@keyword.type"] = { link = "Keyword" }
		highlights["@keyword.import"] = { link = "Keyword" }
		highlights["@keyword.operator"] = { style = { bold = true } }
	end,
}
