return {
	color_overrides = {
		mocha = {
			base = "#000000",
		},
		macchiato = {
			base = "#000000",
		},
		frappe = {
			base = "#000000",
		},
	},

	flavour = "macchiato",
	no_italic = false,
	no_bold = false,
	no_underline = false,

	styles = {
		comments = { "italic" },
	},

    transparent_background = true,

	integrations = {
		cmp = true,
		treesitter = true,
		treesitter_context = true,
		fidget = true,
		gitsigns = true,
	},

	custom_highlights = function(colors)
		return {
			Boolean = {
				fg = colors.maroon,
				style = { "bold" },
			},
			Constant = {
				style = { "bold" },
			},
			Keyword = {
				style = { "italic" },
			},

			["@function.builtin"] = { style = { "bold" } },
			["@constant.builtin"] = { style = { "bold" } },
			["@keyword.operator"] = { fg = colors.maroon, style = { "bold" } },
		}
	end,
}
