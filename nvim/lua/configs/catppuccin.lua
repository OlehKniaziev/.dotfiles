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

	integrations = {
		cmp = true,
		treesitter = true,
		fidget = true,
		barbar = true,
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
			["@function.builtin"] = { style = { "bold" } },
			["@constant.builtin"] = { style = { "bold" } },
			["@keyword.operator"] = { fg = colors.red, style = { "bold" } },
		}
	end,
}
