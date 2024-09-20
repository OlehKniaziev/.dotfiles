return {
	compile = false,
	undercurl = true,
	theme = "wave",
	colors = {
		theme = {
			all = {
				ui = {
					bg_gutter = "none",
				},
			},
		},
	},

	overrides = function(c)
		local p = c.palette

		return {
			-- Special = { bold = true, fg = c.magenta },

			["@keyword.modifier"] = { fg = p.peachRed },
			["@lsp.type.modifier"] = { fg = p.peachRed },

			["@function.builtin"] = { bold = true, fg = p.surimiOrange },
			["@variable.builtin"] = { italic = true, fg = p.waveRed },
			["@constant.builtin"] = { bold = true, fg = p.springBlue },

			["@module"] = { italic = true, fg = p.sakuraPink },
			["@lsp.typemod.namespace"] = { italic = true, fg = p.sakuraPink },

			["@lsp.type.enumMember"] = { bold = true, fg = p.springBlue },

			["@lsp.type.interface"] = { fg = p.lightBlue },
			["@lsp.mod.trait"] = { fg = p.lightBlue },

			["@lsp.type.macro"] = { bold = true, fg = p.peachRed },

			["@lsp.type.decorator"] = { fg = p.peachRed },
		}
	end,
}
