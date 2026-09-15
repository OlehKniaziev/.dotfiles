---@module "blink.cmp"
---@type blink.cmp.Config
return {
	keymap = { preset = "default" },

	appearance = {
		nerd_font_variant = "mono"
	},

	completion = {
		documentation = {
			auto_show = true,
			auto_show_delay_ms = 0,
		}
	},

	signature = {
		enabled = true,
	},

	snippets = {
		preset = "luasnip",
	},

	sources = {
		default = { "lsp", "path", "snippets", "buffer" },
	},

	-- (Default) Rust fuzzy matcher for typo resistance and significantly better performance
	-- You may use a lua implementation instead by using `implementation = "lua"` or fallback to the lua implementation,
	-- when the Rust fuzzy matcher is not available, by using `implementation = "prefer_rust"`
	--
	-- See the fuzzy documentation for more information
	fuzzy = { implementation = "prefer_rust_with_warning" }
}
