local luasnip = require("luasnip")
local cmp = require("cmp")
-- local cmp_autopairs = require("nvim-autopairs.completion.cmp")
-- cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())

local source_map = {
	nvim_lsp = "LSP",
	nvim_lsp_signature_help = "LSP",
	buffer = "BUF",
	text = "TEXT",
	path = "PATH",
	luasnip = "SNIP",
	neorg = "NORG",
}

local icons = {
	Class = "τ",
	Color = "",
	Constant = "π",
	Constructor = "σ",
	Enum = "Δ",
	EnumMember = "Δ",
	Field = "Ξ",
	File = "",
	Folder = "",
	Function = "λ",
	Interface = "ω",
	Keyword = "κ",
	Method = "ƒ",
	Module = "󰏗",
	Property = "ψ",
	Snippet = "󰘍",
	Struct = "τ",
	Text = "",
	Unit = "",
	Value = "β",
	Variable = "α",
}

return {
	window = {
		completion = cmp.config.window.bordered(),
		documentation = cmp.config.window.bordered(),
	},
	snippet = {
		expand = function(args)
			luasnip.lsp_expand(args.body)
		end,
	},
	formatting = {
		fields = { "abbr", "menu", "kind" },
		format = function(entry, item)
			local source = source_map[entry.source.name]
			item.kind = string.format("[%s] %s %s", source or "", icons[item.kind], item.kind)

			return item
		end,
	},
	mapping = cmp.mapping.preset.insert({
		["<C-k>"] = cmp.mapping(function(fallback)
			if luasnip.locally_jumpable(-1) then
				luasnip.jump(-1)
			else
				fallback()
			end
		end, { "i", "s" }),

		["<C-j>"] = cmp.mapping(function(fallback)
			if luasnip.expand_or_jumpable() then
				luasnip.expand_or_jump()
			else
				fallback()
			end
		end, { "i", "s" }),
	}),
	sources = {
		{ name = "nvim_lsp" },
		{ name = "nvim_lsp_signature_help" },
		{ name = "nvim_lua" },
		{ name = "luasnip" },
		{ name = "path" },
		{ name = "buffer" },
		{ name = "neorg" },
	},
}
