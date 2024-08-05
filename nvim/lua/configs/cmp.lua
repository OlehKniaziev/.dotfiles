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
			-- local icons = {
			-- 	Class = "τ",
			-- 	Color = "",
			-- 	Constant = "π",
			-- 	Constructor = "σ",
			-- 	Enum = "Δ",
			-- 	EnumMember = "Δ",
			-- 	Field = "Ξ",
			-- 	File = "",
			-- 	Folder = "",
			-- 	Function = "λ",
			-- 	Interface = "ω",
			-- 	Keyword = "κ",
			-- 	Method = "ƒ",
			-- 	Module = "󰏗",
			-- 	Property = "ψ",
			-- 	Snippet = "󰘍",
			-- 	Struct = "τ",
			-- 	Text = "",
			-- 	Unit = "",
			-- 	Value = "β",
			-- 	Variable = "α",
			-- }

			local source = source_map[entry.source.name]
			item.kind = string.format("[%s] %s", source or "", item.kind)

			return item
		end,
	},
	mapping = cmp.mapping.preset.insert({
		["<C-u>"] = cmp.mapping.scroll_docs(-4), -- Up
		["<C-d>"] = cmp.mapping.scroll_docs(4), -- Down
		["<C-Space>"] = cmp.mapping.complete(),
		["<C-y>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.confirm({
					select = true,
				})
			else
				fallback()
			end
		end),

		["<Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			else
				fallback()
			end
		end, { "i", "s" }),

		["<S-Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			else
				fallback()
			end
		end, { "i", "s" }),

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
