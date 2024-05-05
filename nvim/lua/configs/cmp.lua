local luasnip = require("luasnip")
local cmp = require("cmp")

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
		format = function(_, item)
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

			if icons[item.kind] then
				item.kind = icons[item.kind] .. " " .. item.kind
			end

			return item
		end,
	},
	mapping = cmp.mapping.preset.insert({
		["<C-u>"] = cmp.mapping.scroll_docs(-4), -- Up
		["<C-d>"] = cmp.mapping.scroll_docs(4), -- Down
		["<C-Space>"] = cmp.mapping.complete(),
		["<CR>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				if luasnip.expandable() then
					luasnip.expand()
				else
					cmp.confirm({
						select = true,
					})
				end
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
			if luasnip.locally_jumpable(1) then
				luasnip.jump(1)
			else
				fallback()
			end
		end, { "i", "s" }),
	}),
	sources = {
		{ name = "nvim_lsp" },
		{ name = "luasnip" },
	},
}
