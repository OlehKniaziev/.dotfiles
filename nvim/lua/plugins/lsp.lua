return {
	{
		"neovim/nvim-lspconfig",
		event = { "BufReadPre" },
		dependencies = {
			{
				"j-hui/fidget.nvim",
				opts = function()
					return require("configs.fidget")
				end,
				config = function(_, opts)
					require("fidget").setup(opts)
				end,
			},
			{ "williamboman/mason.nvim",           config = true },
			{ "williamboman/mason-lspconfig.nvim", config = true },
			{
				"folke/lazydev.nvim",
				ft = "lua",
				opts = function()
					return require("configs.lazydev")
				end,
				config = function(_, opts)
					require("lazydev").setup(opts)
				end,
			},
		},

		config = function()
			require("configs.lsp-config")
		end,
	},
	{
		"saghen/blink.cmp",
		dependencies = {
			{
				"L3MON4D3/LuaSnip",
				opts = function()
					return require("configs.luasnip")
				end,
			},
		},

		version = "1.*",

		opts = function()
			return require("configs.blink")
		end,

		opts_extend = { "sources.default" }
	},
	{
		"windwp/nvim-autopairs",
		-- event = "InsertEnter",
		config = true,
	},
	{
		"stevearc/conform.nvim",
		opts = function()
			return require("configs.conform")
		end,
		config = function(_, opts)
			require("conform").setup(opts)
		end,
	},
	"Hoffs/omnisharp-extended-lsp.nvim",
}
