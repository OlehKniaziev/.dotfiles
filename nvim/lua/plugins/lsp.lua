return {
	{
		"neovim/nvim-lspconfig",
		event = { "BufRead", "BufNewFile" },
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
			{ "folke/neodev.nvim", opts = {} },
		},

		config = function()
			require("configs.lsp-config")
		end,
	},
	{
		"hrsh7th/nvim-cmp",
		event = "InsertEnter",
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-cmdline",
			"saadparwaiz1/cmp_luasnip",
			{
				"L3MON4D3/LuaSnip",
				opts = function()
					return require("configs.luasnip")
				end,
				config = function(_, opts)
					require("luasnip").setup(opts)
				end,
			},
		},

		opts = function()
			return require("configs.cmp")
		end,
		config = function(_, opts)
			require("cmp").setup(opts)
		end,
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
	{
		"rebelot/kanagawa.nvim",
		opts = function()
			return require("configs.kanagawa")
		end,
		config = function(_, opts)
			require("kanagawa").setup(opts)
		end,
	},
}
