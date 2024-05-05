return {
	{
		"nvim-treesitter/nvim-treesitter",
		event = { "BufReadPost", "BufNewFile" },
		dependencies = {
			"nvim-treesitter/nvim-treesitter-context",
		},
		opts = function()
			return require("configs.tree-sitter")
		end,
		config = function(_, opts)
			require("nvim-treesitter.configs").setup(opts)
		end,
	},
	{
		"williamboman/mason.nvim",
		dependencies = {
			{
				"williamboman/mason-lspconfig.nvim",
			},
		},
		lazy = false,
		opts = function()
			return require("configs.mason-lspconfig")
		end,
		config = function(_, opts)
			require("mason").setup()
			require("mason-lspconfig").setup(opts)
		end,
	},
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
			{
				"windwp/nvim-autopairs",
				event = "InsertEnter",
				config = true,
			},
		},

		opts = function()
			return require("configs.cmp")
		end,
		config = function(_, opts)
			local autopairs_cmp = require("nvim-autopairs.completion.cmp")
			local cmp = require("cmp")

			cmp.event:on("confirm_done", autopairs_cmp.on_confirm_done())
			cmp.setup(opts)
		end,
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
	{
		"nvim-lualine/lualine.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		opts = function()
			return require("configs.lualine")
		end,
		config = function(_, opts)
			require("lualine").setup(opts)
		end,
		lazy = false,
	},
	{ "nvim-telescope/telescope.nvim", dependencies = { "nvim-lua/plenary.nvim" } },
	{
		"lewis6991/gitsigns.nvim",
		lazy = false,
		opts = function()
			return require("configs.gitsigns")
		end,
		config = function(_, opts)
			require("gitsigns").setup(opts)
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
	{
		"Mofiqul/dracula.nvim",
		opts = function()
			return require("configs.dracula")
		end,
		config = function(_, opts)
			require("dracula").setup(opts)
		end,
	},
	"folke/tokyonight.nvim",
	"sainnhe/sonokai",
	"AlexvZyl/nordic.nvim",
	"savq/melange-nvim",
	{
		"sainnhe/everforest",
		config = function(_)
			require("configs.everforest")
		end,
	},
	{
		"ribru17/bamboo.nvim",
		config = function()
			require("bamboo").setup({})
		end,
	},
	{ "mcchrish/zenbones.nvim", dependencies = { "rktjmp/lush.nvim" } },
	{ "folke/neodev.nvim", opts = {} },
	{ "bluz71/vim-moonfly-colors", name = "moonfly" },
	{
		"catppuccin/nvim",
		name = "catppuccin",
		opts = function()
			return require("configs.catppuccin")
		end,
		config = function(_, opts)
			require("catppuccin").setup(opts)
		end,
	},
}
