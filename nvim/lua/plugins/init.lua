return {
	{
		"nvim-treesitter/nvim-treesitter",
		event = { "BufReadPost", "BufNewFile" },
		opts = function()
			return require("configs.tree-sitter")
		end,
		config = function(_, opts)
			require("nvim-treesitter.configs").setup(opts)
		end,
	},
	{
		"neovim/nvim-lspconfig",
		event = { "BufRead", "BufNewFile" },
		dependencies = {
			{ "j-hui/fidget.nvim", config = true },
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
			"L3MON4D3/LuaSnip",
		},

		opts = function()
			return require("configs.cmp")
		end,
		config = function(_, opts)
			require("cmp").setup(opts)
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
	{ "lewis6991/gitsigns.nvim", lazy = false, config = true },
	"Hoffs/omnisharp-extended-lsp.nvim",
	"rebelot/kanagawa.nvim",
	"folke/tokyonight.nvim",
	"sainnhe/sonokai",
	"nvimdev/zephyr-nvim",
	"ray-x/starry.nvim",
	"oxfist/night-owl.nvim",
	"AlexvZyl/nordic.nvim",
	"savq/melange-nvim",
	"EdenEast/nightfox.nvim",
	"uloco/bluloco.nvim",
	{
		"sainnhe/everforest",
		config = function(opts)
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
