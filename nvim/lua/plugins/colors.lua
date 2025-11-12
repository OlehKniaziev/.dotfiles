return {
	{
		"Mofiqul/dracula.nvim",
		opts = function()
			return require("configs.dracula")
		end,
		config = function(_, opts)
			require("dracula").setup(opts)
		end,
	},
	{
		"miikanissi/modus-themes.nvim",
		opts = function()
			return require("configs.modus")
		end,
		config = function(_, opts)
			require("modus-themes").setup(opts)
		end,
	},
	"nyoom-engineering/oxocarbon.nvim",
	"sainnhe/gruvbox-material",
	{
		"folke/tokyonight.nvim",
		opts = function()
			return require("configs.tokyonight")
		end,
		config = function(_, opts)
			require("tokyonight").setup(opts)
		end,
	},
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
		opts = function()
			return require("configs.bamboo")
		end,
		config = function(_, opts)
			require("bamboo").setup(opts)
		end,
	},
	{ "mcchrish/zenbones.nvim",    dependencies = { "rktjmp/lush.nvim" } },
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
	"RRethy/base16-nvim",
	{
		"navarasu/onedark.nvim",
		opts = function()
			return require("configs.onedark")
		end,
		config = function(_, opts)
			require("onedark").setup(opts)
		end,
	},
	"projekt0n/github-nvim-theme",
	{
		"rebelot/kanagawa.nvim",
		opts = function()
			return require("configs.kanagawa")
		end,
		config = function(_, opts)
			require("kanagawa").setup(opts)
		end,
	},
	"junegunn/seoul256.vim",
	"zootedb0t/citruszest.nvim",
	"maxmx03/solarized.nvim",
	"craftzdog/solarized-osaka.nvim",
    {
	"rose-pine/neovim",
	name = "rose-pine",
	config = function(_, _)
	    require("rose-pine").setup({
		dim_inactive_windows = true,
		highlight_groups = {
		    ["@function"] = { italic = false },
		    ["@variable"] = { italic = false },
		    ["@constant"] = { bold = true },
		},
	    })
	end,
    }
}
