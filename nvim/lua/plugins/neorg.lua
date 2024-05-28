return {
	{
		"vhyrro/luarocks.nvim",
		priority = 1000,
		config = true,
	},
	{
		"nvim-neorg/neorg",
		dependencies = {
			"vhyrro/luarocks.nvim",
		},
		lazy = false,
		opts = function()
			return require("configs.neorg")
		end,
		config = function(_, opts)
			require("neorg").setup(opts)
		end,
		version = "*",
	},
}
