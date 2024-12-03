return {
	"stevearc/oil.nvim",
	dependencies = { "nvim-tree/nvim-web-devicons" },
	-- lazy = false,
	opts = function()
		return require("configs.oil")
	end,
	config = function(_, opts)
		require("oil").setup(opts)
	end,
}
