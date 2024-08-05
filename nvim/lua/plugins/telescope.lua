return {
	"nvim-telescope/telescope.nvim",
	dependencies = { "nvim-lua/plenary.nvim" },
	opts = function ()
		return require("configs.telescope")
	end,
	config = function (_, opts)
		require("telescope").setup(opts)
	end
}
