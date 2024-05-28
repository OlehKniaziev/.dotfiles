return {
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
}
