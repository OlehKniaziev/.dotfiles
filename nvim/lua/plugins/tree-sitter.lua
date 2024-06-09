return {
	"nvim-treesitter/nvim-treesitter",
	event = { "BufReadPost", "BufNewFile" },
	dependencies = {
		{
			"nvim-treesitter/nvim-treesitter-context",
			opts = function()
				return require("configs.tree-sitter-context")
			end,
			config = function (_, opts)
				require("treesitter-context").setup(opts)
			end
		},
	},
	opts = function()
		return require("configs.tree-sitter")
	end,
	config = function(_, opts)
		require("nvim-treesitter.configs").setup(opts)
	end,
}
