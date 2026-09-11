return {
	"nvim-treesitter/nvim-treesitter",
	branch = "main",
	event = { "BufReadPost", "BufNewFile" },
	dependencies = {
		{
			"nvim-treesitter/nvim-treesitter-context",
			opts = function()
				return require("configs.tree-sitter-context")
			end,
			config = function(_, opts)
				require("treesitter-context").setup(opts)
			end
		},
	},
	opts = function()
		return require("configs.tree-sitter")
	end,
}
