return {
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
}
