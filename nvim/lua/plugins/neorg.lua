return {
	"nvim-neorg/neorg",
	ft = "norg",
	cmd = "Neorg",
	opts = function()
		return require("configs.neorg")
	end,
	config = function(_, opts)
		require("neorg").setup(opts)
	end,
}
