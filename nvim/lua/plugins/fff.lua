return {
	"dmtrKovalenko/fff",
	build = function()
		require("fff.download").download_or_build_binary()
	end,
	opts = {
		debug = {
			enabled = true,
			show_scores = true,
		},
	},
	lazy = false,
	keys = {
		{ "<leader>ff", function() require("fff").find_files() end },
		{
			"<leader>fg",
			function()
				require("fff").live_grep({
					grep = { modes = { "fuzzy", "plain" } },
				})
			end,
		},
		{
			"fw",
			function() require("fff").live_grep_under_cursor() end,
			mode = { "n", "x" },
		},
	},
}
