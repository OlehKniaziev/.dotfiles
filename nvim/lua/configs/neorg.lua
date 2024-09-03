return {
	load = {
		["core.defaults"] = {},
		["core.concealer"] = {
			config = {
				folds = false,
				icons = {
					todo = false,
				},
			},
		},
		["core.completion"] = {
			config = {
				engine = "nvim-cmp",
			},
		},
		["core.dirman"] = {
			config = {
				workspaces = {
					notes = "~/notes",
				},
			},
		},
	},
}
