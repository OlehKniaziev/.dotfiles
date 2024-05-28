return {
	load = {
		["core.defaults"] = {},
		["core.dirman"] = {
			config = {
				workspaces = {
					notes = "~/notes",
					exercise = "~/exercise-progress",
				},
			},
		},
		["core.completion"] = {
			config = {
				engine = "nvim-cmp",
			},
		},
		["core.integrations.treesitter"] = {},
	},
}
