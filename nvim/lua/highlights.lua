local c_statement_groups = {
	"cTypedef",
	"cStructure",
}

for _, g in ipairs(c_statement_groups) do
	vim.api.nvim_set_hl(0, g, {
		link = "cStatement",
	})
end

local italicize_groups = {
	"StorageClass",
	"Boolean"
}

for _, group_name in ipairs(italicize_groups) do
	local group = vim.api.nvim_get_hl(0, {
		create = false,
		link = true,
		name = group_name,
	})

	local updated_group = vim.deepcopy(group)
	updated_group = vim.tbl_deep_extend("force", updated_group, { italic = true })

	vim.api.nvim_set_hl(0, group_name, updated_group)
end
