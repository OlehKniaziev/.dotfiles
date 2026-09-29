local c_statement_groups = {
	"cTypedef",
	"cStructure",
}

for _, g in ipairs(c_statement_groups) do
	vim.api.nvim_set_hl(0, g, {
		link = "cStatement",
	})
end
