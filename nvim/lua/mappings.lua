vim.keymap.set("n", "<space>e", vim.diagnostic.open_float)
vim.keymap.set("n", "[d", function()
	vim.diagnostic.jump({
		wrap = true,
		count = -1,
	})
end)
vim.keymap.set("n", "]d", function()
	vim.diagnostic.jump({
		wrap = true,
		count = 1,
	})
end)
vim.keymap.set("n", "<space>q", vim.diagnostic.setloclist)

local builtin = require("telescope.builtin")

vim.keymap.set("n", "<leader>fb", builtin.buffers, {})
vim.keymap.set("n", "<leader>fh", builtin.help_tags, {})
vim.keymap.set("n", "<leader>fc", builtin.git_commits, {})
vim.keymap.set("n", "<leader>fs", builtin.lsp_document_symbols, {})
vim.keymap.set("n", "<leader>fS", builtin.lsp_workspace_symbols, {})
vim.keymap.set("n", "<leader>/", builtin.current_buffer_fuzzy_find, {})

vim.keymap.set("n", "<leader>nh", function()
	vim.cmd("noh")
end)

vim.keymap.set("n", "<leader>bd", function()
	vim.cmd("bd")
end)

vim.keymap.set("n", "<leader>s", function()
	vim.cmd("source %")
end)
