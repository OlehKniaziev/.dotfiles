vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable",
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup("plugins", {
	change_detection = { notify = false },
	defaults = {
		lazy = true,
	},
})

require("mappings")
require("autocmds")
require("options")
require("misc")

-- vim.cmd("set statusline=%!v:lua.require'line'.line()")

vim.cmd("let g:gruvbox_material_background='medium'")
vim.cmd("let g:gruvbox_material_disable_italic_comment=0")
vim.cmd("let g:gruvbox_material_diagnostic_virtual_text='colored'")
vim.cmd("let g:gruvbox_material_better_performance=1")

vim.cmd.colorscheme("rose-pine");
