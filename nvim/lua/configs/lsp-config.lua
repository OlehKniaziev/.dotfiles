local servers = require("configs.servers")

local cmp = require("blink.cmp")

for _, server in ipairs(servers) do
	vim.lsp.enable(server)
	vim.lsp.config(server, {
		capabilities = cmp.get_lsp_capabilities(),
	})
end

local border = "rounded"

local orig = vim.lsp.util.open_floating_preview
function vim.lsp.util.open_floating_preview(context, syntax, opts, ...)
	opts = opts or {}
	opts.border = opts.border or border
	return orig(context, syntax, opts, ...)
end
