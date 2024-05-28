require("neodev").setup({})
local lspconfig = require("lspconfig")

local capabilities = require("cmp_nvim_lsp").default_capabilities()
local servers = require("configs.servers")

for _, server in ipairs(servers) do
	lspconfig[server].setup({
		capabilities = capabilities,
	})
end

lspconfig.lua_ls.setup({
	settings = {
		Lua = {
			diagnostics = {
				globals = { "vim" },
			},
		},
	},
	capabilities = capabilities,
})

local omnisharp_extended = require("omnisharp_extended")

lspconfig.omnisharp.setup({
	cmd = { "/home/oleg/.local/share/nvim/mason/bin/omnisharp" },
	settings = {
		RoslyExtensionOptions = {
			EnableImportCompletion = true,
		},
	},

	handlers = {
		["textDocument/definition"] = omnisharp_extended.definition_handler,
		["textDocument/references"] = omnisharp_extended.references_handler,
		["textDocument/implementation"] = omnisharp_extended.implementation_handler,
	},
})

local border = "rounded"

local orig = vim.lsp.util.open_floating_preview
function vim.lsp.util.open_floating_preview(context, syntax, opts, ...)
	opts = opts or {}
	opts.border = opts.border or border
	return orig(context, syntax, opts, ...)
end
