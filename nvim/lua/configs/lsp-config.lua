local lspconfig = require("lspconfig")

local capabilities = require("cmp_nvim_lsp").default_capabilities()
local servers = require("configs.servers")

for _, server in ipairs(servers) do
	lspconfig[server].setup({
		capabilities = capabilities,
	})
end

lspconfig.clojure_lsp.setup({
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

	capabilities = capabilities,
})

local configs = require("lspconfig.configs")

if not configs.lexical then
	configs.lexical = {
		default_config = {
			filetypes = { "elixir", "eelixir", "heex" },
			cmd = { "/home/oleg/.local/share/nvim/mason/bin/lexical" },
			root_dir = function(filename)
				return lspconfig.util.root_pattern("mix.exs")(filename) or vim.loop.os_homedir()
			end,
			settings = {},
		},
	}
end

lspconfig.lexical.setup({
	capabilities = capabilities,
})

local border = "rounded"

local orig = vim.lsp.util.open_floating_preview
function vim.lsp.util.open_floating_preview(context, syntax, opts, ...)
	opts = opts or {}
	opts.border = opts.border or border
	return orig(context, syntax, opts, ...)
end
