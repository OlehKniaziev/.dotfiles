require("neodev").setup({})
local lspconfig = require("lspconfig")

local capabilities = require("cmp_nvim_lsp").default_capabilities()
local servers = { "clangd", "rust_analyzer", "pyright", "tsserver", "jdtls", "gopls", "tailwindcss", "gleam", "metals" }

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
	cmd = { "dotnet", "/home/oleg/software/omnisharp/OmniSharp.dll" },
	enable_roslyn_analyzers = true,
	enable_import_completion = true,

	handlers = {
		["textDocument/definition"] = omnisharp_extended.definition_handler,
		["textDocument/references"] = omnisharp_extended.references_handler,
		["textDocument/implementation"] = omnisharp_extended.implementation_handler,
	},
})
local configs = require("lspconfig.configs")

local lexical_config = {
	filetypes = { "elixir", "eelixir", "heex" },
	cmd = { "/home/oleg/software/lexical/_build/dev/package/lexical/bin/start_lexical.sh" },
	settings = {},
}

if not configs.lexical then
	configs.lexical = {
		default_config = {
			filetypes = lexical_config.filetypes,
			cmd = lexical_config.cmd,
			root_dir = function(fname)
				return lspconfig.util.root_pattern("mix.exs", ".git")(fname) or vim.loop.os_homedir()
			end,
			settings = lexical_config.settings,
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
