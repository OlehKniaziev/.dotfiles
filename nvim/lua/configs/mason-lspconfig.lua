local lsps = require("configs.servers")
local additional = { "lua_ls", "omnisharp" }

vim.list_extend(lsps, additional)

return {
	ensure_installed = lsps,
}
