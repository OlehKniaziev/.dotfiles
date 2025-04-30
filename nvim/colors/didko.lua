local fg = "#FFFFE7"
local fg_inactive = "#D4D4C1"
local bg = "#000000"
local bg1 = "#202020"
local bg2 = "#303030"
local bg3 = "#505050"
local gray = "#b3b3b3"
local yellow = "#f2de4b"
local quartz = "#F4ABFF"
local pink = "#FF66D8"
local pink3 = "#FF5B4D"
local pale_yellow = "#fff59c"
local red = "#DE1A1A"
local red2 = "#FC2856"
local brown = "#A77464"
local light_brown = "#C79484"
local olive = "#B8C480"
local green = "#84AB5B"
local forest_green = "#82BD92"
local indigo = "#9740D9"
local pumpkin = "#FF8C42"
local pumpkin2 = "#FCAC32"
local coral = "#FC573A"
local cocoa = "#C96F36"
local snow = "#F0F8FC"
local blue = "#42CAFD"
local dark_blue = "#3F88C5"
local acid = "#9AFE22"
local very_green = "#108040"
local night_blue = "#4059AD"
local azure = "#007FFF"
local grape = "#665687"
local swamp = "#77b500"
local ash = "#ACC3A6"
local rose = "#CA2E55"
local blueish = "#6dafce"
local vista = "#10A6FF"
local light_blue = "#98abb1"
local main = yellow

vim.api.nvim_set_hl(0, "Normal", { bg = bg, fg = fg })
vim.api.nvim_set_hl(0, "Cursor", { fg = main, bg = bg })
vim.api.nvim_set_hl(0, "Comment", { fg = brown, italic = true })
vim.api.nvim_set_hl(0, "Keyword", { fg = main, bold = true })
vim.api.nvim_set_hl(0, "String", { fg = swamp })
vim.api.nvim_set_hl(0, "Identifier", { fg = fg })
vim.api.nvim_set_hl(0, "@variable", { fg = fg })
vim.api.nvim_set_hl(0, "Function", { fg = rose })
vim.api.nvim_set_hl(0, "@string.escape", { fg = vista })
vim.api.nvim_set_hl(0, "Error", { fg = fg, bg = coral, bold = true })
vim.api.nvim_set_hl(0, "ErrorMsg", { link = "Error" })
vim.api.nvim_set_hl(0, "WarningMsg", { fg = pumpkin, bold = true })
vim.api.nvim_set_hl(0, "Number", { fg = snow })
vim.api.nvim_set_hl(0, "Constant", { fg = green, bold = true })
vim.api.nvim_set_hl(0, "@constant", { link = "Constant" })
vim.api.nvim_set_hl(0, "Boolean", { link = "Constant" })
vim.api.nvim_set_hl(0, "PreProc", { fg = red2 })
vim.api.nvim_set_hl(0, "Type", { fg = ash })
vim.api.nvim_set_hl(0, "Todo", { fg = main, bold = true })
vim.api.nvim_set_hl(0, "Delimiter", { fg = gray })
vim.api.nvim_set_hl(0, "Operator", { fg = gray })
vim.api.nvim_set_hl(0, "Special", { fg = pale_yellow })
vim.api.nvim_set_hl(0, "Visual", { bg = bg3 })
vim.api.nvim_set_hl(0, "CursorLine", { bg = bg2 })
vim.api.nvim_set_hl(0, "CurSearch", { fg = bg, bg = yellow })
vim.api.nvim_set_hl(0, "IncSearch", { fg = bg, bg = yellow })
vim.api.nvim_set_hl(0, "Search", { fg = bg, bg = pale_yellow })
vim.api.nvim_set_hl(0, "Substitute", { link = "Search" })
vim.api.nvim_set_hl(0, "LineNr", { fg = bg3 })
vim.api.nvim_set_hl(0, "CursorLineNr", { fg = yellow, bold = true })
vim.api.nvim_set_hl(0, "MatchParen", { bg = blue, fg = bg })
vim.api.nvim_set_hl(0, "StatusLine", { bg = bg3, fg = fg })
vim.api.nvim_set_hl(0, "StatusLineNC", { bg = bg1, fg = fg_inactive })
vim.api.nvim_set_hl(0, "DiffAdd", { bg = green })
vim.api.nvim_set_hl(0, "DiffDelete", { fg = coral })
vim.api.nvim_set_hl(0, "DiffChange", { bg = blue })
vim.api.nvim_set_hl(0, "DiffText", { bg = blue })
vim.api.nvim_set_hl(0, "StatusLineNC", { bg = bg1, fg = fg_inactive })
vim.api.nvim_set_hl(0, "StatusLineNC", { bg = bg1, fg = fg_inactive })

vim.api.nvim_set_hl(0, "@lsp.type.macro", { fg = very_green, bold = true })
vim.api.nvim_set_hl(0, "@module", { fg = dark_blue })

vim.api.nvim_set_hl(0, "DiagnosticError", { fg = coral })
vim.api.nvim_set_hl(0, "DiagnosticInfo", { fg = vista })
vim.api.nvim_set_hl(0, "DiagnosticWarn", { fg = pumpkin2 })
