local ls = require("luasnip")

local s = ls.snippet
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt
local f = ls.function_node

ls.add_snippets("all", {
	s(
		"curtime",
		f(function()
			return os.date("%D - %H:%M")
		end)
	),
})

ls.add_snippets("go", {
	s(
		"iferr",
		fmt(
			[[
		if err != nil {{
			{}
		}}
		]],
			i(1)
		)
	),
})

ls.add_snippets("lua", {
	s(
		"req",
		fmt([[local {} = require("{}")]], {
			f(function(import_names)
				local parts = vim.split(import_names[1][1], ".", { plain = true })
				return parts[#parts]
			end, { 1 }),
			i(1),
		})
	),
})

ls.add_snippets("javascript", {
	s(
		"func",
		fmt(
			[[
			function {}({}) {{
			    {}
			}}
		]],
			{
				i(1),
				i(2),
				i(3),
			}
		)
	),
	s(
		"lambda",
		fmt(
			[[
			({}) => {{
			    {}
			}}
		]],
			{
				i(1),
				i(2),
			}
		)
	),
})

return {
	history = true,
	enable_autosnippets = true,
	updateevents = "TextChanged,TextChangedI",
}
