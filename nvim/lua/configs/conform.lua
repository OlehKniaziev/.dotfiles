return {
	formatters_by_ft = {
		javascript = { "prettier" },
		typescript = { "prettier" },
		javascriptreact = { "prettier" },
		typescriptreact = { "prettier" },
		css = { "prettier" },
		html = { "prettier" },

		lua = { "stylua" },
		c = { "clang_format" },
		cpp = { "clang_format" },
		go = { "gofmt" },
		cs = { "csharpier" },
		gleam = { "gleam" },
		rust = { "rustfmt" },
		scala = { "scalafmt" },
		ocaml = { "ocamlformat" },
		python = { "black" },
		zig = { "zigfmt" },
	},
}
