local wezterm = require("wezterm")

local config = wezterm.config_builder()

local font_name = "CaskaydiaCove Nerd Font"

config.font = wezterm.font(font_name)
config.font_rules = {
	{
		intensity = "Normal",
		italic = true,
		font = wezterm.font(font_name, { italic = true }),
	},
	{
		intensity = "Bold",
		italic = false,
		font = wezterm.font(font_name, { weight = "Bold" }),
	},
	{
		intensity = "Bold",
		italic = true,
		font = wezterm.font(font_name, { weight = "Bold", italic = true }),
	},
}

config.freetype_load_flags = 'NO_HINTING'
config.front_end = "WebGpu"

config.font_size = 17

config.harfbuzz_features = { "calt", "dlig", "zero" }

config.window_background_opacity = 0.75

config.window_padding = {
	left = 0,
	right = 0,
	top = 0,
	bottom = 0,
}

config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.enable_tab_bar = true

config.color_scheme = "Catppuccin Mocha"
config.colors = {
	background = "black",
}

-- This function returns the suggested title for a tab.
-- It prefers the title that was set via `tab:set_title()`
-- or `wezterm cli set-tab-title`, but falls back to the
-- title of the active pane in that tab.
function tab_title(tab_info)
	local title = tab_info.tab_title
	-- if the tab title is explicitly set, take that
	if title and #title > 0 then
		return title
	end
	-- Otherwise, use the title from the active pane
	-- in that tab
	return tab_info.active_pane.title
end

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
	local background = "Black"
	local foreground = "#808080"

	if tab.is_active then
		background = "Black"
		foreground = "White"
	elseif hover then
		background = "#212121"
		foreground = "#909090"
	end

	local title = tab_title(tab)

	-- ensure that the titles fit in the available space,
	-- and that we have room for the edges.
	title = wezterm.truncate_right(title, max_width - 2)

	return {
		{ Background = { Color = background } },
		{ Foreground = { Color = foreground } },
		{ Text = " " },
		{ Background = { Color = background } },
		{ Foreground = { Color = foreground } },
		{ Text = tostring(tab.tab_index + 1) .. ": " },
		{ Background = { Color = background } },
		{ Foreground = { Color = foreground } },
		{ Text = "wezterm" },
		{ Background = { Color = background } },
		{ Foreground = { Color = foreground } },
		{ Text = " " },
	}
end)

config.keys = {
	{ key = "t", mods = "ALT", action = wezterm.action.SpawnTab("CurrentPaneDomain") },
}

return config
