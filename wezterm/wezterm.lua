local wezterm = require("wezterm")

local config = wezterm.config_builder()

-- config.font = wezterm.font("Iosevka Nerd Font Mono")
config.font = wezterm.font("JetBrains Mono")
-- config.font = wezterm.font("Martian Mono")
-- config.font = wezterm.font("Input Mono")
-- config.font = wezterm.font("Geist Mono")
-- config.font = wezterm.font("Monaspace Xenon")
-- config.font = wezterm.font("Fira Code Nerd Font Mono")
-- config.font = wezterm.font("Input Mono")

config.font_size = 16

config.harfbuzz_features = {  }

config.color_scheme = "3024 Day (Gogh)"

config.window_padding = {
	left = 0,
	right = 0,
	top = 0,
	bottom = 0,
}

config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.enable_scroll_bar = false
config.show_new_tab_button_in_tab_bar = false

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

return config
