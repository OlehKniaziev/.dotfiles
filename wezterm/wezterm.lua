local wezterm = require("wezterm")

local config = wezterm.config_builder()

-- local font_name = "Cascadia Code"
-- local font_name = "JetBrains Mono"
-- local font_name = "Iosevka Classy"
-- local font_name = "Iosevka"
-- local font_name = "Iosevka Cozy"
-- local font_name = "Berkeley Mono"
-- local font_name = "IBM Plex Mono"
local font_name = "MonoLisa"
-- local font_name = "Terminus"

-- config.font = wezterm.font_with_fallback({ font_name, "CascadiaCode NF" })
config.font = wezterm.font(font_name)
config.font_rules = {
	{
		intensity = "Normal",
		italic = false,
		font = wezterm.font(font_name, { weight = "Regular" }),
	},
	{
		intensity = "Normal",
		italic = true,
		font = wezterm.font(font_name, { weight = "Regular", italic = true }),
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

config.freetype_load_flags = "NO_HINTING"
config.front_end = "WebGpu"

config.font_size = 13
-- config.harfbuzz_features = { "ss01" }
-- config.harfbuzz_features =
-- 	{ "calt", "liga", "ss01", "ss02", "ss03", "ss04", "ss05", "ss06", "ss07", "ss08", "ss09", "zero" }
-- config.harfbuzz_features = { "ss01", "zero" }
-- config.harfbuzz_features = { "zero", "ss01", "cv11", "cv08", "cv06" }
-- config.harfbuzz_features = { "calt", "dlig", "ss02", "ss05", "ss09", "ss03", "cv28", "cv25", "cv26", "ss07" }
-- config.harfbuzz_features = { "calt=0", "dlig" }
-- config.harfbuzz_features = { "ss01", "ss02", "ss03", "ss04", "ss05" }
-- config.harfbuzz_features = { "ss02", "calt", "dlig" }

config.window_padding = {
	left = 1,
	right = 1,
	top = 1,
	bottom = 1,
}

config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.enable_tab_bar = true

-- config.color_scheme = "tokyonight_day"
config.color_scheme = "Campbell (Gogh)"
config.colors = {
	background = "black",
    foreground = "#FFFFFF",
}

config.default_cursor_style = "BlinkingBlock"
config.animation_fps = 1

if wezterm.target_triple == "x86_64-pc-windows-msvc" then
	config.default_prog = { "powershell.exe" }
end

-- config.window_background_opacity = 0.5

-- This function returns the suggested title for a tab.
-- It prefers the title that was set via `tab:set_title()`
-- or `wezterm cli set-tab-title`, but falls back to the
-- title of the active pane in that tab.
--local function tab_title(tab_info)
--	local title = tab_info.tab_title
--	-- if the tab title is explicitly set, take that
--	if title and #title > 0 then
--		return title
--	end
--	-- Otherwise, use the title from the active pane
--	-- in that tab
--	return tab_info.active_pane.title
--end

-- wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
-- 	local background = "Black"
-- 	local foreground = "#808080"
--
-- 	if tab.is_active then
-- 		background = "Black"
-- 		foreground = "White"
-- 	elseif hover then
-- 		background = "#212121"
-- 		foreground = "#909090"
-- 	end
--
-- 	local title = tab_title(tab)
--
-- 	-- ensure that the titles fit in the available space,
-- 	-- and that we have room for the edges.
-- 	title = wezterm.truncate_right(title, max_width - 2)
--
-- 	return {
-- 		{ Background = { Color = background } },
-- 		{ Foreground = { Color = foreground } },
-- 		{ Text = " " },
-- 		{ Background = { Color = background } },
-- 		{ Foreground = { Color = foreground } },
-- 		{ Text = tostring(tab.tab_index + 1) .. ": " },
-- 		{ Background = { Color = background } },
-- 		{ Foreground = { Color = foreground } },
-- 		{ Text = "wezterm" },
-- 		{ Background = { Color = background } },
-- 		{ Foreground = { Color = foreground } },
-- 		{ Text = " " },
-- 	}
-- end)

config.keys = {
	{ key = "t", mods = "ALT", action = wezterm.action.SpawnTab("CurrentPaneDomain") },
}

return config
