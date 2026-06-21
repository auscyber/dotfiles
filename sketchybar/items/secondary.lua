local function config_dir()
	return os.getenv("CONFIG_DIR") or ((os.getenv("HOME") or "") .. "/.config/sketchybar")
end

local plugin_dir = config_dir() .. "/plugins"

-- Arrangement IDs of displays that should show the minimal secondary bar
-- (just the workspaces for that display plus the focused window name).
-- Workspaces are added per-display by items/spaces.lua.
local secondary_displays = { "2" }

_G.secondary_window_name_items = {}

for _, display_id in ipairs(secondary_displays) do
	local item_name = "window_name_" .. display_id

	local window_name = sbar.add("item", item_name, {
		position = "left",
		display = display_id,
		icon = { drawing = false },
		script = plugin_dir .. "/front_app.sh",
	})

	window_name:subscribe("front_app_switched", function(env)
		window_name:set({ label = env.INFO })
	end)

	table.insert(_G.secondary_window_name_items, item_name)
end
