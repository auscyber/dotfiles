sbar = require("sketchybar")

sbar.begin_config()
sbar.add("event", "rift_windows_changed")
sbar.add("event", "rift_workspace_changed")
sbar.add("event", "rift_windows_title")

require("bar")
require("default")
require("items")
sbar.end_config()

sbar.exec("sketchybar --subscribe volume volume_change")
sbar.exec("sketchybar --subscribe power power_source_change")
sbar.exec("sketchybar --subscribe battery system_woke power_source_change mouse.entered mouse.exited")
sbar.exec("sketchybar --subscribe volume_options mouse.clicked")
sbar.exec("sketchybar --subscribe front_app front_app_switched rift_windows_title")

if _G.secondary_window_name_items then
	for _, item_name in ipairs(_G.secondary_window_name_items) do
		sbar.exec("sketchybar --subscribe " .. item_name .. " front_app_switched rift_windows_title")
	end
end

sbar.exec("sketchybar --update")

sbar.event_loop()
