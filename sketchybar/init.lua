sbar = require("sketchybar")

sbar.begin_config()

require("bar")
require("default")
require("items")

-- Window-manager-specific bar integration -- the spaces/workspaces widget,
-- focused-window title, and the workspace-change events -- is injected by
-- whichever WM aspect is enabled (rift, paneru, ...), shipped as a `wm` Lua
-- module on the require path. It registers its own events and items and owns
-- the front_app/secondary title labels. Absent on hosts with no WM, so the
-- require is guarded.
pcall(require, "wm")

sbar.end_config()

sbar.exec("sketchybar --subscribe volume volume_change")
sbar.exec("sketchybar --subscribe power power_source_change")
sbar.exec("sketchybar --subscribe battery system_woke power_source_change mouse.entered mouse.exited")
sbar.exec("sketchybar --subscribe volume_options mouse.clicked")

sbar.exec("sketchybar --update")

sbar.event_loop()
