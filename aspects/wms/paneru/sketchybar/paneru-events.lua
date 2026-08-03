-- paneru window-manager sketchybar provider (paneru's side).
--
-- Shipped by den.aspects.paneru as the `paneru_events` Lua module on *paneru's*
-- embedded Lua require path (services.paneru.extraLuaPackages, resolved via
-- $PANERU_LUA_PATH/$PANERU_LUA_CPATH) and required from the generated init.lua.
-- Unlike sketchybar-side `wm` modules, this runs inside paneru's process and
-- pushes updates to sketchybar directly through SbarLua (`require("sketchybar")`)
-- — paneru.on handlers fire synchronously on paneru's own event loop, so the
-- bar updates the moment paneru's state changes rather than on a polling or
-- subscribe bridge.
--
-- It owns only the *incremental* repaints. Creating the items and the initial
-- paint belong to sketchybar (aspects/wms/paneru/sketchybar/wm.lua), which
-- queries paneru itself; all this side does about startup is announce it, by
-- triggering the `paneru_load` event sketchybar resyncs on. That keeps the two
-- in sync in both restart directions: paneru restarting fires paneru_load,
-- sketchybar restarting re-queries on its own.
--
-- The drawing itself is `paneru_bar`, the same module sketchybar loads.

local sbar = require("sketchybar")
local bar = require("paneru_bar")

-- First paint is sketchybar's, not ours: this file runs at paneru's script-load
-- time, where there is no world to query yet (`paneru.query*` errors outside a
-- handler). `processes_loaded` is the first event paneru emits once its process
-- list is up — by then the daemon can answer sketchybar's query, so announcing
-- is enough.
paneru.on("processes_loaded", function()
	sbar.exec("sketchybar --trigger paneru_load")
end)

-- Driven straight off paneru's own event loop (paneru.on dispatches
-- synchronously, in-process, on paneru's main thread) — no polling, no
-- shelling out to `paneru subscribe`/query per tick. Each event only redraws
-- what it can actually change: workspace membership/visibility colour
-- (render_workspaces) and the focused-window title (update_titles) are
-- independent, so e.g. a title change doesn't also re-walk every workspace
-- row, and a window moving between rows doesn't also re-fetch the title.
--
-- render_workspaces: workspace membership, which apps are on a row, and the
-- per-app visible/hidden colour change. application_hidden/visible are in here
-- because ⌘H flips every window of an app to hidden at once — the per-app
-- colour is wrong until the row is re-walked, and no per-window event fires.
for _, event in ipairs({
	--	"window_moved",
	--	"window_minimized",
	--	"window_deminimized",
	"application_hidden",
	"application_visible",
	"space_created",
	"space_destroyed",
}) do
	paneru.on(event, bar.render_workspaces)
end

-- update_titles: only the focused window's title changes.
paneru.on("window_title_changed", bar.update_titles)

-- Both: which row/window is focused (and so what's selected/titled) changes.
for _, event in ipairs({
	"window_focused",
	"window_destroyed",
	"space_changed",
	"display_changed",
}) do
	paneru.on(event, bar.render)
end
