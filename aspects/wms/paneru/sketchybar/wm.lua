-- paneru window-manager sketchybar provider (sketchybar's side).
--
-- Shipped by den.aspects.paneru as the `wm` Lua module on sketchybar's require
-- path (programs.sketchybar.extraLuaPackages), so the base config's
-- `require("wm")` resolves to this when paneru is the enabled WM — the same
-- contract aspects/wms/rift/sketchybar/wm.lua fulfils for rift, and the reason
-- sketchybar's own config stays WM-agnostic.
--
-- This half owns *initialisation*: it creates the workspace items and paints
-- them from a state query of its own, read through the paneru client module
-- (`require("paneru")`, pkgs.paneru.luaModule) over the daemon's socket. It
-- does that at two moments, so the bar is in sync no matter which of the two
-- processes started first:
--   * config load — paneru is already running, sketchybar (re)started; and
--   * `paneru_load` — paneru (re)started and announced itself, whether or not
--     sketchybar restarted with it.
-- Neither one waits on paneru asking us to draw. A query at config load with no
-- daemon up simply paints nothing, and the `paneru_load` that follows fills it
-- in.
--
-- Incremental repaints are the other half: paneru drives those from its own
-- event loop, in its own process, through the same `paneru_bar` module — see
-- aspects/wms/paneru/sketchybar/paneru-events.lua.
--
-- Relies on globals set by the base config: `sbar` (init.lua),
-- `_G.reorder_left_items` (items/left.lua), `_G.secondary_window_name_items`
-- (items/secondary.lua) — all set before init.lua's `require("wm")`.

local bar = require("paneru_bar")

-- Triggered by paneru itself on launch (paneru-events.lua), so a restarted
-- daemon resyncs the bar rather than leaving it showing the state it had when
-- the daemon died.
sbar.add("event", "paneru_load")

bar.create_items()
bar.render()

local observer = sbar.add("item", "paneru_observer", { drawing = false })

observer:subscribe("paneru_load", function()
	bar.render()
end)

-- front_app_switched is sketchybar's own signal and fires for app switches
-- paneru may not report as a focus change (e.g. into an unmanaged window), so
-- the title label follows it too.
observer:subscribe("front_app_switched", function()
	bar.update_titles()
end)
