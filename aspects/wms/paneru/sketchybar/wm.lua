-- paneru window-manager bar provider (the bar's side).
--
-- Shipped by den.aspects.paneru as the `wm` Lua module on the bar's require
-- path — programs.sketchybar.extraLuaPackages for sketchybar, or as a plain
-- file next to `rsbarrc` for rsbar — so the base config's `require("wm")`
-- resolves to this when paneru is the enabled WM. The same contract
-- aspects/wms/rift/sketchybar/wm.lua fulfils for rift, and the reason a bar's
-- own config stays WM-agnostic.
--
-- One file for both bars: rsbar installs its API under `sbar` (and
-- `require("sketchybar")`) exactly as SbarLua does, so nothing here has to know
-- which of the two it is running in.
--
-- This half owns *initialisation*: it creates the workspace items and paints
-- them from a state query of its own, read through the paneru client module
-- (`require("paneru")`, pkgs.paneru.luaModule) over the daemon's socket. It
-- does that at two moments, so the bar is in sync no matter which of the two
-- processes started first:
--   * config load — paneru is already running, the bar (re)started; and
--   * `paneru_load` — paneru (re)started and announced itself, whether or not
--     the bar restarted with it.
-- Neither one waits on paneru asking us to draw. A query at config load with no
-- daemon up simply paints nothing, and the `paneru_load` that follows fills it
-- in.
--
-- Incremental repaints reach here differently per bar, which is what the three
-- finer-grained events below are for:
--   * sketchybar — they never fire. paneru repaints sketchybar from its own
--     process, through the same `paneru_bar` module, because SbarLua is
--     loadable into paneru's interpreter. See ../sketchybar/paneru-events.lua.
--   * rsbar — nothing loadable exists for paneru to draw with, so paneru only
--     triggers, and the redraw happens here off a state query. See
--     ../rsbar/paneru-events.lua.
--
-- Relies on globals set by the base config: `sbar` (init.lua),
-- `_G.reorder_left_items` (items/left.lua), `_G.secondary_window_name_items`
-- (items/secondary.lua) — all set before init.lua's `require("wm")`. All three
-- are optional; `paneru_bar` degrades without the latter two.

local bar = require("paneru_bar")

-- Triggered by paneru itself (../sketchybar/paneru-events.lua,
-- ../rsbar/paneru-events.lua), so a restarted daemon resyncs the bar rather
-- than leaving it showing the state it had when the daemon died.
local repaints = {
	paneru_load = bar.render,
	paneru_render = bar.render,
	paneru_workspaces = bar.render_workspaces,
	paneru_titles = bar.update_titles,
}

for event in pairs(repaints) do
	sbar.add("event", event)
end

bar.create_items()
bar.render()

local observer = sbar.add("item", "paneru_observer", { drawing = false })

for event, repaint in pairs(repaints) do
	observer:subscribe(event, function()
		repaint()
	end)
end

-- front_app_switched is the bar's own signal and fires for app switches paneru
-- may not report as a focus change (e.g. into an unmanaged window), so the
-- title label follows it too.
observer:subscribe("front_app_switched", function()
	bar.update_titles()
end)
