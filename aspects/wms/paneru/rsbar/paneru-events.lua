-- paneru window-manager rsbar provider (paneru's side).
--
-- The rsbar twin of ../sketchybar/paneru-events.lua, and it exists as a
-- separate file for one reason: how the repaint reaches the bar.
--
-- Against sketchybar, paneru's own Lua VM draws the bar itself — SbarLua is a
-- loadable C module, so `require("sketchybar")` inside paneru's interpreter
-- gives a real client and `paneru_bar` runs in-process, synchronously, on
-- paneru's event loop. rsbar has no equivalent module to load: its Lua API is
-- a Rust crate whose loadable `module`-feature cdylib is not something the
-- rsbar package builds, so nothing in paneru's interpreter can speak to the
-- daemon.
--
-- So this side does not draw. It pokes: each paneru event becomes one
-- `rsbard --trigger`, and rsbar's own config (../rsbar/wm.lua, which is
-- ../sketchybar/wm.lua — the same file, since rsbar answers to `sbar`)
-- re-renders from a state query of its own. That is exactly the path the
-- sketchybar side already uses for `paneru_load`, just applied to every event
-- rather than only to startup.
--
-- The three triggers match the three repaint granularities `paneru_bar`
-- distinguishes, so a title change still does not re-walk every workspace row:
--   * paneru_render      — render()            (focus/space/display changes)
--   * paneru_workspaces  — render_workspaces() (membership, hidden/visible)
--   * paneru_titles      — update_titles()     (focused window title only)

local RSBARD = "rsbard"

-- `paneru.exec(program, args)` takes an argv list, not a shell line, and is
-- async: the handler suspends while the child runs instead of holding the
-- interpreter. A failed trigger is logged and swallowed — rsbar not being up
-- yet (or being mid-reload) must not abort the rest of the handler, which is
-- the failure mode ../../../../patches/paneru/echild-exec.patch exists for on
-- the sketchybar side.
local function trigger(event)
	local ok, err = pcall(paneru.exec, RSBARD, { "--trigger", event })
	if not ok then
		paneru.log("rsbar trigger " .. event .. " failed: " .. tostring(err))
	end
end

local function on(events, event_name)
	for _, event in ipairs(events) do
		paneru.on(event, function()
			trigger(event_name)
		end)
	end
end

-- First paint is rsbar's, not ours: this file runs at paneru's script-load
-- time, where there is no world to query yet (`paneru.query*` errors outside a
-- handler). `processes_loaded` is the first event paneru emits once its process
-- list is up — by then the daemon can answer rsbar's query, so announcing is
-- enough.
paneru.on("processes_loaded", function()
	trigger("paneru_load")
end)

on({
	"application_hidden",
	"application_visible",
	"space_created",
	"space_destroyed",
}, "paneru_workspaces")

on({ "window_title_changed" }, "paneru_titles")

on({
	"window_focused",
	"window_destroyed",
	"space_changed",
	"display_changed",
}, "paneru_render")
