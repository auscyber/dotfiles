-- paneru window-manager sketchybar provider.
--
-- Shipped by den.aspects.paneru as a Lua module on *paneru's own* embedded
-- Lua require path (services.paneru.extraLuaPackages, resolved via
-- $PANERU_LUA_PATH/$PANERU_LUA_CPATH), required at the end of the generated
-- init.lua. Unlike the WM-side `wm.lua` modules for other window managers
-- (which run inside sketchybar's own process and get polled/triggered from
-- outside), this one runs inside paneru's process and pushes updates to
-- sketchybar directly through SbarLua (`require("sbar")`) — paneru.on(...)
-- handlers fire synchronously on paneru's own event loop, so the bar updates
-- the moment paneru's state changes rather than on a polling/subscribe
-- bridge. sketchybar's config stays WM-agnostic; it never loads anything
-- paneru-specific.
--
-- `colors` / `icon_map` are paneru's own copies (den.aspects.paneru's
-- colorsModule/iconMapModule), not sketchybar's — this runs in a separate Lua
-- VM with no access to sketchybar's config-dir files.
--
-- State comes from `paneru.query_json(kind)`, which hands back the decoded
-- table for one slice of paneru's state document. `"state"` is the whole
-- document — everything drawn here — so a redraw takes exactly one call:
--   { version, timestamp,
--     active = { display_id, native_workspace_id, virtual_workspace_number,
--                focused_window_id, focused_bundle_id, focused_app_name,
--                focused_window_title },
--     virtual_workspaces = [ { number, native_workspace_id, active,
--       windows = [ { window_id, bundle_id, app_name, title, focused,
--         floating, display_id, frame, visible } ] } ] }
-- The narrower kinds (`"active"`, `"virtual-workspaces"`, `"on-screen"`) are
-- the same fields sliced out; `"active"` is used for the title-only redraw.
-- Every query in one dispatch answers from a single snapshot (paneru extracts
-- it lazily, at most once per event), so asking twice never tears — but it
-- also never needs to.
--
-- Two constraints the shape below follows from:
--   * queries only answer inside a `paneru.on` handler or `paneru.bind`
--     callback — at script top level there is no world to read, and the call
--     errors rather than returning stale data. Hence the initial paint hangs
--     off `processes_loaded` instead of running inline.
--   * optional fields arrive as a lightuserdata null sentinel, not `nil`
--     (mlua serializes JSON null that way), so they go through `nn` below.
--
-- Virtual workspaces are the vertical rows WITHIN a native macOS space, so the
-- same `number` recurs across native spaces; the widget scopes to the active
-- native space (active.native_workspace_id). `windows[].visible` is true for
-- windows actually on-screen right now (not minimized/hidden) — distinct from
-- merely being present on a (possibly not currently laid-out) virtual
-- workspace row; it colours each app icon individually — an app with a window
-- actually on screen draws in full colour, one whose windows are all
-- minimized/hidden draws dimmed.

local sbar = require("sketchybar")
local colors = require("colors")
local icon_map = require("icon_map")

-- Displays that show the minimal secondary bar (front-app title only). Must
-- match sketchybar/items/secondary.lua's `secondary_displays` — duplicated
-- here since that list lives in sketchybar's own process/config dir, which
-- this file (running inside paneru) has no access to.
local secondary_displays = { "2" }

-- Absent optional fields come back as mlua's JSON-null lightuserdata, which is
-- truthy in Lua and compares equal to nothing useful; every optional field read
-- off a query result goes through here first.
local function nn(value)
	if value == nil or type(value) == "userdata" then
		return nil
	end
	return value
end

-- One slice of paneru's state, or nil if the query can't answer (called
-- outside a handler, or the world couldn't be extracted).
local function query(kind)
	local ok, result = pcall(paneru.query_json, kind)
	if not ok or type(result) ~= "table" then
		return nil
	end
	return result
end

local function get_background_style(selected)
	if selected then
		return {
			drawing = true,
			color = colors.with_alpha(colors.selection, 0.75),
			border_color = colors.with_alpha(colors.selection, 0.75),
			corner_radius = 30,
			height = 21,
		}
	else
		return {
			drawing = false,
			color = colors.with_alpha(colors.background or colors.black, 0.45),
			border_color = colors.with_alpha(colors.background or colors.black, 0.15),
			corner_radius = 30,
		}
	end
end

local function icon_for_app(app_name)
	if app_name == nil or app_name == "" then
		return ""
	end
	return icon_map[app_name] or ""
end

-- The apps on a workspace row, deduplicated by app name in first-seen window
-- order, each with the icon to draw and whether any of that app's windows is
-- actually on screen right now (as opposed to present-but-hidden/minimized).
-- Visibility is per app, not per row: one row can hold a visible Firefox and a
-- minimized editor, and they are coloured differently below.
--
-- -> [ { icon = "<glyph>", visible = <bool> } ]
local function apps_on_row(windows)
	local index = {}
	local apps = {}
	if type(windows) ~= "table" then
		return apps
	end
	for _, window in ipairs(windows) do
		if type(window) == "table" and type(window.app_name) == "string" then
			local icon = icon_for_app(window.app_name)
			-- Apps with no glyph in the icon map were never drawn; keep it that
			-- way rather than opening a hole in the strip.
			if icon ~= "" then
				local existing = index[window.app_name]
				if existing then
					apps[existing].visible = apps[existing].visible or window.visible == true
				else
					table.insert(apps, { icon = icon, visible = window.visible == true })
					index[window.app_name] = #apps
				end
			end
		end
	end
	return apps
end

local dummy_space = {
	icon = {
		padding_left = 7,
		padding_right = -5,
		size = 11.0,
		drawing = true,
	},
	label = {
		padding_right = 10,
		padding_left = 7,
		font = "sketchybar-app-font:Regular:16.0",
		drawing = false,
	},
	padding_right = 0,
	background = { border_color = colors.selection },
}

-- A row draws as several bar items, because sketchybar gives one colour per
-- label string and each app icon needs its own:
--   space.<n>            the row number (drawn only while the row is empty)
--   space.<n>.app.<i>    one app icon each, coloured by that app's visibility
--   bracket.space.<n>    the selection pill wrapping the two above
-- Bracket membership is fixed when the bracket is created, so each row gets a
-- fixed pool of MAX_APPS icon slots up front; a slot with no app is hidden, and
-- hidden members take up no width, so the pill still hugs what's drawn.
local MAX_APPS = 8

-- Full colour = this app has a window on screen right now; dimmed = all of its
-- windows on this row are minimized/hidden.
local function app_icon_color(visible)
	if visible then
		return colors.foreground
	end
	return colors.with_alpha(colors.foreground, 0.4)
end

-- item_names[number] = "space.<number>" for every virtual workspace row we've
-- created bar items for, with app_item_names/bracket_names holding that row's
-- icon slots and pill. paneru rows are dynamic, so items are created lazily the
-- first time a number appears and hidden (not destroyed) when gone.
local item_names = {}
local app_item_names = {}
local bracket_names = {}
local last_item = nil

-- Re-anchor chevron/front_app after the last workspace item. Equivalent to
-- sketchybar/items/left.lua's `reorder_left_items`, reimplemented here (it's a
-- global set inside sketchybar's own Lua VM, unreachable from paneru's) —
-- it's just two `sketchybar --move` shell calls under the hood, safe to issue
-- directly via `sbar.exec` from any process.
local function reorder_left_items(item_name)
	if not item_name or item_name == "" then
		return
	end
	sbar.exec("sketchybar --move chevron after " .. item_name)
	sbar.exec("sketchybar --move front_app after chevron")
end

local function ensure_item(number)
	local existing = item_names[number]
	if existing then
		return existing
	end

	local item_name = "space." .. number
	-- Fire-and-forget CLI call, run by sketchybar's own process (not paneru's)
	-- — no in-process callback round trip needed, unlike `sbar.subscribe`,
	-- which would require paneru to also run SbarLua's blocking `event_loop()`
	-- to receive the click. Shared by the row and its app icons, so clicking
	-- any part of the pill jumps to that row.
	local click_script = "paneru send-cmd window virtualnum " .. tostring(number)

	sbar.add("item", item_name, {
		icon = {
			string = tostring(number),
			padding_left = dummy_space.icon.padding_left,
			padding_right = dummy_space.icon.padding_right,
			font = { size = dummy_space.icon.size },
			drawing = dummy_space.icon.drawing,
		},
		position = "left",
		label = dummy_space.label,
		padding_right = dummy_space.padding_right,
		-- The pill is the bracket's, not this item's: it has to span the app
		-- icons too.
		background = { drawing = false, border_color = dummy_space.background.border_color },

		drawing = true,
		click_script = click_script,
	})

	-- Icon slots, created in order right after the row item so bar order is
	-- creation order — no `--move` per redraw, only the one-off re-anchor of
	-- chevron/front_app below.
	local app_items = {}
	for i = 1, MAX_APPS do
		local app_item = item_name .. ".app." .. i
		sbar.add("item", app_item, {
			position = "left",
			drawing = false,
			icon = {
				string = "",
				font = dummy_space.label.font,
				padding_left = 2,
				padding_right = 2,
				color = app_icon_color(true),
			},
			label = { drawing = false },
			padding_left = 0,
			padding_right = 0,
			background = { drawing = false },
			click_script = click_script,
		})
		app_items[i] = app_item
		last_item = app_item
	end
	app_item_names[number] = app_items

	local bracket_name = "bracket.space." .. number
	local members = { item_name }
	for _, app_item in ipairs(app_items) do
		table.insert(members, app_item)
	end
	sbar.add("bracket", bracket_name, members, {
		background = get_background_style(false),
	})
	bracket_names[number] = bracket_name

	item_names[number] = item_name
	reorder_left_items(last_item)
	return item_name
end

for i = 1, 9 do
	ensure_item(i)
end

local function render_workspaces()
	local state = query("state")
	if not state then
		return
	end

	local workspaces = state.virtual_workspaces
	local active = state.active
	if type(workspaces) ~= "table" or type(active) ~= "table" then
		return
	end

	local active_native_id = nn(active.native_workspace_id)
	if active_native_id == nil then
		return
	end

	local present = {}
	-- Highlight follows focus: the row holding the focused window, reported in
	-- `active` as virtual_workspace_number. Fall back to each row's own
	-- `active` flag if the focused number is missing.
	local focused_number = nn(active.virtual_workspace_number)

	-- Stable order: ascending virtual-workspace number.
	table.sort(workspaces, function(a, b)
		return (a.number or 0) < (b.number or 0)
	end)

	for _, ws in ipairs(workspaces) do
		if type(ws) == "table" and ws.number ~= nil and ws.native_workspace_id == active_native_id then
			local number = ws.number
			present[number] = true
			local item_name = ensure_item(number)

			local apps = apps_on_row(ws.windows)
			local has_apps = #apps > 0
			local selected
			if focused_number ~= nil then
				selected = number == focused_number
			else
				selected = ws.active == true
			end

			-- Number only while the row is empty; once it has apps the icons
			-- speak for it.
			sbar.set(item_name, {
				drawing = true,
				icon = { string = tostring(number), drawing = not has_apps },
				label = { drawing = false },
			})

			-- One slot per app, each coloured by that app's own visibility.
			-- Rows with more than MAX_APPS distinct apps drop the overflow
			-- rather than growing the bar without bound.
			for i, app_item in ipairs(app_item_names[number]) do
				local app = apps[i]
				if app then
					sbar.set(app_item, {
						drawing = true,
						icon = { string = app.icon, color = app_icon_color(app.visible) },
					})
				else
					sbar.set(app_item, { drawing = false })
				end
			end

			sbar.set(bracket_names[number], { background = get_background_style(selected) })
		end
	end

	-- Hide items for rows that don't exist on the active native space.
	for number, item_name in pairs(item_names) do
		if not present[number] then
			sbar.set(item_name, { drawing = false })
			for _, app_item in ipairs(app_item_names[number]) do
				sbar.set(app_item, { drawing = false })
			end
			sbar.set(bracket_names[number], { background = get_background_style(false) })
		end
	end

	reorder_left_items(last_item)
end

-- Focused-window title on front_app + the secondary display's title items.
local function update_titles()
	local active = query("active")
	local title = ""
	if active and type(nn(active.focused_window_title)) == "string" then
		title = active.focused_window_title
	end
	sbar.set("front_app", { label = title })
	for _, display_id in ipairs(secondary_displays) do
		sbar.set("window_name_" .. display_id, { label = title })
	end
end

-- First paint. Not inline: this file runs at paneru's script-load time, where
-- there is no world to query yet (`paneru.query*` errors outside a handler), so
-- the initial draw waits for the first event paneru emits once its process list
-- is up.
paneru.on("processes_loaded", function()
	render_workspaces()
	update_titles()
end)

-- Driven straight off paneru's own event loop (paneru.on dispatches
-- synchronously, in-process, on paneru's main thread) — no polling, no
-- shelling out to `paneru subscribe`/query per tick. Each event only redraws
-- what it can actually change: workspace membership/visibility color
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
	paneru.on(event, render_workspaces)
end

-- update_titles: only the focused window's title changes.
paneru.on("window_title_changed", update_titles)

-- Both: which row/window is focused (and so what's selected/titled) changes.
for _, event in ipairs({
	"window_focused",
	"window_destroyed",
	"space_changed",
	"display_changed",
}) do
	paneru.on(event, function()
		render_workspaces()
		update_titles()
	end)
end
