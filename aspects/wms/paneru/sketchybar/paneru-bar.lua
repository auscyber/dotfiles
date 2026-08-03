-- paneru <-> sketchybar bar drawing, shared by both hosts.
--
-- Shipped by den.aspects.paneru as the `paneru_bar` Lua module onto *two*
-- separate require paths, because two processes draw this widget:
--   * sketchybar's own Lua VM (programs.sketchybar.extraLuaPackages), where
--     `wm.lua` creates the bar items and paints them from a state query at
--     config load and on every `paneru_load` event; and
--   * paneru's embedded Lua VM (services.paneru.extraLuaPackages), where
--     `paneru_events.lua` repaints them straight off paneru's event loop.
--
-- The same code runs in both because the state API is spelled identically in
-- each: paneru's runtime exposes it as the `paneru` global, and the loadable
-- client module (`require("paneru")`, pkgs.paneru.luaModule) exposes the same
-- `query_json` over the daemon's Unix socket. `api` below picks whichever host
-- this happens to be running in.
--
-- The division of labour: item *creation* only ever happens in sketchybar (see
-- `create_items`), never in paneru — item names are pure functions of the row
-- number and slot index, so paneru's process can `set` them without holding any
-- creation bookkeeping of its own, and the two processes cannot disagree about
-- what exists.
--
-- `colors` / `icon_map` resolve to each host's own copy of the shared modules
-- (aspects/desktop/sketchybar/_lua-modules.nix), pushed onto both require paths.
--
-- State comes from `query_json(kind)`, which hands back the decoded table for
-- one slice of paneru's state document. `"state"` is the whole document —
-- everything drawn here — so a redraw takes exactly one call:
--   { version, timestamp,
--     active = { display_id, native_workspace_id, virtual_workspace_number,
--                focused_window_id, focused_bundle_id, focused_app_name,
--                focused_window_title },
--     virtual_workspaces = [ { number, native_workspace_id, active,
--       windows = [ { window_id, bundle_id, app_name, title, focused,
--         floating, display_id, frame, visible } ] } ] }
-- The narrower kinds (`"active"`, `"virtual-workspaces"`, `"on-screen"`) are
-- the same fields sliced out; `"active"` is used for the title-only redraw.
--
-- Two constraints the shape below follows from:
--   * inside paneru, queries only answer within a `paneru.on` handler or
--     `paneru.bind` callback — at script top level there is no world to read
--     and the call errors. Over the socket (sketchybar's side) they answer
--     whenever the daemon is up, and fail cleanly when it is not, which is what
--     lets sketchybar paint itself at config load.
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

local sbar = _G.sbar or require("sketchybar")
local colors = require("colors")
local icon_map = require("icon_map")

local M = {}

-- The state API, whichever host this is loaded in: paneru's runtime sets a
-- `paneru` global; everyone else loads the client module, which talks to the
-- daemon over its socket. nil (module absent) degrades to "draw nothing"
-- rather than erroring out of the caller's config load.
local api = _G.paneru
if api == nil then
	local ok, module = pcall(require, "paneru")
	if ok then
		api = module
	end
end

-- Displays that show the minimal secondary bar (front-app title only). Must
-- match sketchybar/items/secondary.lua's `secondary_displays`; that list lives
-- in a global sketchybar's own config sets, which is only reachable when this
-- runs inside sketchybar — hence the fallback for paneru's process.
local secondary_displays = { "2" }

-- Rows and per-row icon slots are a fixed pool: bracket membership is fixed at
-- creation time, and pre-creating every slot is what lets rendering be pure
-- `set` calls from either process. Rows beyond MAX_WORKSPACES and apps beyond
-- MAX_APPS are dropped rather than growing the bar without bound (the keybinds
-- in aspects/wms/paneru/default.nix reach row 10).
M.MAX_WORKSPACES = 10
local MAX_APPS = 8

local function row_item(number)
	return "space." .. number
end

local function app_item(number, index)
	return "space." .. number .. ".app." .. index
end

local function bracket_item(number)
	return "bracket.space." .. number
end

-- Last item in creation (= bar) order, what chevron/front_app anchor after.
M.last_item = app_item(M.MAX_WORKSPACES, MAX_APPS)

-- Absent optional fields come back as mlua's JSON-null lightuserdata, which is
-- truthy in Lua and compares equal to nothing useful; every optional field read
-- off a query result goes through here first.
local function nn(value)
	if value == nil or type(value) == "userdata" then
		return nil
	end
	return value
end

-- One slice of paneru's state, or nil if the query can't answer (daemon not
-- running, called outside a handler in paneru's own runtime, or the world
-- couldn't be extracted).
local function query(kind)
	if api == nil then
		return nil
	end
	local ok, result = pcall(api.query_json, kind)
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

-- Full colour = this app has a window on screen right now; dimmed = all of its
-- windows on this row are minimized/hidden.
local function app_icon_color(visible)
	if visible then
		return colors.foreground
	end
	return colors.with_alpha(colors.foreground, 0.4)
end

-- Re-anchor chevron/front_app after the last workspace item. Inside sketchybar
-- that's items/left.lua's own global; from paneru's process it's the same two
-- `sketchybar --move` shell calls, safe to issue from any process.
local function reorder_left_items()
	if _G.reorder_left_items then
		_G.reorder_left_items(M.last_item)
		return
	end
	sbar.exec("sketchybar --move chevron after " .. M.last_item)
	sbar.exec("sketchybar --move front_app after chevron")
end

-- The bar items for every row: the row number, its icon slots, and the pill
-- bracketing them.
--
--   space.<n>            the row number (drawn only while the row is empty)
--   space.<n>.app.<i>    one app icon each, coloured by that app's visibility
--   bracket.space.<n>    the selection pill wrapping the two above
--
-- A row draws as several items because sketchybar gives one colour per label
-- string and each app icon needs its own. Hidden members take up no width, so
-- the pill still hugs what's actually drawn.
--
-- Only ever called from sketchybar's own config (see wm.lua) — items are
-- created inside its begin_config/end_config, and paneru's process only ever
-- `set`s them.
function M.create_items()
	for number = 1, M.MAX_WORKSPACES do
		-- Fire-and-forget CLI call, run by sketchybar's own process — no
		-- in-process callback round trip needed, unlike `sbar.subscribe`.
		-- Shared by the row and its app icons, so clicking any part of the
		-- pill jumps to that row.
		local click_script = "paneru send-cmd window virtualnum " .. tostring(number)

		sbar.add("item", row_item(number), {
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
			-- The pill is the bracket's, not this item's: it has to span the
			-- app icons too.
			background = { drawing = false, border_color = dummy_space.background.border_color },
			-- Hidden until a query says the row exists; the first paint
			-- follows immediately (M.render), so an unreachable daemon leaves
			-- an empty strip rather than ten phantom rows.
			drawing = false,
			click_script = click_script,
		})

		-- Icon slots, created in order right after the row item so bar order is
		-- creation order — no `--move` per redraw, only the one-off re-anchor
		-- of chevron/front_app below.
		local members = { row_item(number) }
		for i = 1, MAX_APPS do
			local item = app_item(number, i)
			sbar.add("item", item, {
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
			table.insert(members, item)
		end

		sbar.add("bracket", bracket_item(number), members, {
			background = get_background_style(false),
		})
	end

	_G.last_space_item = M.last_item
	reorder_left_items()
end

-- Repaint every row from one state snapshot. Pure `set` calls — the items are
-- whatever `create_items` made of them, so this is safe to run from paneru's
-- process too.
function M.render_workspaces()
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

	for _, ws in ipairs(workspaces) do
		local number = type(ws) == "table" and ws.number or nil
		if
			type(number) == "number"
			and number >= 1
			and number <= M.MAX_WORKSPACES
			and ws.native_workspace_id == active_native_id
		then
			present[number] = true

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
			sbar.set(row_item(number), {
				drawing = true,
				icon = { string = tostring(number), drawing = not has_apps },
				label = { drawing = false },
			})

			-- One slot per app, each coloured by that app's own visibility.
			for i = 1, MAX_APPS do
				local app = apps[i]
				if app then
					sbar.set(app_item(number, i), {
						drawing = true,
						icon = { string = app.icon, color = app_icon_color(app.visible) },
					})
				else
					sbar.set(app_item(number, i), { drawing = false })
				end
			end

			sbar.set(bracket_item(number), { background = get_background_style(selected) })
		end
	end

	-- Hide rows that don't exist on the active native space.
	for number = 1, M.MAX_WORKSPACES do
		if not present[number] then
			sbar.set(row_item(number), { drawing = false })
			for i = 1, MAX_APPS do
				sbar.set(app_item(number, i), { drawing = false })
			end
			sbar.set(bracket_item(number), { background = get_background_style(false) })
		end
	end

	reorder_left_items()
end

-- Focused-window title on front_app + the secondary display's title items.
function M.update_titles()
	local active = query("active")
	local title = ""
	if active and type(nn(active.focused_window_title)) == "string" then
		title = active.focused_window_title
	end
	sbar.set("front_app", { label = title })
	if _G.secondary_window_name_items then
		for _, item_name in ipairs(_G.secondary_window_name_items) do
			sbar.set(item_name, { label = title })
		end
	else
		for _, display_id in ipairs(secondary_displays) do
			sbar.set("window_name_" .. display_id, { label = title })
		end
	end
end

-- Everything a full resync needs: rows and titles both from the daemon's state.
function M.render()
	M.render_workspaces()
	M.update_titles()
end

return M
