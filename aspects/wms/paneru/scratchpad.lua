-- Named scratchpads for paneru, as a Lua module.
--
-- Shipped by den.aspects.paneru as the `paneru_scratchpad` module on *paneru's*
-- embedded Lua require path (services.paneru.extraLuaPackages, resolved via
-- $PANERU_LUA_PATH/$PANERU_LUA_CPATH) and driven from the generated init.lua by
-- a single `require("paneru_scratchpad").setup{...}` call. Like paneru_events it
-- runs inside paneru's process, on paneru's own Lua thread — `paneru.on`
-- handlers and `paneru.bind` callbacks are dispatched to that thread, so the
-- blocking `os.execute` in the spawn path delays only this script's own queue,
-- never window dragging or focus tracking.
--
-- A port of xmonad's XMonad.Util.NamedScratchpad. A pad is a window you toggle
-- in and out of view; `toggle` has xmonad's three branches in xmonad's order:
-- spawn if nothing matches, put it away if it is here, otherwise show it and
-- focus. `group` is addExclusives — showing one pad puts its group-mates away
-- first.
--
-- WHERE A HIDDEN PAD GOES, and why not xmonad's NSP workspace
--
-- xmonad parks an unwanted pad on a workspace you never look at, and paneru's
-- CONFIGURATION.md ports it that way. That does not survive contact with
-- paneru: a pad is a *floating* window, a parked one has to be tiled to go away
-- with its row, and `ws:sink(id):shift(id, row)` puts both of those in one
-- returned set. `apply_layout_ops` (src/ecs/layout_ops.rs) drains every batch in
-- a single system run, so the `Unmanaged` removal and the `VirtualMoveMarker`
-- flush together; `window_managed_trigger` then runs first, finds the window
-- landing in the *active* strip, and pins it with
-- `reposition_entity(entity, windows.origin(entity))` plus a
-- `VerifyWindowPosition` — at the float rect it currently occupies. The
-- workspace move lands a frame later and cannot shake that off. The pad ends up
-- belonging to the parked row while still painted over the visible one, where
-- nothing that acts on the active strip can reach it. Splitting the two ops
-- does not help: `paneru.windows` pushes onto the same outbox, so both commits
-- are still drained together.
--
-- So a hidden pad stays floating and is simply parked off the display instead —
-- `{ x = -1.0 }` in the same relative-rect coordinates, one full display-width
-- to the left, which is where paneru parks its own hidden workspace rows. One
-- op, no tiled/floating transition, no workspace transition, nothing queued for
-- a later frame to fight with. `visible` in the window record goes false on its
-- own, because paneru works that out from the frame.
--
-- Pads are declared as plain data (`bundle`/`app`/`title` as Rust-regex
-- strings, not predicates) so the whole spec can be generated with
-- lib.generators.toLua from Nix — a `paneru.match{...}` call is not data. The
-- predicates are built here, in `define`.
--
-- ALSO RUNS OUTSIDE THE DAEMON
--
-- Everything below the hooks is host-agnostic: `toggle` is a `function(ws)`
-- over the window set, and the window set, `paneru.match` and `paneru.state`
-- are spelled the same in paneru's loadable client module (crates/lua) as in
-- the embedded runtime. So ./scratchpad-cli.lua loads this same file under a
-- plain LuaJIT with `require("paneru")` as the global, calls `M.load` instead
-- of `M.setup` — pads defined, no hooks, no binds — and hands `M.toggle(name)`
-- to `paneru.windows`. That is the path kanata takes (`paneru-scratchpad
-- <name>`), since `Command::Lua` has no argv encoding and `send-cmd` therefore
-- cannot reach a `paneru.bind` callback.
--
-- Only three things are embedded-only, and each is guarded: `paneru.log`
-- (falls back to stderr), `paneru.bind` and `paneru.on` (registered by `setup`,
-- which the CLI does not call).
--
-- Nothing that has to survive a reload lives in this table. Saving init.lua
-- rebuilds paneru's interpreter and every Lua global with it, so the two pieces
-- of genuinely persistent state — which pads are meant to be on screen, and
-- which window the focus was on last — go in `paneru.state` (SHOWN and FOCUSED
-- below). The pad *declarations* are rebuilt from the script on every reload,
-- which is exactly right: they are part of the script.

local M = {
	pads = {},
	order = {},
	-- Chatter about every toggle decision, through paneru's own log. Off
	-- unless `setup{ debug = true }`; a mismatched `bundle` pattern is
	-- otherwise indistinguishable from a dead keybind, because both land in
	-- the spawn branch and do nothing visible.
	debug = false,
	-- How much of a parked pad is left past the edge, in pixels. Wants to be
	-- paneru's own `options.sliver_width` (which defaults to 5) — that is the
	-- width at which it stops calling a window visible, and matching it puts a
	-- parked pad exactly where paneru parks a window on a row you are not
	-- looking at.
	sliver = 5,
}

-- `paneru.state` keys. Values are JSON-shaped and the store is shared with the
-- CLI (`paneru state get scratchpad.shown`) and the client module, so the names
-- are namespaced rather than bare.
--
--   SHOWN    pad name -> true, for the pads that are meant to be on screen.
--   FOCUSED  the window id of the last window_focused, for hide_on_focus_loss.
local SHOWN = "scratchpad.shown"
local FOCUSED = "scratchpad.focused"

-- `paneru.log` is the embedded runtime's; the client module has no logging of
-- its own, so under scratchpad-cli.lua this goes to stderr instead — where the
-- shell that ran it (or kanata's log) can pick it up.
local function log(...)
	if not M.debug then
		return
	end
	local line = "scratchpad: " .. table.concat({ ... }, " ")
	if paneru.log then
		paneru.log(line)
	else
		io.stderr:write(line, "\n")
	end
end

local function wants(name)
	local shown = paneru.state.get(SHOWN)
	return shown ~= nil and shown[name] == true
end

-- `mutate` rather than get-then-set: the toggle callback and the focus hooks
-- can all be writing this, and a lost write is a pad stuck off screen.
local function want(name, shown)
	paneru.state.mutate(SHOWN, function(current)
		current = current or {}
		current[name] = shown or nil
		return current
	end)
end

-- Where a hidden pad waits: exactly where paneru puts its own hidden windows.
--
-- Off the left, at the pad's own size and its own `y`, shifted so that only
-- `sliver` pixels of it remain past the display's left edge. That is what a
-- window on a workspace row you are not looking at does — Messages parked at
-- `x = -1317` with a width of 1322 has its right edge at +5 — and it is not an
-- accident that it stops there rather than going further:
--
--   * macOS tolerates a window sitting a little past an edge but clamps a big
--     overshoot back onto the screen, so aiming far off (`x = -1.0`, or the
--     bottom-right corner) comes back as a large visible chunk. Aiming just
--     past the edge is accepted.
--   * `window_visibility` (src/ecs/state.rs) calls a window hidden once its
--     overlap with every display is no wider than `sliver_width`, so a sliver
--     is all it takes. That is also what `toggle` reads back through
--     `window.visible` to decide which way it is going.
--
-- The `y` and the size are the pad's own, so a toggle only ever moves the
-- window sideways — it never resizes it, and it never drops it down the screen.
--
-- Fractions of the display are all `ws:float` takes, so the sliver — a pixel
-- count — has to be converted against the display the window is actually on.
-- `ws:display_of` gives those bounds. Without them, land the right edge exactly
-- on the display's left edge, which is the same thing with a zero sliver.
--
-- COSTS AN ANIMATION. paneru's `animate_entities` interpolates every reposition,
-- so parking here is not a jump — it is a slide of roughly a full window width,
-- paid again in the other direction on the way back out. At the aspect's old
-- `animation_speed = 20` that measured ~300ms per direction over 22-29 frames,
-- with an ease-out tail that spends its last third crawling the final few
-- pixels; the pad is unusable for the whole of it. Because a parked pad is
-- further from its shown rect than anything else paneru moves, the scratchpads
-- feel the animation setting harder than the tiling does — which is why
-- aspects/wms/paneru/default.nix now runs `animation_speed = 500`.
function M.parked_rect(display, rect)
	local x = -rect.width
	if display and display.width and display.width > 0 then
		x = M.sliver / display.width - rect.width
	end
	return { x = x, y = rect.y, width = rect.width, height = rect.height }
end

-- `spec` is the declarative pad: { bundle?, app?, title?, spawn, group?,
-- float?, key? }. The match fields are Rust `regex` patterns, compiled here
-- into the predicate `pad.match` the rest of the module uses.
function M.define(name, spec)
	local pad = {
		spawn = spec.spawn,
		group = spec.group,
		float = spec.float,
		key = spec.key,
		match = paneru.match({
			app = spec.app,
			bundle = spec.bundle,
			title = spec.title,
		}),
	}
	M.pads[name] = pad
	table.insert(M.order, name)
	return pad
end

-- The pad a window belongs to, if any. Declaration order decides ties.
function M.pad_of(window)
	for _, name in ipairs(M.order) do
		if M.pads[name].match(window) then
			return name, M.pads[name]
		end
	end
end

-- Put one pad away. `ws:float` is what carries a rect — the layout engine owns
-- a tiled window's geometry and puts a bare SetFrame straight back — and the
-- window is floating already, so this is a reposition and nothing more.
function M.park(ws, pad, id)
	return ws:float(id, M.parked_rect(ws:display_of(id), pad.float))
end

-- Bring one pad out at its rect and focus it.
function M.show(ws, pad, id)
	return ws:float(id, pad.float):focus(id)
end

-- Put away every pad in `names` that is currently on screen.
function M.hide(ws, names)
	for _, name in ipairs(names) do
		local pad = M.pads[name]
		local window = ws:find(pad.match)
		if window and window.visible then
			want(name, false)
			ws = M.park(ws, pad, window.id)
		end
	end
	return ws
end

function M.hide_all(ws)
	return M.hide(ws, M.order)
end

-- Everything declared in the same group as `name`, except itself.
function M.group_of(name)
	local group, mine = {}, M.pads[name].group
	if not mine then
		return group
	end
	for _, other in ipairs(M.order) do
		if other ~= name and M.pads[other].group == mine then
			table.insert(group, other)
		end
	end
	return group
end

-- `window.visible` rather than the stored intent decides which way the toggle
-- goes: paneru derives it from the frame, so it is the one answer that cannot
-- drift from what is actually on the screen.
function M.toggle(name)
	return function(ws)
		local pad = M.pads[name]
		local window = ws:find(pad.match)
		if not window then
			-- Not running: start it, and say so, so `focus_hook` floats
			-- the window at the pad's rect when it lands.
			log(name, "no window matches; spawning")
			want(name, true)
			os.execute(pad.spawn .. " &")
			return
		end
		if window.visible then
			log(name, "window", window.id, "is on screen; parking it")
			want(name, false)
			return M.park(ws, pad, window.id)
		end
		log(name, "showing window", window.id)
		ws = M.hide(ws, M.group_of(name))
		want(name, true)
		return M.show(ws, pad, window.id)
	end
end

-- Place a pad at its rect when it takes the focus — xmonad's per-pad `hook` and
-- its customFloating.
--
-- Three things arrive here. A pad window appearing for the first time, which is
-- tiled and has to be floated (paneru has no window-created event for scripts —
-- the window has no id yet at that point — so first focus is the hook xmonad's
-- manage hook becomes). A pad the toggle just showed, which is already floating
-- and wanted, and returns without touching the set. And a pad that got the
-- focus while parked: cmd-tabbing to it, clicking its Dock icon, or an app like
-- 1Password that makes itself frontmost on a timer. Treating that last one as a
-- summon is what stops a self-raising app from sitting off screen with the
-- focus on it — you asked for it, so it comes out.
local function focus_hook(event, ws)
	local window = ws:window(event.window_id)
	if not window then
		return
	end
	local name, pad = M.pad_of(window)
	if not pad or not pad.float then
		return
	end
	if wants(name) and window.floating then
		return -- already out, and already floating at its rect
	end
	log(name, "placing window", window.id)
	want(name, true)
	return ws:float(window.id, pad.float)
end

-- Put a pad away when the focus leaves it (xmonad's nsHideOnFocusLoss).
--
-- The window that was focused a moment ago is the one thing here that has to
-- outlive a reload of init.lua, so it lives in `paneru.state` rather than in a
-- Lua global — a reload would otherwise lose the focus this is comparing
-- against and let a pad sit on screen until the next focus change.
local function focus_loss_hook(event, ws)
	local previous = paneru.state.get(FOCUSED)
	paneru.state.set(FOCUSED, event.window_id)
	if not previous or previous == event.window_id then
		return
	end
	local window = ws:window(previous)
	if not window then
		return
	end
	local name, pad = M.pad_of(window)
	-- Only a pad we actually asked for: one the toggle just parked is already
	-- where it belongs.
	if not pad or not pad.float or not wants(name) then
		return
	end
	log(name, "focus left window", previous, "; parking it")
	want(name, false)
	return M.park(ws, pad, previous)
end

-- The single entry point, mirroring `paneru.setup{...}`'s shape:
--
--   { hide_on_focus_loss = true, debug = false,
--     order = { "discord", ... },
--     pads = { discord = { bundle = "...", spawn = "...", key = "alt - d" } } }
--
-- `order` is explicit because a generated Lua table is unordered and
-- declaration order is what breaks `pad_of` ties.
--
-- Both hooks register for window_focused; paneru commits each handler's
-- returned set independently, so they compose with one another and with
-- paneru_events' own window_focused handler without any of them knowing about
-- the others. They act on different windows — the one that just took the focus,
-- and the one that just lost it — so they never contradict each other.
--
-- `load` is the half of this that needs no host: the declarations, and nothing
-- registered. scratchpad-cli.lua calls it (see the header) — a one-shot process
-- has nothing to hook and nothing to bind, and `paneru.on`/`paneru.bind` do not
-- exist in the client module anyway.
function M.load(spec)
	M.debug = spec.debug or false
	M.sliver = spec.sliver or M.sliver
	-- Declarations are rebuilt from the spec, never added to: `define` appends
	-- to `M.order`, and a second `load` in the same interpreter would otherwise
	-- leave every pad in it twice.
	M.pads, M.order = {}, {}

	for _, name in ipairs(spec.order or {}) do
		M.define(name, spec.pads[name])
	end

	return M
end

function M.setup(spec)
	M.load(spec)

	for _, name in ipairs(M.order) do
		local pad = M.pads[name]
		if pad.key then
			paneru.bind(pad.key, M.toggle(name))
		end
	end

	paneru.on("window_focused", focus_hook)
	if spec.hide_on_focus_loss then
		paneru.on("window_focused", focus_loss_hook)
	end

	return M
end

return M
