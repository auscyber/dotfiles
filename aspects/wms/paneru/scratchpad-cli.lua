-- `paneru-scratchpad` — the named scratchpads of ./scratchpad.lua, driven from
-- outside paneru's process so that kanata can reach them.
--
-- WHY THIS EXISTS
--
-- The pads are Lua callbacks registered with `paneru.bind`, and `Command::Lua`
-- has no argv encoding: nothing sent over paneru's socket with `paneru
-- send-cmd` can name a Lua callback, so a keybind daemon has no way to ask for
-- a pad by name. The workaround in aspects/wms/paneru/default.nix was to have
-- kanata emit the bare `alt + shift - <letter>` chord and let paneru's own
-- event tap match the binding — which works, but means the chord has to stay
-- typeable, and puts the pad names in two places.
--
-- This is the other half of the API instead. paneru's loadable client module
-- (`require("paneru")`, crates/lua/src/client.rs) speaks the same window-set
-- protocol over the socket that a `paneru.on` handler is given in-process:
-- `paneru.windows(fn)` fetches the set, runs `fn` on it, and commits whatever
-- ops come back. `paneru.match` and `paneru.state` are likewise the same names
-- against the same store. So ./scratchpad.lua's `toggle` — a plain
-- `function(ws)` — runs here unchanged, and kanata calls
-- `paneru-scratchpad discord` like any other command.
--
-- The in-process binds stay registered alongside this: the chords keep working
-- with paneru alone, and both paths end in the same `toggle` over the same
-- `scratchpad.shown` state, so they cannot disagree about where a pad is.
--
-- WHAT IS STUBBED
--
-- The toggle path is real and complete. What a one-shot process cannot do is
-- the reactive half: `focus_hook` (place a pad at its rect when it first
-- appears) and `focus_loss_hook` (hide_on_focus_loss) are `paneru.on`
-- handlers, and they stay in the daemon where the events are. Running paneru
-- without its Lua feature would lose them; this program does not replace
-- `require("paneru_scratchpad").setup{...}` in init.lua.

-- ./scratchpad.lua reads `paneru` as a global, the way the embedded runtime
-- hands it over. Here it is the client module, resolved through LUA_CPATH.
_G.paneru = require("paneru")

local scratchpad = require("paneru_scratchpad")
-- The same `{ order = ..., pads = ... }` table init.lua passes to `setup`,
-- generated once by the aspect and shipped as its own module so the two hosts
-- cannot drift. `load`, not `setup`: no hooks, no binds.
local spec = require("paneru_scratchpad_spec")
scratchpad.load(spec)

local program = "paneru-scratchpad"

local function die(message)
	io.stderr:write(program, ": ", message, "\n")
	os.exit(1)
end

local function names()
	return table.concat(scratchpad.order, ", ")
end

local function pad_named(name)
	if not name then
		die("expected a pad name (one of: " .. names() .. ")")
	end
	if not scratchpad.pads[name] then
		die("no such pad '" .. name .. "' (have: " .. names() .. ")")
	end
	return scratchpad.pads[name]
end

-- Each verb is a `function(ws)` over the window set, exactly as the in-process
-- handlers are, so `paneru.windows` commits them the same way.
local verbs = {}

function verbs.toggle(name)
	local _ = pad_named(name)
	return scratchpad.toggle(name)
end

function verbs.show(name)
	local pad = pad_named(name)
	return function(ws)
		local window = ws:find(pad.match)
		if not window then
			-- Same branch as `toggle`'s: start it, and let the daemon's
			-- focus_hook place it when the window lands.
			paneru.state.mutate("scratchpad.shown", function(shown)
				shown = shown or {}
				shown[name] = true
				return shown
			end)
			os.execute(pad.spawn .. " &")
			return
		end
		ws = scratchpad.hide(ws, scratchpad.group_of(name))
		return scratchpad.show(ws, pad, window.id)
	end
end

function verbs.hide(name)
	local _ = pad_named(name)
	return function(ws)
		return scratchpad.hide(ws, { name })
	end
end

verbs["hide-all"] = function()
	return function(ws)
		return scratchpad.hide_all(ws)
	end
end

-- Read-only: what each pad matches, and where it is right now. Returning
-- nothing means `paneru.windows` commits nothing.
function verbs.list()
	return function(ws)
		for _, name in ipairs(scratchpad.order) do
			local window = ws:find(scratchpad.pads[name].match)
			local where = "not running"
			if window then
				where = window.visible and "shown" or "hidden"
			end
			print(string.format("%-14s %s", name, where))
		end
	end
end

local function usage()
	io.stderr:write(([[
usage: %s <name>                 toggle a scratchpad
       %s toggle|show|hide <name>
       %s hide-all
       %s list

pads: %s
]]):format(program, program, program, program, names()))
	os.exit(2)
end

local first = arg[1]
if not first or first == "-h" or first == "--help" then
	usage()
end

-- `paneru-scratchpad discord` is `paneru-scratchpad toggle discord`: the bare
-- form is what kanata calls, and toggling is the only thing a keybind wants.
local verb, name = "toggle", first
if verbs[first] then
	verb, name = first, arg[2]
end

local ok, err = pcall(function()
	paneru.windows(verbs[verb](name))
end)
if not ok then
	-- Almost always a daemon that is not running: the client module reports a
	-- refused connect to /tmp/paneru.socket (or $PANERU_SOCKET) as a Lua error.
	die(tostring(err))
end
