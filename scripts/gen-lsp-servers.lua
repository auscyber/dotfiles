-- Read the spawn contract out of nvim-lspconfig by *evaluating* it: every
-- `lsp/<name>.lua` is a lua chunk that returns a `vim.lsp.Config`, so the honest
-- way to learn what it spawns is to load it in a real nvim and look at the table
-- -- and, where `cmd` is a function that lspconfig builds at runtime, to call it
-- with the rpc entry points stubbed out and record the argv it was about to run.
-- Nothing here greps the source.
--
-- Everything derived is derived the same way: the filetype -> extension map
-- comes off nvim's own `extension` table (reached through the upvalue of
-- `vim.filetype.match`), not off a parse of filetype.lua.
--
-- The reader's own machine is kept out of the result by evaluating each config
-- in a fake world: HOME, the XDG dirs and TMPDIR are pointed at dummy paths, and
-- the globals no env var reaches (`vim.fn.tempname`, `vim.fn.getcwd`,
-- `vim.uv.os_homedir`, `vim.uv.os_tmpdir`) are overwritten with the same. Every
-- path a config then builds is made of pieces we planted, so they can be
-- replaced straight back out on the way to the output: powershell_es' log path
-- comes out as `@XDG_CACHE_HOME@/nvim/powershell_es.log`, a token to substitute
-- rather than someone's home directory.
--
-- Each config is evaluated TWICE, in two worlds whose dummy paths differ. What
-- survives substitution identically is upstream's; what still differs is derived
-- from something we did not plant -- the project being edited (jdtls' per-root
-- `-data <workspace>`), a tempname, a pid -- and is dropped with a note rather
-- than frozen into the table.
--
-- Driven by scripts/gen-lsp-servers.sh; see there for the CLI.

local opts = {
  src = nil,
  format = 'json',
  only = nil,
  extensions = true,
  fields = nil, -- nil = every field the format emits by default
}

local argv_in = _G.arg or {}
local i = 1
while i <= #argv_in do
  local a = argv_in[i]
  local function next_arg()
    i = i + 1
    return argv_in[i]
  end
  local function set_of(csv)
    local set = {}
    for word in tostring(csv):gmatch('[^,%s]+') do
      set[word] = true
    end
    return set
  end

  if a == '--src' then
    opts.src = next_arg()
  elseif a == '--format' then
    opts.format = next_arg()
  elseif a == '--only' then
    opts.only = set_of(next_arg())
  elseif a == '--fields' then
    opts.fields = set_of(next_arg())
  elseif a == '--no-extensions' then
    opts.extensions = false
  else
    io.stderr:write('unknown argument: ' .. a .. '\n')
    os.exit(2)
  end
  i = i + 1
end

if not opts.src or vim.fn.isdirectory(opts.src .. '/lsp') == 0 then
  io.stderr:write('--src must point at an nvim-lspconfig checkout (no lsp/ under ' .. tostring(opts.src) .. ')\n')
  os.exit(2)
end

if opts.format ~= 'json' and opts.format ~= 'nix' then
  io.stderr:write('--format must be json or nix\n')
  os.exit(2)
end

-- lspconfig's own lua/ (its util helpers) has to be findable: a few configs require it.
vim.opt.runtimepath:append(opts.src)

--------------------------------------------------------------------------------
-- Evaluating a config without letting it spawn anything
--------------------------------------------------------------------------------

local SENTINEL = 'lspmux-gen-captured-cmd'

-- The two rpc entry points are replaced for the whole run rather than around
-- each call: a config can hand back a `cmd` it built at *load* time (gdscript
-- keeps a `vim.lsp.rpc.connect` handle at the top of the file), which a stub
-- installed later would miss -- and the real one would then go looking for a
-- running godot. They stay inert until a capture is armed, so nothing else in
-- this nvim gets its own lsp client broken.
local capture, armed
local real_start, real_connect = vim.lsp.rpc.start, vim.lsp.rpc.connect

vim.lsp.rpc.start = function(...)
  if not armed then
    return real_start(...)
  end
  capture = { argv = (select(1, ...)) }
  error(SENTINEL, 0)
end

vim.lsp.rpc.connect = function(host, port)
  local real = real_connect(host, port)
  return function(...)
    if not armed then
      return real(...)
    end
    capture = { tcp = { host = tostring(host), port = tostring(port) } }
    error(SENTINEL, 0)
  end
end

--- Everything a config can reach for that belongs to the reader's machine
--- rather than to nvim-lspconfig, and the token it is replaced with on the way
--- out. The env vars are planted in the environment; the rest are globals with
--- no env var behind them, so they get overwritten for the duration instead.
local SLOTS = {
  { name = 'HOME', env = true },
  { name = 'XDG_CACHE_HOME', env = true },
  { name = 'XDG_CONFIG_HOME', env = true },
  { name = 'XDG_DATA_HOME', env = true },
  { name = 'XDG_STATE_HOME', env = true },
  { name = 'XDG_RUNTIME_DIR', env = true },
  { name = 'TMPDIR', env = true },
  { name = 'TEMPFILE', global = { vim.fn, 'tempname' } },
  { name = 'CWD', global = { vim.fn, 'getcwd' } },
  { name = 'HOME', global = { vim.uv, 'os_homedir' } },
  { name = 'TMPDIR', global = { vim.uv, 'os_tmpdir' } },
}

--- One fake world to evaluate a config in. Two of these, differing in every
--- planted path, is what separates "upstream passes this flag" from "this is
--- where my cache happens to live".
local function make_probe(tag, project)
  local base = '/lspmux-gen/' .. tag
  local probe = { env = {}, globals = {}, subs = {}, root_dir = base .. '/' .. project }

  local function plant(name)
    local value = base .. '/' .. name
    probe.subs[#probe.subs + 1] = { value = value, token = '@' .. name .. '@' }
    return value
  end

  for _, slot in ipairs(SLOTS) do
    local value = plant(slot.name)
    if slot.env then
      probe.env[slot.name] = value
    else
      probe.globals[#probe.globals + 1] = {
        tbl = slot.global[1],
        key = slot.global[2],
        value = function()
          return value
        end,
      }
    end
  end

  -- The project root is planted too, but deliberately with a different basename
  -- per probe: a value derived from *which* project is open is not a constant of
  -- the cmd, and has to come out as a difference rather than as a token.
  probe.subs[#probe.subs + 1] = { value = probe.root_dir, token = '@ROOT@' }
  return probe
end

local probes = { make_probe('probe-one', 'project-alpha'), make_probe('probe-two', 'project-beta') }

--- Run `fn` inside the probe's world, then put the real one back. `vim.fn.stdpath`
--- re-reads the XDG vars on every call, so planting them is enough to move every
--- cache/log/session path a config might build.
local function with_env(probe, fn)
  local saved_env, saved_globals = {}, {}
  for key, value in pairs(probe.env) do
    saved_env[key] = vim.env[key]
    vim.env[key] = value
  end
  for idx, override in ipairs(probe.globals) do
    saved_globals[idx] = override.tbl[override.key]
    override.tbl[override.key] = override.value
  end

  local results = { pcall(fn) }

  for key in pairs(probe.env) do
    vim.env[key] = saved_env[key]
  end
  for idx, override in ipairs(probe.globals) do
    override.tbl[override.key] = saved_globals[idx]
  end
  return unpack(results)
end

--- Put the tokens back where this probe's planted paths ended up.
local function substitute(word, probe)
  for _, sub in ipairs(probe.subs) do
    word = word:gsub(vim.pesc(sub.value), sub.token)
  end
  return word
end

local function substituted(argv, probe)
  local out_argv = {}
  for idx, word in ipairs(argv) do
    out_argv[idx] = substitute(word, probe)
  end
  return out_argv
end

--- Load `lsp/<name>.lua` and return the config table it evaluates to.
local function eval_config(path, probe)
  local ok, result = with_env(probe, function()
    local chunk, load_err = loadfile(path)
    if not chunk then
      error(load_err, 0)
    end
    return chunk()
  end)
  if not ok then
    return nil, tostring(result)
  end
  if type(result) ~= 'table' then
    return nil, 'config did not return a table'
  end
  return result
end

--- What this config would spawn in this probe's world: `{ argv = {...} }`, or
--- `{ tcp = ... }` for a server you are expected to have already running.
---
--- A runtime `cmd` is handed the server's own table (glint reads its declared
--- `init_options` back out of it) plus a `root_dir` that does not exist, so the
--- "prefer the project's node_modules/.bin copy" branch every JS server has
--- falls through to the bare binary name -- which is exactly the name the lspmux
--- shim shadows on $PATH.
local function spawn_of(cfg, probe, name)
  local cmd = cfg.cmd

  if type(cmd) == 'table' then
    local argv = {}
    for _, word in ipairs(cmd) do
      argv[#argv + 1] = tostring(word)
    end
    return { argv = argv, dynamic = false }
  end

  if type(cmd) ~= 'function' then
    return nil, 'lspconfig ships no cmd; it has to be set by hand'
  end

  local client_config = { name = name, root_dir = probe.root_dir, detached = false }
  for key, value in pairs(cfg) do
    if key ~= 'cmd' and client_config[key] == nil then
      client_config[key] = value
    end
  end

  capture, armed = nil, true
  local ok, err = with_env(probe, function()
    return cmd({}, client_config)
  end)
  armed = false

  if capture and capture.tcp then
    return { tcp = capture.tcp, dynamic = true }
  end
  if capture then
    local argv = {}
    for _, word in ipairs(capture.argv) do
      argv[#argv + 1] = tostring(word)
    end
    return { argv = argv, dynamic = true }
  end
  return nil, 'could not evaluate cmd: ' .. (ok and 'it returned without spawning' or tostring(err))
end

--- filetype -> sorted extensions, read off nvim's own extension table. It is a
--- local in filetype.lua, but `vim.filetype.match` closes over it, so it is
--- reachable as an upvalue -- the live table, no parse of the source. Entries
--- whose value is a detect function are resolved by asking `vim.filetype.match`;
--- the ones that need the buffer's contents to decide (`.h` -> c or cpp) answer
--- nil there and are left out.
local function extensions_by_filetype()
  local extension
  for idx = 1, 64 do
    local name, value = debug.getupvalue(vim.filetype.match, idx)
    if not name then
      break
    end
    if name == 'extension' and type(value) == 'table' then
      extension = value
      break
    end
  end
  if not extension then
    io.stderr:write("warning: nvim's extension table moved; skipping extensionToLanguage\n")
    return {}
  end

  local by_ft = {}
  for ext, ft in pairs(extension) do
    if type(ft) ~= 'string' then
      local ok, matched = pcall(vim.filetype.match, { filename = 'lspmuxprobe.' .. ext })
      ft = ok and matched or nil
    end
    if type(ft) == 'string' then
      by_ft[ft] = by_ft[ft] or {}
      table.insert(by_ft[ft], ext)
    end
  end
  for _, exts in pairs(by_ft) do
    table.sort(exts)
  end
  return by_ft
end

--------------------------------------------------------------------------------
-- One server
--------------------------------------------------------------------------------

--- Everything the generator knows about one `lsp/<name>.lua`, from two
--- evaluations of it. A word that survived substitution identically in both
--- worlds is upstream's and is kept -- tokens and all. A word that still differs
--- came from something we could not plant, and is dropped with a note.
local function read_server(name, path)
  local entry = { filetypes = {}, notes = {}, placeholders = {} }

  local cfg, err = eval_config(path, probes[1])
  if not cfg then
    return nil, err
  end

  for _, ft in ipairs(cfg.filetypes or {}) do
    if type(ft) == 'string' then
      entry.filetypes[#entry.filetypes + 1] = ft
    end
  end

  local first, why = spawn_of(cfg, probes[1], name)
  if not first then
    entry.notes[#entry.notes + 1] = why
    return entry
  end
  if first.tcp then
    entry.notes[#entry.notes + 1] =
      ('connects to a server already listening on %s:%s, spawns nothing'):format(first.tcp.host, first.tcp.port)
    return entry
  end
  if #first.argv == 0 then
    entry.notes[#entry.notes + 1] = 'lspconfig ships an empty cmd; it has to be set by hand'
    return entry
  end

  local argv = substituted(first.argv, probes[1])

  entry.dynamicCmd = first.dynamic
  entry.cmd = argv
  entry.exe = vim.fs.basename(argv[1])
  entry.args = vim.list_slice(argv, 2)

  local other = eval_config(path, probes[2])
  local second = other and spawn_of(other, probes[2], name)
  if not second or not second.argv then
    entry.notes[#entry.notes + 1] = 'the second evaluation disagreed; treat exe/args as unconfirmed'
    return entry
  end
  local other_argv = substituted(second.argv, probes[2])

  if other_argv[1] ~= argv[1] then
    entry.notes[#entry.notes + 1] = 'the binary is resolved from the environment: ' .. table.concat(argv, ' ')
  end

  for idx = 2, math.max(#argv, #other_argv) do
    if argv[idx] ~= other_argv[idx] then
      entry.notes[#entry.notes + 1] = 'args vary with the project, left out: ' .. table.concat(argv, ' ')
      entry.args = {}
      break
    end
  end

  -- A `nil` that reached the cmd is upstream stringifying a value it expects the
  -- reader to have configured (powershell_es' bundle path). Worth saying so.
  for _, word in ipairs(entry.args) do
    if word:find('%f[%w]nil%f[%W]') then
      entry.notes[#entry.notes + 1] = 'an unset config value stringified to `nil` in the cmd; set it before use'
      break
    end
  end

  -- Whatever tokens are left in what we kept have to be substituted by whoever
  -- spawns this, so say which.
  local seen = {}
  for _, word in ipairs({ entry.exe, unpack(entry.args) }) do
    for token in word:gmatch('@[A-Z_]+@') do
      if not seen[token] then
        seen[token] = true
        entry.placeholders[#entry.placeholders + 1] = token
      end
    end
  end
  table.sort(entry.placeholders)
  if #entry.placeholders > 0 then
    entry.notes[#entry.notes + 1] = 'substitute before spawning: ' .. table.concat(entry.placeholders, ' ')
  end

  return entry
end

--------------------------------------------------------------------------------
-- Collect
--------------------------------------------------------------------------------

local by_ft = opts.extensions and extensions_by_filetype() or {}

local names = {}
for file, kind in vim.fs.dir(opts.src .. '/lsp') do
  local stem = kind == 'file' and file:match('^(.+)%.lua$')
  if stem and (not opts.only or opts.only[stem]) then
    names[#names + 1] = stem
  end
end
table.sort(names)

local servers = {}
for _, name in ipairs(names) do
  local entry, err = read_server(name, opts.src .. '/lsp/' .. name .. '.lua')
  if not entry then
    io.stderr:write(('skip %s: %s\n'):format(name, err))
  else
    if opts.extensions then
      local map = {}
      for _, ft in ipairs(entry.filetypes) do
        for _, ext in ipairs(by_ft[ft] or {}) do
          map['.' .. ext] = ft
        end
      end
      entry.extensionToLanguage = map
    end
    servers[name] = entry
  end
end

--------------------------------------------------------------------------------
-- Emit
--------------------------------------------------------------------------------

--- Which fields an emitter writes. `--fields exe,args,dynamicCmd` is the subset
--- the lspmux submodule actually declares.
local function want(field)
  if opts.fields then
    return opts.fields[field] == true
  end
  return true
end

-- Both emitters sort their keys: the output is diffed and committed, so it has
-- to be stable across runs and independent of lua's table order.
local function sorted_keys(tbl)
  local keys = {}
  for key in pairs(tbl) do
    keys[#keys + 1] = key
  end
  table.sort(keys)
  return keys
end

local out = {}
local function emit(line)
  out[#out + 1] = line
end

local function json_list(list)
  local parts = {}
  for _, value in ipairs(list) do
    parts[#parts + 1] = vim.json.encode(value)
  end
  return '[' .. table.concat(parts, ', ') .. ']'
end

local function emit_json()
  local emitted = sorted_keys(servers)
  emit('{')
  for idx, name in ipairs(emitted) do
    local e = servers[name]
    local fields = {}
    local function field(key, value)
      fields[#fields + 1] = ('    %s: %s'):format(vim.json.encode(key), value)
    end

    if e.exe then
      if want('exe') then
        field('exe', vim.json.encode(e.exe))
      end
      if want('args') then
        field('args', json_list(e.args))
      end
      if want('dynamicCmd') then
        field('dynamicCmd', tostring(e.dynamicCmd))
      end
      if want('cmd') then
        field('cmd', json_list(e.cmd))
      end
    end
    if want('filetypes') then
      field('filetypes', json_list(e.filetypes))
    end
    if e.extensionToLanguage and want('extensionToLanguage') then
      local pairs_out = {}
      for _, ext in ipairs(sorted_keys(e.extensionToLanguage)) do
        pairs_out[#pairs_out + 1] =
          ('%s: %s'):format(vim.json.encode(ext), vim.json.encode(e.extensionToLanguage[ext]))
      end
      field('extensionToLanguage', '{' .. table.concat(pairs_out, ', ') .. '}')
    end
    if e.placeholders and #e.placeholders > 0 and want('placeholders') then
      field('placeholders', json_list(e.placeholders))
    end
    if #e.notes > 0 and want('notes') then
      field('notes', json_list(e.notes))
    end

    emit(('  %s: {'):format(vim.json.encode(name)))
    emit(table.concat(fields, ',\n'))
    emit('  }' .. (idx < #emitted and ',' or ''))
  end
  emit('}')
end

local function nix_str(s)
  return '"' .. s:gsub('\\', '\\\\'):gsub('"', '\\"'):gsub('\n', '\\n'):gsub('%$', '\\$') .. '"'
end

--- One line while it fits, one item per line once it does not -- roughly what
--- nixfmt would do to the result anyway.
local function nix_list(list, indent)
  if #list == 0 then
    return '[ ]'
  end
  local parts = {}
  for _, value in ipairs(list) do
    parts[#parts + 1] = nix_str(value)
  end
  local one_line = '[ ' .. table.concat(parts, ' ') .. ' ]'
  if #one_line + #indent <= 96 then
    return one_line
  end
  return '[\n' .. indent .. '  ' .. table.concat(parts, '\n' .. indent .. '  ') .. '\n' .. indent .. ']'
end

local function emit_nix()
  emit('# Generated by scripts/gen-lsp-servers.sh from nvim-lspconfig. Do not edit.')
  emit('# Source: ' .. opts.src)
  emit('#')
  emit('# The spawn contract only -- `package` (and the editor-side names) stay')
  emit('# hand-written, since nvim-lspconfig does not know them. `filetypes` is not')
  emit('# an option on the lspmux submodule: drop it (--fields exe,args,dynamicCmd)')
  emit('# or declare it before merging an entry in as-is.')
  emit('#')
  emit('# An `@XDG_CACHE_HOME@`/`@HOME@`/`@TMPDIR@`/... token in an arg is a path')
  emit('# lspconfig builds from the environment; substitute it before spawning.')
  emit('{')
  for _, name in ipairs(sorted_keys(servers)) do
    local e = servers[name]
    emit(('  %s = {'):format(name:match('^[%a_][%w_%-]*$') and name or nix_str(name)))
    for _, note in ipairs(e.notes) do
      emit('    # ' .. note)
    end
    if e.exe then
      if want('exe') then
        emit(('    exe = %s;'):format(nix_str(e.exe)))
      end
      if #e.args > 0 and want('args') then
        emit(('    args = %s;'):format(nix_list(e.args, '    ')))
      end
      if e.dynamicCmd and want('dynamicCmd') then
        emit('    dynamicCmd = true;')
      end
    end
    if want('filetypes') then
      emit(('    filetypes = %s;'):format(nix_list(e.filetypes, '    ')))
    end
    if e.placeholders and #e.placeholders > 0 and want('placeholders') then
      emit(('    placeholders = %s;'):format(nix_list(e.placeholders, '    ')))
    end
    if e.extensionToLanguage and next(e.extensionToLanguage) and want('extensionToLanguage') then
      emit('    extensionToLanguage = {')
      for _, ext in ipairs(sorted_keys(e.extensionToLanguage)) do
        emit(('      %s = %s;'):format(nix_str(ext), nix_str(e.extensionToLanguage[ext])))
      end
      emit('    };')
    end
    emit('  };')
  end
  emit('}')
end

if opts.format == 'json' then
  emit_json()
else
  emit_nix()
end

io.write(table.concat(out, '\n'), '\n')
io.stdout:flush()
-- Leave before nvim tears down the scratch buffer it opened for `-l`.
os.exit(0)
