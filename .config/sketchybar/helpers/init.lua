local home = os.getenv("HOME") or ""

package.cpath = package.cpath .. ";" .. home .. "/.local/share/sketchybar_lua/?.so"

local config_dir = os.getenv("CONFIG_DIR") or (home .. "/.config/sketchybar")
local helper_dir = config_dir .. "/helpers"

local function shell_quote(value)
    return "'" .. value:gsub("'", "'\\''") .. "'"
end

local function file_mtime(path)
    local handle = io.popen("stat -f %m " .. shell_quote(path) .. " 2>/dev/null")
    if not handle then
        return nil
    end

    local output = handle:read("*a") or ""
    handle:close()

    return tonumber(output:match("%d+"))
end

local menus_dir = helper_dir .. "/menus"
local menus_src = menus_dir .. "/menus.c"
local menus_bin = menus_dir .. "/bin/menus"

local source_mtime = file_mtime(menus_src)
local binary_mtime = file_mtime(menus_bin)

if not binary_mtime or (source_mtime and source_mtime > binary_mtime) then
    os.execute("(cd " .. shell_quote(menus_dir) .. " && make)")
end
