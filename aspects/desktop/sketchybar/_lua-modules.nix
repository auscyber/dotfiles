# Shared builders for Lua modules pushed onto a sketchybar-Lua-compatible
# `extraLuaPackages` require path (`programs.sketchybar.extraLuaPackages` and
# `services.paneru.extraLuaPackages` share the same `luaPs: [ package ]` shape
# — see aspects/desktop/sketchybar/sketchybar.nix and aspects/wms/paneru).
# Kept here rather than duplicated per-aspect since both the sketchybar config
# itself and any WM that drives sketchybar directly from its own Lua runtime
# (paneru) need the same theme colours and app-icon lookup.
{ pkgs, lib }:
{
  # `colourConfig` is a stylix base16-derived attrset of bare hex strings
  # (e.g. `{ foreground = "e6e8ef"; ... }`); the generated module prefixes
  # full alpha, merges in a fallback for any missing key, and adds
  # `colors.with_alpha`.
  mkColorsModule =
    colourConfig: luaPs:
    luaPs.toLuaModule (
      pkgs.runCommandLocal "sketchybar-colors" { } ''
        install -Dm644 ${pkgs.writeText "colors.lua" ''
          local raw = ${lib.generators.toLua { } colourConfig}

          local colors = {}
          for key, value in pairs(raw) do
            colors[key] = tonumber("0x" .. "ff" .. value)
          end

          local fallback = {
            foreground = 0xffe6e8ef,
            background = 0xff101418,
            yellow = 0xfff2c14e,
            selection = 0xff2b3544,
            black = 0xff000000,
            white = 0xffffffff,
          }

          for key, value in pairs(fallback) do
            if type(colors[key]) ~= "number" then
              colors[key] = value
            end
          end

          colors.transparent = 0x00000000

          function colors.with_alpha(color, alpha)
            if type(color) ~= "number" or type(alpha) ~= "number" then
              return color
            end
            if alpha > 1.0 or alpha < 0.0 then
              return color
            end
            local base = color & 0x00ffffff
            local a = math.floor(alpha * 255.0) & 0xff
            return (a << 24) | base
          end

          return colors
        ''} "$out/share/lua/${luaPs.lua.luaversion}/colors.lua"
      ''
    );

  mkIconMapModule =
    luaPs:
    luaPs.toLuaModule (
      pkgs.runCommandLocal "sketchybar-icon-map" { } ''
        install -Dm644 ${pkgs.sketchybar-app-font}/lib/sketchybar-app-font/icon_map.lua "$out/share/lua/${luaPs.lua.luaversion}/icon_map.lua"
      ''
    );
}
