{
  config,
  lib,
  lua,
  runCommand,
  toLuaModule,
}:

let
  inherit (lib.generators) toLua mkLuaInline;

  # toLua emits a *value*, so prepend `return` to make it a loadable module
  src =
    builtins.toFile "internal_colors.lua"
      # lua
      ''
            local colors = ${toLua { } config}


        return colors'';
in
toLuaModule (
  runCommand "internal_colors" { } ''
    install -Dm644 ${src} "$out/share/lua/${lua.luaversion}/internal_colors.lua"
  ''
)
