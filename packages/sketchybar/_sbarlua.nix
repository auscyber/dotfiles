{
  lib,
  fetchFromGitHub,
  gcc,
  luaPackages,
  readline,
}:
let
  inherit (luaPackages) lua;
in
luaPackages.buildLuaPackage {
  pname = "sbarLua";
  version = "0-unstable-2026-03-06";

  src = fetchFromGitHub {
    owner = "FelixKratz";
    repo = "SbarLua";
    rev = "dba9cc421b868c918d5c23c408544a28aadf2f2f";
    hash = "sha256-lhLTrdufA3ALJ2S5HLdgNOr5seWIWEHkVhZNPObzbvI=";
  };

  # Upstream vendors the Lua 5.5 sources and statically links them into the
  # module, which pins it to that one interpreter.  Teach the makefile to build
  # against an external Lua (any of 5.1 - 5.5 or LuaJIT) instead.
  patches = [ ./sbarlua-external-lua.patch ];

  nativeBuildInputs = [ gcc ];

  buildInputs = [ readline ];

  makeFlags = [
    "LUA_CFLAGS=-I${lib.getDev lua}/include"
    "INSTALL_DIR=$(out)/lib/lua/${lua.luaversion}"
  ];

  meta = {
    description = "Lua API for SketchyBar";
    homepage = "https://github.com/FelixKratz/SbarLua/";
    license = lib.licenses.gpl3;
    maintainers = [
      lib.maintainers.khaneliman
      lib.maintainers.kaynetik
    ];
    platforms = lib.platforms.darwin;
  };
}
