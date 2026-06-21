{
  source,
  fetchPnpmDeps,
  pnpmConfigHook,
  nodejs,
  pnpm_10,
  installFont,
  runCommand,
  stdenv,
}:
stdenv.mkDerivation (finalAttrs: {
  inherit (source) pname version src;

  nativeBuildInputs = [
    nodejs # in case scripts are run outside of a pnpm call
    pnpmConfigHook
    pnpm_10 # At least required by pnpmConfigHook, if not other (custom) phases
    installFont
  ];

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    pnpm = pnpm_10;
    fetcherVersion = 4;
    hash = "...";
  };

  preInstall = ''
    mkdir -p $out/lua
    cp -r dist/icon_map.lua $out/lua
  '';

  passthru.luaPackage =
    ps:
    let
      inherit (ps) toLuaModule lua;
    in
    toLuaModule (
      runCommand "${finalAttrs.pname}-luamodule" { } ''
        mkdir -p "$out/share/lua/${lua.luaversion}"
        ln -s ${finalAttrs.finalPackage}/lua/icon_map.lua "$out/share/lua/${lua.luaversion}/icon_map.lua"
      ''
    );

})
