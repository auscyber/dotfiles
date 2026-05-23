{
  source,
  vimPlugins,
  lib,
  vimUtils,
  rustPlatform,
  stdenv,
  llvmPackages,
}:
let
  inherit (source) version src;
  difftastic-lib = rustPlatform.buildRustPackage {
    pname = "difftastic-nvim-lib";
    inherit version src;
    cargoDeps = rustPlatform.importCargoLock source.cargoLock."Cargo.lock";
    postInstall = ''
      ln -s $out/lib/libdifftastic_nvim${stdenv.hostPlatform.extensions.sharedLibrary} $out/lib/difftastic_nvim.so
    '';
    env.RUSTFLAGS = lib.optionalString stdenv.hostPlatform.isDarwin "-C link-arg=-undefined -C link-arg=dynamic_lookup";
  };
in
vimUtils.buildVimPlugin {
  pname = "difftastic-nvim";
  inherit version src;
  dependencies = [ vimPlugins.nui-nvim ];

  postPatch = ''
    substituteInPlace lua/difftastic-nvim/binary.lua \
    	--replace-fail \
    	'release_dir = plugin_root .. "/target/release"' \
    	"release_dir = '${difftastic-lib}/lib'"
  '';

  passthru = {

    # needed for the update script
  };

  meta = {
    maintainers = with lib.maintainers; [
      auscyber
    ];
  };
}
