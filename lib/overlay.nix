{ inputs }:
final: prev:
let
  auscyberLib = import ./default.nix { inherit inputs; };
in
{

  inherit (auscyberLib.flake.lib.file)
    importModulesRecursive
    ;

  inherit (auscyberLib.flake.lib) system file extra;

  inherit (inputs.home-manager.lib) hm;

}
