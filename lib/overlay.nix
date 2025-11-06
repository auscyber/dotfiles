{ inputs }:
final: prev:
let
  auscyberLib = import ./default.nix { inherit inputs; };
in
{

  maintainers = prev.maintainers // {
    auscyber = {
      name = "Ivy Pierlot";
      email = "ivyp@outlook.com.au";
    };
  };
  inherit (auscyberLib.flake.lib.file)
    importModulesRecursive
    ;

  inherit (auscyberLib.flake.lib) system file extra;

  inherit (inputs.home-manager.lib) hm;

}
