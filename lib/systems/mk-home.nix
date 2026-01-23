{
  inputs,
  importedHomeModules,
  standaloneHomeModules,
  common,
}:
{
  system,
  hostname,
  username ? "ivypierlot",
  modules ? [ ],
  ...
}:
let

  flake = inputs.self;
  extendedLib = common.mkExtendedLib flake inputs.nixpkgs;

in
inputs.home-manager.lib.homeManagerConfiguration {
  pkgs = import inputs.nixpkgs {
    inherit system;
    inherit (common.mkNixpkgsConfig flake) config overlays;
  };

  extraSpecialArgs =
    common.mkSpecialArgs {
      inherit
        inputs
        hostname
        username
        system
        extendedLib
        ;

    }
    // {
	  systemIdentifier = "${username}@${hostname}-${system}";
      isInside = false;
    };

  modules =
    importedHomeModules
    ++ standaloneHomeModules
    ++ [
      { _module.args.lib = extendedLib; }

      ../../modules/common/secrets.nix
      ../../modules/common/nix

      ../../modules/common/allConfigs.nix

      {
      }
    ]
    ++ (extendedLib.importModulesRecursive ../../modules/home)
    ++ [
      ../../modules/home/default.nix
      {
        home = {
          inherit username;
          homeDirectory =
            if system == "x86_64-darwin" || system == "aarch64-darwin" then
              "/Users/${username}"
            else
              "/home/${username}";
        };
      }
      (
        { config, lib, ... }:
        {
          auscybernix.standalone.enable = true;
          auscybernix.secrets.enable = true;
        }
      )
    ]
    ++ modules;

}
