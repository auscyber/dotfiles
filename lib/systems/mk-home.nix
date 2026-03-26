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
      {
      }
    ]
    ++ [
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
    ]
    ++ modules;

}
