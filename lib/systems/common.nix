{ inputs }:
let
  inherit (inputs.nixpkgs.lib) filterAttrs mapAttrs';
  externalHmModules = [
    inputs._1password-shell-plugins.hmModules.default
    inputs.zen-browser.homeModules.default
    inputs.nixvim.homeModules.default
    inputs.nix-index-database.homeModules.nix-index
    inputs.sops-nix.homeManagerModules.sops
  ];
in
{
  inherit externalHmModules;
  mkExtendedLib = flake: nixpkgs: nixpkgs.lib.extend flake.lib.overlay;

  mkNixpkgsConfig = flake: {
    overlays = builtins.attrValues flake.overlays or { };
    config = {
      allowUnfree = true;
    };

  };
  mkSpecialArgs =
    {
      inputs,
      hostname,
      username ? "ivypierlot",
      system,
      extendedLib,
    }:
    {
      inherit (inputs) self;
      inherit username;
      inherit inputs hostname system;
      lib = extendedLib;
    };
  mkHomeConfigs =
    {
      flake,
      system,
      hostname,
      homesPath ? ../../homes,
    }:
    let
      inherit (flake.lib.file) parseHomeConfigurations;
      allHomes = parseHomeConfigurations homesPath;
    in
    filterAttrs (
      _name: homeConfig: homeConfig.system == system && homeConfig.hostname == hostname
    ) allHomes;
  mkHomeManagerConfig =
    {
      extendedLib,
      inputs,
      system,
      matchingHomes,
      isNixOS ? true,
    }:
    if matchingHomes != { } then
      {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          extraSpecialArgs = {
            inherit inputs system;
            inherit (inputs) self;
            lib = extendedLib;
            flake-parts-lib = inputs.flake-parts.lib;
          };
          sharedModules = [
            { _module.args.lib = extendedLib; }
          ]
          ++ (
            if isNixOS then
              [
                inputs.home-manager.flakeModules.home-manager
              ]
            else
              [ ]
          )
          ++ [
            ../../modules/common/secrets.nix

          ]
          ++ externalHmModules
          ++ (extendedLib.importModulesRecursive ../../modules/home);
          users = mapAttrs' (_name: homeConfig: {
            name = homeConfig.username;
            value = {
              imports = [ homeConfig.path ];
              home = {
                inherit (homeConfig) username;
                homeDirectory = inputs.nixpkgs.lib.mkDefault (
                  if isNixOS then "/home/${homeConfig.username}" else "/Users/${homeConfig.username}"
                );
              };
            }
            // (
              if isNixOS then
                {
                  _module.args.username = homeConfig.username;
                }
              else
                { }
            );
          }) matchingHomes;
        };
      }
    else
      { };

}
