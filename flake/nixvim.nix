{
  config,
  pkgs,
  self,
  inputs,
  ...
}:
let
  inherit (self.lib.extraModules) mkOption mkSubmoduleOptions types;
  inherit (inputs.nixpkgs.lib) mapAttrsToList optionals concatLists;
  nvimType = types.submodule (submod: {
    options = {
      name = mkOption {
        type = types.str;
        description = "The name of the configuration, used for the package name.";
      };
      derivation = mkOption {
        type = types.functionTo types.package;
      };
      homeModule = mkOption {
        type = types.unspecified;
      };
      darwinModule = mkOption {
        type = types.unspecified;
      };

    };
  });
in
{

  options.flake.auscybernix.nixvim = mkSubmoduleOptions {
    nixvimConfigurations = mkOption {
      type = types.attrsOf nvimType;
      default = { };
    };

  };
  config = {
    flake.auscybernix.nixvim.nixvimConfigurations.nixvim = self.lib.system.mkNixVim {
      modules = [ ];
      name = "nixvim";

    };
    perSystem =
      { system, pkgs, ... }:
      {
        packages = builtins.mapAttrs (
          name: config: (config.derivation { inherit pkgs system; })
        ) config.flake.auscybernix.nixvim.nixvimConfigurations;
      };
    flake.auscybernix.importedDarwinModules = (
      mapAttrsToList (
        name: config: config.darwinModule
      ) config.flake.auscybernix.nixvim.nixvimConfigurations
    );
    flake.auscybernix.importedHomeModules = (
      mapAttrsToList (
        name: config: config.homeModule
      ) config.flake.auscybernix.nixvim.nixvimConfigurations
    );
  };

}
