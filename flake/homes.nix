{
  inputs,
  self,
  lib,
  ...
}:
let
  inherit (self.lib.file) parseHomeConfigurations;

  homesPath = ../homes;
  allHomes = parseHomeConfigurations homesPath;

  generateHomeConfiguration =
    name:
    {
      system,
      username,
      userAtHost,
      hostname,
      path,
      ...
    }:
    {
      name = userAtHost; # Use the full "username@hostname" as key
      value = self.lib.system.mkHome {
        inherit
          inputs
          hostname
          username
          ;
        system = lib.strings.removeSuffix "-rpi" system; # Strip -rpi suffix for rpi homes
        modules = [ path ];
      };
    };
in
{
  imports = [ inputs.home-manager.flakeModules.home-manager ];

  flake = {
    homeModules = rec {
      default = ../modules/home;
      recursive = {
        imports = lib.importModulesRecursive ../modules/home ++ lib.externalHmModules ++ [ default ];

      };
    };

    # Dynamically generated home configurations
    homeConfigurations = lib.mapAttrs' generateHomeConfiguration allHomes;
  };
  perSystem =
    {
      config,
      system,
      pkgs,
      ...
    }:
    {

    };
}
