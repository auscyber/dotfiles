{
  self,
  inputs,
  lib,
  ...
}:
let
  inherit (self.lib.file) parseHomeConfigurations;
  # Only the Raspberry Pi home is built as a standalone homeConfiguration
  # here.  All other hosts/users are handled by den (flake/den.nix) which
  # creates both NixOS-embedded and standalone homeConfigurations via its
  # homeManager user class.
  rpiHomes = lib.filterAttrs (
    _: { system, ... }: lib.hasSuffix "rpi" system
  ) (parseHomeConfigurations ../homes);
in
{
  flake = {
    homeModules = rec {
      default = ../modules/home;
      recursive = {
        imports = [ default ] ++ self.lib.file.importModulesRecursive ../modules/home;
      };
    };

    # Standalone home configurations for Raspberry Pi users (non-standard
    # builder — kept outside den).
    homeConfigurations = lib.mapAttrs' (
      _:
      {
        system,
        username,
        userAtHost,
        hostname,
        path,
        ...
      }:
      {
        name = userAtHost;
        value = self.lib.system.mkHome {
          inherit inputs hostname username;
          system = lib.strings.removeSuffix "-rpi" system;
          modules = [ path ];
        };
      }
    ) rpiHomes;
  };
}
