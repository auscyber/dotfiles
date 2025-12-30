{ config, pkgs, ... }:
{
  stylix.enableReleaseChecks = false;
  environment.variables.NH_DARWIN_FLAKE = "${config.auscybernix.nix.flake}";
  home-manager.sharedModules = [
    {
      auscybernix.nix.flake = config.auscybernix.nix.flake;
    }
  ];
  environment.shellAliases = {
    "${config.auscybernix.nix.reloadProgram}" = "nh darwin switch";

  };

}
