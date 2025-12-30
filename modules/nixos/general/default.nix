{ config, pkgs, ... }:
{
  environment.sessionVariables.NH_OS_FLAKE = "${config.auscybernix.nix.flake}";

  home-manager.sharedModules = [
    {
      auscybernix.nix.flake = config.auscybernix.nix.flake;
    }
  ];
  environment.shellAliases = {
    "${config.auscybernix.nix.reloadProgram}" = "nh os switch";
  };
}
