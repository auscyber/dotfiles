{ config, pkgs, ... }:
{
  environment.sessionVariables.NH_OS_FLAKE = "${config.auscybernix.nix.flake}";

  age.secrets.ivy-password = {
    rekeyFile = ../../../secrets/ivy-password.age;
    intermediary = true;
  };
  nix.optimise = {
    automatic = true;

    dates = [
      "03:45"
    ];
  };
  age.secrets.ivy-pwd-hash = {
    generator = {
      dependencies = [ config.age.secrets.ivy-password ];
      script =
        {
          pkgs,
          lib,
          decrypt,
          deps,
          ...
        }:
        ''
          ${decrypt} ${lib.escapeShellArg (lib.head deps).file} | \
              ${pkgs.openssl}/bin/openssl passwd -6 -stdin
        '';
    };
  };
  home-manager.sharedModules = [
    {
      auscybernix.nix.flake = config.auscybernix.nix.flake;
    }
  ];
  environment.shellAliases = {
    "${config.auscybernix.nix.reloadProgram}" = "nh os switch";
  };
}
