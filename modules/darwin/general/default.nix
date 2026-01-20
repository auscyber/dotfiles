{
  config,
  pkgs,
  hostname,
  system,
  ...
}:
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
  #  programs.gnupg.package = pkgs.gnupg-wrapped;
  auscybernix.secrets.configId = "${system}-${hostname}";
  #  age.rekey.localStorageDir = ../../.. + "/secrets/rekeyed/${hostname}";
  nix.optimise = {
    automatic = true;
    interval = [
      {
        Hour = 4;
        Minute = 15;
        Weekday = 7;
      }
    ];
  };

}
