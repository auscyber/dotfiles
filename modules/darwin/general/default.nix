{
  config,
  pkgs,
  hostname,
  system,
  lib,
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
  system.systemBuilderCommands =
    let
      packagelist = pkgs.writeText "extra-builder-commands" (
        lib.concatMapAttrsStringSep "\n" (x: _: x) (
          lib.genAttrs' config.environment.systemPackages (x: {
            name = x.name;
            value = x;
          })
        )
      );
    in
    ''
      		ln -s ${packagelist} $out/packagelist
      	  ''

  ;

  #  programs.gnupg.package = pkgs.gnupg-wrapped;
  auscybernix.secrets.configId = "${system}-${hostname}";
  #  age.rekey.localStorageDir = ../../.. + "/secrets/rekeyed/${hostname}";
  #  determinateNix.enable = true;
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
