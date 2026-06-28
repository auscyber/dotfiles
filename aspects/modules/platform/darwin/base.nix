{ den, lib, ... }:
{
  den.aspects.darwin-base = {
    includes = [
      den.aspects.darwin-finder
      den.aspects.darwin-general
      den.aspects.darwin-hmApps
      den.aspects.pam-touchid
    ];

    darwin = {
      documentation.enable = lib.mkDefault true;
      programs.zsh.enable = lib.mkDefault true;
      programs.fish.enable = lib.mkDefault true;
      programs.gnupg.agent.enable = lib.mkDefault true;
      system.defaults.NSGlobalDomain = {
        AppleInterfaceStyle = lib.mkDefault "Dark";
        AppleShowAllFiles = lib.mkDefault true;
        ApplePressAndHoldEnabled = lib.mkDefault false;
        InitialKeyRepeat = lib.mkDefault 10;
        KeyRepeat = lib.mkDefault 3;
      };
    };
  };

  den.schema.host.includes = [ den.aspects.darwin-base ];
}
