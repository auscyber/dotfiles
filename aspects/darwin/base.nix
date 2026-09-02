{
  den,
  lib,
  ...
}:
{
  ff.darwin = {
    url = "github:nix-darwin/nix-darwin";
    inputs.nixpkgs.follows = "nixpkgs";
    meta.addRegistry = true;
    patch.enable = true;
  };
  den.aspects.darwin-base = {
    includes = [
      den.aspects.darwin-finder
      den.aspects.darwin-general
      den.aspects.darwin-hmApps
      den.aspects.pam-touchid
    ];

    darwin = { pkgs, ... }: {
      environment.systemPackages = with pkgs; [
        mole-cleaner
        fd
      ];
      documentation.enable = lib.mkDefault true;
      programs.zsh.enable = lib.mkDefault true;
      programs.gnupg.agent.enable = lib.mkDefault true;
      system.defaults.NSGlobalDomain = {
        AppleInterfaceStyle = lib.mkDefault "Dark";
        AppleShowAllFiles = lib.mkDefault true;
        ApplePressAndHoldEnabled = lib.mkDefault false;
        InitialKeyRepeat = lib.mkDefault 10;
        KeyRepeat = lib.mkDefault 3;
      };

      # No log by default -- nix-darwin only wires this up to
      # `serviceConfig.StandardErrorPath` when set (modules/services/nix-daemon.nix),
      # otherwise nix-daemon's stderr goes nowhere convenient. Needed for
      # anything past `launchctl list`: a stuck/cycling remote builder,
      # a substituter timing out, gc.automatic runs, etc.
      services.nix-daemon.logFile = lib.mkDefault "/var/log/nix-daemon.log";

      # Default kern.tty.ptmx_max (511) gets exhausted under heavy pty churn
      # (many terminal/editor sessions, nix-build allocating one per builder to
      # give tools a tty for colored output). Once the pool fills, every new
      # pty request -- including nix-build's -- fails with "opening
      # pseudoterminal master: Device not configured". `activate-system` reruns
      # this at every boot and `darwin-rebuild switch`, so it doesn't need a
      # dedicated launchd daemon.
      system.activationScripts.extraActivation.text = ''
        /usr/sbin/sysctl -w kern.tty.ptmx_max=970 || true
      '';
    };
  };

  den.schema.host.includes = [ den.aspects.darwin-base ];
}
