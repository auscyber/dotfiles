{
  den,
  lib,
  inputs,
  ...
}:
{
  den.aspects.darwin-base = {
    inputs.darwin = {
      url = "github:nix-darwin/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
      patch.enable = true;
    };

    # Declared here rather than as `meta.addRegistry`: that shorthand is read
    # back off `flake-file.inputsWithMeta`, which only sees inputs still
    # declared at file level. An aspect-declared input would need the layer plan
    # to be forced on every build to be seen at all.
    nix.registry.darwin.flake = inputs.darwin;

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
      # pseudoterminal master: Device not configured".
      #
      # This runs at every `darwin-rebuild switch`, NOT at boot: the boot job
      # `org.nixos.activate-system` execs `activate-system-start`, which only
      # relinks `/run/current-system`, sets gcroots, checks the nixbld users and
      # rebuilds `/etc` -- it does not rerun `activate`, so neither
      # `extraActivation` nor `postActivation` fire at boot. The kernel keeps the
      # raised limit until the next reboot, after which it is back to the default
      # until the first switch. (This is the same reboot gap `wrapperd` exists to
      # close for `/run/wrappers/bin`; a plain sysctl has not warranted the same
      # treatment.)
      system.activationScripts.extraActivation.text = ''
        /usr/sbin/sysctl -w kern.tty.ptmx_max=970 || true
      '';
    };
  };

  den.schema.host.includes = [ den.aspects.darwin-base ];
}
