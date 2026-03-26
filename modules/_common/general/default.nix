{
  config,
  pkgs,
  lib,
  hostname,
  system,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
in
lib.mkMerge [
  # ── Shared by both NixOS and nix-darwin ──────────────────────────────────
  {
    # Forward the flake path to every Home Manager user on this system.
    home-manager.sharedModules = [
      { auscybernix.nix.flake = config.auscybernix.nix.flake; }
    ];

    # Shell alias to rebuild / switch the system.
    environment.shellAliases = {
      "${config.auscybernix.nix.reloadProgram}" =
        if isDarwin then "nh darwin switch" else "nh os switch";
    };
  }

  # ── Scheduled store optimisation — syntax differs per platform ───────────
  (lib.mkIf isLinux {
    nix.optimise = {
      automatic = true;
      dates = [ "03:45" ];
    };
  })
  (lib.mkIf isDarwin {
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
  })

  # ── NixOS-only additions ──────────────────────────────────────────────────
  (lib.mkIf isLinux {
    environment.sessionVariables.NH_OS_FLAKE = "${config.auscybernix.nix.flake}";

    age.secrets.ivy-password = {
      rekeyFile = ../../../secrets/ivy-password.age;
      intermediary = true;
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
  })

  # ── nix-darwin-only additions ─────────────────────────────────────────────
  (lib.mkIf isDarwin {
    stylix.enableReleaseChecks = false;
    environment.variables.NH_DARWIN_FLAKE = "${config.auscybernix.nix.flake}";
    auscybernix.secrets.configId = "${system}-${hostname}";
  })
]
