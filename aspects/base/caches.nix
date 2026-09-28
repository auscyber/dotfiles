{
  inputs,
  den,
  lib,
  config,
  ...
}:
let
  celler = import ./celler/_lib.nix { inherit lib; };
  caches = {
    "https://nix-community.cachix.org" =
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";
    #    "https://iohk.cachix.org" = "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=";
    "https://cache.nixos.org" = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
    "https://devenv.cachix.org" = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    #    "https://auscyber.cachix.org" =
    #      "auscyber.cachix.org-1:RPlENxXc/irvLimM0Yz8Au3ntk/sxZ8bwXPwuXL3v5c=";
    #    "https://attic.xuyh0120.win/lantian" = "lantian:EeAUQ+W+6r7EtwnmYjeVwx5kOGEBpjlBfPlzGlTNvHc=";
  };

in
{
  debug = true;

  # In the `packages` registry, which overlays.nix walks regardless of
  # inclusion, so the perSystem `pkgs` (update-celler-keys, celler-token) has
  # `pkgs.celler` whether or not any host is resolved there. Hosts get it the
  # normal way, by `includes`-ing it -- the same shape as ./ivy-fetch.nix.
  den.aspects.packages.celler.overlays.celler = lib.optional (
    inputs ? celler
  ) inputs.celler.overlays.default;

  den.default.nix.settings = {
    # An unreachable substituter is not a reason to fail; it is a reason to
    # build the thing.
    #
    # nix defaults this to false, which makes a narinfo fetch that cannot
    # connect FATAL rather than a miss. The practical effect is that whenever a
    # celler server is down -- boxes we run, on a home connection, so this is
    # not rare -- every nix command that touches an uncached path dies after
    # ~25s of retries, including ones with nothing to do with building:
    # `secret-edit` could not open a secret, and `nix run .#tailscale-client-key`
    # could not mint a key, purely because a cache was unreachable.
    #
    # The cost is the honest one: a broken *upstream* cache now also degrades to
    # a local build, so an outage at cache.nixos.org buys a long rebuild instead
    # of a loud error. That is the better failure for this fleet -- every host
    # here can build what it substitutes, and none of them would rather stop.
    fallback = true;

    trusted-substituters = builtins.attrNames caches;
    substituters = builtins.attrNames caches;
    trusted-public-keys = builtins.attrValues caches;
  };

  # secondpc runs the ncps binary cache (served directly on :8501) plus celler
  # (cache.ivymect.in, ./celler/server.nix). Attached to the `nix` aspect's
  # per-host provider so they only land on secondpc.
  den.aspects.nix.provides.secondpc = {
    includes = [
      den.aspects.nginx
      den.aspects.celler
      den.policies.celler-caches
      {
        celler-use.secondpc.pull = [ "main" ];
        nixos =
          { celler-caches, celler-use, ... }:
          let
            used = celler.use celler-caches celler-use;
          in
          {
            services.ncps.cache.upstream = {
              urls = lib.concatMap (c: map (cache: "${c.endpoint}/${cache}") (builtins.attrNames c.keys)) used;
              publicKeys = lib.concatMap (c: builtins.attrValues c.keys) used;
            };
          };
      }
    ];
    nixos.services.ncps = {
      enable = true;
      cache = {
        hostName = "secondpc";
        storage.local = "/mnt/hdd/ncps";
        maxSize = "200G";
        lru.schedule = "0 2 * * *"; # Clean up daily at 2 AM
        upstream = {
          urls = builtins.attrNames caches;
          publicKeys = builtins.attrValues caches;
        };
      };
      server.addr = "0.0.0.0:8501";
      prometheus.enable = true;
      # Traces to tempo, loopback and plaintext like kanidm's -- "insecure"
      # is the scheme ncps's own grpcURL doc asks for to mean exactly that
      # (unqualified/"https" would mean TLS, which tempo's receiver doesn't
      # speak).
      openTelemetry = {
        enable = true;
        grpcURL = "insecure://127.0.0.1:4317";
      };
    };
  };

  #  patchedInputs.celler = {
  #    patches = [
  #      ../../patches/celler/split.patch
  #    ];
  #  };

  # The `celler` input, owned by one aspect that every celler aspect includes,
  # so its layer falls out of the hosts that use it.
  den.aspects.celler-input.inputs.celler = {
    url = "github:auscyber/celler/main";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.crane.follows = "crane";
    inputs.flake-parts.follows = "flake-parts";
    inputs.flake-compat.follows = "flake-compat";
  };

  flake-file.nixConfig = {
    extra-substituters = builtins.attrNames caches;
    extra-trusted-public-keys = builtins.attrValues caches;
    # Several aspects (celler's cargo vendoring, agenix-rekey's patched
    # input) resolve derivations during evaluation. Without this, eval fails
    # outright ("cannot build ... because 'allow-import-from-derivation' is
    # disabled") on any machine that hasn't set it in its own nix.conf.
    allow-import-from-derivation = true;
  };

  # `nix run .#write-flake-without-celler`: `write-flake`, minus the celler
  # caches in nixConfig, for when they are down.
  perSystem =
    { pkgs, ... }:
    let
      # Whatever the celler servers added to nixConfig (./celler/server.nix).
      drop =
        lib.subtractLists (builtins.attrNames caches) config.flake-file.nixConfig.extra-substituters
        ++ lib.subtractLists (builtins.attrValues caches) config.flake-file.nixConfig.extra-trusted-public-keys;
      writeFlakeWithoutCeller = pkgs.writeShellApplication {
        name = "write-flake-without-celler";
        runtimeInputs = [
          pkgs.gnugrep
          pkgs.coreutils
        ];
        text = ''
          nix run .#write-flake -- "$@"
          grep -vF ${
            lib.concatMapStringsSep " " (x: "-e ${lib.escapeShellArg "\"${x}\""}") drop
          } flake.nix >flake.nix.tmp || true
          mv flake.nix.tmp flake.nix
        '';
      };
    in
    {
      apps.write-flake-without-celler = {
        type = "app";
        program = lib.getExe writeFlakeWithoutCeller;
      };
    };
}
