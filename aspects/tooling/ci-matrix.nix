{
  inputs,
  lib,
  ...
}:
# `flake.ciMatrix` -- split out of ./ci.nix because it is the only thing here that
# needs `nix-github-actions`, and it lives in the `dev` partition so that input
# stays out of the root flake.lock (see ../../partition-map.nix).
#
#   * `.#ciMatrix.matrix` is a GitHub matrix (one row per host, each system mapped
#     to a runner) that .github/workflows/systems.yml evaluates AT RUN TIME
#     (`nix eval` in a matrix job, `fromJSON` in the build job) -- a proper dynamic
#     matrix, not baked into the YAML. `.#ciMatrix.checks.<system>."<name>"` is the
#     buildable toplevel each row targets.
#   * The build jobs run `nix build` against THIS flake with `accept-flake-config`,
#     so they pick up substituters + trusted-public-keys straight from the flake's
#     nixConfig -- everything derived from aspects/base/celler-keys.json -- with
#     nothing hardcoded in the workflow.
let
  self = inputs.self;

  # Escape hatch: hosts to leave out of CI entirely. Empty by default -- a host
  # that fails to build just goes red on its own job without affecting the rest
  # (that is the whole point of fail-fast: false).
  excludeHosts = [ ];

  systemOf = cfg: cfg.config.nixpkgs.hostPlatform.system;
  # Standalone home-manager configs don't carry `config.nixpkgs.hostPlatform`
  # (useGlobalPkgs disables the nixpkgs module), and their buildable toplevel is
  # `activationPackage`, not `system.build.toplevel` -- so they key off `pkgs`.
  homeSystemOf = cfg: cfg.pkgs.stdenv.hostPlatform.system;
  keep = configs: removeAttrs configs excludeHosts;

  # { <system> = { "<class>-<name>" = <toplevel>; }; } -- the exact shape
  # nix-github-actions.lib.mkGithubMatrix consumes. Its default `platforms` maps
  # each of our systems to a runner (x86_64-linux -> ubuntu-24.04, aarch64-linux
  # -> ubuntu-24.04-arm, aarch64-darwin -> macos-14). homeConfigurations join the
  # same map under the `home-` class, so systems.yml buckets and builds them by
  # arch exactly like the nixos/darwin hosts, with no workflow changes.
  checksBySystem =
    let
      add =
        {
          systemF,
          toplevelF,
        }:
        class: acc: name: cfg:
        let
          s = systemF cfg;
        in
        acc
        // {
          ${s} = (acc.${s} or { }) // {
            "${class}-${name}" = toplevelF cfg;
          };
        };
      addSystem = add {
        systemF = systemOf;
        toplevelF = cfg: cfg.config.system.build.toplevel;
      };
      addHome = add {
        systemF = homeSystemOf;
        toplevelF = cfg: cfg.activationPackage;
      };
    in
    lib.foldlAttrs (addHome "home") (lib.foldlAttrs (addSystem "darwin") (lib.foldlAttrs
      (addSystem "nixos")
      { }
      (keep (self.nixosConfigurations or { }))
    ) (keep (self.darwinConfigurations or { }))) (keep (self.homeConfigurations or { }));
in
{
  ff.nix-github-actions = {
    url = "github:nix-community/nix-github-actions";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  # attrPrefix -> the flake attr each matrix row builds: since mkGithubMatrix
  # returns `{ inherit checks; matrix = {...}; }`, assigning it to flake.ciMatrix
  # makes `.#ciMatrix.checks.<system>."<name>"` a real buildable attribute and
  # `.#ciMatrix.matrix` the runtime-evaluated GitHub matrix.
  flake.ciMatrix = inputs.nix-github-actions.lib.mkGithubMatrix {
    checks = checksBySystem;
    attrPrefix = "ciMatrix.checks";
  };
}
