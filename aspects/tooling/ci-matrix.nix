{
  inputs,
  lib,
  ...
}:
# `flake.ciMatrix` -- split out of ./ci.nix because it is the only thing here that
# needs `nix-github-actions`, and it lives in the `dev` partition so that input
# stays out of the root flake.lock (see ../../partition-map.nix).
#
#   * `.#ciMatrix.checks.<system>` is what CI actually consumes: an attrset of
#     buildable toplevels for that system, keyed `"<class>-<name>"`. The three
#     per-system workflows (.github/workflows/build-<system>.yml) hand the whole
#     attrset to nix-fast-build, which evaluates every host in parallel and starts
#     each build as its evaluation lands -- so no host list is ever baked into the
#     YAML, and no job exists just to enumerate one.
#   * `.#ciMatrix.matrix` is the GitHub matrix nix-github-actions derives from the
#     same checks (one row per host, each system mapped to a runner). CI no longer
#     reads it -- nix-fast-build replaced the dynamic-matrix fan-out -- it is kept
#     because `mkGithubMatrix` is what produces `checks` above, and it stays useful
#     for anything that does want a row per host.
#   * The build jobs run against THIS flake with `accept-flake-config`, so they
#     pick up substituters + trusted-public-keys straight from the flake's
#     nixConfig -- everything derived from aspects/base/celler-keys.json -- with
#     nothing hardcoded in the workflow.
let
  self = inputs.self;

  # Escape hatch: hosts to leave out of CI entirely. Empty by default -- a host
  # that fails to build does not stop its siblings (nix-fast-build does not fail
  # fast unless asked to), it just turns that system's job red at the end.
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
  # same map under the `home-` class, so they land in their arch's `checks.<system>`
  # exactly like the nixos/darwin hosts and that arch's workflow picks them up with
  # no workflow changes.
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
  # makes `.#ciMatrix.checks.<system>` a real buildable attrset -- which is the
  # `--flake` target each per-system workflow passes to nix-fast-build -- and
  # `.#ciMatrix.matrix` the GitHub matrix (unused by CI, see the note above).
  flake.ciMatrix = inputs.nix-github-actions.lib.mkGithubMatrix {
    checks = checksBySystem;
    attrPrefix = "ciMatrix.checks";
  };
}
