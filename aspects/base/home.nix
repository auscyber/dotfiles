top@{
  den,
  lib,
  inputs,
  ...
}:
let
  hmPlatforms =
    {
      class,
      aspect-chain,
      host,
      user,
    }:
    den.provides.forward {
      each = [
        "Linux"
        "Darwin"
      ];
      fromClass = platform: "hm${platform}";
      intoClass = _: "homeManager";
      intoPath = _: [ ];
      fromAspect = _: lib.head aspect-chain;
      guard = { pkgs, ... }: platform: lib.mkIf pkgs.stdenv."is${platform}";
      adaptArgs = lib.id;
    };

  hmAlias =
    {
      class,
      aspect-chain,
    }:
    den.batteries.forward {
      each = lib.singleton true;
      fromClass = _: "hm";
      intoClass = _: "homeManager";
      intoPath = _: [ ];
      fromAspect = _: lib.head aspect-chain;
      adaptArgs = { config, ... }: { osConfig = config; };
    };

in
{
  ff.home-manager = {
    url = "github:nix-community/home-manager";
    inputs.nixpkgs.follows = "nixpkgs";
    meta.addRegistry = true;
  };
  # Patches are auto-included from ./patches/home-manager/*.patch.
  patchedInputs.home-manager = { };
  den.default.os.home-manager = {
    useGlobalPkgs = true;
    extraSpecialArgs.inputs = inputs;
  };
  den.schema.home =
    {
      name,
      config,
      ...
    }:
    let
      parts = builtins.split "@" name;
      nameWithHost = builtins.length parts > 1;
      userName = lib.head parts;
      hostName = if nameWithHost then lib.last parts else null;
      hostByName = if hostName != null then den.hosts.${config.system}.${hostName} or null else null;

      # A home named `user@host` carries a host identity even when that host
      # isn't declared in `den.hosts`. Synthesize a minimal `{ name = ...; }`
      # so host-keyed provides/policies (which match on `host.name`) resolve
      # for an otherwise-standalone home — without instantiating a real host
      # entity, which would pull in its platform builder (e.g. nix-darwin).
      # A declared host always wins and remains the only thing that wires
      # `osConfig`.
      #
      # Only `host` is synthesized, never `user`: a synthetic host alone fires
      # `{ host }`-keyed policies (gated on `host ? class` so OS-class routing
      # stays inert for a classless synthetic host), while `{ host, user }`-keyed
      # OS batteries (define-user, user-to-host, …) keep their existing
      # null-user gating and fall back to their home-scope path.

      homeManagerConfiguration =
        if nameWithHost && hostByName != null then
          { pkgs, modules }:
          inputs.home-manager.lib.homeManagerConfiguration {
            inherit pkgs modules;
            extraSpecialArgs.osConfig = lib.attrByPath (
              [ "flake" ] ++ hostByName.intoAttr ++ [ "config" ]
            ) null top.config;
            extraSpecialArgs.inputs = inputs;
          }
        else
          {
            pkgs,
            modules,
            ...
          }:
          inputs.home-manager.lib.homeManagerConfiguration {
            inherit pkgs modules;
            extraSpecialArgs.inputs = inputs;
          };
    in
    {
      config.instantiate = homeManagerConfiguration;
    };
  # Route hmStandalone-class content into a standalone home's homeManager (e.g.
  # `nix.package`, standalone-only settings). Gated on `{ home }` — every den.home is
  # a standalone homeManager instance. A SIMPLE route (no sourceAspect), not
  # den.batteries.forward: the complex forward's spawn fallback re-spawns the source at
  # the home's own scope, and a top-level home has no parent host, so spawnRoot ==
  # parent == self (spawnNode collapses to zero fleet peers). The home's hmStandalone
  # content lives in its own scope, so a same-scope route collects it with no spawn.
  den.policies.home-standalone-route =
    { home, ... }:
    [
      (den.lib.policy.route {
        fromClass = "hmStandalone";
        intoClass = "homeManager";
        path = [ ];
      })
    ];
  den.default.includes = [
    hmPlatforms
    #  hmAlias
    den.policies.home-standalone-route
  ];
}
