{
  den,
  lib,
  inputs,
  ...
}:
let
  # Forward nvim class content into homeManager at programs.nixvim.
  #
  # `sourceAspect` must be an already-resolved entity aspect (as produced by
  # `den.lib.resolveEntity`). Policies do not receive `aspect-chain` — that is
  # an aspect-parametric arg — so the source has to be resolved explicitly,
  # the same way den's own `home-env` battery does it.
  nvimForward =
    sourceAspect:
    den.provides.forward {
      each = lib.singleton true;
      fromClass = _: "nvim";
      intoClass = _: "homeManager";
      intoPath = _: [
        "programs"
        "nixvim"
      ];
      fromAspect = _: sourceAspect;
    };
in
{
  ff.nixvim = {
    url = "github:nix-community/nixvim";
  };

  # Register the nvim class
  den.classes.nvim.description = "Portable nixvim configuration (standalone or home-manager)";

  # Register nixvim as a class that nixvim modules target
  den.classes.nixvim.description = "Nixvim module evaluation context";

  # ---------------------------------------------------------------------------
  # Schema: nixvim entity
  # ---------------------------------------------------------------------------

  # Set collision policy to allow aspect args to win over module-system args
  den.schema.nvim.collisionPolicy = "aspect-wins";
  den.schema.nixvim.collisionPolicy = "aspect-wins";

  # ---------------------------------------------------------------------------
  # Standalone builder
  # ---------------------------------------------------------------------------

  # Resolve nvim class content using resolveImports (skips entity instantiation)
  # Don't inject pkgs here - let nixvim's makeNixvimWithModule provide it
  den.lib.nixvim.module = nvimAspect: ctx: den.lib.aspects.resolve "nvim" nvimAspect;

  # Build a standalone neovim package
  den.lib.nixvim.package =
    pkgs: nvimAspect: ctx:
    let
      system = pkgs.stdenv.hostPlatform.system;
      resolved = den.lib.nixvim.module nvimAspect ctx;
    in
    (inputs.nixvim.lib.evalNixvim {
      inherit system;
      modules = [
        resolved
        { nixpkgs.pkgs = pkgs; }
      ];
    }).config.build.package;

  # Flexible package builder for perSystem
  den.lib.nixvim.mkPackage =
    {
      pkgs,
      aspect ? den.aspects.nixvim,
      ctx ? { },
    }:
    den.lib.nixvim.package pkgs aspect ctx;

  # ---------------------------------------------------------------------------
  # Policies
  # ---------------------------------------------------------------------------

  # Provide nixvim HM module to users with homeManager class
  den.policies.nixvim-hm-module =
    {
      host,
      ...
    }:
    (den.lib.policy.provide {
      class = "homeManager";
      module = {
        key = "den:nixvim-hm-module";
        imports = [ inputs.nixvim.homeModules.nixvim ];
        programs.nixvim.enable = lib.mkDefault true;
      };
    });

  den.policies.nixvim-include-global-pkgs =
    ctx:
    den.lib.policy.provide {
      class = "nvim";
      module = {
        key = "den:nixvim";

        nixpkgs.useGlobalPackages = true;
      };
    };

  # User-scope policy: forward nvim content into homeManager
  den.policies.nixvim-user-forward =
    { host, user, ... }:
    den.lib.policy.include (nvimForward (den.lib.resolveEntity "user" { inherit host user; }));

  # Home-scope policy: same, for standalone `den.homes` entities, which have no
  # owning host and so are never reached by the user-scope policy above.
  den.policies.nixvim-home-forward =
    { home, ... }:
    den.lib.policy.include (nvimForward (den.lib.resolveEntity "home" { inherit home; }));

  # ---------------------------------------------------------------------------
  # Schema includes
  # ---------------------------------------------------------------------------

  #  den.aspects.nixvim.includes = [ den.policies.nixvim-hm-module den.policies.nixvim-user-forward ];

  den.default.includes = [
    den.policies.nixvim-hm-module
    den.policies.nixvim-include-global-pkgs
    den.policies.nixvim-user-forward
    den.policies.nixvim-home-forward
  ];
}
