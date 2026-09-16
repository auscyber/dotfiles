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
  den.aspects.nvim = {
    layers = [ "dev" ];

    inputs.nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };
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

  # OPT-IN, not fleet-wide.
  #
  # This was a policy in `den.default.includes`, so EVERY homeManager user got
  # the nixvim module plus `enable = mkDefault true`. Measured with
  # `--trace-function-calls`: nixvim's lazyload runs 458 `evalModules` and
  # nixpkgs' neovim plugin submodule another 249, on every host, whether or not
  # anything there wanted an editor.
  #
  # It bought nothing even for the users who did want it: all seven hosts using
  # neovim already include `den.aspects.neovim`, whose own `homeManager` sets
  # `programs.nixvim.enable = true` outright. So the module import moves onto
  # `den.aspects.nixvim` -- which `den.aspects.neovim` includes -- and arrives
  # exactly where the class is declared.
  #
  # Not `den.lib.whenAspect`: that guard takes aspect-shaped content and rejects
  # policies outright ("policies are dispatched before guards run and would
  # never see `hasAspect`"), so gating meant moving the content out of the
  # policy rather than wrapping it.
  den.aspects.nixvim.homeManager.imports = [ inputs.nixvim.homeModules.nixvim ];

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
    {
      host,
      user,
      ...
    }:
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
    # The aspect that DECLARES `inputs.nixvim`. Without it the aspect is
    # included by nothing, so the input reaches no layer and no root entry and
    # `inputs.nixvim` is simply absent. Declaration only -- enabling nixvim is
    # `den.aspects.neovim`'s job, per host.
    den.aspects.nvim
  ];

  # The forwards fire UNCONDITIONALLY -- `nvimForward` uses
  # `each = lib.singleton true`, so every entity they reach gets a
  # `programs.nixvim` definition whether or not it has any nvim content. At
  # default scope that is every user and every home, while the module itself is
  # imported by `den.aspects.nixvim` alone: entities without the aspect end up
  # with definitions and no option tree ("the option `programs.nixvim' does not
  # exist" -- measured on pentestvm/admin and wsl-nixos/nixos, via searchix's
  # option-docs rebuild). Scope them to the aspect that supplies the module, so
  # definitions and declarations always travel together.
  den.aspects.nixvim.includes = [
    den.policies.nixvim-include-global-pkgs
    den.policies.nixvim-user-forward
    den.policies.nixvim-home-forward
  ];
}
