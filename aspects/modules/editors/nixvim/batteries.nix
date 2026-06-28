{
  den,
  lib,
  inputs,
  ...
}:
let
  # Check if user has homeManager class
  isHMUser =
    ctx:
    (ctx.__entityKind or null) == "user"
    && builtins.elem "homeManager" ((ctx.user or { }).classes or [ ]);

  # Forward nvim class content into homeManager at programs.nixvim
  nvimForward =
    { host, user }:
    den.batteries.forward {
      each = lib.singleton true;
      fromClass = _: "nvim";
      intoClass = _: "homeManager";
      intoPath = _: [
        "programs"
        "nixvim"
      ];
      fromAspect = _: den.lib.resolveEntity "user" { inherit host user; };
    };
in
{
  flake-file.inputs.nixvim = {
    url = "github:nix-community/nixvim";
    inputs.nixpkgs.follows = "nixpkgs";
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
  den.lib.nixvim.module =
    nvimAspect: ctx:
    let
      entityCtx = den.lib.resolveEntity "nvim" ctx;

      aspect = {
        includes = [ nvimAspect ];
        __scopeHandlers = entityCtx.__scopeHandlers or { };
      };
    in
    den.lib.aspects.resolveImports "nvim" aspect;

  # Build a standalone neovim package
  den.lib.nixvim.package =
    pkgs: nvimAspect: ctx:
    let
      system = pkgs.stdenv.hostPlatform.system;
      resolved = den.lib.nixvim.module nvimAspect ctx;
    in
    inputs.nixvim.legacyPackages.${system}.makeNixvimWithModule {
      inherit pkgs;
      module = resolved;
    };

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
    ctx:
    if isHMUser ctx then
      [
        (den.lib.policy.provide {
          class = "homeManager";
          module = {
            key = "den:nixvim-hm-module";
            imports = [ inputs.nixvim.homeModules.nixvim ];
            programs.nixvim.enable = lib.mkDefault true;
          };
        })
      ]
    else
      [ ];

  # User-scope policy: forward nvim content into homeManager
  den.policies.nixvim-user-forward =
    ctx:
    let
      host = ctx.host or null;
      user = ctx.user or null;
    in
    if isHMUser ctx && host != null && user != null then
      [
        (den.lib.policy.include (nvimForward {
          inherit host user;
        }))
      ]
    else
      [ ];

  # ---------------------------------------------------------------------------
  # Schema includes
  # ---------------------------------------------------------------------------

  den.schema.user.includes = [
    den.policies.nixvim-hm-module
    den.policies.nixvim-user-forward
  ];
}
