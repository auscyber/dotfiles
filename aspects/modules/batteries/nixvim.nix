
{
  den,
  lib,
  inputs,
  ...
}:
{
  # Declare the input next to the module that uses it (dendritic convention).
  # After adding this file, run `nix run .#write-flake` to regenerate flake.nix.
  flake-file.inputs.nixvim = {
    url = "github:nix-community/nixvim";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  # Register the class so `den.aspects.<x>.nvim = { ... }` is understood.
  den.classes.nvim.description = "Portable nixvim configuration (standalone or home-manager)";

  # ---------------------------------------------------------------------------
  # Standalone builder
  # ---------------------------------------------------------------------------

  # Resolve an aspect's `nvim` class into a single nixvim (root-path) module.
  den.lib.nixvim.module =
    nvimAspect: ctx:
    let
      nvimClass =
        { class, aspect-chain }:
        den.batteries.forward {
          each = lib.singleton true;
          fromClass = _: "nvim";
          intoClass = _: "nixvim";
          intoPath = _: [ ]; # nixvim options live at the module root
          fromAspect = _: lib.head aspect-chain;
          adaptArgs = lib.id;
        };

      aspect = den.lib.parametric.fixedTo ctx {
        includes = [
          nvimClass
          nvimAspect
        ];
      };
    in
    den.lib.aspects.resolve "nixvim" aspect;

  # Build a standalone neovim package from an aspect's `nvim` class.
  den.lib.nixvim.package =
    pkgs: nvimAspect: ctx:
    inputs.nixvim.legacyPackages.${pkgs.stdenv.hostPlatform.system}.makeNixvimWithModule {
      inherit pkgs;
      module = den.lib.nixvim.module nvimAspect ctx;
    };

  # ---------------------------------------------------------------------------
  # Home Manager bridge — default-included in the user schema
  # ---------------------------------------------------------------------------
  #
  # Fires once per user scope (same hook the home-manager/maid/hjem batteries
  # use). For every user carrying the `homeManager` class it:
  #   - imports nixvim's home-manager module into the user's homeManager class
  #     (keyed, so it imports once), enabling programs.nixvim by default; and
  #   - routes the user's `nvim` class content into programs.nixvim.
  #
  # den's home-manager battery then forwards homeManager into
  # home-manager.users.<name>. No-op for non-home-manager users.
  #
  # NOTE: `enable` is `lib.mkDefault true`, so adding `nvim` content to a user
  # Just Works. Consequences / knobs:
  #   - A home-manager user with NO `nvim` content still gets a vanilla nixvim.
  #     To make this plumbing-only (transfer config but never auto-enable),
  #     delete the `programs.nixvim.enable = lib.mkDefault true;` line below.
  #   - Per-user override: home-manager.users.<name>.programs.nixvim.enable = false;
  den.schema.user.includes =
    let
      isHMUser =
        ctx:
        (ctx.__entityKind or null) == "user"
        && builtins.elem "homeManager" ((ctx.user or { }).classes or [ ]);

      provideNixvimModule =
        ctx:
        if isHMUser ctx then
          den.lib.policy.provide {
            class = "homeManager";
            module = {
              key = "den:nixvim-hm-module";
              imports = [ inputs.nixvim.homeModules.nixvim ];
              programs.nixvim.enable = lib.mkDefault true;
            };
          }
        else
          { };

      routeNvimIntoNixvim =
        ctx:
        if isHMUser ctx then
          den.lib.policy.route {
            fromClass = "nvim";
            intoClass = "homeManager";
            intoPath = [
              "programs"
              "nixvim"
            ];
          }
        else
          { };
    in
    [
      provideNixvimModule
      routeNvimIntoNixvim
    ];
}
