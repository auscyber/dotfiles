# External modules sourced from flake inputs.
# Registering them here means the system builders can pick them up via
# `builtins.attrValues config.flake.modules.*` alongside local modules.
{ inputs, ... }:
{
  flake.modules = {
    # ── Home-Manager external modules ──────────────────────────────────────
    homeManager = {
      ext-1password-shell-plugins = inputs._1password-shell-plugins.hmModules.default;
      ext-zen-browser             = inputs.zen-browser.homeModules.default;
      ext-nixvim                  = inputs.nixvim.homeModules.default;
      ext-nix-index               = inputs.nix-index-database.homeModules.nix-index;
      ext-sops                    = inputs.sops-nix.homeManagerModules.sops;
      ext-agenix                  = inputs.agenix.homeManagerModules.default;
      ext-agenix-rekey            = inputs.agenix-rekey.homeManagerModules.default;
      ext-vscode-server           = inputs.vscode-server.homeModules.default;
      ext-niri                    = inputs.niri.homeModules.niri;
    };

    # ── NixOS external modules ─────────────────────────────────────────────
    nixos = {
      ext-stylix       = inputs.stylix.nixosModules.stylix;
      ext-arion        = inputs.arion.nixosModules.arion;
      ext-lanzaboote   = inputs.lanzaboote.nixosModules.lanzaboote;
      ext-impermanence = inputs.impermanence.nixosModules.impermanence;
      ext-home-manager = inputs.home-manager.nixosModules.home-manager;
      ext-nixos-wsl    = inputs.nixos-wsl.nixosModules.default;
      ext-agenix       = inputs.agenix.nixosModules.default;
      ext-agenix-rekey = inputs.agenix-rekey.nixosModules.default;
      ext-sops         = inputs.sops-nix.nixosModules.sops;
      ext-attic        = inputs.attic.nixosModules.atticd;
    };

    # ── nix-darwin external modules ────────────────────────────────────────
    darwin = {
      ext-stylix       = inputs.stylix.darwinModules.stylix;
      ext-nix-homebrew = inputs.nix-homebrew.darwinModules.nix-homebrew;
      ext-home-manager = inputs.home-manager.darwinModules.home-manager;
      ext-sops         = inputs.sops-nix.darwinModules.sops;
      ext-agenix       = inputs.agenix.darwinModules.default;
      ext-agenix-rekey = inputs.agenix-rekey.nixosModules.default;
    };
  };
}
