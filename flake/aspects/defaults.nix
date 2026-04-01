{
  config,
  inputs,
  # Enable <den/brackets> syntax for den battery references.
  __findFile ? __findFile,
  den,
  ...
}:
{
  # ── NixOS defaults ──────────────────────────────────────────────────────────
  den.default.nixos = {
    imports = [
      inputs.stylix.nixosModules.stylix
      inputs.arion.nixosModules.arion
      inputs.lanzaboote.nixosModules.lanzaboote
      inputs.impermanence.nixosModules.impermanence
      inputs.home-manager.nixosModules.home-manager
      inputs.nixos-wsl.nixosModules.default
      inputs.agenix.nixosModules.default
      inputs.agenix-rekey.nixosModules.default
      inputs.sops-nix.nixosModules.sops
      inputs.attic.nixosModules.atticd
      ../../modules/common/vpn.nix
      ../../modules/common/builders
      ../../modules/common/secrets.nix
      ../../modules/common/nix
      ../../modules/common/common
      ../../modules/common/allConfigs.nix
      ../../modules/common/hm
      ../../modules/common/ssh-keys.nix
      ../../modules/nixos/bootlogo
      ../../modules/nixos/builders
      ../../modules/nixos/games
      ../../modules/nixos/general
      ../../modules/nixos/secrets
      ../../modules/nixos/ssh
    ];
    nixpkgs.config.allowUnfree = true;
    nixpkgs.overlays = builtins.attrValues inputs.self.overlays;
    auscybernix.secrets.enable = true;
  };

  # ── Darwin defaults ─────────────────────────────────────────────────────────
  den.default.darwin = {
    imports = [
      inputs.stylix.darwinModules.stylix
      inputs.nix-homebrew.darwinModules.nix-homebrew
      inputs.home-manager.darwinModules.home-manager
      inputs.sops-nix.darwinModules.sops
      inputs.agenix.darwinModules.default
      inputs.agenix-rekey.nixosModules.default
      ../../modules/common/builders
      ../../modules/common/vpn.nix
      ../../modules/common/nix
      ../../modules/common/secrets.nix
      ../../modules/common/hm
      ../../modules/common/common
      ../../modules/common/allConfigs.nix
      ../../modules/common/kmonad
      ../../modules/common/ssh-keys.nix
      ../../modules/darwin/builders
      ../../modules/darwin/finder
      ../../modules/darwin/general
      ../../modules/darwin/hmApps
      ../../modules/darwin/homebrew
      ../../modules/darwin/keys
      ../../modules/darwin/network
    ];
    nixpkgs.config.allowUnfree = true;
    nixpkgs.overlays = builtins.attrValues inputs.self.overlays;
    auscybernix.secrets.enable = true;
  };

  # ── Home-manager defaults (all managed users on all hosts) ──────────────────
  den.default.homeManager = {
    imports = [
      inputs._1password-shell-plugins.hmModules.default
      inputs.zen-browser.homeModules.default
      inputs.nixvim.homeModules.default
      inputs.nix-index-database.homeModules.nix-index
      inputs.sops-nix.homeManagerModules.sops
      inputs.agenix.homeManagerModules.default
      inputs.agenix-rekey.homeManagerModules.default
      inputs.vscode-server.homeModules.default
      inputs.niri.homeModules.niri
      ../../modules/common/secrets.nix
      ../../modules/common/nix
      ../../modules/common/allConfigs.nix
    ];
    auscybernix.secrets.enable = true;
  };

  # ── Standalone home extras (den.homes.*) ────────────────────────────────────
  den.schema.home.imports = [
    inputs.stylix.homeModules.stylix
  ];

  # ── State-version defaults ──────────────────────────────────────────────────
  den.default = {
    nixos.system.stateVersion = "25.05";
    darwin.system.stateVersion = 6;
    homeManager.home.stateVersion = "25.05";
  };

  # ── Den batteries ───────────────────────────────────────────────────────────
  # mutual-provider: enables ${host}.provides.${user} and ${user}.provides.${host}
  # hostname:        automatically sets networking.hostName
  # define-user:     automatically creates users.users.<username> on each host
  den.default.includes = [
    <den/mutual-provider>
    <den/hostname>
    <den/define-user>
  ];
}
