{ inputs, ... }:
{
  imports = [
    inputs.flake-parts.flakeModules.modules
  ];

  # Register every local feature module under a named key so system and home
  # configurations can reference them by name via config.flake.modules.* rather
  # than by hard-coded paths.  Subdirectories are excluded from import-tree
  # auto-discovery via the _ prefix; this file is their dendritic entry point.
  flake.modules = {

    # ── NixOS system modules ────────────────────────────────────────────────
    nixos = {
      # shared (common/)
      vpn             = ./_common/vpn.nix;
      builders        = ./_common/builders;
      secrets         = ./_common/secrets.nix;
      nix             = ./_common/nix;
      common          = ./_common/common;
      allConfigs      = ./_common/allConfigs.nix;
      hm              = ./_common/hm;
      ssh-keys        = ./_common/ssh-keys.nix;
      # nixos-specific (_nixos/)
      bootlogo        = ./_nixos/bootlogo;
      nixos-builders  = ./_nixos/builders;
      games           = ./_nixos/games;
      general         = ./_nixos/general;
      nixos-secrets   = ./_nixos/secrets;
      ssh             = ./_nixos/ssh;
    };

    # ── nix-darwin system modules ────────────────────────────────────────────
    darwin = {
      # shared (common/)
      vpn                 = ./_common/vpn.nix;
      builders            = ./_common/builders;
      secrets             = ./_common/secrets.nix;
      nix                 = ./_common/nix;
      common              = ./_common/common;
      allConfigs          = ./_common/allConfigs.nix;
      hm                  = ./_common/hm;
      kmonad              = ./_common/kmonad;
      ssh-keys            = ./_common/ssh-keys.nix;
      # darwin-specific (_darwin/)
      darwin-builders     = ./_darwin/builders;
      finder              = ./_darwin/finder;
      general             = ./_darwin/general;
      hmApps              = ./_darwin/hmApps;
      homebrew            = ./_darwin/homebrew;
      karabiner-driver    = ./_darwin/keybinds/karabiner_driver;
      keys                = ./_darwin/keys;
      network             = ./_darwin/network;
      security-pam        = ./_darwin/security/pam;
      security-secrets    = ./_darwin/security/secrets;
      security-sudoagents = ./_darwin/security/sudoagents;
    };

    # ── Home-Manager modules ────────────────────────────────────────────────
    homeManager = {
      # base (_home/)
      default     = ./_home/default.nix;
      standalone  = ./_home/standalone;
      secrets     = ./_home/secrets;
      file        = ./_home/file;
      gpg         = ./_home/gpg;
      ssh         = ./_home/ssh;
      # shell
      shell       = ./_home/shell;
      fish        = ./_home/shell/fish;
      zsh         = ./_home/shell/zsh;
      # editors
      neovim      = ./_home/editors/neovim;
      zed         = ./_home/editors/zed;
      emacs       = ./_home/emacs.nix;
      kakoune     = ./_home/kakoune.nix;
      idris2      = ./_home/idris2.nix;
      # browsers
      zen-browser = ./_home/browsers/zen;
      helium      = ./_home/browsers/helium;
      # programs
      onepassword = ./_home/programs/1password;
      openclaw    = ./_home/programs/openclaw;
      sketchybar  = ./_home/programs/sketchybar;
      zotero      = ./_home/programs/zotero;
      # terminal
      ghostty     = ./_home/term/ghostty;
      # window managers
      wms         = ./_home/wms;
      hyprland    = ./_home/wms/hyprland;
      niri        = ./_home/wms/niri;
      rift        = ./_home/wms/rift;
      yabai       = ./_home/wms/yabai;
      # keybinds
      kanata      = ./_home/keybinds/kanata;
      skhd        = ./_home/keybinds/skhd;
      # languages
      agda        = ./_home/languages/agda;
      # services
      mopidy      = ./_home/services/mopidy;
      # misc
      picom       = ./_home/picom.nix;
      minecraft   = ./_home/minecraft.nix;
      vencord     = ./_home/vencord.nix;
    };
  };
}
