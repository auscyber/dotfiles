{

  description = "AusCyber nix flake config";
  inputs = {
    nixpkgs-nvmd.url = "github:nvmd/nixpkgs/modules-with-keys-25.05";
    nixos-images = {
      # url = "github:nix-community/nixos-images";
      url = "github:nvmd/nixos-images/sdimage-installer";
      # url = "git+file:../nixos-images?shallow=1";
      inputs.nixos-stable.follows = "nixpkgs-nvmd";
      inputs.nixos-unstable.follows = "nixpkgs-nvmd";
    };
    impermanence.url = "github:nix-community/impermanence";
    nixcord = {
      url = "github:kaylorben/nixcord";
    };
    qanata.url = "github:veyxov/qanata";
    kanata = {
      url = "github:jtroo/kanata";
      flake = false;
    };
    nixos-raspberrypi.url = "github:nvmd/nixos-raspberrypi/main";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-compat = {
      url = "https://git.lix.systems/lix-project/flake-compat/archive/main.tar.gz";
      # Optional:
      flake = false;
    };
    devenv.url = "github:cachix/devenv";
    cursor_shader = {
      url = "github:Crackerfracks/synesthaxia.glsl";
      flake = false;
    };
    kmonad = {
      url = "git+https://github.com/kmonad/kmonad?submodules=1&dir=nix";
    };
    lix = {
      url = "https://git.lix.systems/lix-project/lix/archive/main.tar.gz";
      flake = false;
    };
    yabai = {
      url = "github:koekeishiya/yabai?ref=pull/2644/head";
      flake = false;
    };
    jankyborders = {
      url = "github:FelixKratz/JankyBorders";
      flake = false;
    };

    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/main.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.lix.follows = "lix";
    };
    ghostty-shaders = {
      url = "github:hackr-sh/ghostty-shaders";
      flake = false;
    };
    nix-colors.url = "github:misterio77/nix-colors";
    #Non flakes
    nixos-conf-editor.url = "github:snowfallorg/nixos-conf-editor";
    hyprpanel.url = "github:jas-singhfsu/hyprpanel";
    hyprpanel.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs-master = {
      url = "github:nixos/nixpkgs/master";
    };
    hyprland = {
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1";

    };
    nix-homebrew.url = "github:zhaofengli/nix-homebrew";

    # Optional: Declarative tap management
    homebrew-core = {
      url = "github:homebrew/homebrew-core";
      flake = false;
    };
    homebrew-cask = {
      url = "github:homebrew/homebrew-cask";
      flake = false;
    };
    zen-browser.url = "github:0xc000022070/zen-browser-flake";
    zen-browser.inputs.nixpkgs.follows = "nixpkgs";
    zen-browser.inputs.home-manager.follows = "home-manager";
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };
    picom = {
      url = "github:ibhagwan/picom";
      flake = false;
    };
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:nix-community/stylix";
    stylix.inputs.nixpkgs.follows = "nixpkgs";
    wezterm = {
      url = "git+https://github.com/wezterm/wezterm?submodules=1";
      flake = false;
    };
    opnix.url = "github:brizzbuzz/opnix";
    #    nh.url = "github:nix-community/nh";
    #    nh.inputs.nixpkgs.follows = "nixpkgs";
    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    my-nur = {
      url = "github:auscyber/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    input-leap = {
      url = "git+https://github.com/input-leap/input-leap?submodules=1";
      flake = false;
    };
    #flakes
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    darwin.url = "github:auscyber/nix-darwin/karabiner";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    eww.url = "github:elkowar/eww";
    _1password-shell-plugins.url = "github:1Password/shell-plugins";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
    lanzaboote = {
      url = "github:nix-community/lanzaboote/v0.4.2";
      inputs.nixpkgs.follows = "nixpkgs";

    };
    french-accents = {
      url = "github:ottopiramuthu/espanso-package-example";
      flake = false;
    };
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    idris2-pkgs.url = "github:claymager/idris2-pkgs";
    local-nixpkgs.url = "github:auscyberman/nixpkgs";
    nixos-wsl.url = "github:nix-community/NixOS-WSL/main";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    arion.url = "github:hercules-ci/arion";

    #idris2-pkgs.url = " github:claymager/idris2-pkgs ";
    idris2.url = "github:idris-lang/Idris2";
    rnix.url = "github:nix-community/rnix-lsp";
    neovim.url = "github:nix-community/neovim-nightly-overlay";
    neovim.inputs.nixpkgs.follows = "nixpkgs";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    nixos-mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
    emacs.url = "github:/nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    #nixpkgs
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    nixpkgs.follows = "unstable";

  };
  outputs =
    inputs@{
      self,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } (
      top@{
        config,
        withSystem,
        moduleWithSystem,
        ...
      }:
      {
        imports = [
          # Optional: use external flake logic, e.g.
          # inputs.foo.flakeModules.default
          ./flake
        ];
        flake = {
          # Put your original flake attributes here.
        };
        systems = [
          # systems for which you want to build the `perSystem` attributes
          "aarch64-darwin"
          "x86_64-linux"
          # ...
        ];
        perSystem =
          { config, pkgs, ... }:
          {
            # Recommended: move all package definitions here.
            # e.g. (assuming you have a nixpkgs input)
            # packages.foo = pkgs.callPackage ./foo/package.nix { };
            # packages.bar = pkgs.callPackage ./bar/package.nix {
            #   foo = config.packages.foo;
            # };
          };
      }
    );
  nixConfig = {
    extra-substituters = [
      "https://nixos-raspberrypi.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
    ];
  };
}
