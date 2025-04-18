{
  description = "AusCyber nix flake config";
  inputs = {
    #Non flakes
    picom = {
      url = "github:ibhagwan/picom";
      flake = false;
    };
    wezterm = {
      url = "github:wezterm/wezterm?submodules=1";
      flake = false;
    };
    #flakes
    agenix.url = "github:ryantm/agenix";
    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "unstable";
    eww.url = "github:elkowar/eww";
    _1password-shell-plugins.url = "github:1Password/shell-plugins";
    rust-overlay.url = "github:oxalica/rust-overlay";
    lanzaboote = {
      url = "github:nix-community/lanzaboote/v0.4.2";
      inputs.nixpkgs.follows = "nixpkgs";

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
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    nixos-mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
    emacs.url = "github:/nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    #nixpkgs
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    nixpkgs.follows = "unstable";

  };
  outputs =
    inputs@{ self
    , flake-utils
    , nixpkgs
    , home-manager
    , neovim
    , picom
    , rnix
    , idris2
    , rust-overlay
    , eww
    , nixos-mailserver
    , agenix
    , nix-doom-emacs
    , darwin
    , idris2-pkgs
    , nixos-wsl
    , arion
    , ...
    }:
      with nixpkgs.lib;
      let
        config = {
          allowBroken = true;
          allowUnfree = true;
        };
        filterNixFiles = k: v: v == "
      regular
      " && hasSuffix ".nix " k;
        importNixFiles =
          path:
          (lists.forEach (
            mapAttrsToList (name: _: path + ("/" + name)) (filterAttrs filterNixFiles (builtins.readDir path))
          ))
            import;
        overlays = [
          inputs.emacs.overlays.default
          rust-overlay.overlays.default
          (
            final: prev:
              let
                system = final.stdenv.hostPlatform.system;
              in
              {

                eww = eww.packages.${system}.eww;
                rnix-lsp = rnix.packages."${system}".rnix-lsp;
                picom = (
                  prev.picom.overrideAttrs (attrs: {
                    src = picom;
                  })
                );
                #            idris2 = idris2.packages."${system}".idris2;
                #            wezterm = (masterp {inherit system;}).wezterm;
                #              discord = (import master { inherit system config; }).discord;
                #wezterm = prev.wezterm.overrideAttrs (attrs: rec {
                #  src = inputs.wezterm;
                #  cargoDeps = attrs.cargoDeps.overrideAttrs (cattrs: {
                #    inherit src;
                #    outputHash =
                #      "sha256-iNv9JEu1aQBxhwlugrl2GdoSvF9cYgM6TXBqamrPjFo=";
                #  });
                #});

                idris2 = final.idris2Pkgs.idris2;
                idris2Pkgs = idris2-pkgs.packages."${system}";
                #            minecraft-server = (import master { inherit system config; }).minecraft-server;
              }
          )
          neovim.overlays.default
        ];

        #    ++ (importNixFiles ./overlays);

      in
      (
        {
          darwinConfigurations = {
            "Chriss-Mac-mini" = import ./systems/macmini {
              modules = [ ./modules/common.nix ];
              home-manager-modules = [
                inputs._1password-shell-plugins.hmModules.default
                ./hm/modules/zsh.nix
                ./hm/modules/neovim.nix
                ./hm/.
                ./hm/mac.nix
                ./hm/modules/1password.nix
              ];
              inherit nixpkgs config overlays inputs darwin home-manager;

            };

            "Ivys-MacBook-Pro" = import ./systems/macbook {
              modules = [ ./modules/common.nix ];
              home-manager-modules = [
                inputs._1password-shell-plugins.hmModules.default
                ./hm/modules/zsh.nix
                ./hm/modules/neovim.nix
                ./hm/.
                ./hm/modules/zotero.nix
                ./hm/mac.nix
                ./hm/modules/1password.nix
              ];
              inherit
                nixpkgs
                config
                overlays
                inputs
                darwin
                home-manager
                ;
            };
          };
        }
        // (flake-utils.lib.eachDefaultSystem (
          system:
          let
            pkgs = import nixpkgs {
              inherit system overlays;
            };
          in
          {
            apps.nvim = flake-utils.lib.mkApp {
              name = "nvim";
              drv = pkgs.neovim;
            };
          }
        ))
        // {
          nixosConfigurations = {
            wsl-nixos = import ./systems/wsl-nixos {
              modules = [
                nixos-wsl.nixosModules.default
                home-manager.nixosModules.home-manager
                ./modules/common.nix
              ];
              home-manager-modules = [
                ./hm/.
                ./hm/modules/neovim.nix
                ./hm/modules/zsh.nix
              ];
              inherit
                nixpkgs
                config
                overlays
                inputs
                home-manager
                ;

            };
            auspc = import ./systems/auspc {
              modules = [ ./modules/common.nix inputs.lanzaboote.nixosModules.lanzaboote ];
              home-manager-modules = [
                ./hm/.
                ./hm/modules/neovim.nix
                ./hm/ui.nix
                ./hm/modules/desktop.nix
                ./hm/modules/zsh.nix
              ];
              inherit
                nixpkgs
                config
                overlays
                inputs
                agenix
                home-manager
                ;
            };
            surfacelaptop = import ./systems/surfacelaptop {
              home-manager-modules = [
                ./hm/.
                ./hm/modules/neovim.nix
                ./hm/ui.nix
                ./hm/laptop.nix
              ];
              modules = [
                inputs._1password-shell-plugins.hmModules.default
                inputs.nixos-hardware.nixosModules.microsoft-surface-laptop-amd
                home-manager.nixosModules.home-manager
                ./modules/1password.nix
                ./modules/common.nix
              ];
              inherit
                nixpkgs
                config
                overlays
                inputs
                agenix
                ;
            };

            secondpc = import ./systems/secondpc {
              modules = [ ./modules/common.nix arion.nixosModules.arion ];
              home-manager-modules = [
                #./hm/arch.nix
                #              ./hm/modules/agda.nix
                #                  ./hm/modules/emacs.nix
                ./hm/modules/neovim.nix
                ./hm/modules/zsh.nix
                #              ./hm/modules/kakoune.nix
                #              ./hm/modules/idris2.nix
                ./hm/.
              ];
              inherit
                nixpkgs
                config
                overlays
                inputs
                nixos-mailserver
                home-manager
                ;
            };

          };
          homeConfigurations = {
            arch =
              let
                pkgs = import nixpkgs {
                  config.allowUnfree = true;
                  system = "x86_64-linux";
                  inherit overlays;
                };
              in
              home-manager.lib.homeManagerConfiguration {
                inherit pkgs;
                modules = [
                  ./hm/arch.nix
                  #   ./hm/modules/agda.nix
                  #                  ./hm/modules/emacs.nix
                  ./hm/modules/neovim.nix
                  #   ./hm/modules/kakoune.nix
                  #  ./hm/modules/idris2.nix
                  ./hm/.
                  nix-doom-emacs.hmModule
                  {
                    home.username = "auscyber";
                    home.homeDirectory = "/home/auscyber";
                  }
                ];
              };
          };

        }
      );
}

