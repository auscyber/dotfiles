conf@{ config, pkgs, system, lib, ... }: 


let     impConf = fil: import fil conf;
         myHaskellPackages = import ./haskell.nix { inherit pkgs; };
in   
rec {
   imports = [./vim.nix ./alacritty.nix ./rofi.nix  ./emacs.nix ./picom.nix ];
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
    }))
  ];
  programs = {
    vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [ vim-airline vim-addon-nix ];
    settings = { ignorecase = true; };
    extraConfig = ''
      set mouse=a
    '';
  };
    command-not-found.enable = true;
    home-manager.enable = true;
  };
  services.dunst = {
    enable = false;
  };

  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
  #Development
#  st
  ((pkgs.gradleGen.override {
    java = jdk8;
  }).gradle_latest)
  firefox tmux rust-analyzer     wineWowPackages.stable   emacs kotlin
  pcmanfm fzf vscode openjdk8 xorg.xmodmap xorg.xmessage
  multimc skypeforlinux
  arandr ccls libreoffice steam
  jetbrains.idea-ultimate  libnotify stack
  xclip ripgrep discord
  polybarFull  nodejs git playerctl htop eclipses.eclipse-java
  fish feh maim teams
  lua  (spotify.overrideAttrs (attrs : { nativeInputs = [ gnutls];} ))
  unzip scala
  starship ardour slack
  luaPackages.lua-lsp 
  ] ++ (with myHaskellPackages; [stylish-haskell agda-stdlib Agda haskell-language-server taffybar])
    ++ ([(myHaskellPackages.ghcWithPackages (pk: with pk; [discord-haskell microlens-th microlens taffybar dbus xmonad-contrib cabal-install]))])
    ++ (with nodePackages; [yarn typescript-language-server typescript purescript-language-server p3x-onenote])
    ++ (with ocamlPackages; [utop dune ocaml opam merlin]);
  home.username = "auscyber";
  home.homeDirectory ="/home/auscyber";
  home.sessionVariables.EDITOR = "nvim";
  home.stateVersion = "21.03";
}
