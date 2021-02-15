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
 
  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
  #Development
#  st
  ((pkgs.gradleGen.override {
    java = jdk8;
  }).gradle_latest)
  firefox tmux rust-analyzer     wineWowPackages.stable   emacs kotlin
  pcmanfm fzf vscode openjdk8 xorg.xmodmap
  multimc skypeforlinux
  arandr ccls libreoffice
  jetbrains.idea-ultimate  libnotify
  xclip ripgrep discord
  polybarFull  nodejs git playerctl htop eclipses.eclipse-java
  fish feh maim teams
  spotify lua
  unzip
  starship ardour slack
  luaPackages.lua-lsp 
  ] ++ (with myHaskellPackages; [stylish-haskell agda-stdlib Agda haskell-language-server])
    ++ ([(myHaskellPackages.ghcWithPackages (pk: with pk; [discord-haskell microlens-th microlens dbus xmonad-contrib cabal-install]))])
    ++ (with nodePackages; [yarn typescript-language-server typescript purescript-language-server p3x-onenote])
    ++ (with ocamlPackages; [utop dune ocaml opam merlin]);
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "auscyber";
  home.homeDirectory ="/home/auscyber";
  home.sessionVariables.EDITOR = "nvim";
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
