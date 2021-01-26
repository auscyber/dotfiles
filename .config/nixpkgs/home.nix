conf@{ config, pkgs, system, lib, ... }: 


let      pkgs = import <nixpkgs> {
          config = config.nixpkgs.config; 
          overlays = [   (import ./st.nix)  (import ./powercord.nix)  ];
          packageOverrides = pkgs: {
              openssl = pkgs.libressl; 
          };

          };
    impConf = fil: import fil conf;
in   
rec {
   imports = [ ./alacritty.nix ./rofi.nix ./vim.nix ./emacs.nix ./picom.nix ];
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
  firefox tmux rust-analyzer     wineWowPackages.stable carnix  gitAndTools.gh
  pcmanfm   fzf vimHugeX jdk jre
   multimc razergenie  lutris
  osu-lazer gimp arandr ccls
  jetbrains.idea-ultimate 
  xclip ripgrep discord-canary
  cabal-install cargo 
  polybarFull  git nodejs  playerctl htop   
  fish feh maim 
  spotify libnotify
  opam clojure clojure-lsp
  starship 
  ] ++ (with pkgs.haskellPackages; [haskell-language-server]) 
    ++ ([(pkgs.haskellPackages.ghcWithPackages (pk: with pk; [dbus xmonad-contrib]))])
    ++ (with nodePackages; [typescript-language-server typescript purescript-language-server])
    ++ (with ocamlPackages; [dune ocaml opam]);
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "auscyber";
  home.homeDirectory ="/home/auscyber";
  home.sessionVariables.EDITOR = "vim";
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
