conf@{ config, pkgs, system, lib, ... }: 


let      pkgs = import <nixpkgs> {
          config = config.nixpkgs.config; 
          overlays = [   (import ./st.nix)  (import ./powercord.nix)  ];
          packageOverrides = pkgs: {
              openssl = pkgs.libressl; 
          };

          };
    impConf = fil: import fil conf;
    haskellPacks = with pkgs.haskellPackages; [  haskell-language-server ];
    neovim = impConf ./vim.nix;
    zsh = impConf ./zsh.nix ;
    alacritty = impConf ./alacritty.nix ;
    picom = impConf ./picom.nix ;
    rofi = impConf ./rofi.nix ;
    emacs = impConf ./emacs.nix;
in   
rec {
    
  programs = {
    vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [ vim-airline vim-addon-nix ];
    settings = { ignorecase = true; };
    extraConfig = ''
      set mouse=a
    '';
  };
    inherit rofi zsh neovim alacritty emacs;
    home-manager.enable = true;
  };
  services = {
    emacs.enable = true; 
    inherit picom;
  };
  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
  #Development
#  st
  firefox
  pcmanfm (blender.override {
    cudaSupport = true;
  })
  fzf
  dolphin steam multimc 
  osu-lazer gimp
  jetbrains.idea-ultimate jdk
  xclip ripgrep discord-canary
  cabal-install 
  polybarFull  git nodejs  playerctl htop   
  fish feh maim 
    spotify libnotify
  opam clojure clojure-lsp
  
  ] ++ (with pkgs.haskellPackages; [haskell-language-server]) 
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
