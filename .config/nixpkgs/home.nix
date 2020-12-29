conf@{ config, pkgs, system, lib, ... }:

let impConf = fil: import fil conf;
    haskellPacks = with pkgs.haskellPackages; [  haskell-language-server ];
    neovim = impConf ./vim.nix;
    zsh = impConf ./zsh.nix ;
    alacritty = impConf ./alacritty.nix ;
    picom = impConf ./picom.nix ;
    rofi = impConf ./rofi.nix ;
in   
{
    
  programs = {
     vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [ vim-airline vim-addon-nix ];
    settings = { ignorecase = true; };
    extraConfig = ''
      set mouse=a
    '';
  };
    inherit rofi zsh neovim alacritty;
    home-manager.enable = true;
  };
  services = {
    inherit picom;
  };
  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
  #Development
  jetbrains.idea-ultimate jdk
  xclip
  polybarFull  git nodejs  playerctl htop   
  #rice
  fish feh starship maim discord spotify 
  



  ] ++ haskellPacks;


  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "auscyber";
  home.homeDirectory = "/home/auscyber";
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
