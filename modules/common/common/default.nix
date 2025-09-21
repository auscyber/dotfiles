{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

{

  imports = [ ];
  fonts.packages =
    with pkgs;
    (map (x: nerd-fonts.${x}) [
      "fira-code"
      "inconsolata"
      "hasklug"
      "roboto-mono"
    ]);

  environment.systemPackages = with pkgs; [
    home-manager
    git
    vim
    neovim
    wget
    htop
    pinentry
    curl
  ];

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.zsh.enable = true;

}
