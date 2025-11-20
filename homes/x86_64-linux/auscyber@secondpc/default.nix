{
  config,
  pkgs,
  lib,
  ...
}:
{
  programs.home-manager.enable = true;

  programs.gpg.enable = true;
  sops.age.sshKeyPaths = [ "/home/auscyber/.ssh/id_ed25519" ];
  auscybernix = {
    programs.neovim.enable = true;
    shell = {
      enable = true;
      fish = {
        enable = true;
      };
    };
  };
  #  programs.neovim.enable = true;
  home.stateVersion = "25.05";
}
