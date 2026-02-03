{
  config,
  pkgs,
  lib,
  ...
}:
{
  age.rekey.hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPhUgRIXS75ItfXrTu1zi9cCgBgWZQRL0u0At374LkJe";
  programs.home-manager.enable = true;

  programs.gpg.enable = true;
  #sops.age.sshKeyPaths = [ "/home/auscyber/.ssh/id_ed25519" ];
  auscybernix = {
    programs.neovim.enable = true;
    shell = {
      enable = true;
      fish = {
        enable = true;
      };
    };
  };
  home.packages = with pkgs; [ pinentry-curses];
  #  programs.neovim.enable = true;
  home.stateVersion = "25.05";
}
