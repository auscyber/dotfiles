{
  config,
  pkgs,
  lib,
  ...
}:
{
  auscybernix.modules.enable = {
    allConfigs        = true;
    nix               = true;
    secrets           = true;
    secrets-platform  = true;
    standalone        = true;
    default           = true;
    file              = true;
    gpg               = true;
    neovim            = true;
    fish              = true;
    shell             = true;
    ext-nixvim        = true;
    ext-sops          = true;
    ext-agenix        = true;
    ext-agenix-rekey  = true;
    ext-nix-index     = true;
  };
  auscybernix.secrets.enable = true;
  auscybernix.standalone.enable = true;

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
