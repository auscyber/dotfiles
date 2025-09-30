{
  config,
  pkgs,
  lib,
  ...
}:
{
  programs.home-manager.enable = true;

  programs.gpg.enable = true;
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

  auscybernix = {
    shell = {
      enable = true;
      fish = {
        enable = true;
      };
    };
  };
  programs.neovim.enable = true;
  home.stateVersion = "25.05";

}
