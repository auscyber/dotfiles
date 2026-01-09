{ config, pkgs, ... }:
{
  sops.age.keyFile = "/home/ivy/.config/sops/age/keys.txt";
  age.rekey.hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOwzECwhRtEus12VIOPw8UrOkBuBwH69VKodEWEuXAsX ivy@imflopet";
    auscybernix = {
    nix.flake = "/home/ivy/dotfiles";
    secrets.enable = true;
    shell = {
      enable = true;
      fish = {
        enable = true;
      };
    };
    programs.neovim.enable = true;
  };
  programs.git.settings = {
    safe.directory = "/nixos-config";

  };
  home.packages = with pkgs; [ gnupg ];
  home.stateVersion = "24.05";

}
