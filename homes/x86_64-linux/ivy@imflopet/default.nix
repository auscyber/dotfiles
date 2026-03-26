{ config, pkgs, ... }:
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
  auscybernix.standalone.enable = true;

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

    safe.directory = [
      "/nixos-config"
      "/nixos-config/services/loft"
    ];

  };
  programs.gpg.settings = {
    "use-agent" = "";

  };
  home.sessionVariables = {
    NH_OS_FLAKE = "/nixos-config";
  };
  home.packages = with pkgs; [ gnupg ];
  home.stateVersion = "24.05";

}
