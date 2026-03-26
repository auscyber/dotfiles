{
  nixpkgs,

  ...
}:
{
  auscybernix.meta.description = "Main Gaming desktop";

  # Feature modules registered in flake.modules.nixos / flake.modules.generic
  # are all enabled by default.  Set a name to false to exclude it from this
  # system's evaluation.  Available names: common-allConfigs, common-common,
  # common-hm, common-nix, common-ssh-keys, common-vpn, general,
  # nix-builds-options, nix-builds-platform, secrets-options, secrets-platform,
  # system-1password, ssh-service, nixos-bootlogo, nixos-games.
  #
  # auscybernix.modules.enable = {
  #   "nixos-bootlogo" = false;  # skip boot logo on this host
  # };

  imports = [
    ./configuration.nix
    ./hardware-configuration.nix
    ./graphics.nix
    #./../../modules/system/grub.nix
    #./boot.nix
    #    ./mailserver.nix
    #    ./minecraft.nix
  ];

}
