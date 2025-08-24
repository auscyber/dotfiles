{
  nixpkgs,

  ...
}:
{
  imports = [
    ./configuration.nix
    ./hardware-configuration.nix
    ./games.nix
    ./graphics.nix
    #./../../modules/system/grub.nix
    #./boot.nix
    #    ./mailserver.nix
    #    ./minecraft.nix
  ];

}
