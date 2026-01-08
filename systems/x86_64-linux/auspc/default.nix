{
  nixpkgs,

  ...
}:
{
auscybernix.meta.description = "Main Gaming desktop";
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
