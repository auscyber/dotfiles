{
  inputs,
  ...
}:
{
  imports = [
    ./configuration.nix
    #   ./minecraft.nix
    ./hardware-configuration.nix
    #    ./../../modules/system/grub.nix
    #    ./mailserver.nix
    #    ./minecraft.nix
#    inputs.nixos-mailserver.nixosModule

  ];

}
