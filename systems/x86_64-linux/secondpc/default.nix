{
  inputs,
  ...
}:
{
  auscybernix.meta.description = "Home server";
  imports = [
    ./configuration.nix
    #   ./minecraft.nix
    ./hardware-configuration.nix
    ./services.nix
    ./grafana.nix
    ./cache.nix
    #	./jitsi.nix
    ./navidrome.nix
    #    ./../../modules/system/grub.nix
    #    ./mailserver.nix
    #    ./minecraft.nix
    #    inputs.nixos-mailserver.nixosModule

  ];

}
